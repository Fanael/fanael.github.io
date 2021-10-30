// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.pygments;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import greenspun.dom.Node;
import greenspun.dom.Tag;
import greenspun.util.Trace;
import greenspun.util.collection.ImmutableList;
import greenspun.util.condition.Condition;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A connection to the Pygments server.
 * <p>
 * This class is thread-safe: multiple threads can safely request code to be syntax-highlighted at the same time.
 * <p>
 * Internally, this class forms a connection <em>pool</em> for greater concurrency than a serialized connection
 * to a single Pygments server process would allow.
 */
public final class PygmentsServer implements AutoCloseable {
    /**
     * Initializes a new Pygments server connection pool using the given temporary file to run processes from.
     * <p>
     * The temporary file should exist for at least as long as the pool itself, because the pool may decide to spawn
     * a new Pygments server process at any time.
     */
    public PygmentsServer(final @NotNull ServerCodeTemporaryFile file) {
        sourceCodePath = file.path();
    }

    /**
     * Shuts down the Pygments server connection pool, closing all active connections.
     * This operation waits for all processes to exit.
     * <p>
     * This method should only be called when there are no more outstanding requests.
     * <p>
     * If an {@link IOException} is thrown during the shutdown process, or a Pygments server process reports an error
     * during shutdown throwing an {@link ShutdownErrorException}, the exception is caught and suppressed and
     * the process is forcefully destroyed.
     */
    @Override
    public void close() {
        lock.lock();
        try {
            assert freeConnections.size() == activeConnectionCount.get()
                : "Attempted to shut down the connection pool with outstanding busy connections";
            for (final var connection : freeConnections) {
                connection.initiateShutdown();
            }
            // Now that the processes are shutting down concurrently, wait for them to finish.
            for (final var connection : freeConnections) {
                if (!connection.stillAlive) {
                    continue;
                }
                connection.waitForShutdown();
                connection.destroy();
            }
            freeConnections.clear();
            activeConnectionCount.set(0);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Highlights the syntax of the given code using the given language name for determining syntactic rules.
     * <p>
     * If successful, returns an {@link ImmutableList} of DOM {@link Node}s representing the highlighted code.
     * <p>
     * It is safe for multiple threads to call this method on the same {@code PygmentsServer} instance at the same time
     * without external synchronization.
     * <p>
     * If the operation cannot be completed for any reason, a fatal condition is signaled:
     * <ul>
     * <li>{@link IOExceptionCondition} if an I/O error occurs.
     * <li>{@link PygmentsServerErrorCondition} if the Pygments server process reported an error.
     * </ul>
     */
    public @NotNull ImmutableList<@NotNull Node> highlightCode(
        final @NotNull String code,
        final @NotNull String languageName
    ) {
        try (final var trace = new Trace(() -> "Highlighting code in language " + languageName)) {
            trace.use();
            final var connection = acquireConnection();
            try {
                return connection.highlightCode(code, languageName);
            } finally {
                releaseConnection(connection);
            }
        }
    }

    private @NotNull Connection acquireConnection() {
        final var connection = popFreeConnection();
        if (connection != null) {
            assert connection.stillAlive;
            return connection;
        }
        return openNewConnection();
    }

    private void releaseConnection(final @NotNull Connection connection) {
        if (connection.stillAlive) {
            lock.lock();
            try {
                freeConnections.add(connection);
            } finally {
                lock.unlock();
            }
        } else {
            activeConnectionCount.decrementAndGet();
        }
    }

    private @Nullable Connection popFreeConnection() {
        lock.lock();
        try {
            return freeConnections.isEmpty() ? null : freeConnections.remove(freeConnections.size() - 1);
        } finally {
            lock.unlock();
        }
    }

    private @NotNull Connection openNewConnection() {
        try (final var trace = new Trace("Spawning a new pygments server process")) {
            trace.use();
            final var builder = new ProcessBuilder("python3", sourceCodePath.toString());
            builder.redirectErrorStream(true);
            try {
                final var connection = new Connection(builder.start());
                activeConnectionCount.incrementAndGet();
                return connection;
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private final @NotNull Path sourceCodePath;
    private final AtomicInteger activeConnectionCount = new AtomicInteger(0);
    private final ReentrantLock lock = new ReentrantLock();
    private final ArrayList<@NotNull Connection> freeConnections = new ArrayList<>();

    private static final class Connection {
        private Connection(final @NotNull Process process) {
            this.process = process;
            writer = new OutputStreamWriter(process.getOutputStream(), StandardCharsets.UTF_8);
            reader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
        }

        private @NotNull ImmutableList<@NotNull Node> highlightCode(
            final @NotNull String code,
            final @NotNull String languageName
        ) {
            try {
                sendSimpleString(":highlight");
                sendSimpleString(languageName);
                sendMultilineString(code);
                writer.flush();
                return receiveNodeStream();
            } catch (final IOException e) {
                process.destroy();
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }

        private void initiateShutdown() {
            try (final var trace = new Trace("Initiating the shutdown of a pygments server process")) {
                trace.use();
                sendSimpleString(":quit");
                writer.flush();
                final var response = reader.readLine();
                if (!":done".equals(response)) {
                    // We cannot use the regular recovery routine here because that might signal and unwind. Just
                    // throw something and let the catch block forcefully kill the connection.
                    throw new ShutdownErrorException("Unexpected response to quit command: " + response);
                }
            } catch (final IOException | ShutdownErrorException e) {
                destroy();
                ConditionContext.signalSuppressedException(e);
            }
        }

        private void waitForShutdown() {
            try {
                process.waitFor();
            } catch (final InterruptedException e) {
                // This is not meant to be an interruption point, so set the interrupt flag back.
                Thread.currentThread().interrupt();
            }
        }

        private void destroy() {
            try (final var trace = new Trace("Cleaning up a pygments server process")) {
                trace.use();
                stillAlive = false;
                ConditionContext.withSuppressedExceptions(writer::close);
                ConditionContext.withSuppressedExceptions(reader::close);
                process.destroy();
            }
        }

        private @NotNull ImmutableList<@NotNull Node> receiveNodeStream() throws IOException {
            final var accumulator = new NodeAccumulator();
            loop:
            while (true) {
                final var response = reader.readLine();
                switch (response) {
                    case ":done" -> {
                        break loop;
                    }
                    case ":s" -> {
                        final var cssClass = receiveSimpleString();
                        accumulator.accumulate(cssClass, receiveSimpleString());
                    }
                    case ":m" -> {
                        final var cssClass = receiveSimpleString();
                        accumulator.accumulate(cssClass, receiveMultilineString());
                    }
                    case null, default -> throw recoverFromServerError(response);
                }
            }
            return accumulator.finish();
        }

        private void sendSimpleString(final @NotNull String string) throws IOException {
            assert string.indexOf('\n') == -1 : "Multiline string sent as a simple string";
            writer.write(string);
            writer.write('\n');
        }

        private void sendMultilineString(final @NotNull String string) throws IOException {
            int index = 0;
            final var length = string.length();
            while (index < length) {
                final var lineFeedPosition = string.indexOf('\n', index);
                if (lineFeedPosition == -1) {
                    break;
                }
                sendMultilineFragment(string, index, lineFeedPosition);
                index = lineFeedPosition + 1;
            }
            if (index < length) {
                sendMultilineFragment(string, index, length);
            }
            sendSimpleString(":done");
        }

        private void sendMultilineFragment(
            final @NotNull String string,
            final int offset,
            final int end
        ) throws IOException {
            writer.write('>');
            writer.write(string, offset, end - offset);
            writer.write('\n');
        }

        private @NotNull String receiveSimpleString() throws IOException {
            final var line = reader.readLine();
            // NB: ":error" is a perfectly cromulent simple string that can occur as a token value in regular code, so
            // do NOT check for it here.
            if (line == null) {
                throw recoverFromServerError(null);
            }
            return line;
        }

        private @NotNull String receiveMultilineString() throws IOException {
            final var builder = new StringBuilder();
            while (true) {
                final var line = reader.readLine();
                if (line == null) {
                    throw recoverFromServerError(null);
                } else if (line.startsWith(">")) {
                    builder.append(line, 1, line.length());
                    builder.append('\n');
                } else if (":done".equals(line)) {
                    break;
                } else {
                    throw recoverFromServerError(line);
                }
            }
            return builder.toString();
        }

        private @NotNull UnhandledErrorError recoverFromServerError(
            final @Nullable String firstLineOfResponse
        ) {
            try (final var trace = new Trace("Attempting to recover from pygments server error")) {
                trace.use();
                @NotNull Condition condition;
                if (":error".equals(firstLineOfResponse)) {
                    // Regular error, retrieve the error message and let the server live.
                    try {
                        final var errorMessage = receiveMultilineString();
                        condition = new PygmentsServerErrorCondition("Server reported an error", errorMessage);
                    } catch (final IOException e) {
                        // Okay, we can't let it live after all because we don't know what state it's in after possibly
                        // half-completed I/O.
                        destroy();
                        condition = new IOExceptionCondition(e);
                    }
                } else if (firstLineOfResponse == null) {
                    destroy();
                    condition = new PygmentsServerErrorCondition("Broken pipe");
                } else {
                    destroy();
                    condition = new PygmentsServerErrorCondition("Unexpected response: " + firstLineOfResponse);
                }
                throw ConditionContext.error(condition);
            }
        }

        private final @NotNull Process process;
        private final @NotNull OutputStreamWriter writer;
        private final @NotNull BufferedReader reader;
        private boolean stillAlive = true;
    }

    // Due to how the Pygments library works internally, it's very common to see *tons* of consecutive nodes with
    // the same CSS class (usually null) that tend to be very short: each typically consisting of only a handful of
    // characters, sometimes even just *one*. This class cleans up that mess by merging them in order to decrease memory
    // use and make it easier on code that visits every DOM node, like the DOM verifier or serializer. Since we're
    // caching each highlighted snippet in DOM subtree form in the Pygments cache, this is worth it.
    private static final class NodeAccumulator {
        private void accumulate(final @NotNull String cssClass, final @NotNull String string) {
            if (!lastClass.equals(cssClass)) {
                flushBuilder();
                lastClass = cssClass;
            }
            builder.append(string);
        }

        private @NotNull ImmutableList<@NotNull Node> finish() {
            flushBuilder();
            return nodes.freeze();
        }

        private void flushBuilder() {
            if (builder.isEmpty()) {
                return;
            }
            nodes.add(makeNode());
            builder.setLength(0);
        }

        private @NotNull Node makeNode() {
            final var textNode = new Node.Text(builder.toString());
            final var cssClass = lastClass;
            return cssClass.isEmpty() ? textNode : Node.build(Tag.SPAN, span -> {
                span.set("class", cssClass);
                span.append(textNode);
            });
        }

        private final ImmutableList.Builder<@NotNull Node> nodes = new ImmutableList.Builder<>();
        private final StringBuilder builder = new StringBuilder();
        private @NotNull String lastClass = "";
    }
}
