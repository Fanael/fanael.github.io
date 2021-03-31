// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.pygments;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import greenspun.util.Trace;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
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
     * Initializes a new Pygments server connection pool using the given path to run processes from.
     * <p>
     * The file under the given path should exist for at least as long as the pool itself, because the pool may decide
     * to spawn a new Pygments server process at any time.
     * <p>
     * Since the path is used as a program path for Python, it should be a regular file system path.
     */
    public PygmentsServer(final @NotNull Path sourceCodePath) {
        this.sourceCodePath = sourceCodePath;
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
            assert freeConnections.size() == activeConnectionCount
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
            activeConnectionCount = 0;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Highlights the syntax of the given code using the given language name for determining syntactic rules.
     * <p>
     * If successful, returns a string representing the highlighted code in HTSL form. Use the
     * {@link greenspun.article.HtslConverter} to convert it into DOM tree nodes.
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
    public @NotNull String highlightCode(final @NotNull String code, final @NotNull String languageName) throws Unwind {
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

    private @NotNull Connection acquireConnection() throws Unwind {
        final @NotNull Connection connection;
        lock.lock();
        try {
            if (activeConnectionCount == 0) {
                connection = openNewConnection();
            } else if (!freeConnections.isEmpty()) {
                connection = freeConnections.remove(freeConnections.size() - 1);
            } else {
                connection = waitForFreeConnection();
            }
        } finally {
            lock.unlock();
        }
        assert connection.stillAlive;
        return connection;
    }

    private void releaseConnection(final @NotNull Connection connection) {
        lock.lock();
        try {
            if (connection.stillAlive) {
                freeConnections.add(connection);
                hasFreeConnections.signal();
            } else {
                activeConnectionCount -= 1;
            }
        } finally {
            lock.unlock();
        }
    }

    private @NotNull Connection waitForFreeConnection() throws Unwind {
        if (shortWaitCount == shortWaitMaxCount) {
            // If we've hit the short wait count limit, open a new connection to relieve the contention a bit.
            shortWaitCount = 0;
            return openNewConnection();
        }

        var nanosecondsRemaining = TimeUnit.MILLISECONDS.toNanos(shortWaitMilliseconds);
        try {
            while (freeConnections.isEmpty() && nanosecondsRemaining > 0) {
                nanosecondsRemaining = hasFreeConnections.awaitNanos(nanosecondsRemaining);
            }
        } catch (final InterruptedException e) {
            // Set the interrupt flag back, short waits aren't intended to be interruption points.
            Thread.currentThread().interrupt();
        }

        if (freeConnections.isEmpty()) {
            return openNewConnection();
        } else {
            shortWaitCount += 1;
            return freeConnections.remove(freeConnections.size() - 1);
        }
    }

    private @NotNull Connection openNewConnection() throws Unwind {
        try (final var trace = new Trace("Spawning a new pygments server process")) {
            trace.use();
            final var builder = new ProcessBuilder("python3", sourceCodePath.toString());
            builder.redirectErrorStream(true);
            try {
                final var connection = new Connection(builder.start());
                activeConnectionCount += 1;
                return connection;
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private static final int shortWaitMilliseconds = 3;
    private static final int shortWaitMaxCount = 10;

    private final @NotNull Path sourceCodePath;
    private final ReentrantLock lock = new ReentrantLock();
    private final Condition hasFreeConnections = lock.newCondition();
    private final ArrayList<@NotNull Connection> freeConnections = new ArrayList<>();
    private int activeConnectionCount = 0;
    private int shortWaitCount = 0;

    private static final class Connection {
        private Connection(final @NotNull Process process) {
            this.process = process;
            this.writer = new OutputStreamWriter(process.getOutputStream(), StandardCharsets.UTF_8);
            this.reader = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
        }

        private @NotNull String highlightCode(
            final @NotNull String code,
            final @NotNull String languageName
        ) throws Unwind {
            try {
                sendSimpleString(":highlight");
                sendSimpleString(languageName);
                sendMultilineString(code);
                writer.flush();
                return receiveMultilineResponse();
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

        private @NotNull String receiveMultilineResponse() throws IOException, Unwind {
            final var builder = new StringBuilder();
            while (true) {
                final var line = reader.readLine();
                if (line == null) {
                    recoverFromServerError(null);
                } else if (line.startsWith(">")) {
                    builder.append(line, 1, line.length());
                    builder.append('\n');
                } else if (":done".equals(line)) {
                    break;
                } else {
                    recoverFromServerError(line);
                }
            }
            return builder.toString();
        }

        private void recoverFromServerError(final @Nullable String firstLineOfResponse) throws Unwind {
            try (final var trace = new Trace("Attempting to recover from pygments server error")) {
                trace.use();
                @NotNull greenspun.util.condition.Condition condition;
                if (":error".equals(firstLineOfResponse)) {
                    // Regular error, retrieve the error message and let the server live.
                    try {
                        final var errorMessage = receiveMultilineResponse();
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
}