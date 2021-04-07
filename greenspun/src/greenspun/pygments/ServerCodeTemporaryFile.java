// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.pygments;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import greenspun.util.Trace;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;

/**
 * Temporary file containing the Pygments server source code, as an {@link AutoCloseable} resource.
 */
public final class ServerCodeTemporaryFile implements AutoCloseable {
    private ServerCodeTemporaryFile(final @NotNull Path path) {
        this.path = path;
    }

    /**
     * Saves the Pygments server source code to a temporary file.
     * <p>
     * If an I/O error of any kind occurs, {@link IOExceptionCondition} is signaled as a fatal condition.
     */
    public static @NotNull ServerCodeTemporaryFile save() throws Unwind {
        try (final var trace = new Trace("Saving the pygments server code to a temporary file")) {
            trace.use();
            try (final var input = openInputStream()) {
                final var path = Files.createTempFile(null, ".py");
                try (final var output = Files.newOutputStream(path)) {
                    input.transferTo(output);
                }
                return new ServerCodeTemporaryFile(path);
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    /**
     * Retrieves the path to the temporary file.
     */
    public @NotNull Path path() {
        return path;
    }

    /**
     * Closes the resource, deleting the temporary file.
     * <p>
     * If an I/O error of any kind occurs, the exception is signaled as a <em>non-fatal</em>
     * {@link greenspun.util.condition.SuppressedExceptionCondition}.
     */
    @Override
    public void close() {
        try (final var trace = new Trace("Removing extracted Pygments server temporary file")) {
            trace.use();
            ConditionContext.withSuppressedExceptions(() -> Files.delete(path));
        }
    }

    private static @NotNull InputStream openInputStream() throws IOException {
        final var resourceStream = ServerCodeTemporaryFile.class.getResourceAsStream(resourcePath);
        if (resourceStream != null) {
            return resourceStream;
        }
        return Files.newInputStream(Path.of(filePath));
    }

    private final @NotNull Path path;

    private static final String resourcePath = "/pygments_server.py";
    private static final String filePath = "resources" + resourcePath;
}
