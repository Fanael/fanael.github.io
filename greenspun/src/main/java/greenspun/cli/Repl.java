// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.cli;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import greenspun.generator.Generator;
import greenspun.pygments.PygmentsCache;
import greenspun.pygments.PygmentsServer;
import greenspun.util.Trace;
import greenspun.util.UncheckedInterruptedException;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;

final class Repl {
    Repl(
        final @NotNull Path sourceDirectory,
        final @NotNull Path destinationDirectory,
        final @NotNull ExecutorService executorService,
        final @NotNull PygmentsServer pygmentsServer
    ) {
        pygmentsCache = new PygmentsCache(pygmentsServer);
        generator = new Generator(sourceDirectory, destinationDirectory, executorService, pygmentsCache);
    }

    void run() throws Unwind {
        while (true) {
            final var command = readCommand();
            switch (command) {
                case "", "build", "generate" -> build();
                case "exit", "q", "quit" -> {
                    return;
                }
                default -> unknownCommand(command);
            }
        }
    }

    private void build() throws Unwind {
        ConditionContext.withRestart("return-to-repl", restart -> {
            final var startTime = System.nanoTime();
            try {
                generator.generate(Instant.now());
            } catch (final InterruptedException e) {
                throw new UncheckedInterruptedException(e);
            }
            pygmentsCache.nextGeneration();
            final var endTime = System.nanoTime();
            try (final var streams = acquireStreams()) {
                streams.out().println("Build took " + TimeUnit.NANOSECONDS.toMillis(endTime - startTime) + " ms");
            }
            return this;
        });
    }

    private static void unknownCommand(final @NotNull String command) {
        try (final var streams = acquireStreams()) {
            streams.out().println("Unknown command: " + command);
        }
    }

    private static @NotNull String readCommand() throws Unwind {
        try (final var trace = new Trace("Reading a command")) {
            trace.use();
            try (final var streams = acquireStreams()) {
                streams.out().print("> ");
                try {
                    final var line = streams.in().readLine();
                    return (line != null) ? line.trim() : "quit";
                } catch (final IOException e) {
                    throw ConditionContext.error(new IOExceptionCondition(e));
                }
            }
        }
    }

    private static @NotNull Streams acquireStreams() {
        try {
            return Streams.acquire();
        } catch (final InterruptedException e) {
            throw new UncheckedInterruptedException(e);
        }
    }

    private final @NotNull PygmentsCache pygmentsCache;
    private final @NotNull Generator generator;
}
