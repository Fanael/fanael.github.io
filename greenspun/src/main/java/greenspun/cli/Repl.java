// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
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
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.exception.IOExceptionCondition;

final class Repl {
    Repl(
        final Path sourceDirectory,
        final Path destinationDirectory,
        final ExecutorService executorService,
        final PygmentsServer pygmentsServer
    ) {
        pygmentsCache = new PygmentsCache(pygmentsServer);
        generator = new Generator(sourceDirectory, destinationDirectory, executorService, pygmentsCache);
    }

    void run() {
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

    private void build() {
        ConditionContext.withRestart("return-to-repl", restart -> {
            final var startTime = System.nanoTime();
            generator.generate(Instant.now());
            pygmentsCache.nextGeneration();
            final var endTime = System.nanoTime();
            try (final var streams = Streams.acquire()) {
                streams.out().println("Build took " + TimeUnit.NANOSECONDS.toMillis(endTime - startTime) + " ms");
            }
            return this;
        });
    }

    private static void unknownCommand(final String command) {
        try (final var streams = Streams.acquire()) {
            streams.out().println("Unknown command: " + command);
        }
    }

    private static String readCommand() {
        try (final var trace = new Trace("Reading a command")) {
            trace.use();
            try (final var streams = Streams.acquire()) {
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

    private final PygmentsCache pygmentsCache;
    private final Generator generator;
}
