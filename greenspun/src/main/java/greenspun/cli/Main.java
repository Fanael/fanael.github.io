// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.cli;

import java.nio.file.Path;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import greenspun.pygments.PygmentsServer;
import greenspun.pygments.ServerCodeTemporaryFile;
import greenspun.util.UncheckedInterruptedException;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Handler;
import greenspun.util.condition.Unwind;
import org.jetbrains.annotations.NotNull;

final class Main {
    private Main() {
    }

    public static void main(final @NotNull String[] args) throws Unwind, InterruptedException {
        try {
            System.exit(mainImpl(args).value);
        } catch (final UncheckedInterruptedException e) {
            throw e.getCause();
        }
    }

    private static void enterRepl(
        final @NotNull ExecutorService executorService,
        final @NotNull Path sourceDirectory,
        final @NotNull Path destinationDirectory
    ) throws Unwind {
        try (
            final var pygmentsServerCode = ServerCodeTemporaryFile.save();
            final var pygmentsServer = new PygmentsServer(pygmentsServerCode.path())
        ) {
            final var repl = new Repl(sourceDirectory, destinationDirectory, executorService, pygmentsServer);
            repl.run();
        }
    }

    private static @NotNull ExitCode mainImpl(final @NotNull String[] args) throws InterruptedException, Unwind {
        if (args.length != 2) {
            try (final var streams = Streams.acquire()) {
                streams.err().println("Exactly two arguments <source directory> <destination directory> expected");
                return ExitCode.USAGE;
            }
        }
        final var sourceDirectoryPath = Path.of(args[0]);
        final var destinationDirectoryPath = Path.of(args[1]);

        try (final var handler = new Handler(FallbackHandler.instance())) {
            handler.use();
            final var threadPool = createThreadPool();
            try {
                final var exitCode = ConditionContext.withRestart("abort-process", restart -> {
                    enterRepl(threadPool, sourceDirectoryPath, destinationDirectoryPath);
                    return ExitCode.SUCCESS;
                });
                return (exitCode != null) ? exitCode : ExitCode.ERROR;
            } finally {
                shutDownThreadPool(threadPool);
            }
        }
    }

    private static @NotNull ThreadPoolExecutor createThreadPool() {
        final var threadId = new AtomicInteger(0);
        return new ThreadPoolExecutor(
            Runtime.getRuntime().availableProcessors(),
            Integer.MAX_VALUE,
            0,
            TimeUnit.NANOSECONDS,
            new LinkedBlockingQueue<>(),
            runnable -> new Thread(runnable, "worker-thread-" + threadId.addAndGet(1))
        );
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private static void shutDownThreadPool(final @NotNull ThreadPoolExecutor threadPool) throws InterruptedException {
        threadPool.shutdownNow();
        threadPool.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
    }

    private enum ExitCode {
        SUCCESS(0),
        ERROR(1),
        USAGE(64);

        ExitCode(final int value) {
            this.value = value;
        }

        private final int value;
    }
}
