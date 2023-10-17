// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.cli;

import java.io.IOException;
import java.io.PrintStream;
import greenspun.util.Trace;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.Condition;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.HandlerProcedure;
import greenspun.util.condition.Restart;
import greenspun.util.condition.SignaledCondition;
import greenspun.util.condition.SuppressedExceptionCondition;

final class FallbackHandler implements HandlerProcedure.ThreadSafe {
    private FallbackHandler() {
    }

    static FallbackHandler instance() {
        return instance;
    }

    @Override
    public void handle(final SignaledCondition condition) {
        if (!condition.isFatal()) {
            if (condition.condition() instanceof final SuppressedExceptionCondition c) {
                showSuppressedExceptionCondition(c);
            }
            return;
        }
        final var restarts = Seq.fromIterable(ConditionContext.restarts());
        try (final var streams = Streams.acquire()) {
            showCondition(streams, condition.condition(), "A fatal condition");
            chooseRestart(streams, restarts).unwindTo();
        }
    }

    private static void showSuppressedExceptionCondition(final SuppressedExceptionCondition condition) {
        try (final var streams = Streams.acquire()) {
            showCondition(streams, condition, "A condition");
        }
    }

    private static void showCondition(
        final Streams streams,
        final Condition condition,
        final String prefix
    ) {
        final var err = streams.err();
        err.println(prefix + " of type " + condition.getClass().getName() + " has been signaled.");
        err.println("\nDetailed message:");
        err.println(condition.detailedMessage().stripTrailing());
        err.println("\nOperation trace:");
        for (final var traceMessage : Trace.activeTraces()) {
            err.println(" - " + traceMessage);
        }
        err.println();
    }

    private static Restart chooseRestart(
        final Streams streams,
        final Seq<Restart> restarts
    ) {
        if (restarts.isEmpty()) {
            throw new RuntimeException("No restarts available");
        }

        final var err = streams.err();
        showRestarts(err, restarts);
        while (true) {
            err.print("Enter restart number > ");
            try {
                final var line = streams.in().readLine();
                if (line == null) {
                    err.println("End of input found, arbitrarily picking the last restart.");
                    return restarts.last();
                }
                final var index = Integer.parseInt(line.strip());
                if (index >= 1 && index <= restarts.exactSize()) {
                    return restarts.get(index - 1);
                } else {
                    err.println("Invalid restart index " + index + ", value out of bounds.");
                }
            } catch (final NumberFormatException e) {
                err.println("Restart index not an integer: " + e);
            } catch (final IOException e) {
                err.println("I/O error occurred: " + e);
                err.println("Arbitrarily picking the last restart.");
                return restarts.last();
            }
        }
    }

    private static void showRestarts(final PrintStream stream, final Seq<Restart> restarts) {
        long index = 1;
        stream.println("Available restarts:");
        for (final var restart : restarts) {
            stream.println(" " + index + ". " + restart.name());
            index += 1;
        }
        stream.println();
    }

    private static final FallbackHandler instance = new FallbackHandler();
}
