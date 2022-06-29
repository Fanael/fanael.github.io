// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import java.util.Iterator;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.function.Function;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import org.jetbrains.annotations.NotNull;

/**
 * A wrapper around {@link ExecutorService} providing convenient methods for concurrent operations on collections.
 */
public final class CollectionExecutorService {
    /**
     * Initializes a new collection executor service that will submit tasks to the given executor service.
     */
    public CollectionExecutorService(final @NotNull ExecutorService executorService) {
        this.executorService = executorService;
    }

    /**
     * Returns a fresh {@link Seq} containing the results of applying the given function to the elements of the given
     * iterable.
     * <p>
     * Elements are processed concurrently by submitting tasks to the associated {@link ExecutorService}. The tasks
     * inherit the {@link ConditionContext} state from the calling thread. Unwinds that escape a task are propagated to
     * the calling thread.
     * <p>
     * This method waits for all the submitted tasks to finish before returning.
     */
    public <T, R> @NotNull Seq<R> map(
        final @NotNull Iterable<? extends T> iterable,
        final @NotNull Function<? super T, ? extends R> function
    ) {
        return collectResults(submitTasks(iterable, function));
    }

    /**
     * Applies the given consumer to the elements of the given iterable.
     * <p>
     * Elements are processed concurrently by submitting tasks to the associated {@link ExecutorService}. The tasks
     * inherit the {@link ConditionContext} state from the calling thread. Unwinds that escape a task are propagated to
     * the calling thread.
     * <p>
     * This method waits for all the submitted tasks to finish before returning.
     */
    public <T> void forEach(
        final @NotNull Iterable<? extends T> iterable,
        final @NotNull Consumer<? super T> consumer
    ) {
        waitForResults(submitTasks(iterable, value -> {
            consumer.accept(value);
            return null;
        }));
    }

    private <T, R> @NotNull Seq<@NotNull Future<R>> submitTasks(
        final @NotNull Iterable<? extends T> iterable,
        final @NotNull Function<? super T, ? extends R> function
    ) {
        final var inheritedState = ConditionContext.saveInheritableState();
        return Seq.mapIterable(iterable, (item) -> executorService.submit(() -> {
            final var previousState = ConditionContext.inheritState(inheritedState);
            try (final var trace = new Trace(() -> "Executing a task in thread " + Thread.currentThread().getName())) {
                trace.use();
                return function.apply(item);
            } finally {
                ConditionContext.restoreState(previousState);
            }
        }));
    }

    private static <T> @NotNull Seq<T> collectResults(final @NotNull Seq<@NotNull Future<T>> futures) {
        final var builder = new Seq.Builder<T>();
        new AwaitImpl<>(futures.iterator(), builder::append).awaitAll();
        return builder.toSeq();
    }

    private static void waitForResults(final @NotNull Seq<? extends @NotNull Future<?>> futures) {
        new AwaitImpl<>(futures.iterator(), (value) -> {
        }).awaitAll();
    }

    private final @NotNull ExecutorService executorService;

    private static final class AwaitImpl<T> {
        private AwaitImpl(
            final @NotNull Iterator<? extends @NotNull Future<? extends T>> iterator,
            final @NotNull Consumer<? super T> consumer
        ) {
            this.iterator = iterator;
            this.consumer = consumer;
        }

        private void awaitAll() {
            try {
                while (iterator.hasNext()) {
                    awaitOne(iterator.next());
                }
                checkInterruptions();
            } finally {
                cancelIfNeeded();
            }
        }

        private void checkInterruptions() {
            if (foundInterrupt) {
                throw new AssertionError("A task was interrupted, but no other task threw anything concrete");
            }
        }

        private void cancelIfNeeded() {
            if (needsCancellation) {
                while (iterator.hasNext()) {
                    iterator.next().cancel(true);
                }
            }
        }

        private void awaitOne(final @NotNull Future<? extends T> future) {
            try {
                consumer.accept(future.get());
            } catch (final InterruptedException e) {
                throw SneakyThrow.doThrow(e);
            } catch (final ExecutionException e) {
                recover(e);
            }
        }

        private void recover(final @NotNull ExecutionException executionException) {
            needsCancellation = true;
            final var cause = executionException.getCause();
            switch (cause) {
                // Cross-thread unwind to a restart found, rethrow it to continue unwinding in the parent thread.
                case Unwind unwind -> throw SneakyThrow.doThrow(unwind);
                // Continue looking, some other future will likely have a more concrete throwable.
                case InterruptedException ignored -> foundInterrupt = true;
                // Errors should be passed through directly.
                case Error error -> throw error;
                default -> throw new AssertionError("An exception escaped from a worker through a future", cause);
            }
        }

        private final @NotNull Iterator<? extends @NotNull Future<? extends T>> iterator;
        private final @NotNull Consumer<? super T> consumer;
        private boolean foundInterrupt = false;
        private boolean needsCancellation = false;
    }
}
