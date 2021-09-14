// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.function.Function;
import greenspun.util.collection.ImmutableList;
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
     * Returns a fresh {@link ArrayList} containing the results of applying the given function to the elements of
     * the given collection, in order of the collection's iterator if the collection guarantees an order.
     * <p>
     * Elements are processed concurrently by submitting tasks to the associated {@link ExecutorService}. The tasks
     * inherit the {@link ConditionContext} state from the calling thread. Unwinds that escape a task are propagated to
     * the calling thread.
     * <p>
     * This method waits for all the submitted tasks to finish before returning.
     */
    public <T, R> @NotNull ArrayList<R> map(
        final @NotNull Collection<? extends T> collection,
        final @NotNull Function<? super T, ? extends R> function
    ) {
        return collectResults(submitTasks(collection, function));
    }

    /**
     * Applies the given consumer to the elements of the given collection.
     * <p>
     * Elements are processed concurrently by submitting tasks to the associated {@link ExecutorService}. The tasks
     * inherit the {@link ConditionContext} state from the calling thread. Unwinds that escape a task are propagated to
     * the calling thread.
     * <p>
     * This method waits for all the submitted tasks to finish before returning.
     */
    public <T> void forEach(
        final @NotNull Collection<? extends T> collection,
        final @NotNull Consumer<? super T> consumer
    ) {
        waitForResults(submitTasks(collection, value -> {
            consumer.accept(value);
            return null;
        }));
    }

    private <T, R> @NotNull ImmutableList<@NotNull Future<R>> submitTasks(
        final @NotNull Collection<? extends T> collection,
        final @NotNull Function<? super T, ? extends R> function
    ) {
        final var inheritedState = ConditionContext.saveInheritableState();
        return ImmutableList.map(collection, element -> executorService.submit(() -> {
            final var previousState = ConditionContext.inheritState(inheritedState);
            try (final var trace = new Trace(() -> "Executing a task in thread " + Thread.currentThread().getName())) {
                trace.use();
                return function.apply(element);
            } finally {
                ConditionContext.restoreState(previousState);
            }
        }));
    }

    private static <T> @NotNull ArrayList<T> collectResults(final @NotNull ImmutableList<@NotNull Future<T>> futures) {
        final var result = new ArrayList<T>(futures.size());
        new AwaitImpl<>(futures.iterator(), result::add).awaitAll();
        return result;
    }

    private static void waitForResults(final @NotNull ImmutableList<@NotNull Future<Object>> futures) {
        new AwaitImpl<>(futures.iterator(), value -> {
        }).awaitAll();
    }

    private final @NotNull ExecutorService executorService;

    private static final class AwaitImpl<T> {
        private AwaitImpl(final @NotNull Iterator<Future<T>> iterator, final @NotNull Consumer<T> consumer) {
            this.iterator = iterator;
            this.consumer = consumer;
        }

        private void awaitAll() {
            try {
                iterate(this::awaitOne);
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
                iterate(future -> future.cancel(true));
            }
        }

        private void iterate(final @NotNull FutureConsumer<T> futureConsumer) {
            while (iterator.hasNext()) {
                futureConsumer.accept(iterator.next());
            }
        }

        private void awaitOne(final @NotNull Future<T> future) {
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

        private final @NotNull Iterator<Future<T>> iterator;
        private final @NotNull Consumer<T> consumer;
        private boolean foundInterrupt = false;
        private boolean needsCancellation = false;

        @FunctionalInterface
        private interface FutureConsumer<T> {
            void accept(@NotNull Future<T> future);
        }
    }
}
