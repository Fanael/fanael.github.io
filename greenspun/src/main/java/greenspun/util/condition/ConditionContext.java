// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import java.util.Iterator;
import java.util.NoSuchElementException;
import greenspun.util.SneakyThrow;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A condition context keeps track of currently registered handlers and restart points.
 * <p>
 * Each thread has its own local condition context, independent of other threads' contexts. Instances are not accessible
 * directly, static methods operating on the current thread's context are provided instead.
 *
 * @see Handler
 * @see Restart
 */
public final class ConditionContext {
    private ConditionContext() {
    }

    /**
     * Signals the given condition.
     * <p>
     * Signaling a conditions entails invoking currently registered handlers in order from the newest one to the oldest.
     * If any handler performs a non-local control flow transfer, later handlers are not invoked.
     * <p>
     * If all handlers decline handling the condition, that is they all return normally, this method returns normally
     * as well.
     * <p>
     * Since handlers are allowed to unwind to a restart point, this method may throw {@link Unwind}.
     */
    public static void signal(final @NotNull Condition condition) {
        localContext().signal(new SignaledCondition(condition, false));
    }

    /**
     * Signals the given exception as a non-fatal condition of type {@link SuppressedExceptionCondition}.
     * <p>
     * As this method is sometimes called as a part of cleanup process during stack unwinding, handlers are
     * <strong>not allowed</strong> to unwind to a restart point as a response to that condition.
     */
    public static void signalSuppressedException(final @NotNull Exception exception) {
        try {
            SneakyThrow.<Unwind>pretendThrows();
            signal(new SuppressedExceptionCondition(exception));
        } catch (final Unwind u) {
            throw new AssertionError("A handler attempted to unwind a suppressed exception condition", u);
        }
    }

    /**
     * Signals the given condition as an error.
     * <p>
     * This method behaves like {@link ConditionContext#signal(Condition)}, with one difference: if all handlers decline
     * handling the condition, a fatal error of type {@link UnhandledErrorError} is thrown.
     * <p>
     * A condition signaled with this method is called <dfn>fatal</dfn>.
     * <p>
     * Since this method never returns normally, it's declared to return {@link UnhandledErrorError} that can be
     * "thrown" at call sites to help the compiler's control flow analysis.
     */
    public static @NotNull UnhandledErrorError error(final @NotNull Condition condition) {
        localContext().signal(new SignaledCondition(condition, true));
        throw new UnhandledErrorError(condition);
    }

    /**
     * Calls the given callback, suppressing the exceptions it throws.
     * <p>
     * If the callback throws an exception, it is caught and signaled as a non-fatal condition of type
     * {@link SuppressedExceptionCondition}.
     * <p>
     * As this method is sometimes called as a part of cleanup process during stack unwinding, handlers are
     * <strong>not allowed</strong> to unwind to a restart point as a response to that condition.
     */
    public static void withSuppressedExceptions(final @NotNull ThrowingCallback callback) {
        try {
            callback.run();
        } catch (final Exception e) {
            signalSuppressedException(e);
        }
    }

    /**
     * Executes the given function with a restart point around it.
     *
     * @param restartName The user-readable name of this restart point.
     * @param callback    The function to execute with a restart around it. The restart object is passed as an argument.
     * @return If the act of executing {@code callback} did not transfer control flow to this restart point, the value
     * returned by {@code callback}. Otherwise, if executing {@code callback} unwound to this restart point,
     * {@code null} instead.
     */
    public static <T> @Nullable T withRestart(
        final @NotNull String restartName,
        final @NotNull RestartCallback<? extends T> callback
    ) {
        final var restart = new Restart(restartName);
        try {
            return callback.call(restart);
        } catch (final Unwind unwind) {
            if (unwind.target() != restart) {
                throw SneakyThrow.doThrow(unwind);
            }
            return null;
        } finally {
            restart.unlink();
        }
    }

    /**
     * Returns an iterable containing all active restart points, ordered from the newest one to the oldest.
     */
    public static @NotNull Iterable<@NotNull Restart> restarts() {
        return localContext().new RestartIterable();
    }

    /**
     * Saves the inheritable state, that is, restarts and thread-safe handlers, to allow them to be used by a child
     * thread.
     *
     * @see #inheritState(InheritedState)
     */
    public static @NotNull InheritedState saveInheritableState() {
        final var context = localContext();
        // We can just use the regular handler chain unchanged, because signal() checks if the handler is usable in
        // the calling thread.
        return new InheritedState(context.firstHandler, context.firstRestart);
    }

    /**
     * Inherits the given condition context state in a child thread.
     * <p>
     * The child thread's condition context has to be empty, with no handlers or restarts at all.
     *
     * @return The state before inheritance, to be restored by {@link #restoreState(PreviousState)}.
     * @see #saveInheritableState()
     */
    public static @NotNull PreviousState inheritState(final @NotNull InheritedState inheritedState) {
        final var context = localContext();
        assert context.firstHandler == null : "Attempted to inherit state into a thread that already has handlers";
        assert context.firstRestart == null : "Attempted to inherit state into a thread that already has restarts";
        context.firstHandler = inheritedState.firstHandler;
        context.firstRestart = inheritedState.firstRestart;
        return PreviousState.instance;
    }

    /**
     * Restores the original state of the current thread's condition context.
     */
    public static void restoreState(@SuppressWarnings("unused") final @NotNull PreviousState previousState) {
        final var context = localContext();
        context.firstHandler = null;
        context.firstRestart = null;
    }

    static @NotNull ConditionContext localContext() {
        return localContext.get();
    }

    private void signal(final @NotNull SignaledCondition condition) {
        for (var handler = findFirstHandler(); handler != null; handler = handler.next) {
            if (!handler.usableIn(this)) {
                continue;
            }
            final var currentSave = currentHandler;
            currentHandler = handler;
            try {
                handler.handle(condition);
            } finally {
                currentHandler = currentSave;
            }
        }
    }

    private @Nullable Handler findFirstHandler() {
        // If we're signaling a condition while executing a handler, make sure to consider only handlers that *follow*
        // the one currently executing, to avoid recursion.
        return (currentHandler == null) ? firstHandler : currentHandler.next;
    }

    @Nullable Handler firstHandler = null;
    @Nullable Restart firstRestart = null;
    private @Nullable Handler currentHandler = null;

    private static final ThreadLocal<@NotNull ConditionContext> localContext =
        ThreadLocal.withInitial(ConditionContext::new);

    /**
     * A generic callback with no particular semantics declared to throw checked Exceptions.
     * <p>
     * Intended to be used with {@link #withSuppressedExceptions(ThrowingCallback)}.
     */
    @FunctionalInterface
    public interface ThrowingCallback {
        void run() throws Exception;
    }

    /**
     * An opaque type representing saved inheritable state of a condition context.
     */
    public static final class InheritedState {
        private InheritedState(final @Nullable Handler firstHandler, final @Nullable Restart firstRestart) {
            this.firstHandler = firstHandler;
            this.firstRestart = firstRestart;
        }

        private final @Nullable Handler firstHandler;
        private final @Nullable Restart firstRestart;
    }

    /**
     * An opaque type representing saved previous state of a condition context.
     */
    public static final class PreviousState {
        private PreviousState() {
        }

        private static final PreviousState instance = new PreviousState();
    }

    private final class RestartIterable implements Iterable<@NotNull Restart> {
        @Override
        public @NotNull Iterator<@NotNull Restart> iterator() {
            return new RestartIterator(firstRestart);
        }
    }

    private static final class RestartIterator implements Iterator<@NotNull Restart> {
        private RestartIterator(final @Nullable Restart firstRestart) {
            current = firstRestart;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public @NotNull Restart next() {
            final var result = current;
            if (result == null) {
                throw new NoSuchElementException("No more restarts left");
            }
            current = result.next;
            return result;
        }

        private @Nullable Restart current;
    }
}
