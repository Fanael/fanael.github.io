// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A trace message, intended to be used within try-with-resources.
 * <p>
 * Trace messages are intended to be <em>user-readable</em> messages that provide context for operations and place
 * a condition was signaled at; they're <em>not</em> meant to be a machine stack trace.
 * <p>
 * Trace objects should <em>never</em> be used outside of the thread they were created by.
 */
public final class Trace implements AutoCloseable {
    {
        final var context = ConditionContext.localContext();
        next = context.firstTrace;
        context.firstTrace = this;
    }

    /**
     * Initializes a new trace with the given <em>lazily evaluated</em> message. The trace is automatically registered
     * within the current thread's condition {@link ConditionContext}.
     * <p>
     * The message supplier is called at most once.
     */
    public Trace(final @NotNull MessageSupplier supplier) {
        messageOrSupplier = supplier;
    }

    /**
     * Initializes a new trace with the given message. The trace is automatically registered within the current
     * thread's condition {@link ConditionContext}.
     */
    public Trace(final @NotNull String message) {
        messageOrSupplier = message;
    }

    /**
     * Dummy method that does nothing, to silence compiler warnings about unreferenced auto-closeable resources.
     */
    @SuppressWarnings("EmptyMethod")
    public void use() {
    }

    /**
     * Unregisters the trace from the current thread's condition {@link ConditionContext}.
     */
    @Override
    public void close() {
        final var context = ConditionContext.localContext();
        assert context.firstTrace == this : "Trace chain corrupt";
        context.firstTrace = next;
    }

    @NotNull String message() {
        return (messageOrSupplier instanceof String s) ? s : runSupplier();
    }

    private @NotNull String runSupplier() {
        assert messageOrSupplier instanceof MessageSupplier : "runSupplier called with no supplier present";
        final var supplier = (MessageSupplier) messageOrSupplier;
        final var string = supplier.get();
        messageOrSupplier = string;
        return string;
    }

    final @Nullable Trace next;
    // If a String, it's a message, otherwise it's assumed to be a MessageSupplier.
    private @NotNull Object messageOrSupplier;
}
