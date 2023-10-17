// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import java.util.Iterator;
import java.util.NoSuchElementException;
import greenspun.util.condition.MessageSupplier;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A trace message, intended to be used within try-with-resources.
 * <p>
 * Trace messages are intended to be <em>user-readable</em> messages that provide context for operations and place
 * a problem occurs; they're <em>not</em> meant to be a machine stack trace.
 * <p>
 * Trace objects should <em>never</em> be used outside the thread they were created by.
 */
public final class Trace implements AutoCloseable {
    /**
     * Initializes a new trace with the given <em>lazily evaluated</em> message. The trace is automatically registered
     * as the first active trace in the calling thread.
     * <p>
     * The message supplier is called at most once.
     */
    public Trace(final MessageSupplier supplier) {
        this((Object) supplier);
    }

    /**
     * Initializes a new trace with the given message. The trace is automatically registered as the first active trace
     * in the calling thread.
     */
    public Trace(final String message) {
        this((Object) message);
    }

    private Trace(final Object object) {
        final var context = localContext();
        next = context.firstTrace;
        messageOrSupplier = object;
        ownerContext = context;
        context.firstTrace = this;
    }

    /**
     * Returns an iterable over the calling thread's active trace messages. Traces are returned in the order of their
     * construction, starting with the most recently established one.
     */
    public static Iterable<String> activeTraces() {
        return IterableImpl.instance;
    }

    /**
     * Dummy method that does nothing, to silence compiler warnings about unreferenced auto-closeable resources.
     */
    @SuppressWarnings("EmptyMethod")
    public void use() {
    }

    /**
     * Unregisters the trace from the current thread's trace chain.
     * <p>
     * This method should never be called manually: use try-with-resources with trace objects instead.
     */
    @Override
    public void close() {
        checkUnlinkInvariants();
        ownerContext.firstTrace = next;
    }

    @SuppressWarnings("MethodOnlyUsedFromInnerClass")
    private String message() {
        return (messageOrSupplier instanceof final String string) ? string : runSupplier();
    }

    private String runSupplier() {
        assert messageOrSupplier instanceof MessageSupplier : "runSupplier called with no supplier present";
        final var supplier = (MessageSupplier) messageOrSupplier;
        final var string = supplier.get();
        messageOrSupplier = string;
        return string;
    }

    private void checkUnlinkInvariants() {
        assert ownerContext == localContext() : "Trace closed by a different thread";
        assert ownerContext.firstTrace == this : "Trace chain corrupt";
    }

    private static Context localContext() {
        return context.get();
    }

    @SuppressWarnings("nullness:type.argument") // Not actually nullable, CF doesn't understand withInitial.
    private static final ThreadLocal<Context> context = ThreadLocal.withInitial(Context::new);

    private final @Nullable Trace next;
    // If a String, it's a message, otherwise it's assumed to be a MessageSupplier.
    private Object messageOrSupplier;
    private final Context ownerContext;

    private static final class Context {
        private @Nullable Trace firstTrace = null;
    }

    private static final class IterableImpl implements Iterable<String> {
        @Override
        public @NonNull Iterator<String> iterator() {
            return new IteratorImpl(localContext().firstTrace);
        }

        private static final IterableImpl instance = new IterableImpl();
    }

    private static final class IteratorImpl implements Iterator<String> {
        private IteratorImpl(final @Nullable Trace firstTrace) {
            current = firstTrace;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public String next() {
            final var result = current;
            if (result == null) {
                throw new NoSuchElementException("No more traces left");
            }
            current = result.next;
            return result.message();
        }

        private @Nullable Trace current;
    }
}
