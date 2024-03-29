// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A condition handler, intended to be used within try-with-resources.
 * <p>
 * Whenever a condition is signaled, the procedures of all installed handlers are executed in order from the last
 * installed to the first.
 */
public final class Handler implements AutoCloseable {
    /**
     * Initializes a new condition handler with the given procedure. The handler is automatically registered within
     * the current thread's condition {@link ConditionContext}.
     * <p>
     * The handler procedure is called whenever a condition is signaled and no later handler diverts the control flow.
     */
    public Handler(final HandlerProcedure procedure) {
        final var context = ConditionContext.localContext();
        next = context.firstHandler;
        this.procedure = procedure;
        ownerContext = context;
        context.firstHandler = this;
    }

    /**
     * Dummy method that does nothing, to silence compiler warnings about unreferenced auto-closeable resources.
     */
    @SuppressWarnings("EmptyMethod")
    public void use() {
    }

    /**
     * Unregisters the handler from the current thread's condition {@link ConditionContext}.
     */
    @Override
    public void close() {
        checkUnlinkInvariants();
        ownerContext.firstHandler = next;
    }

    void handle(final SignaledCondition condition) {
        procedure.handle(condition);
    }

    boolean usableIn(final ConditionContext context) {
        return ownerContext == context || procedure instanceof HandlerProcedure.ThreadSafe;
    }

    private void checkUnlinkInvariants() {
        assert ownerContext == ConditionContext.localContext() : "Handler closed by a different thread";
        assert ownerContext.firstHandler == this : "Handler chain corrupt";
    }

    final @Nullable Handler next;
    private final HandlerProcedure procedure;
    private final ConditionContext ownerContext;
}
