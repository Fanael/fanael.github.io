// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;

/**
 * A functional interface representing {@link Handler} procedures.
 */
@FunctionalInterface
public interface HandlerProcedure {
    /**
     * Processes the given condition.
     * <p>
     * Each handler may either decline to handle a condition by returning normally, or handle it by performing a
     * non-local control flow transfer, for example by transferring control to a restart point by using
     * {@link Restart#unwindTo()} or throwing an exception.
     */
    void handle(@NotNull SignaledCondition condition) throws Unwind;

    /**
     * A functional interface representing a {@link Handler} procedure that is thread-safe.
     * <p>
     * Thread-safe handlers will be inherited by child threads by {@link ConditionContext#saveInheritableState()}
     * and {@link ConditionContext#inheritState(ConditionContext.InheritedState)}.
     */
    @FunctionalInterface
    interface ThreadSafe extends HandlerProcedure {
    }
}
