// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import greenspun.auki.annotations.Open;
import org.jetbrains.annotations.NotNull;

/**
 * The base type for all conditions.
 * <p>
 * Unlike exceptions, conditions can represent any occurrence that may be of interest to code at different levels of
 * the call stack, and unlike exceptions, condition handlers execute <em>before</em> the stack is unwound, allowing
 * handlers to access state, like restart points, that was created way after the handler was established.
 */
public abstract class Condition {
    /**
     * Initializes a new condition with the given user-readable message.
     */
    protected Condition(final @NotNull String message) {
        this.message = message;
    }

    /**
     * Retrieves the user-readable message representing this condition.
     */
    public final @NotNull String message() {
        return message;
    }

    /**
     * Retrieves the full, detailed, user-readable message representing this condition.
     */
    @Open
    public @NotNull String detailedMessage() {
        return message;
    }

    @Open
    @Override
    public @NotNull String toString() {
        return getClass().getName() + ": " + message;
    }

    private final @NotNull String message;
}
