// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import greenspun.util.function.ThrowingCallback;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type indicating that an exception has been suppressed during a cleanup operation.
 * <p>
 * This condition should typically not be fatal, and handlers are discouraged from transferring control flow to
 * a restart as a response to it, as it is usually signaled by
 * {@link ConditionContext#withSuppressedExceptions(ThrowingCallback)} or
 * {@link ConditionContext#signalSuppressedException(Exception)}, which disallow unwinding to a restart.
 */
public final class SuppressedExceptionCondition extends Condition {
    /**
     * Initializes a new suppressed exception condition indicating that the given exception was suppressed.
     */
    public SuppressedExceptionCondition(final @NotNull Exception exception) {
        super("Suppressed exception: " + exception);
    }
}
