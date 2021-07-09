// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;

/**
 * The error type thrown when none of the handlers performed non-local control flow transfer as a result of a call to
 * {@link ConditionContext#error(Condition)}.
 * <p>
 * Since this represents a programming error, this class extends {@link AssertionError}.
 */
public final class UnhandledErrorError extends AssertionError {
    UnhandledErrorError(final @NotNull Condition condition) {
        super("Fatal condition signaled, but no condition handler unwound; condition: " + condition);
    }
}
