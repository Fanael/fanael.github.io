// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;

/**
 * A functional interface representing the callback function for
 * {@link ConditionContext#withRestart(String, RestartCallback)}. Has no particular semantics of its own.
 */
@FunctionalInterface
public interface RestartCallback<T> {
    @SuppressWarnings("RedundantThrows")
    @NotNull T call(@NotNull Restart restart) throws Unwind;
}
