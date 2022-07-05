// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

/**
 * A functional interface representing the callback function for
 * {@link ConditionContext#withRestart(String, RestartCallback)}. Has no particular semantics of its own.
 */
@FunctionalInterface
public interface RestartCallback<T> {
    @SuppressWarnings("RedundantThrows")
    T call(Restart restart) throws Unwind;
}
