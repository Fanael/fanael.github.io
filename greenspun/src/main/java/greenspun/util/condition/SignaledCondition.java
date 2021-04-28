// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;

/**
 * A convenience record bundling together the condition being signaled and auxiliary information.
 *
 * @param condition The condition being signaled.
 * @param isFatal   {@code true} iff the condition was signaled with {@link ConditionContext#error(Condition)}.
 */
public record SignaledCondition(@NotNull Condition condition, boolean isFatal) {
}
