// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

/**
 * A convenience record bundling together the condition being signaled and auxiliary information.
 *
 * @param condition The condition being signaled.
 * @param isFatal   {@code true} iff the condition was signaled with {@link ConditionContext#error(Condition)}.
 */
public record SignaledCondition(Condition condition, boolean isFatal) {
}
