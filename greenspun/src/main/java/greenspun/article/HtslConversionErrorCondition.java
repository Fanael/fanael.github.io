// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.article;

import greenspun.util.condition.Condition;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type indicating that an HTSL form could not be successfully converted into DOM nodes.
 */
public final class HtslConversionErrorCondition extends Condition {
    HtslConversionErrorCondition(final @NotNull String message) {
        super(message);
    }
}
