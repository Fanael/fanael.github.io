// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.article;

import greenspun.util.condition.Condition;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type indicating that the {@link Parser} could not turn S-expressions into an {@link Article}.
 */
public final class ArticleParseErrorCondition extends Condition {
    ArticleParseErrorCondition(final @NotNull String message) {
        super(message);
    }
}
