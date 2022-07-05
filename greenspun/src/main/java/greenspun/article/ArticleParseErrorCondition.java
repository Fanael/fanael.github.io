// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import greenspun.util.condition.Condition;

/**
 * A condition type indicating that the {@link Parser} could not turn S-expressions into an {@link Article}.
 */
public final class ArticleParseErrorCondition extends Condition {
    ArticleParseErrorCondition(final String message) {
        super(message);
    }
}
