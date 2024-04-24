// Copyright © 2021-2024  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import greenspun.util.condition.Condition;

/**
 * A condition type indicating that an article was parsed successfully, but its section graph is malformed in some way.
 */
public final class SectionLinkingErrorCondition extends Condition {
    SectionLinkingErrorCondition(final String message) {
        super(message);
    }
}
