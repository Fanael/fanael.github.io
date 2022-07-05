// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp.reader;

import greenspun.util.condition.Condition;

/**
 * A condition type indicating that the Lisp source could not be parsed.
 */
public final class ReadErrorCondition extends Condition {
    /**
     * Initializes a new read error with the given user-readable message and associated error location.
     */
    ReadErrorCondition(final String rawMessage, final SourceLocation location) {
        super(rawMessage);
        sourceLocation = location;
    }

    @Override
    public String detailedMessage() {
        return message() + '\n' + sourceLocation;
    }

    private final SourceLocation sourceLocation;
}
