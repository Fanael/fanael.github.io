// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp.reader;

import greenspun.util.condition.Condition;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type indicating that the Lisp source could not be parsed.
 */
public final class ReadErrorCondition extends Condition {
    /**
     * Initializes a new read error with the given user-readable message and associated error location.
     */
    ReadErrorCondition(final @NotNull String rawMessage, final @NotNull SourceLocation location) {
        super(rawMessage);
        sourceLocation = location;
    }

    @Override
    public @NotNull String detailedMessage() {
        return message() + '\n' + sourceLocation;
    }

    private final @NotNull SourceLocation sourceLocation;
}
