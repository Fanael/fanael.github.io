// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.pygments;

import greenspun.util.condition.Condition;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A condition type indicating that the Pygments server has reported an error.
 * The detailed error message, if any, is available through the {@link #detailedMessage()} method.
 */
public final class PygmentsServerErrorCondition extends Condition {
    PygmentsServerErrorCondition(final String message) {
        super(message);
        detailedMessage = null;
    }

    @SuppressWarnings("SameParameterValue")
    PygmentsServerErrorCondition(final String message, final @NonNull String detailedMessage) {
        super(message);
        this.detailedMessage = detailedMessage;
    }

    @Override
    public String detailedMessage() {
        return (detailedMessage != null)
            ? (message() + '\n' + detailedMessage)
            : message();
    }

    private final @Nullable String detailedMessage;
}
