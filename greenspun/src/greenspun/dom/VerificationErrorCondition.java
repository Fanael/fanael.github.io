// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import java.util.List;
import greenspun.util.condition.Condition;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type representing a failed DOM tree verification.
 * <p>
 * The associated detailed message contains the list of errors formatted in human-readable way.
 */
public final class VerificationErrorCondition extends Condition {
    VerificationErrorCondition(final @NotNull List<@NotNull VerificationError> errors) {
        super("DOM verification failed");
        this.errors = errors;
    }

    @Override
    public @NotNull String detailedMessage() {
        final var builder = new StringBuilder(message());
        for (final var error : errors) {
            builder.append("\n - ");
            builder.append(error.message());
            builder.append("\n   Ancestors (from outermost):");
            for (final var ancestor : error.ancestorTags()) {
                builder.append("\n    - ");
                builder.append(ancestor.htmlName());
            }
        }
        return builder.toString();
    }

    private final @NotNull List<@NotNull VerificationError> errors;
}
