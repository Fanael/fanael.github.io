// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import org.jetbrains.annotations.NotNull;

/**
 * Error type signifying that control flow reached a point that should be unreachable.
 * <p>
 * Since this represents a programming error, this class extends {@link AssertionError}.
 */
public final class UnreachableCodeReachedError extends AssertionError {
    public UnreachableCodeReachedError() {
        super("Execution reached a point expected to be unreachable");
    }

    public UnreachableCodeReachedError(final @NotNull String message) {
        super(message);
    }
}
