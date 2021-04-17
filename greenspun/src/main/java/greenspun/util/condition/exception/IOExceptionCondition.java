// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.condition.exception;

import java.io.IOException;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type indicating that an I/O error of some error occurred.
 */
public final class IOExceptionCondition extends ExceptionCondition<IOException> {
    /**
     * Initializes a new {@code IOExceptionCondition} representing the given {@link IOException}.
     */
    public IOExceptionCondition(final @NotNull IOException exception) {
        super(exception);
    }
}
