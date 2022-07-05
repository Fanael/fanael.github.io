// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition.exception;

import java.io.IOException;

/**
 * A condition type indicating that an I/O error of some error occurred.
 */
public final class IOExceptionCondition extends ExceptionCondition<IOException> {
    /**
     * Initializes a new {@code IOExceptionCondition} representing the given {@link IOException}.
     */
    public IOExceptionCondition(final IOException exception) {
        super(exception);
    }
}
