// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition.exception;

import javax.xml.transform.TransformerException;

/**
 * A condition type indicating that there was an error during XML transformation.
 */
public final class TransformerExceptionCondition extends ExceptionCondition<TransformerException> {
    /**
     * Initializes a new {@code TransformerExceptionCondition} representing the given {@link TransformerException}.
     */
    public TransformerExceptionCondition(final TransformerException exception) {
        super(exception);
    }
}
