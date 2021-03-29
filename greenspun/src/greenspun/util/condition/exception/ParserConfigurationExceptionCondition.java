// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.condition.exception;

import javax.xml.parsers.ParserConfigurationException;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type indicating that there was a serious error with XML DOM parser configuration.
 */
public final class ParserConfigurationExceptionCondition extends ExceptionCondition<ParserConfigurationException> {
    /**
     * Initializes a new {@code ParserConfigurationExceptionCondition} representing the given
     * {@link ParserConfigurationException}.
     */
    public ParserConfigurationExceptionCondition(final @NotNull ParserConfigurationException exception) {
        super(exception);
    }
}
