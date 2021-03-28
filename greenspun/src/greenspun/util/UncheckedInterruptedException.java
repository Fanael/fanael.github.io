// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util;

import org.jetbrains.annotations.NotNull;

/**
 * A nasty hack that allows smuggling {@link InterruptedException} as an unchecked exception.
 */
public final class UncheckedInterruptedException extends RuntimeException {
    /**
     * Initializes a new unchecked interrupted exception with the given cause.
     */
    public UncheckedInterruptedException(final @NotNull InterruptedException cause) {
        super(cause);
    }

    /**
     * Retrieves the original {@link InterruptedException}.
     */
    public @NotNull InterruptedException getCause() {
        final var cause = super.getCause();
        assert cause instanceof InterruptedException;
        return (InterruptedException) cause;
    }
}
