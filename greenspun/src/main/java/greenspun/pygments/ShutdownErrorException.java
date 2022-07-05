// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.pygments;

/**
 * An exception type indicating that the Pygments server failed to shut down gracefully and had to be killed.
 * <p>
 * Never actually <em>thrown</em>, but signaled as a {@link greenspun.util.condition.SuppressedExceptionCondition}
 * when the Pygments server pool is shutting down.
 */
public final class ShutdownErrorException extends Exception {
    ShutdownErrorException(final String message) {
        super(message);
    }
}
