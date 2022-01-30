// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;

/**
 * Throwable type used internally by the restart mechanism for transferring control flow to a given restart point.
 * <p>
 * Exposed so that functions can be marked as throwing {@code Unwind}. <strong>Catching or throwing objects of
 * this type manually is strongly discouraged</strong> except when necessary, for example to propagate a restart
 * through a thread boundary.
 * <p>
 * Since objects of this class are never intended to be thrown or caught, and it doesn't represent a serious
 * unrecoverable error, it doesn't extend either {@link Exception} nor {@link Error}, but rather {@link Throwable}
 * directly.
 * <p>
 * This is arguably an abuse of exceptions — they're intended for exceptional situations, not for control flow.
 * Unfortunately, this appears to be the least bad option for non-local control flow on the JVM.
 */
@SuppressWarnings("ExtendsThrowable")
public final class Unwind extends Throwable {
    Unwind(final @NotNull Restart target) {
        super("Unwinding to a restart point");
        this.target = target;
    }

    @NotNull Restart target() {
        return target;
    }

    // Mark it as transient mostly to silence static analysis warnings: unwinds are not intended to be serializable,
    // they just unfortunately inherit serializability from Throwable.
    private final transient @NotNull Restart target;
}
