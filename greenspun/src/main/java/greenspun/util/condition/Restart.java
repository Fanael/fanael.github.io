// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A restart point.
 * <p>
 * At any point, the program can iterate over the active restarts and transfer the control flow to any of them using
 * {@link ConditionContext#restarts()}.
 */
public final class Restart {
    Restart(final @NotNull String name) {
        final var context = ConditionContext.localContext();
        next = context.firstRestart;
        context.firstRestart = this;
        this.name = name;
    }

    /**
     * Retrieves the user-readable name of this restart point.
     */
    public @NotNull String name() {
        return name;
    }

    /**
     * Transfers the control flow to this restart point.
     * <p>
     * This method never returns.
     *
     * @throws Unwind The exception used to perform non-local control flow transfer.
     */
    public void unwindTo() throws Unwind {
        throw new Unwind(this);
    }

    // NB: since restarts are only created by the condition context and not user code, this can be a package-private
    // method instead of public close required by AutoCloseable.
    void unlink() {
        final var context = ConditionContext.localContext();
        assert context.firstRestart == this : "Restart chain corrupt";
        context.firstRestart = next;
    }

    final @Nullable Restart next;
    private final @NotNull String name;
}
