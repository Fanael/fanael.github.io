// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.test;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.TimeUnit;
import org.jetbrains.annotations.NotNull;

final class TestExecutorService extends AbstractExecutorService {
    @Override
    public void shutdown() {
        terminated = true;
    }

    @Override
    public @NotNull List<Runnable> shutdownNow() {
        terminated = true;
        return Collections.emptyList();
    }

    @Override
    public boolean isShutdown() {
        return terminated;
    }

    @Override
    public boolean isTerminated() {
        return terminated;
    }

    @Override
    public boolean awaitTermination(final long timeout, final @NotNull TimeUnit unit) {
        assert terminated;
        return true;
    }

    @Override
    public void execute(final @NotNull Runnable command) {
        command.run();
    }

    private boolean terminated = false;
}
