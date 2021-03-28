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
        stillActive = false;
    }

    @Override
    public @NotNull List<Runnable> shutdownNow() {
        stillActive = false;
        return Collections.emptyList();
    }

    @Override
    public boolean isShutdown() {
        return !stillActive;
    }

    @Override
    public boolean isTerminated() {
        return stillActive;
    }

    @Override
    public boolean awaitTermination(final long timeout, final @NotNull TimeUnit unit) {
        return true;
    }

    @Override
    public void execute(final @NotNull Runnable command) {
        command.run();
    }

    private boolean stillActive = true;
}
