// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.cli;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.concurrent.locks.ReentrantLock;
import greenspun.util.SneakyThrow;
import org.checkerframework.checker.nullness.qual.NonNull;

final class Streams implements AutoCloseable {
    // The corresponding unlock is in close(), so this is fine.
    @SuppressWarnings("LockAcquiredButNotSafelyReleased")
    private Streams() {
        try {
            Holder.lock.lockInterruptibly();
        } catch (final InterruptedException e) {
            throw SneakyThrow.doThrow(e);
        }
    }

    static Streams acquire() {
        return new Streams();
    }

    @Override
    public void close() {
        Holder.lock.unlock();
    }

    @SuppressWarnings({"MethodMayBeStatic", "SameReturnValue", "UseOfSystemOutOrSystemErr"})
    PrintStream out() {
        return System.out;
    }

    @SuppressWarnings({"MethodMayBeStatic", "SameReturnValue", "UseOfSystemOutOrSystemErr"})
    PrintStream err() {
        return System.err;
    }

    @SuppressWarnings("MethodMayBeStatic")
    BufferedReader in() {
        return Holder.inputReader;
    }

    // Put the state in a separate class for lazy initialization.
    private static final class Holder {
        private static Charset getInputCharset() {
            final var console = System.console();
            return (console == null) ? Charset.defaultCharset() : console.charset();
        }

        private static final ReentrantLock lock = new ReentrantLock();
        private static final BufferedReader inputReader =
            new BufferedReader(new InputStreamReader(new StandardInput(), getInputCharset()));
    }

    private static final class StandardInput extends InputStream {
        @Override
        public int read(final byte @NonNull [] bytes) throws IOException {
            return System.in.read(bytes);
        }

        @Override
        public int read(final byte @NonNull [] bytes, final int offset, final int length) throws IOException {
            return System.in.read(bytes, offset, length);
        }

        @Override
        public int available() throws IOException {
            return System.in.available();
        }

        @Override
        public void close() throws IOException {
            System.in.close();
        }

        @Override
        public int read() throws IOException {
            return System.in.read();
        }
    }
}
