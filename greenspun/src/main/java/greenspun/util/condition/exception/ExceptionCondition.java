// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition.exception;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import greenspun.auki.annotations.Open;
import greenspun.util.condition.Condition;
import org.jetbrains.annotations.NotNull;

abstract class ExceptionCondition<E extends Exception> extends Condition {
    ExceptionCondition(final @NotNull E exception) {
        super(String.valueOf(exception.getMessage()));
        this.exception = exception;
    }

    @Open
    @Override
    public @NotNull String detailedMessage() {
        final var charset = StandardCharsets.UTF_8;
        final var stream = new ByteArrayOutputStream();
        final var printStream = new PrintStream(stream, false, charset);
        exception.printStackTrace(printStream);
        printStream.flush();
        return stream.toString(charset);
    }

    @Open
    @Override
    public @NotNull String toString() {
        return getClass().getName() + ": " + exception;
    }

    private final @NotNull E exception;
}
