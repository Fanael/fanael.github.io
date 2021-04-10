// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.sexp.reader;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import greenspun.sexp.Sexp;
import greenspun.sexp.SymbolTable;
import greenspun.util.UnreachableCodeReachedError;
import greenspun.util.collections.ImmutableList;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;
import greenspun.util.condition.Unwind;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The S-expression reader: the primary means of converting a stream of bytes into a stream of {@link Sexp} objects.
 */
public final class Reader {
    /**
     * Initializes a new S-expression reader that will read bytes from the given stream.
     * <p>
     * All symbols read will be interned into the given symbol table.
     * <p>
     * Note that the input stream is internally buffered to provide lookahead, so the passed stream doesn't need to be
     * buffered itself.
     */
    public Reader(final @NotNull InputStream stream, final @NotNull SymbolTable symbolTable) {
        buffer = new ReadBuffer(stream);
        this.symbolTable = symbolTable;
    }

    /**
     * Attempts to parse the next top-level S-expression.
     *
     * <ul>
     * <li>If an S-expression was correctly parsed, its object representation in the form of a {@link Sexp} is returned.
     * <li>If the end of input is reached, {@code null} is returned.
     * <li>If a parse error occurs, a fatal {@link ReadErrorCondition} condition is signaled.
     * <li>If an I/O error occurs, the {@link IOException} is caught and signaled as a fatal
     * {@link IOExceptionCondition}.
     * </ul>
     */
    public @Nullable Sexp readTopLevelForm() throws Unwind {
        if (skipSkippables().hitEof()) {
            return null;
        }
        topLevelFormLine = lineNumber;
        currentDepth = 0;
        return readForm();
    }

    private @NotNull HitEof skipSkippables() throws Unwind {
        while (true) {
            final var maybeByte = buffer.peekByte();
            if (maybeByte == endOfInput) {
                return HitEof.YES;
            }
            final var b = (byte) maybeByte;
            if (ByteClass.of(b) != ByteClass.SKIPPABLE) {
                return HitEof.NO;
            }
            buffer.discardPeek();
            switch (b) {
                case '\n' -> lineNumber += 1;
                case ';' -> {
                    if (buffer.skipToLineFeed().hitEof()) {
                        return HitEof.YES;
                    }
                }
            }
        }
    }

    private @Nullable Sexp readForm() throws Unwind {
        currentDepth += 1;
        try {
            if (currentDepth > maxDepth) {
                throw signalReadError("Recursion limit reached, try to limit nesting");
            }

            final var maybeByte = buffer.peekByte();
            if (maybeByte == endOfInput) {
                return null;
            }
            final var b = (byte) maybeByte;
            switch (ByteClass.of(b)) {
                case RESERVED -> throw signalReservedCharacterError(b);
                case SKIPPABLE -> throw new UnreachableCodeReachedError(
                    "readForm called without preceding skipSkippables");
            }
            buffer.discardPeek();

            return switch (b) {
                case ')' -> throw signalReadError("Expected a form, but found ')' instead");
                case '(' -> readList();
                case '"' -> readString();
                default -> readSymbol(b);
            };
        } finally {
            currentDepth -= 1;
        }
    }

    private @NotNull Sexp.List readList() throws Unwind {
        final var list = new ArrayList<@NotNull Sexp>(initialListCapacity);
        while (true) {
            if (skipSkippables().hitEof()) {
                throw signalUnterminatedListError();
            }
            final var maybeByte = buffer.peekByte();
            if (maybeByte == endOfInput) {
                throw signalUnterminatedListError();
            }
            if (maybeByte == ')') {
                buffer.discardPeek();
                break;
            }
            final var form = readForm();
            if (form != null) {
                list.add(form);
            } else {
                throw signalUnterminatedListError();
            }
        }
        return new Sexp.List(ImmutableList.freeze(list));
    }

    private @NotNull Sexp.String readString() throws Unwind {
        final var contentsBytes = new ByteArrayOutputStream(initialStringCapacity);
        var inEscapeSequence = false;
        outerLoop:
        while (true) {
            final var maybeByte = buffer.peekByte();
            if (maybeByte == endOfInput) {
                throw signalReadError("Expected closing '\"' but found end of input instead");
            }
            buffer.discardPeek();
            final var b = (byte) maybeByte;
            if (b == '\n') {
                lineNumber += 1;
            }
            if (inEscapeSequence) {
                inEscapeSequence = false;
                contentsBytes.write(b);
            } else {
                switch (b) {
                    case '"' -> {
                        break outerLoop;
                    }
                    case '\\' -> inEscapeSequence = true;
                    default -> contentsBytes.write(b);
                }
            }
        }

        return new Sexp.String(convertUtf8(contentsBytes.toByteArray()));
    }

    private @NotNull Sexp readSymbol(final byte firstByte) throws Unwind {
        final var symbolNameBytes = new ByteArrayOutputStream(initialSymbolCapacity);
        symbolNameBytes.write(firstByte);
        while (true) {
            final var maybeByte = buffer.peekByte();
            if (maybeByte == endOfInput) {
                break;
            }
            final var b = (byte) maybeByte;
            if (ByteClass.of(b) != ByteClass.REGULAR) {
                break;
            }
            buffer.discardPeek();
            symbolNameBytes.write(b);
        }
        final var symbolName = convertUtf8(symbolNameBytes.toByteArray());
        return resolveSymbol(symbolName);
    }

    private @NotNull Sexp resolveSymbol(final @NotNull String symbolName) {
        final var isNumeric = (symbolName.startsWith("+") || symbolName.startsWith("-"))
            ? allAsciiDigits(symbolName, 1)
            : allAsciiDigits(symbolName, 0);
        if (isNumeric) {
            try {
                return new Sexp.Integer(new BigInteger(symbolName));
            } catch (final NumberFormatException e) {
                // This should never happen, because the string has already been confirmed to consist only of an
                // optional sign symbol followed by ASCII digits.
                throw new UnreachableCodeReachedError();
            }
        } else {
            return symbolTable.intern(symbolName);
        }
    }

    private @NotNull String convertUtf8(final byte[] bytes) throws Unwind {
        try {
            final var decoder = StandardCharsets.UTF_8.newDecoder();
            decoder.onMalformedInput(CodingErrorAction.REPORT);
            decoder.onUnmappableCharacter(CodingErrorAction.REPORT);
            final var charBuffer = decoder.decode(ByteBuffer.wrap(bytes));
            return charBuffer.toString();
        } catch (final CharacterCodingException e) {
            throw signalReadError("Invalid UTF-8 byte sequence detected");
        }
    }

    private @NotNull UnhandledErrorError signalUnterminatedListError() throws Unwind {
        throw signalReadError("Expected closing ')' but found end of input instead");
    }

    private @NotNull UnhandledErrorError signalReservedCharacterError(final byte b) throws Unwind {
        final var message = (b <= lastControlByte)
            ? String.format("Reserved control character U+%04X found", b)
            : ("Reserved character '" + (char) b + "' found");
        throw signalReadError(message);
    }

    private @NotNull UnhandledErrorError signalReadError(final @NotNull String message) throws Unwind {
        throw ConditionContext.error(new ReadErrorCondition(message, new SourceLocation(lineNumber, topLevelFormLine)));
    }

    private static boolean allAsciiDigits(final @NotNull String string, final int startIndex) {
        final var length = string.length();
        for (int i = startIndex; i < length; i += 1) {
            final var ch = string.charAt(i);
            if (ch < '0' || ch > '9') {
                return false;
            }
        }
        return true;
    }

    private static final byte lastControlByte = 0x1F;
    private static final int endOfInput = -1;
    private static final int initialListCapacity = 8;
    private static final int initialStringCapacity = 256;
    private static final int initialSymbolCapacity = 32;
    private static final int maxDepth = 150;
    private final @NotNull ReadBuffer buffer;
    private final @NotNull SymbolTable symbolTable;
    private int lineNumber = 1;
    private int topLevelFormLine = 0;
    private int currentDepth = 0;

    private static final class ReadBuffer {
        private ReadBuffer(final @NotNull InputStream stream) {
            this.stream = stream;
        }

        private int peekByte() throws Unwind {
            return (position < bufferSize) ? Byte.toUnsignedInt(buffer[position]) : refill();
        }

        private void discardPeek() {
            position += 1;
        }

        private @NotNull HitEof skipToLineFeed() throws Unwind {
            while (true) {
                if (position < bufferSize) {
                    final var lineFeedPosition = findLineFeed();
                    if (lineFeedPosition != -1) {
                        position = lineFeedPosition;
                        return HitEof.NO;
                    }
                }
                if (refill() == endOfInput) {
                    return HitEof.YES;
                }
            }
        }

        private int findLineFeed() {
            for (int i = position; i < bufferSize; i += 1) {
                if (buffer[i] == '\n') {
                    return i;
                }
            }
            return -1;
        }

        private int refill() throws Unwind {
            try {
                final var numberOfBytesRead = stream.read(buffer);
                if (numberOfBytesRead > 0) {
                    position = 0;
                    bufferSize = numberOfBytesRead;
                    return Byte.toUnsignedInt(buffer[0]);
                } else {
                    return endOfInput;
                }
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }

        private static final int bufferCapacity = 8192;

        private final @NotNull InputStream stream;
        private final byte[] buffer = new byte[bufferCapacity];
        private int position = 0;
        private int bufferSize = 0;
    }

    private enum HitEof {
        NO(false),
        YES(true);

        HitEof(final boolean booleanValue) {
            this.booleanValue = booleanValue;
        }

        private boolean hitEof() {
            return booleanValue;
        }

        private final boolean booleanValue;
    }

    private enum ByteClass {
        REGULAR,
        SKIPPABLE,
        SEPARATOR,
        RESERVED;

        private static @NotNull ByteClass of(final byte b) {
            return byteClasses[Byte.toUnsignedInt(b)];
        }

        private static final @NotNull ByteClass[] byteClasses;

        static {
            final var classes = new ByteClass[256];
            Arrays.fill(classes, REGULAR);
            classes[' '] = SKIPPABLE;
            classes['\r'] = SKIPPABLE;
            classes['\n'] = SKIPPABLE;
            classes['\t'] = SKIPPABLE;
            classes['\u000B'] = SKIPPABLE;
            classes['\u000C'] = SKIPPABLE;
            classes[';'] = SKIPPABLE;
            classes['('] = SEPARATOR;
            classes[')'] = SEPARATOR;
            classes['"'] = SEPARATOR;
            classes['\''] = RESERVED;
            classes['#'] = RESERVED;
            classes['|'] = RESERVED;
            classes['\\'] = RESERVED;
            classes['\0'] = RESERVED;
            byteClasses = classes;
        }
    }
}
