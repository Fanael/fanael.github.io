// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp.reader;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import greenspun.sexp.Sexp;
import greenspun.sexp.SymbolTable;
import greenspun.util.UnreachableCodeReachedError;
import greenspun.util.collection.ImmutableList;
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
     * Initializes a new S-expression reader that will read bytes from the given byte stream.
     * <p>
     * All symbols read will be interned into the given symbol table.
     */
    public Reader(final @NotNull ByteStream stream, final @NotNull SymbolTable symbolTable) {
        this.stream = stream;
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
            if (stream.reachedEnd()) {
                return HitEof.YES;
            }
            final var b = stream.peek();
            if (ByteClass.of(b) != ByteClass.SKIPPABLE) {
                return HitEof.NO;
            }
            stream.discardPeek();
            switch (b) {
                case '\n' -> lineNumber += 1;
                case ';' -> {
                    if (stream.skipToLineFeed().hitEof()) {
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

            if (stream.reachedEnd()) {
                return null;
            }
            final var b = stream.peek();
            switch (ByteClass.of(b)) {
                case RESERVED -> throw signalReservedCharacterError(b);
                case SKIPPABLE -> throw new UnreachableCodeReachedError(
                    "readForm called without preceding skipSkippables");
            }
            stream.discardPeek();

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
            if (stream.reachedEnd()) {
                throw signalUnterminatedListError();
            }
            if (stream.peek() == ')') {
                stream.discardPeek();
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
            if (stream.reachedEnd()) {
                throw signalReadError("Expected closing '\"' but found end of input instead");
            }
            final var b = stream.peek();
            stream.discardPeek();
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
            if (stream.reachedEnd()) {
                break;
            }
            final var b = stream.peek();
            if (ByteClass.of(b) != ByteClass.REGULAR) {
                break;
            }
            stream.discardPeek();
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
            return utf8Decoder.decode(ByteBuffer.wrap(bytes)).toString();
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

    private static @NotNull CharsetDecoder newUtf8Decoder() {
        final var decoder = StandardCharsets.UTF_8.newDecoder();
        decoder.onMalformedInput(CodingErrorAction.REPORT);
        decoder.onUnmappableCharacter(CodingErrorAction.REPORT);
        return decoder;
    }

    private static final byte lastControlByte = 0x1F;
    private static final int initialListCapacity = 8;
    private static final int initialStringCapacity = 256;
    private static final int initialSymbolCapacity = 16;
    private static final int maxDepth = 150;

    private final @NotNull ByteStream stream;
    private final @NotNull SymbolTable symbolTable;
    private final @NotNull CharsetDecoder utf8Decoder = newUtf8Decoder();
    private int lineNumber = 1;
    private int topLevelFormLine = 0;
    private int currentDepth = 0;

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
