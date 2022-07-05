// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp;

import java.math.BigInteger;
import greenspun.util.annotation.Nullable;
import greenspun.util.collection.seq.Seq;

/**
 * A utility class containing common operations on S-expressions.
 */
public final class Sexps {
    private Sexps() {
    }

    /**
     * Returns the integer the given {@code sexp} represents, or {@code null} if it doesn't represent an integer.
     */
    public static @Nullable BigInteger asInteger(final Sexp sexp) {
        return (sexp instanceof Sexp.Integer integer) ? integer.value() : null;
    }

    /**
     * Returns the string the given {@code sexp} represents, or {@code null} if it doesn't represent a string.
     */
    public static @Nullable String asString(final Sexp sexp) {
        return (sexp instanceof Sexp.String string) ? string.value() : null;
    }

    /**
     * Returns the list the given {@code sexp} represents, or {@code null} if it doesn't represent a list.
     * <p>
     * Note that the known symbol {@code nil} represents an empty list.
     */
    public static @Nullable Seq<Sexp> asList(final Sexp sexp) {
        if (sexp instanceof Sexp.List list) {
            return list.value();
        } else if (sexp == Sexp.KnownSymbol.NIL) {
            return Seq.empty();
        } else {
            return null;
        }
    }

    /**
     * Returns the symbol the given {@code sexp} represents, or {@code null} if it doesn't represent a symbol.
     * <p>
     * Note that empty lists represent the known symbol {@code nil}.
     */
    public static @Nullable Sexp.Symbol asSymbol(final Sexp sexp) {
        return switch (sexp) {
            case Sexp.Symbol symbol -> symbol;
            case Sexp.List list && list.value().isEmpty() -> Sexp.KnownSymbol.NIL;
            default -> null;
        };
    }

    /**
     * Returns the passed sexp iff the given {@code symbol} represents a Lisp keyword, or {@code null} otherwise.
     * <p>
     * A Lisp keyword is any symbol whose name begins with a colon.
     */
    public static @Nullable Sexp.Symbol asKeyword(final Sexp sexp) {
        return (sexp instanceof Sexp.Symbol symbol && symbol.symbolName().startsWith(":")) ? symbol : null;
    }

    /**
     * Returns {@code true} iff the given {@code sexp} represents the symbol {@code nil}.
     * <p>
     * Note that empty lists represent {@code nil}.
     */
    public static boolean isNil(final Sexp sexp) {
        return sexp == Sexp.KnownSymbol.NIL || (sexp instanceof Sexp.List list && list.value().isEmpty());
    }

    /**
     * Pretty-prints the given S-expression into a string. Intended primarily for debugging.
     */
    public static String prettyPrint(final Sexp sexp) {
        final var prettyPrinter = new PrettyPrinter();
        prettyPrinter.prettyPrint(sexp);
        return prettyPrinter.builder.toString();
    }

    private static final class PrettyPrinter {
        private void prettyPrint(final Sexp sexp) {
            appendDispatch(sexp, 1);
        }

        private void appendDispatch(final Sexp sexp, final int level) {
            switch (sexp) {
                case Sexp.Integer integer -> append(integer.value());
                case Sexp.String string -> append(string.value());
                case Sexp.List list -> append(list.value(), level);
                case Sexp.Symbol symbol -> append(symbol);
            }
        }

        private void append(final BigInteger integer) {
            builder.append(integer);
        }

        private void append(final String string) {
            final var replaced = string.replace("\\", "\\\\").replace("\"", "\\\"");
            builder.append('"');
            builder.append(replaced);
            builder.append('"');
        }

        private void append(final Seq<Sexp> list, final int level) {
            final var iterator = list.iterator();
            if (!iterator.hasNext()) {
                builder.append("()");
                return;
            }
            builder.append('(');
            appendDispatch(iterator.next(), level + 1);
            final var indentation = " ".repeat(level);
            while (iterator.hasNext()) {
                builder.append('\n');
                builder.append(indentation);
                appendDispatch(iterator.next(), level + 1);
            }
            builder.append(')');
        }

        private void append(final Sexp.Symbol symbol) {
            builder.append(symbol.symbolName());
        }

        private final StringBuilder builder = new StringBuilder();
    }
}
