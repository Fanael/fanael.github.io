// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp;

import java.math.BigInteger;
import greenspun.util.UnreachableCodeReachedError;
import greenspun.util.collection.ImmutableList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A utility class containing common operations on S-expressions.
 */
public final class Sexps {
    private Sexps() {
    }

    /**
     * Returns the integer the given {@code sexp} represents, or {@code null} if it doesn't represent an integer.
     */
    public static @Nullable BigInteger asInteger(final @NotNull Sexp sexp) {
        return (sexp instanceof Sexp.Integer integer) ? integer.value() : null;
    }

    /**
     * Returns the string the given {@code sexp} represents, or {@code null} if it doesn't represent a string.
     */
    public static @Nullable String asString(final @NotNull Sexp sexp) {
        return (sexp instanceof Sexp.String string) ? string.value() : null;
    }

    /**
     * Returns the list the given {@code sexp} represents, or {@code null} if it doesn't represent a list.
     * <p>
     * Note that the known symbol {@code nil} represents an empty list.
     */
    public static @Nullable ImmutableList<Sexp> asList(final @NotNull Sexp sexp) {
        if (sexp instanceof Sexp.List list) {
            return list.value();
        } else if (sexp == Sexp.KnownSymbol.NIL) {
            return ImmutableList.empty();
        } else {
            return null;
        }
    }

    /**
     * Returns the symbol the given {@code sexp} represents, or {@code null} if it doesn't represent a symbol.
     * <p>
     * Note that empty lists represent the known symbol {@code nil}.
     */
    public static @Nullable Sexp.Symbol asSymbol(final @NotNull Sexp sexp) {
        if (sexp instanceof Sexp.Symbol symbol) {
            return symbol;
        } else if (sexp instanceof Sexp.List list) {
            return list.value().isEmpty() ? Sexp.KnownSymbol.NIL : null;
        } else {
            return null;
        }
    }

    /**
     * Returns the passed sexp iff the given {@code symbol} represents a Lisp keyword, or {@code null} otherwise.
     * <p>
     * A Lisp keyword is any symbol whose name begins with a colon.
     */
    public static @Nullable Sexp.Symbol asKeyword(final @NotNull Sexp sexp) {
        return (sexp instanceof Sexp.Symbol symbol && symbol.symbolName().startsWith(":")) ? symbol : null;
    }

    /**
     * Returns {@code true} iff the given {@code sexp} represents the symbol {@code nil}.
     * <p>
     * Note that empty lists represent {@code nil}.
     */
    public static boolean isNil(final @NotNull Sexp sexp) {
        return sexp == Sexp.KnownSymbol.NIL || (sexp instanceof Sexp.List list && list.value().isEmpty());
    }

    /**
     * Pretty-prints the given S-expression into a string. Intended primarily for debugging.
     */
    public static @NotNull String prettyPrint(final @NotNull Sexp sexp) {
        final var prettyPrinter = new PrettyPrinter();
        prettyPrinter.prettyPrint(sexp);
        return prettyPrinter.builder.toString();
    }

    private static final class PrettyPrinter {
        private void prettyPrint(final @NotNull Sexp sexp) {
            appendDispatch(sexp, 1);
        }

        private void appendDispatch(final @NotNull Sexp sexp, final int level) {
            if (sexp instanceof Sexp.Integer integer) {
                append(integer.value());
            } else if (sexp instanceof Sexp.String string) {
                append(string.value());
            } else if (sexp instanceof Sexp.List list) {
                append(list.value(), level);
            } else if (sexp instanceof Sexp.Symbol symbol) {
                append(symbol);
            } else {
                throw new UnreachableCodeReachedError();
            }
        }

        private void append(final @NotNull BigInteger integer) {
            builder.append(integer);
        }

        private void append(final @NotNull String string) {
            final var replaced = string.replace("\\", "\\\\").replace("\"", "\\\"");
            builder.append('"');
            builder.append(replaced);
            builder.append('"');
        }

        private void append(final @NotNull ImmutableList<Sexp> list, final int level) {
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

        private void append(final @NotNull Sexp.Symbol symbol) {
            builder.append(symbol.symbolName());
        }

        private final StringBuilder builder = new StringBuilder();
    }
}
