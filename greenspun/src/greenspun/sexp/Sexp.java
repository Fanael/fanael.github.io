// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.sexp;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Marker interface serving as the base type of S-expression objects.
 */
public sealed interface Sexp {
    /**
     * Base interface for Lisp symbols.
     * <p>
     * Symbol equality is guaranteed to be the same as object identity.
     */
    sealed interface Symbol extends Sexp {
        /**
         * Retrieves the name of this symbol.
         */
        @NotNull java.lang.String symbolName();
    }

    /**
     * An S-expression integer object.
     */
    @SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
    record Integer(@NotNull BigInteger value) implements Sexp {
    }

    /**
     * An S-expression list object.
     * <p>
     * Dotted lists are unsupported.
     */
    @SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
    record List(@NotNull java.util.List<@NotNull Sexp> value) implements Sexp {
    }

    /**
     * An S-expression string object.
     */
    @SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
    record String(@NotNull java.lang.String value) implements Sexp {
    }

    /**
     * A regular Lisp symbol, not directly used by Java code.
     */
    @SuppressWarnings("ClassCanBeRecord") // It cannot be a record, it relies on equals() being identity comparison.
    final class RegularSymbol implements Sexp.Symbol {
        /**
         * Initializes a new, <em>uninterned</em> symbol.
         * <p>
         * Direct used of this constructor is usually not needed, prefer {@link SymbolTable#intern(java.lang.String)}
         * instead.
         */
        public RegularSymbol(final @NotNull java.lang.String name) {
            this.name = name;
        }

        @Override
        public @NotNull java.lang.String symbolName() {
            return name;
        }

        @Override
        public @NotNull java.lang.String toString() {
            return name;
        }

        private final @NotNull java.lang.String name;
    }

    /**
     * Lisp symbols directly used by Java code.
     */
    enum KnownSymbol implements Sexp.Symbol {
        NIL("nil"),
        T("t"),
        DEFARTICLE("defarticle"),
        DEFSECTION("defsection"),
        KW_CHILDREN(":children"),
        KW_DATE(":date"),
        KW_DESCRIPTION(":description"),
        KW_HEADER(":header"),
        KW_INHIBIT_TABLE_OF_CONTENTS(":inhibit-table-of-contents"),
        KW_TITLE(":title"),
        KW_TOPICS(":topics"),
        HIGHLIGHTED_CODE("highlighted-code"),
        IMAGE_FIGURE("image-figure"),
        KW_LANGUAGE(":language"),
        KW_CPLUSPLUS(":c++"),
        KW_COMMON_LISP(":common-lisp"),
        KW_DIFF(":diff"),
        KW_JAVA(":java");

        KnownSymbol(final @NotNull java.lang.String name) {
            this.name = name;
        }

        static @Nullable KnownSymbol byName(final @NotNull java.lang.String name) {
            return symbolsByName.get(name);
        }

        @Override
        public @NotNull java.lang.String symbolName() {
            return name;
        }

        @Override
        public @NotNull java.lang.String toString() {
            return name;
        }

        private static final Map<java.lang.String, KnownSymbol> symbolsByName =
            Arrays.stream(values()).collect(Collectors.toUnmodifiableMap(KnownSymbol::symbolName, Function.identity()));

        private final @NotNull java.lang.String name;
    }
}