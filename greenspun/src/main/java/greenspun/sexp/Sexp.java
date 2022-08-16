// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp;

import java.math.BigInteger;
import greenspun.util.collection.seq.Seq;

/**
 * Marker interface serving as the base type of S-expression objects.
 * <p>
 * S-expression objects are guaranteed to be immutable.
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
        java.lang.String symbolName();
    }

    /**
     * An S-expression integer object.
     */
    record Integer(BigInteger value) implements Sexp {
    }

    /**
     * An S-expression list object.
     * <p>
     * Dotted lists are unsupported.
     */
    record List(Seq<Sexp> value) implements Sexp {
    }

    /**
     * An S-expression string object.
     */
    record String(java.lang.String value) implements Sexp {
    }

    /**
     * A regular Lisp symbol, not directly used by Java code.
     */
    final class RegularSymbol implements Sexp.Symbol {
        /**
         * Initializes a new, <em>uninterned</em> symbol.
         * <p>
         * Direct used of this constructor is usually not needed, prefer {@link SymbolTable#intern(java.lang.String)}
         * instead.
         */
        public RegularSymbol(final java.lang.String name) {
            this.name = name;
        }

        @Override
        public java.lang.String symbolName() {
            return name;
        }

        @Override
        public java.lang.String toString() {
            return name;
        }

        private final java.lang.String name;
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
        CODE_BLOCK("code-block"),
        HIGHLIGHTED_CODE("highlighted-code"),
        IMAGE_FIGURE("image-figure"),
        SIDENOTE("sidenote"),
        FIGCONTENT("figcontent"),
        INFO_BOX("info-box"),
        KW_LANGUAGE(":language");

        KnownSymbol(final java.lang.String name) {
            this.name = name;
        }

        @Override
        public java.lang.String symbolName() {
            return name;
        }

        @Override
        public java.lang.String toString() {
            return name;
        }

        private final java.lang.String name;
    }
}
