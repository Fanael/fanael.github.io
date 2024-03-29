// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp;

import java.util.concurrent.ConcurrentHashMap;

/**
 * A table for interning Lisp symbols.
 * <p>
 * Interned symbols can be compared for equality using object identity, which is much faster than string comparison.
 * <p>
 * This class is thread-safe: multiple threads can safely intern symbols into the same symbol table at the same time
 * with no external synchronization.
 */
public final class SymbolTable {
    /**
     * Initializes an empty symbol table.
     */
    public SymbolTable() {
        // Populate the map with known symbols first so that trying to intern them returns the correct object.
        for (final var knownSymbol : Sexp.KnownSymbol.values()) {
            symbols.put(knownSymbol.symbolName(), knownSymbol);
        }
    }

    /**
     * Produces a canonical representation of the given symbol in this table.
     * <p>
     * If the symbol name refers to a known symbol, or a symbol with that name already exists in the table, a reference
     * to that existing symbol is returned. Otherwise, a new symbol is created and recorded as the canonical
     * representation of that symbol, that will be returned by future calls with the same symbol name.
     */
    public Sexp.Symbol intern(final String symbolName) {
        // Try retrieving the already-interned symbol first: retrievals are guaranteed to not entail any locking by CHM
        // javadoc, and symbols almost always already exist.
        final var symbol = symbols.get(symbolName);
        if (symbol != null) {
            return symbol;
        }
        return symbols.computeIfAbsent(symbolName, Sexp.RegularSymbol::new);
    }

    private final ConcurrentHashMap<String, Sexp.Symbol> symbols = new ConcurrentHashMap<>();
}
