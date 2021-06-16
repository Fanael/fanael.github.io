// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.sexp;

import java.util.concurrent.ConcurrentHashMap;
import org.jetbrains.annotations.NotNull;

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
     * Produces a canonical representation of the given symbol in this table.
     * <p>
     * If the symbol name refers to a known symbol, or a symbol with that name already exists in the table, a reference
     * to that existing symbol is returned. Otherwise, a new symbol is created and recorded as the canonical
     * representation of that symbol, that will be returned by future calls with the same symbol name.
     */
    public @NotNull Sexp.Symbol intern(final @NotNull String symbolName) {
        final var knownSymbol = Sexp.KnownSymbol.byName(symbolName);
        if (knownSymbol != null) {
            return knownSymbol;
        }
        return symbols.computeIfAbsent(symbolName, Sexp.RegularSymbol::new);
    }

    private final ConcurrentHashMap<String, Sexp.RegularSymbol> symbols = new ConcurrentHashMap<>();
}
