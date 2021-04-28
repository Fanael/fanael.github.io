// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.sexp.reader;

import org.jetbrains.annotations.NotNull;

record SourceLocation(int lineNumber, int topLevelFormLine) {
    @Override
    public @NotNull String toString() {
        return "In line " + lineNumber + ", within top-level form starting at line " + topLevelFormLine;
    }
}
