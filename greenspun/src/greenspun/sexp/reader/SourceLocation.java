// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.sexp.reader;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;

@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
record SourceLocation(int lineNumber, int topLevelFormLine) {
    @Override
    public @NotNull String toString() {
        return "In line " + lineNumber + ", within top-level form starting at line " + topLevelFormLine;
    }
}
