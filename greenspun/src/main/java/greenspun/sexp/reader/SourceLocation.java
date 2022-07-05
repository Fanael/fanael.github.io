// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp.reader;

record SourceLocation(int lineNumber, int topLevelFormLine) {
    @Override
    public String toString() {
        return "In line " + lineNumber + ", within top-level form starting at line " + topLevelFormLine;
    }
}
