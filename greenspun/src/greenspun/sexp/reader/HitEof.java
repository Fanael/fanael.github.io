// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.sexp.reader;

enum HitEof {
    NO,
    YES;

    boolean hitEof() {
        return this == YES;
    }
}
