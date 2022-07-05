// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

sealed interface ChildContext permits Context, ChildContext.None, ChildContext.Transparent {
    static None none() {
        return None.instance;
    }

    static Transparent transparent() {
        return Transparent.instance;
    }

    final class None implements ChildContext {
        private None() {
        }

        private static final None instance = new None();
    }

    final class Transparent implements ChildContext {
        private Transparent() {
        }

        private static final Transparent instance = new Transparent();
    }
}
