// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import org.jetbrains.annotations.NotNull;

sealed interface ChildContext permits Context, ChildContext.None, ChildContext.Transparent {
    static @NotNull None none() {
        return None.instance;
    }

    static @NotNull Transparent transparent() {
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
