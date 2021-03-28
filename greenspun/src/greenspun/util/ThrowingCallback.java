// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util;

/**
 * A generic callback interface that takes no arguments, returns void and is allowed to throw arbitrary exceptions.
 */
@FunctionalInterface
public interface ThrowingCallback<E extends Exception> {
    void call() throws E;
}
