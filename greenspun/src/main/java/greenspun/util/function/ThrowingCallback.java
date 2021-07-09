// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.function;

/**
 * A generic callback interface that takes no arguments, returns void and is allowed to throw arbitrary exceptions.
 */
@FunctionalInterface
public interface ThrowingCallback<E extends Throwable> {
    void call() throws E;
}
