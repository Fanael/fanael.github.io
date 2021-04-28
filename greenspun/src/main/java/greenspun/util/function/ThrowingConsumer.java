// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.function;

/**
 * Like {@link java.util.function.Consumer}, but allowed to throw any throwable of type {@code E}.
 */
@FunctionalInterface
public interface ThrowingConsumer<T, E extends Throwable> {
    void accept(T value) throws E;
}
