// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.function;

/**
 * Like {@link java.util.function.Function}, but allowed to throw any throwable of type {@code E}.
 */
@FunctionalInterface
public interface ThrowingFunction<T, R, E extends Throwable> {
    R apply(T value) throws E;
}
