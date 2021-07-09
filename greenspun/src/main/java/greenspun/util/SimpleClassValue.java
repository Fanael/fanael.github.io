// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import java.util.function.Function;
import org.jetbrains.annotations.NotNull;

/**
 * A simple implementation of {@link ClassValue} that uses a {@link Function} to compute the associated value.
 * <p>
 * The use of this class is typically more convenient than subclassing {@link ClassValue} to override
 * {@link ClassValue#computeValue(Class)}.
 */
public final class SimpleClassValue<T> extends ClassValue<T> {
    /**
     * Initializes a new {@code SimpleClassValue} that uses the given function to compute the associated value.
     */
    public SimpleClassValue(final @NotNull Function<? super @NotNull Class<?>, ? extends T> function) {
        this.function = function;
    }

    @Override
    protected T computeValue(final @NotNull Class<?> clazz) {
        return function.apply(clazz);
    }

    private final @NotNull Function<? super @NotNull Class<?>, ? extends T> function;
}
