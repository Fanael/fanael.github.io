// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import java.util.ArrayList;
import org.jetbrains.annotations.NotNull;

/**
 * A utility class containing miscellaneous operations on {@link Iterable} objects.
 */
public final class IterableUtils {
    private IterableUtils() {
    }

    /**
     * Returns an {@link ArrayList} containing the objects returned by the given iterable's iterator, in the same order
     * they were returned in.
     */
    public static <T> @NotNull ArrayList<T> toList(final @NotNull Iterable<? extends T> iterable) {
        final var list = new ArrayList<T>();
        for (final var element : iterable) {
            list.add(element);
        }
        return list;
    }
}
