// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
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
     * Returns an array containing the objects returned by the given iterable's iterator, in the same order they were
     * returned in.
     * <p>
     * The runtime type of the array is the same as that of the passed array. If the passed array is large enough to fit
     * all objects, they're returned therein; otherwise a new array of the same runtime type and necessary size is
     * allocated.
     */
    public static <T> T @NotNull [] toArray(final @NotNull Iterable<? extends T> iterable, final T @NotNull [] array) {
        final var list = new ArrayList<T>();
        for (final var element : iterable) {
            list.add(element);
        }
        return list.toArray(array);
    }
}
