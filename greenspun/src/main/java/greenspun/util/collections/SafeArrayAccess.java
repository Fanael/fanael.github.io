// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util.collections;

import java.util.ArrayList;
import java.util.Collection;
import greenspun.util.SimpleClassValue;
import org.jetbrains.annotations.NotNull;

final class SafeArrayAccess {
    private SafeArrayAccess() {
    }

    static Object @NotNull [] toSafeArray(final @NotNull Collection<?> collection) {
        return arrayExtractor.get(collection.getClass()).toArray(collection);
    }

    private static Object @NotNull [] toArrayDefensive(final @NotNull Collection<?> collection) {
        // Malicious or buggy collection types could leak the result of toArray() somewhere, which could potentially
        // break the immutability guarantee of ImmutableList, so we need to make a defensive copy here.
        return collection.toArray().clone();
    }

    private static boolean isTrustedCollectionType(final @NotNull Class<?> clazz) {
        // At the moment, trust only ArrayList itself (not its potential child classes), its sub-lists and immutable
        // lists themselves.
        return clazz == ArrayList.class
            || clazz.getDeclaringClass() == ArrayList.class
            || ImmutableList.class.isAssignableFrom(clazz);
    }

    private static @NotNull CollectionArrayExtractor getExtractor(final @NotNull Class<?> clazz) {
        return isTrustedCollectionType(clazz) ? Collection::toArray : SafeArrayAccess::toArrayDefensive;
    }

    private static final SimpleClassValue<@NotNull CollectionArrayExtractor> arrayExtractor =
        new SimpleClassValue<>(SafeArrayAccess::getExtractor);

    @FunctionalInterface
    private interface CollectionArrayExtractor {
        Object @NotNull [] toArray(@NotNull Collection<?> collection);
    }
}
