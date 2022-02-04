// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import org.jetbrains.annotations.NotNull;

final class ArrayOps {
    private ArrayOps() {
    }

    static <T> void forEach(final T @NotNull [] array, final @NotNull Consumer<? super T> action) {
        forEachFrom(array, 0, action);
    }

    static <T> void forEachFrom(
        final T @NotNull [] array,
        final int startIndex,
        final @NotNull Consumer<? super T> action
    ) {
        final var length = array.length;
        for (int i = startIndex; i < length; i += 1) {
            action.accept(array[i]);
        }
    }

    static <T> boolean anySatisfies(final T @NotNull [] array, final @NotNull Predicate<? super T> predicate) {
        for (final var item : array) {
            if (predicate.test(item)) {
                return true;
            }
        }
        return false;
    }

    static <T, U> U @NotNull [] map(
        final @NotNull TypeTag<U, ?> tag,
        final T @NotNull [] array,
        final @NotNull Function<? super T, ? extends U> function
    ) {
        final var length = array.length;
        final var newArray = tag.newArray(length);
        for (int i = 0; i < length; i += 1) {
            newArray[i] = function.apply(array[i]);
        }
        return newArray;
    }

    static <T> T @NotNull [] prepended(final @NotNull TypeTag<T, ?> tag, final T @NotNull [] array, final T object) {
        return prependedSlice(tag, array, array.length, object);
    }

    static <T> T @NotNull [] appended(final T @NotNull [] array, final T object) {
        final var oldLength = array.length;
        final var newArray = Arrays.copyOf(array, oldLength + 1);
        newArray[oldLength] = object;
        return newArray;
    }

    static <T> T @NotNull [] prependedSlice(
        final @NotNull TypeTag<T, ?> tag,
        final T @NotNull [] array,
        final int toIndex,
        final T object
    ) {
        final var newArray = tag.newArray(toIndex + 1);
        newArray[0] = object;
        System.arraycopy(array, 0, newArray, 1, toIndex);
        return newArray;
    }

    static <T> T @NotNull [] appendedSlice(
        final @NotNull TypeTag<T, ?> tag,
        final T @NotNull [] array,
        final int fromIndex,
        final T object
    ) {
        final var oldLength = array.length;
        final var elementsToCopy = oldLength - fromIndex;
        final var newArray = tag.newArray(elementsToCopy + 1);
        System.arraycopy(array, fromIndex, newArray, 0, elementsToCopy);
        newArray[elementsToCopy] = object;
        return newArray;
    }

    static <T> T @NotNull [] concat(
        final @NotNull TypeTag<T, ?> tag,
        final T @NotNull [] left,
        final T @NotNull [] right
    ) {
        final var leftLength = left.length;
        final var rightLength = right.length;
        final var newArray = tag.newArray(leftLength + rightLength);
        System.arraycopy(left, 0, newArray, 0, leftLength);
        System.arraycopy(right, 0, newArray, leftLength, rightLength);
        return newArray;
    }

    static <T, Phantom> @NotNull TaggedSeq<T, Phantom> toSeq(
        final @NotNull TypeTag<T, Phantom> tag,
        final T @NotNull [] array
    ) {
        return toSeq(tag, tag.measureArray(array), array, array.length);
    }

    static <T, Phantom> @NotNull TaggedSeq<T, Phantom> toSeq(
        final @NotNull TypeTag<T, Phantom> tag,
        final long size,
        final T @NotNull [] array
    ) {
        return toSeq(tag, size, array, array.length);
    }

    static <T, Phantom> @NotNull TaggedSeq<T, Phantom> toSeq(
        final @NotNull TypeTag<T, Phantom> tag,
        final long size,
        final T @NotNull [] array,
        final int effectiveLength
    ) {
        return switch (effectiveLength) {
            case 0 -> tag.emptySeq();
            case 1 -> new Single<>(tag, size, array[0]);
            default -> {
                final var middle = effectiveLength / 2;
                final var prefix = Arrays.copyOf(array, middle);
                final var suffix = Arrays.copyOfRange(array, middle, effectiveLength);
                yield new Deep<>(tag, size, prefix, Empty.node(), suffix);
            }
        };
    }
}
