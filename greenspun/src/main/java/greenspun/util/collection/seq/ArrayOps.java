// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import greenspun.util.annotation.NonNullByDefault;

@NonNullByDefault
final class ArrayOps {
    private ArrayOps() {
    }

    static <T> void forEach(final T[] array, final Consumer<? super T> action) {
        forEachFrom(array, 0, action);
    }

    static <T> void forEachFrom(final T[] array, final int start, final Consumer<? super T> action) {
        final var length = array.length;
        for (int i = start; i < length; i += 1) {
            action.accept(array[i]);
        }
    }

    static <T> boolean anySatisfies(final T[] array, final Predicate<? super T> predicate) {
        for (final var item : array) {
            if (predicate.test(item)) {
                return true;
            }
        }
        return false;
    }

    static <T, U> U[] map(final T[] array, final Function<? super T, ? extends U> function) {
        final var length = array.length;
        @SuppressWarnings("unchecked") final var newArray = (U[]) newArray(array, length);
        for (int i = 0; i < length; i += 1) {
            newArray[i] = function.apply(array[i]);
        }
        return newArray;
    }

    static <T> T[] updated(final T[] array, final int index, final T element) {
        final var newArray = array.clone();
        newArray[index] = element;
        return newArray;
    }

    static <T> T[] prepended(final T[] array, final T element) {
        final var oldLength = array.length;
        final var newArray = newArray(array, oldLength + 1);
        newArray[0] = element;
        System.arraycopy(array, 0, newArray, 1, oldLength);
        return newArray;
    }

    static <T> T[] appended(final T[] array, final T element) {
        final var oldLength = array.length;
        final var newArray = Arrays.copyOf(array, oldLength + 1);
        newArray[oldLength] = element;
        return newArray;
    }

    static <T> T[] prependedSlice(final T[] array, final T object) {
        final var elementsToCopy = array.length - Seq.maxChunkLength;
        final var newArray = newArray(array, elementsToCopy + 1);
        newArray[0] = object;
        System.arraycopy(array, 0, newArray, 1, elementsToCopy);
        return newArray;
    }

    static <T> T[] appendedSlice(final T[] array, final T object) {
        final var elementsToCopy = array.length - Seq.maxChunkLength;
        final var newArray = newArray(array, elementsToCopy + 1);
        System.arraycopy(array, Seq.maxChunkLength, newArray, 0, elementsToCopy);
        newArray[elementsToCopy] = object;
        return newArray;
    }

    static <T> T[] concat(final T[] front, final T[] back) {
        assert front.getClass() == back.getClass();
        return appendedRange(front, back, back.length);
    }

    static <T> Split<T> split(final T[] array) {
        final var length = array.length;
        final var midpoint = length / 2;
        return new Split<>(Arrays.copyOf(array, midpoint), Arrays.copyOfRange(array, midpoint, length));
    }

    static <T> Split<T> concatSplitAt(final T[] front, final T[] back, final int index) {
        assert front.getClass() == back.getClass();
        final var frontLength = front.length;
        final var backLength = back.length;
        final var totalLength = frontLength + backLength;
        assert index < totalLength;
        if (index == frontLength) {
            return new Split<>(front, back);
        } else if (index < frontLength) {
            final var newFront = Arrays.copyOf(front, index);
            final var newBack = prependedRange(front, back, index);
            return new Split<>(newFront, newBack);
        } else {
            final var newBackStart = index - frontLength;
            final var newFront = appendedRange(front, back, newBackStart);
            final var newBack = Arrays.copyOfRange(back, newBackStart, backLength);
            return new Split<>(newFront, newBack);
        }
    }

    @SuppressWarnings("unchecked")
    static <T> T[] newArray(final T[] original, final int length) {
        return (T[]) Array.newInstance(original.getClass().getComponentType(), length);
    }

    private static <T> T[] appendedRange(final T[] front, final T[] back, final int backEnd) {
        final var frontLength = front.length;
        final var newLength = frontLength + backEnd;
        if (newLength == 0) {
            return front;
        }
        final var newArray = Arrays.copyOf(front, newLength);
        System.arraycopy(back, 0, newArray, frontLength, backEnd);
        return newArray;
    }

    private static <T> T[] prependedRange(final T[] front, final T[] back, final int frontStart) {
        final var frontLength = front.length;
        final var backLength = back.length;
        final var frontElementCount = frontLength - frontStart;
        final var newArray = newArray(front, back.length + frontLength - frontStart);
        System.arraycopy(front, frontStart, newArray, 0, frontElementCount);
        System.arraycopy(back, 0, newArray, frontElementCount, backLength);
        return newArray;
    }

    record Split<T>(T[] front, T[] back) {
    }
}
