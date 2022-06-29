// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.lang.reflect.Array;
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

    static <T> void forEachFrom(final T @NotNull [] array, final int start, final @NotNull Consumer<? super T> action) {
        final var length = array.length;
        for (int i = start; i < length; i += 1) {
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
        final T @NotNull [] array,
        final @NotNull Function<? super T, ? extends U> function
    ) {
        final var length = array.length;
        @SuppressWarnings("unchecked") final var newArray = (U[]) newArray(array, length);
        for (int i = 0; i < length; i += 1) {
            newArray[i] = function.apply(array[i]);
        }
        return newArray;
    }

    static <T> T @NotNull [] updated(final T @NotNull [] array, final int index, final T element) {
        final var newArray = array.clone();
        newArray[index] = element;
        return newArray;
    }

    static <T> T @NotNull [] prepended(final T @NotNull [] array, final T element) {
        final var oldLength = array.length;
        final var newArray = newArray(array, oldLength + 1);
        newArray[0] = element;
        System.arraycopy(array, 0, newArray, 1, oldLength);
        return newArray;
    }

    static <T> T @NotNull [] appended(final T @NotNull [] array, final T element) {
        final var oldLength = array.length;
        final var newArray = Arrays.copyOf(array, oldLength + 1);
        newArray[oldLength] = element;
        return newArray;
    }

    static <T> T @NotNull [] prependedSlice(final T @NotNull [] array, final T object) {
        final var elementsToCopy = array.length - Seq.maxChunkLength;
        final var newArray = newArray(array, elementsToCopy + 1);
        newArray[0] = object;
        System.arraycopy(array, 0, newArray, 1, elementsToCopy);
        return newArray;
    }

    static <T> T @NotNull [] appendedSlice(final T @NotNull [] array, final T object) {
        final var elementsToCopy = array.length - Seq.maxChunkLength;
        final var newArray = newArray(array, elementsToCopy + 1);
        System.arraycopy(array, Seq.maxChunkLength, newArray, 0, elementsToCopy);
        newArray[elementsToCopy] = object;
        return newArray;
    }

    static <T> T @NotNull [] concat(final T @NotNull [] front, final T @NotNull [] back) {
        assert front.getClass() == back.getClass();
        return appendedRange(front, back, back.length);
    }

    static <T> @NotNull Split<T> split(final T @NotNull [] array) {
        final var length = array.length;
        final var midpoint = length / 2;
        return new Split<>(Arrays.copyOf(array, midpoint), Arrays.copyOfRange(array, midpoint, length));
    }

    static <T> @NotNull Split<T> concatSplitAt(final T @NotNull [] front, final T @NotNull [] back, final int index) {
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
    static <T> T @NotNull [] newArray(final T @NotNull [] original, final int length) {
        return (T[]) Array.newInstance(original.getClass().getComponentType(), length);
    }

    private static <T> T @NotNull [] appendedRange(
        final T @NotNull [] front,
        final T @NotNull [] back,
        final int backEnd
    ) {
        final var frontLength = front.length;
        final var newLength = frontLength + backEnd;
        if (newLength == 0) {
            return front;
        }
        final var newArray = Arrays.copyOf(front, newLength);
        System.arraycopy(back, 0, newArray, frontLength, backEnd);
        return newArray;
    }

    private static <T> T @NotNull [] prependedRange(
        final T @NotNull [] front,
        final T @NotNull [] back,
        final int frontStart
    ) {
        final var frontLength = front.length;
        final var backLength = back.length;
        final var frontElementCount = frontLength - frontStart;
        final var newArray = newArray(front, back.length + frontLength - frontStart);
        System.arraycopy(front, frontStart, newArray, 0, frontElementCount);
        System.arraycopy(back, 0, newArray, frontElementCount, backLength);
        return newArray;
    }

    record Split<T>(T @NotNull [] front, T @NotNull [] back) {
    }
}
