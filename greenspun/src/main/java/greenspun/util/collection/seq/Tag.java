// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Arrays;
import org.jetbrains.annotations.NotNull;

abstract sealed class Tag<T, Phantom> {
    @SuppressWarnings("unchecked")
    static <T> @NotNull Tag<T, Object> unit() {
        return (UnitImpl<T>) UnitImpl.instance;
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Tag<@NotNull Chunk<T>, Chunk<?>> chunk() {
        return (ChunkImpl<T>) ChunkImpl.instance;
    }

    // NB: this would've been highly dangerous, as in ClassCastException galore, if not for the fact that the phantom
    // parameter stays the same.
    @SuppressWarnings("unchecked")
    final <U> @NotNull Tag<U, Phantom> cast() {
        return (Tag<U, Phantom>) this;
    }

    final @NotNull ArraySplit<T> splitArray(final T @NotNull [] array, final int index) {
        final var length = array.length;
        final var midpoint = Math.min(index, length - 1);
        final var front = (midpoint > 0) ? Arrays.copyOf(array, midpoint) : emptyArray();
        final var back = (midpoint < length - 1) ? Arrays.copyOfRange(array, midpoint + 1, length) : emptyArray();
        return new ArraySplit<>(front, array[midpoint], back);
    }

    abstract long measureSingle(T object);

    abstract long measureArray(T @NotNull [] array);

    abstract @NotNull SplitPoint findSplitPoint(T @NotNull [] array, long index, long initialAccumulator);

    abstract @NotNull Shallow<T, Phantom> emptySeq();

    abstract T @NotNull [] emptyArray();

    abstract T @NotNull [] unitArray(T object);

    record SplitPoint(int index, long accumulator) {
    }

    record ArraySplit<T>(T @NotNull [] front, T middle, T @NotNull [] back) {
    }

    private static final class UnitImpl<T> extends Tag<T, Object> {
        @Override
        long measureSingle(final T object) {
            return 1;
        }

        @Override
        long measureArray(final T @NotNull [] array) {
            return array.length;
        }

        @Override
        @NotNull SplitPoint findSplitPoint(final T @NotNull [] array, final long index, final long initialAccumulator) {
            final var distance = index - initialAccumulator;
            final var length = array.length;
            final var inBounds = (distance < length) ? 1 : 0;
            final var arrayIndex = (int) Math.min(length, index - initialAccumulator);
            return new SplitPoint(arrayIndex, initialAccumulator + arrayIndex + inBounds);
        }

        @Override
        @NotNull Shallow<T, Object> emptySeq() {
            return Shallow.emptyUnit();
        }

        @Override
        @SuppressWarnings("unchecked")
        T @NotNull [] emptyArray() {
            return (T[]) emptyArray;
        }

        @Override
        @SuppressWarnings("unchecked")
        T @NotNull [] unitArray(final T object) {
            return (T[]) new Object[]{object};
        }

        private static final Object[] emptyArray = new Object[0];
        private static final UnitImpl<?> instance = new UnitImpl<>();
    }

    private static final class ChunkImpl<T> extends Tag<@NotNull Chunk<T>, Chunk<?>> {
        @Override
        long measureSingle(final @NotNull Chunk<T> object) {
            return object.subtreeSize;
        }

        @Override
        long measureArray(final @NotNull Chunk<T> @NotNull [] array) {
            long sum = 0;
            for (final var chunk : array) {
                sum += chunk.subtreeSize;
            }
            return sum;
        }

        @Override
        @NotNull SplitPoint findSplitPoint(
            final @NotNull Chunk<T> @NotNull [] array,
            final long index,
            final long initialAccumulator
        ) {
            var accumulator = initialAccumulator;
            int i = 0;
            final var length = array.length;
            for (; i < length; i += 1) {
                accumulator += array[i].subtreeSize;
                if (index < accumulator) {
                    break;
                }
            }
            return new SplitPoint(i, accumulator);
        }

        @Override
        @NotNull Shallow<@NotNull Chunk<T>, Chunk<?>> emptySeq() {
            return Shallow.emptyChunk();
        }

        @Override
        @SuppressWarnings("unchecked")
        @NotNull Chunk<T> @NotNull [] emptyArray() {
            return (Chunk<T>[]) emptyArray;
        }

        @Override
        @SuppressWarnings("unchecked")
        @NotNull Chunk<T> @NotNull [] unitArray(final @NotNull Chunk<T> object) {
            return (Chunk<T>[]) new Chunk<?>[]{object};
        }

        private static final Chunk<?>[] emptyArray = new Chunk<?>[0];
        private static final ChunkImpl<?> instance = new ChunkImpl<>();
    }
}
