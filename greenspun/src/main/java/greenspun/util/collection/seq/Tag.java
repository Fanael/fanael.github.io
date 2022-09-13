// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import greenspun.util.UnreachableCodeReachedError;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

abstract sealed class Tag<T extends C, C> {
    private Tag(final Class<C[]> arrayClass) {
        this.arrayClass = arrayClass;
    }

    @SuppressWarnings("unchecked")
    static <T> Tag<T, @Nullable Object> unit() {
        return (OfUnit<T>) OfUnit.instance;
    }

    @SuppressWarnings("unchecked")
    static <T> Tag<Chunk<T>, Chunk<?>> chunk() {
        return (OfChunk<T>) OfChunk.instance;
    }

    // NB: this is safe only because the runtime array component type stays the same.
    @SuppressWarnings("unchecked")
    final <U extends C> Tag<U, C> cast() {
        return (Tag<U, C>) this;
    }

    final boolean arrayTypeMatches(final Class<? extends Object[]> clazz) {
        return arrayClass == clazz;
    }

    final ArraySplit<T> splitArray(final T[] array, final int index) {
        final var length = array.length;
        assert index < length;
        final var front = (index > 0) ? ArrayOps.take(array, index) : emptyArray();
        final var back = (index < length - 1) ? ArrayOps.drop(array, index + 1) : emptyArray();
        return new ArraySplit<>(front, array[index], back);
    }

    abstract long sizeOf(C object);

    abstract long sumOfSizes(C[] array);

    abstract SplitPoint findSplitPoint(C[] array, long index);

    abstract Shallow<T, C> emptySeq();

    abstract @NonNull T[] emptyArray();

    abstract T[] unitArray(T object);

    private final Class<C[]> arrayClass;

    record SplitPoint(int index, long remainder, long prefixSize) {
    }

    record ArraySplit<T>(T[] front, T middle, T[] back) {
    }

    private static final class OfUnit<T> extends Tag<T, @Nullable Object> {
        private OfUnit() {
            super(Object[].class);
        }

        @Override
        long sizeOf(final @Nullable Object object) {
            return 1;
        }

        @Override
        long sumOfSizes(final @Nullable Object[] array) {
            return array.length;
        }

        @Override
        SplitPoint findSplitPoint(final @Nullable Object[] array, final long index) {
            return new SplitPoint((int) index, 0, index);
        }

        @Override
        Shallow<T, @Nullable Object> emptySeq() {
            return Shallow.emptyUnit();
        }

        @Override
        @SuppressWarnings("unchecked")
        @NonNull T[] emptyArray() {
            return (@NonNull T[]) emptyArray;
        }

        @Override
        @SuppressWarnings("unchecked")
        T[] unitArray(final T object) {
            return (T[]) new Object[]{object};
        }

        private static final OfUnit<?> instance = new OfUnit<>();
        private static final Object[] emptyArray = new Object[0];
    }

    private static final class OfChunk<T> extends Tag<Chunk<T>, Chunk<?>> {
        @SuppressWarnings({"unchecked", "RedundantCast"}) // Silly IDEA, casting through Class<?> is necessary.
        private OfChunk() {
            // Silly Java, Chunk<?>[].class should be legal and denote the same object.
            super((Class<Chunk<?>[]>) (Class<?>) Chunk[].class);
        }

        @Override
        long sizeOf(final Chunk<?> chunk) {
            return chunk.subtreeSize;
        }

        @Override
        long sumOfSizes(final Chunk<?>[] chunks) {
            long sum = 0;
            for (final var chunk : chunks) {
                sum += chunk.subtreeSize;
            }
            return sum;
        }

        @Override
        SplitPoint findSplitPoint(final Chunk<?>[] chunks, final long index) {
            long accumulator = 0;
            final var length = chunks.length;
            for (int i = 0; i < length; i += 1) {
                final var nextAccumulator = accumulator + chunks[i].subtreeSize;
                if (index < nextAccumulator) {
                    return new SplitPoint(i, index - accumulator, accumulator);
                }
                accumulator = nextAccumulator;
            }
            throw new UnreachableCodeReachedError("Fell through a chunk array trying to find the split point");
        }

        @Override
        Shallow<Chunk<T>, Chunk<?>> emptySeq() {
            return Shallow.emptyChunk();
        }

        @Override
        @SuppressWarnings("unchecked")
        Chunk<T>[] emptyArray() {
            return (Chunk<T>[]) emptyArray;
        }

        @Override
        @SuppressWarnings("unchecked")
        Chunk<T>[] unitArray(final Chunk<T> object) {
            return (Chunk<T>[]) new Chunk<?>[]{object};
        }

        private static final OfChunk<?> instance = new OfChunk<>();
        private static final Chunk<?>[] emptyArray = new Chunk<?>[0];
    }
}
