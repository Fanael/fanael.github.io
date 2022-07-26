// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Function;

final class Chunk<T> {
    Chunk(final Tag<T, ?> tag, final T[] values) {
        this(tag.measureArray(values), values);
    }

    private Chunk(final long subtreeSize, final T[] values) {
        assert values.length >= Seq.minChunkLength && values.length <= Seq.maxChunkLength;
        this.subtreeSize = subtreeSize;
        this.values = values;
    }

    static <T> Tag.Updater<Chunk<T>> makeUpdater(final Tag<T, ?> tag, final long index, final Tag.Updater<T> updater) {
        return (accumulator, chunk) -> {
            final var splitPoint = tag.findSplitPoint(chunk.values, index, accumulator);
            final var newValues = tag.updatedArray(chunk.values, splitPoint, updater);
            return new Chunk<>(chunk.subtreeSize, newValues);
        };
    }

    <U> Chunk<U> map(final Tag<U, ?> tag, final Function<? super T, ? extends U> function) {
        final var newValues = ArrayOps.map(values, function);
        assert tag.measureArray(newValues) == subtreeSize;
        return new Chunk<>(subtreeSize, newValues);
    }

    final long subtreeSize;
    final T[] values;
}
