// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Function;
import greenspun.util.annotation.NonNullByDefault;

@NonNullByDefault
final class Chunk<T> {
    Chunk(final Tag<T, ?> tag, final T[] values) {
        this(tag.measureArray(values), values);
    }

    private Chunk(final long subtreeSize, final T[] values) {
        assert values.length >= Seq.minChunkLength && values.length <= Seq.maxChunkLength;
        this.subtreeSize = subtreeSize;
        this.values = values;
    }

    <U> Chunk<U> map(final Tag<U, ?> tag, final Function<? super T, ? extends U> function) {
        final var newValues = ArrayOps.map(values, function);
        assert tag.measureArray(newValues) == subtreeSize;
        return new Chunk<>(subtreeSize, newValues);
    }

    final long subtreeSize;
    final T[] values;
}
