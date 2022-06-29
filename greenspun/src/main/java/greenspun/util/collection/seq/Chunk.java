// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Function;
import org.jetbrains.annotations.NotNull;

final class Chunk<T> {
    Chunk(final @NotNull Tag<T, ?> tag, final T @NotNull [] values) {
        this(tag.measureArray(values), values);
    }

    private Chunk(final long subtreeSize, final T @NotNull [] values) {
        assert values.length >= Seq.minChunkLength && values.length <= Seq.maxChunkLength;
        this.subtreeSize = subtreeSize;
        this.values = values;
    }

    <U> @NotNull Chunk<U> map(final @NotNull Tag<U, ?> tag, final @NotNull Function<? super T, ? extends U> function) {
        final var newValues = ArrayOps.map(values, function);
        assert tag.measureArray(newValues) == subtreeSize;
        return new Chunk<>(subtreeSize, newValues);
    }

    final long subtreeSize;
    final T @NotNull [] values;
}
