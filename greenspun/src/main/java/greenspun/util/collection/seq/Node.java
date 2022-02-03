// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Function;
import org.jetbrains.annotations.NotNull;

final class Node<T> {
    Node(final @NotNull TypeTag<T, ?> tag, final T @NotNull [] values) {
        assert values.length >= minLength;
        assert values.length <= maxLength;
        size = tag.measureArray(values);
        this.values = values;
    }

    private Node(final @NotNull TypeTag<T, ?> tag, final long size, final T @NotNull [] values) {
        assert size == tag.measureArray(values);
        this.size = size;
        this.values = values;
    }

    long size() {
        return size;
    }

    T @NotNull [] values() {
        return values;
    }

    <U> @NotNull Node<U> map(
        final @NotNull TypeTag<U, ?> tag,
        final @NotNull Function<? super T, ? extends U> function
    ) {
        return new Node<>(tag, size, ArrayOps.map(tag, values, function));
    }

    static final int maxLength = TaggedSeq.maxAffixLength;
    static final int minLength = maxLength / 2;

    private final long size;
    private final T @NotNull [] values;
}
