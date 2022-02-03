// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Consumer;
import org.jetbrains.annotations.NotNull;

interface ChunkConsumer<T> {
    static <T> @NotNull ChunkConsumer<T> fromConsumer(final @NotNull Consumer<? super T> consumer) {
        return new ChunkConsumer<>() {
            @Override
            public void acceptSingle(final T object) {
                consumer.accept(object);
            }

            @Override
            public void acceptArray(final T @NotNull [] array) {
                ArrayOps.forEach(array, consumer);
            }
        };
    }

    void acceptSingle(T object);

    void acceptArray(T @NotNull [] array);

    default @NotNull ChunkConsumer<@NotNull Node<T>> lift() {
        return new ChunkConsumer<>() {
            @Override
            public void acceptSingle(final @NotNull Node<T> object) {
                ChunkConsumer.this.acceptArray(object.values());
            }

            @Override
            public void acceptArray(final @NotNull Node<T> @NotNull [] array) {
                ArrayOps.forEach(array, node -> ChunkConsumer.this.acceptArray(node.values()));
            }
        };
    }
}
