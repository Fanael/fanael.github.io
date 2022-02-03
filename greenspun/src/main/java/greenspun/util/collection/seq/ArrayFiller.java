// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import org.jetbrains.annotations.NotNull;

final class ArrayFiller<T extends U, U> implements ChunkConsumer<T> {
    ArrayFiller(final U @NotNull [] array) {
        this.array = array;
    }

    @Override
    public void acceptSingle(final T object) {
        array[index] = object;
        index += 1;
    }

    @Override
    public void acceptArray(final T @NotNull [] array) {
        final var length = array.length;
        System.arraycopy(array, 0, this.array, index, length);
        index += length;
    }

    private final U @NotNull [] array;
    private int index = 0;
}
