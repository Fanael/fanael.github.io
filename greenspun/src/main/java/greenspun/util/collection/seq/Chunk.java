// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

final class Chunk<T> {
    private Chunk(final long subtreeSize, final T[] values) {
        assert values.length >= minLength && values.length <= maxLength;
        this.subtreeSize = subtreeSize;
        this.values = values;
    }

    static <T> Chunk<T> make(final Tag<T, ? super T> tag, final T[] values) {
        return new Chunk<>(tag.sumOfSizes(values), values);
    }

    static <T> Chunk<T> make(final Tag<T, ? super T> tag, final long subtreeSize, final T[] values) {
        assert subtreeSize == tag.sumOfSizes(values);
        return new Chunk<>(subtreeSize, values);
    }

    // We want array objects that are 32 elements big, leaving 4 references worth of space for the object header and
    // the array length. In current versions of the HotSpot VM, with 4 byte references, this makes max-sized arrays
    // exactly 128 bytes long, and with 8 byte references is just one element short of 256 bytes, which is roughly
    // 1 to 4 cache lines worth of data on common hardware.
    static final int maxLength = 28;
    static final int minLength = maxLength / 2;

    final long subtreeSize;
    final T[] values;
}
