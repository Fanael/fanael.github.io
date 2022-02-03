// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

final class IndexAccumulator {
    IndexAccumulator(final long remainingIndex) {
        this.remainingIndex = remainingIndex;
    }

    boolean subtract(final long objectSize) {
        final var newValue = remainingIndex - objectSize;
        if (newValue < 0) {
            return true;
        }
        remainingIndex = newValue;
        return false;
    }

    long get() {
        return remainingIndex;
    }

    private long remainingIndex;
}
