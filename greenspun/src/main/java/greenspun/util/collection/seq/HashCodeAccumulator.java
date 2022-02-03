// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Objects;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class HashCodeAccumulator<T> implements ChunkConsumer<T> {
    @Override
    public void acceptSingle(final T object) {
        hashCode = mixHash(hashCode, object);
    }

    @Override
    public void acceptArray(final T @NotNull [] array) {
        int h = hashCode;
        for (final var item : array) {
            h = mixHash(h, item);
        }
        hashCode = h;
    }

    int result() {
        return hashCode;
    }

    private static int mixHash(final int hash, final @Nullable Object object) {
        return 31 * hash + Objects.hashCode(object);
    }

    private int hashCode = 1;
}
