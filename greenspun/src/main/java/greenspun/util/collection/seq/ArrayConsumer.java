// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Consumer;

@FunctionalInterface
interface ArrayConsumer<T> extends Consumer<T[]> {
    @Override
    void accept(T[] array);
}
