// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import org.jetbrains.annotations.NotNull;

abstract sealed class TypeTag<T, Phantom> {
    @SuppressWarnings("unchecked")
    static <T> @NotNull TypeTag<T, Object> unit() {
        return (UnitImpl<T>) UnitImpl.instance;
    }

    @SuppressWarnings("unchecked")
    static <T, Phantom> @NotNull TypeTag<@NotNull Node<T>, Node<Phantom>> node() {
        return (NodeImpl<T, Phantom>) NodeImpl.instance;
    }

    // NB: this would've been highly dangerous if not for the fact that the phantom parameter stays the same.
    @SuppressWarnings("unchecked")
    final <U> @NotNull TypeTag<U, Phantom> cast() {
        return (TypeTag<U, Phantom>) this;
    }

    abstract @NotNull Empty<T, Phantom> emptySeq();

    abstract long measureSingle(T object);

    abstract long measureArray(T @NotNull [] array);

    abstract T @NotNull [] emptyArray();

    abstract T @NotNull [] newArray(int size);

    abstract T @NotNull [] arrayOf(T object);

    private static final class UnitImpl<T> extends TypeTag<T, Object> {
        @Override
        @NotNull Empty<T, Object> emptySeq() {
            return Empty.unit();
        }

        @Override
        long measureSingle(final T object) {
            return 1;
        }

        @Override
        long measureArray(final T @NotNull [] array) {
            return array.length;
        }

        @Override
        @SuppressWarnings("unchecked")
        T @NotNull [] emptyArray() {
            return (T[]) emptyArray;
        }

        @Override
        @SuppressWarnings("unchecked")
        T @NotNull [] newArray(final int size) {
            return (T[]) new Object[size];
        }

        @Override
        @SuppressWarnings("unchecked")
        T @NotNull [] arrayOf(final T object) {
            return (T[]) new Object[]{object};
        }

        private static final Object[] emptyArray = new Object[0];
        private static final UnitImpl<?> instance = new UnitImpl<>();
    }

    private static final class NodeImpl<T, Phantom> extends TypeTag<@NotNull Node<T>, Node<Phantom>> {
        @Override
        @NotNull Empty<@NotNull Node<T>, Node<Phantom>> emptySeq() {
            return Empty.node();
        }

        @Override
        long measureSingle(final @NotNull Node<T> object) {
            return object.size();
        }

        @Override
        long measureArray(final @NotNull Node<T> @NotNull [] array) {
            long sum = 0;
            for (final var node : array) {
                sum += node.size();
            }
            return sum;
        }

        @Override
        @SuppressWarnings("unchecked")
        @NotNull Node<T> @NotNull [] emptyArray() {
            return (Node<T>[]) emptyArray;
        }

        @Override
        @SuppressWarnings("unchecked")
        Node<T> @NotNull [] newArray(final int size) {
            return (Node<T>[]) new Node<?>[size];
        }

        @Override
        @SuppressWarnings("unchecked")
        @NotNull Node<T> @NotNull [] arrayOf(final @NotNull Node<T> object) {
            return (Node<T>[]) new Node<?>[]{object};
        }

        private static final Node<?>[] emptyArray = new Node<?>[0];
        private static final NodeImpl<?, ?> instance = new NodeImpl<>();
    }
}
