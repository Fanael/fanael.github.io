// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.NoSuchElementException;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.util.UnreachableCodeReachedError;
import org.jetbrains.annotations.NotNull;

final class Empty<T, Phantom> extends TaggedSeq<T, Phantom> {
    private Empty(final @NotNull TypeTag<T, Phantom> tag) {
        super(tag, 0);
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Empty<T, Object> unit() {
        return (Empty<T, Object>) unitInstance;
    }

    @SuppressWarnings("unchecked")
    static <T, Phantom> @NotNull Empty<@NotNull Node<T>, Node<Phantom>> node() {
        return (Empty<Node<T>, Node<Phantom>>) nodeInstance;
    }

    @Override
    public @NotNull Seq.Itr<T> iterator() {
        return new Itr<>();
    }

    @Override
    public boolean anySatisfies(final @NotNull Predicate<? super T> predicate) {
        return false;
    }

    @Override
    @SuppressWarnings("unchecked")
    public @NotNull <U> TaggedSeq<U, Phantom> map(final @NotNull Function<? super T, ? extends U> function) {
        return (Empty<U, Phantom>) this;
    }

    @Override
    public T first() {
        throw new NoSuchElementException("first called on an empty sequence");
    }

    @Override
    public T last() {
        throw new NoSuchElementException("last called on an empty sequence");
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedFirst(final T object) {
        throw new NoSuchElementException("updatedFirst called on an empty sequence");
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedLast(final T object) {
        throw new NoSuchElementException("updatedLast called on an empty sequence");
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> prepended(final T object) {
        return new Single<>(tag(), object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> appended(final T object) {
        return new Single<>(tag(), object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutFirst() {
        throw new NoSuchElementException("withoutFirst called on an empty sequence");
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutLast() {
        throw new NoSuchElementException("withoutLast called on an empty sequence");
    }

    @Override
    void forEachChunk(final @NotNull ChunkConsumer<T> action) {
        // Nothing to do here.
    }

    @Override
    T getImpl(final long index) {
        throw new UnreachableCodeReachedError("getImpl called on an empty sequence");
    }

    @Override
    @NotNull TaggedSeq<T, Phantom> concatImpl(final @NotNull TaggedSeq<T, Phantom> other) {
        return other;
    }

    @Override
    @NotNull TreeSplit<T, Phantom> splitTree(final long index, final long accumulator) {
        throw new UnreachableCodeReachedError("splitTree called on an empty sequence");
    }

    private static final Empty<?, Object> unitInstance = new Empty<>(TypeTag.unit());
    private static final Empty<? extends Node<?>, ? extends Node<?>> nodeInstance = new Empty<>(TypeTag.node());

    static final class Itr<T> extends Seq.Itr<T> {
        private Itr() {
        }

        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        @SuppressFBWarnings(value = "IT_NO_SUCH_ELEMENT", justification = "It can, SpotBugs is just confused")
        public T next() {
            throw noMoreElements();
        }

        @Override
        void forEachRemainingImpl(final @NotNull Consumer<? super T> action) {
            // Nothing to do here.
        }
    }
}
