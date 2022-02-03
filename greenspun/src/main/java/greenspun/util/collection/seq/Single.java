// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;

final class Single<T, Phantom> extends TaggedSeq<T, Phantom> {
    Single(final @NotNull TypeTag<T, Phantom> tag, final T value) {
        super(tag, tag.measureSingle(value));
        this.value = value;
    }

    Single(final @NotNull TypeTag<T, Phantom> tag, final long size, final T value) {
        super(tag, size);
        assert size == tag.measureSingle(value);
        this.value = value;
    }

    static <T> @NotNull Single<T, Object> ofUnit(final T object) {
        return new Single<>(TypeTag.unit(), 1, object);
    }

    @Override
    public @NotNull Seq.Itr<T> iterator() {
        return new Itr<>(value);
    }

    @Override
    public boolean anySatisfies(final @NotNull Predicate<? super T> predicate) {
        return predicate.test(value);
    }

    @Override
    public @NotNull <U> TaggedSeq<U, Phantom> map(final @NotNull Function<? super T, ? extends U> function) {
        return new Single<>(tag().cast(), exactSize(), function.apply(value));
    }

    @Override
    public T first() {
        return value;
    }

    @Override
    public T last() {
        return value;
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedFirst(final T object) {
        return new Single<>(tag(), object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedLast(final T object) {
        return new Single<>(tag(), object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> prepended(final T object) {
        final var tag = tag();
        final var newSize = addToSize(object);
        return new Deep<>(tag, newSize, tag.arrayOf(object), Empty.node(), tag.arrayOf(value));
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> appended(final T object) {
        final var tag = tag();
        final var newSize = addToSize(object);
        return new Deep<>(tag, newSize, tag.arrayOf(value), Empty.node(), tag.arrayOf(object));
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutFirst() {
        return tag().emptySeq();
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutLast() {
        return tag().emptySeq();
    }

    @Override
    void forEachChunk(final @NotNull ChunkConsumer<T> action) {
        action.acceptSingle(value);
    }

    @Override
    T getImpl(final long index) {
        return value;
    }

    @Override
    @NotNull TaggedSeq<T, Phantom> concatImpl(final @NotNull TaggedSeq<T, Phantom> other) {
        return other.prepended(value);
    }

    @Override
    @NotNull TreeSplit<T, Phantom> splitTree(final long index, final long accumulator) {
        final var empty = tag().emptySeq();
        return new TreeSplit<>(empty, value, empty);
    }

    private final T value;

    static final class Itr<T> extends Seq.Itr<T> {
        private Itr(final T value) {
            this.value = value;
        }

        @Override
        public boolean hasNext() {
            return !passed;
        }

        @Override
        @SuppressFBWarnings(value = "IT_NO_SUCH_ELEMENT", justification = "It can, SpotBugs is just confused")
        public T next() {
            if (passed) {
                throw noMoreElements();
            }
            passed = true;
            return value;
        }

        @Override
        void forEachRemainingImpl(final @NotNull Consumer<? super T> action) {
            if (!passed) {
                action.accept(value);
                passed = true;
            }
        }

        private final T value;
        private boolean passed = false;
    }
}
