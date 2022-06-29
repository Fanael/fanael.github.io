// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;

final class Shallow<T, Phantom> extends TaggedSeq<T, Phantom> {
    Shallow(final @NotNull Tag<T, Phantom> tag, final long subtreeSize, final T @NotNull [] values) {
        super(tag, subtreeSize);
        assert values.length <= maxChunkLength;
        assert subtreeSize == tag.measureArray(values);
        this.values = values;
    }

    private Shallow(final @NotNull Tag<T, Phantom> tag) {
        this(tag, 0, tag.emptyArray());
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Shallow<T, Object> emptyUnit() {
        return (Shallow<T, Object>) emptyUnitInstance;
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Shallow<@NotNull Chunk<T>, Chunk<?>> emptyChunk() {
        return (Shallow<Chunk<T>, Chunk<?>>) emptyChunkInstance;
    }

    @SafeVarargs
    @SuppressWarnings("varargs")
    static <T> @NotNull Shallow<T, Object> ofUnits(final T... values) {
        return new Shallow<>(Tag.unit(), values.length, values);
    }

    static <T> @NotNull Shallow<@NotNull Chunk<T>, Chunk<?>> ofChunk(final @NotNull Chunk<T> chunk) {
        final var tag = Tag.<T>chunk();
        return new Shallow<>(tag, chunk.subtreeSize, tag.unitArray(chunk));
    }

    @Override
    public @NotNull Itr<T> iterator() {
        return new Itr<>(values);
    }

    @Override
    public boolean anySatisfies(final @NotNull Predicate<? super T> predicate) {
        return ArrayOps.anySatisfies(values, predicate);
    }

    @Override
    public <U> @NotNull TaggedSeq<U, Phantom> map(final @NotNull Function<? super T, ? extends U> function) {
        return new Shallow<>(tag.cast(), subtreeSize, ArrayOps.map(values, function));
    }

    @Override
    public T first() {
        checkNonEmpty("first called on an empty sequence");
        return values[0];
    }

    @Override
    public T last() {
        checkNonEmpty("last called on an empty sequence");
        return values[values.length - 1];
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedFirst(final T object) {
        checkNonEmpty("updatedFirst called on an empty sequence");
        final var newSize = updateSize(object, first());
        return new Shallow<>(tag, newSize, ArrayOps.updated(values, 0, object));
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedLast(final T object) {
        checkNonEmpty("updatedLast called on an empty sequence");
        final var newSize = updateSize(object, last());
        return new Shallow<>(tag, newSize, ArrayOps.updated(values, values.length - 1, object));
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> prepended(final T object) {
        final var newSize = addToSize(object);
        return (values.length < maxChunkLength) ? prependedSimple(newSize, object) : prependedDeep(newSize, object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> appended(final T object) {
        final var newSize = addToSize(object);
        return (values.length < maxChunkLength) ? appendedSimple(newSize, object) : appendedDeep(newSize, object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutFirst() {
        checkNonEmpty("withoutFirst called on an empty sequence");
        return (values.length > 1) ? withoutFirstImpl() : tag.emptySeq();
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutLast() {
        checkNonEmpty("withoutLast called on an empty sequence");
        return (values.length > 1) ? withoutLastImpl() : tag.emptySeq();
    }

    @Override
    void forEachArray(final @NotNull ArrayConsumer<T> action) {
        action.accept(values);
    }

    @Override
    @NotNull GetResult<T> getImpl(final long index, final long accumulator) {
        final var splitPoint = tag.findSplitPoint(values, index, accumulator);
        return getFromArray(values, splitPoint);
    }

    @Override
    boolean eligibleForInsertionSortImpl() {
        return true;
    }

    @Override
    T @NotNull [] toSmallArray() {
        assert eligibleForInsertionSort();
        return values.clone();
    }

    @Override
    @NotNull TaggedSeq<T, Phantom> concatImpl(final @NotNull TaggedSeq<T, Phantom> other) {
        if (isEmpty()) {
            return other;
        }
        return switch (other) {
            case Shallow<T, Phantom> shallow -> concatShallow(shallow);
            case Deep<T, Phantom> deep -> deep.prependedShallow(this);
        };
    }

    @Override
    @NotNull TreeSplit<T, Phantom> splitTree(final long index, final long accumulator) {
        final var splitPoint = tag.findSplitPoint(values, index, accumulator);
        final var split = tag.splitArray(values, splitPoint.index());
        final var middle = split.middle();
        final var middleSize = tag.measureSingle(middle);
        final var front = fromPossiblyEmptyArray(splitPoint.accumulator() - accumulator - middleSize, split.front());
        final var back = fromPossiblyEmptyArray(subtreeSize - front.subtreeSize - middleSize, split.back());
        return new TreeSplit<>(front, middle, back);
    }

    private @NotNull Shallow<T, Phantom> prependedSimple(final long newSize, final T object) {
        return new Shallow<>(tag, newSize, ArrayOps.prepended(values, object));
    }

    private @NotNull Deep<T, Phantom> prependedDeep(final long newSize, final T object) {
        return new Deep<>(tag, newSize, tag.unitArray(object), emptyChunk(), values);
    }

    private @NotNull Shallow<T, Phantom> appendedSimple(final long newSize, final T object) {
        return new Shallow<>(tag, newSize, ArrayOps.appended(values, object));
    }

    private @NotNull Deep<T, Phantom> appendedDeep(final long newSize, final T object) {
        return new Deep<>(tag, newSize, values, emptyChunk(), tag.unitArray(object));
    }

    private @NotNull Shallow<T, Phantom> withoutFirstImpl() {
        final var newSize = subtractFromSize(first());
        return new Shallow<>(tag, newSize, Arrays.copyOfRange(values, 1, values.length));
    }

    private @NotNull Shallow<T, Phantom> withoutLastImpl() {
        final var newSize = subtractFromSize(last());
        return new Shallow<>(tag, newSize, Arrays.copyOf(values, values.length - 1));
    }

    private @NotNull TaggedSeq<T, Phantom> concatShallow(final @NotNull Shallow<T, Phantom> other) {
        if (other.isEmpty()) {
            return this;
        }
        final var newSize = computeNewSize(other.subtreeSize);
        final var totalLength = values.length + other.values.length;
        if (totalLength <= maxChunkLength) {
            return new Shallow<>(tag, newSize, ArrayOps.concat(values, other.values));
        }
        return new Deep<>(tag, newSize, values, emptyChunk(), other.values);
    }

    private @NotNull Shallow<T, Phantom> fromPossiblyEmptyArray(final long size, final T @NotNull [] array) {
        return (array.length != 0) ? new Shallow<>(tag, size, array) : tag.emptySeq();
    }

    private void checkNonEmpty(final @NotNull String message) {
        if (values.length == 0) {
            throw noSuchElement(message);
        }
    }

    final T @NotNull [] values;

    private static final Shallow<?, Object> emptyUnitInstance = new Shallow<>(Tag.unit());
    private static final Shallow<? extends Chunk<?>, Chunk<?>> emptyChunkInstance = new Shallow<>(Tag.chunk());

    static final class Itr<T> extends Seq.Itr<T> {
        private Itr(final T @NotNull [] values) {
            this.values = values;
        }

        @Override
        public boolean hasNext() {
            return index < values.length;
        }

        @Override
        @SuppressFBWarnings(value = "IT_NO_SUCH_ELEMENT", justification = "It can, SpotBugs is just confused")
        public T next() {
            final var idx = index;
            if (idx >= values.length) {
                throw noMoreElements();
            }
            index = idx + 1;
            return values[idx];
        }

        @Override
        public long nextIndex() {
            return index;
        }

        @Override
        void forEachRemainingImpl(final @NotNull Consumer<? super T> action) {
            ArrayOps.forEachFrom(values, index, action);
            index = values.length;
        }

        private int index = 0;
        private final T @NotNull [] values;
    }
}