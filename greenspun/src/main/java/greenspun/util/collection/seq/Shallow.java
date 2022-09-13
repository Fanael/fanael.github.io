// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

final class Shallow<T extends C, C> extends SeqImpl<T, C> {
    private Shallow(final long subtreeSize, final T[] values) {
        super(subtreeSize);
        assert values.length <= maxLength;
        this.values = values;
    }

    @SuppressWarnings("unchecked")
    static <T> Shallow<T, @Nullable Object> emptyUnit() {
        return (Shallow<T, @Nullable Object>) emptyUnit;
    }

    @SuppressWarnings("unchecked")
    static <T> Shallow<Chunk<T>, Chunk<?>> emptyChunk() {
        return (Shallow<Chunk<T>, Chunk<?>>) emptyChunk;
    }

    @SafeVarargs
    @SuppressWarnings("varargs")
    static <T> Shallow<T, @Nullable Object> ofUnits(final T... values) {
        return make(Tag.unit(), values.length, values);
    }

    static <T> Shallow<Chunk<T>, Chunk<?>> ofChunk(final Tag<T, ? super T> tag, final T[] values) {
        final var chunkTag = Tag.<T>chunk();
        final var chunk = Chunk.make(tag, values);
        return make(chunkTag, chunk.subtreeSize, chunkTag.unitArray(chunk));
    }

    static <T extends C, C> Shallow<T, C> make(final Tag<T, C> tag, final long subtreeSize, final T[] values) {
        assert tag.arrayTypeMatches(values.getClass());
        assert subtreeSize == tag.sumOfSizes(values);
        return new Shallow<>(subtreeSize, values);
    }

    @Override
    public Seq.@NonNull Itr<T> iterator() {
        assert Tag.unit().arrayTypeMatches(values.getClass());
        return new Itr<>(values);
    }

    @Override
    public boolean anySatisfies(final Predicate<? super T> predicate) {
        return ArrayOps.anySatisfies(values, predicate);
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
    void forEachArray(final ArrayConsumer<? super T> action) {
        action.accept(values);
    }

    @Override
    <U extends C> SeqImpl<U, C> map(final Tag<T, C> tag, final Function<? super T, ? extends U> function) {
        return make(tag.cast(), subtreeSize, mapArray(tag, values, function));
    }

    @Override
    SeqImpl<T, C> prepended(final Tag<T, C> tag, final T value) {
        return (values.length < maxLength) ? prependedSimple(tag, value) : prependedDeep(tag, value);
    }

    @Override
    SeqImpl<T, C> appended(final Tag<T, C> tag, final T value) {
        return (values.length < maxLength) ? appendedSimple(tag, value) : appendedDeep(tag, value);
    }

    @Override
    SeqImpl<T, C> updatedFirst(final Tag<T, C> tag, final T value) {
        checkNonEmpty("updatedFirst called on an empty sequence");
        final var newSize = updateSize(tag, value, first());
        return make(tag, newSize, ArrayOps.updated(values, 0, value));
    }

    @Override
    SeqImpl<T, C> updatedLast(final Tag<T, C> tag, final T value) {
        checkNonEmpty("updatedLast called on an empty sequence");
        final var newSize = updateSize(tag, value, last());
        return make(tag, newSize, ArrayOps.updated(values, values.length - 1, value));
    }

    @Override
    SeqImpl<T, C> withoutFirst(final Tag<T, C> tag) {
        checkNonEmpty("withoutFirst called on an empty sequence");
        return (values.length > 1) ? withoutFirstImpl(tag) : tag.emptySeq();
    }

    @Override
    SeqImpl<T, C> withoutLast(final Tag<T, C> tag) {
        checkNonEmpty("withoutLast called on an empty sequence");
        return (values.length > 1) ? withoutLastImpl(tag) : tag.emptySeq();
    }

    @Override
    SeqImpl<T, C> concat(final Tag<T, C> tag, final SeqImpl<T, C> other) {
        if (isEmpty()) {
            return other;
        }
        return switch (other) {
            case Shallow<T, C> shallow -> concatShallow(tag, shallow);
            case Deep<T, C> deep -> deep.prependedShallow(tag, this);
        };
    }

    @Override
    GetResult<T> get(final Tag<T, C> tag, final long index) {
        return getFromArray(tag, values, index);
    }

    @Override
    SeqImpl<T, C> updated(final Tag<T, C> tag, final long index, final Updater<T> updater) {
        return make(tag, subtreeSize, updatedArray(tag, values, index, updater));
    }

    @Override
    TreeSplit<T, C> splitTree(final Tag<T, C> tag, final long index) {
        final var point = tag.findSplitPoint(values, index);
        final var split = tag.splitArray(values, point.index());
        final var middle = split.middle();
        final var frontSize = point.prefixSize();
        final var front = fromPossiblyEmptyArray(tag, frontSize, split.front());
        final var back = fromPossiblyEmptyArray(tag, subtreeSize - frontSize - tag.sizeOf(middle), split.back());
        return new TreeSplit<>(front, middle, back);
    }

    @Override
    Class<? extends Object[]> getArrayType() {
        return values.getClass();
    }

    private Shallow<T, C> prependedSimple(final Tag<T, C> tag, final T value) {
        final var newSize = addToSize(tag, value);
        return make(tag, newSize, ArrayOps.prepended(values, value));
    }

    private Deep<T, C> prependedDeep(final Tag<T, C> tag, final T value) {
        final var valueSize = tag.sizeOf(value);
        return Deep.make(tag, computeNewSize(valueSize), valueSize, tag.unitArray(value), emptyChunk(), values);
    }

    private Shallow<T, C> appendedSimple(final Tag<T, C> tag, final T value) {
        final var newSize = addToSize(tag, value);
        return make(tag, newSize, ArrayOps.appended(values, value));
    }

    private SeqImpl<T, C> appendedDeep(final Tag<T, C> tag, final T value) {
        final var newSize = addToSize(tag, value);
        return Deep.make(tag, newSize, subtreeSize, values, emptyChunk(), tag.unitArray(value));
    }

    private Shallow<T, C> withoutFirstImpl(final Tag<T, C> tag) {
        final var newSize = subtractFromSize(tag, first());
        return make(tag, newSize, ArrayOps.drop(values, 1));
    }

    private Shallow<T, C> withoutLastImpl(final Tag<T, C> tag) {
        final var newSize = subtractFromSize(tag, last());
        return make(tag, newSize, ArrayOps.take(values, values.length - 1));
    }

    private SeqImpl<T, C> concatShallow(final Tag<T, C> tag, final Shallow<T, C> other) {
        if (other.isEmpty()) {
            return this;
        }
        final var newSize = computeNewSize(other.subtreeSize);
        final var totalLength = values.length + other.values.length;
        // Note: folding this if into a ternary confuses Checker Framework, don't do it.
        if (totalLength <= maxLength) {
            return make(tag, newSize, ArrayOps.concat(values, other.values));
        }
        return Deep.make(tag, newSize, subtreeSize, values, emptyChunk(), other.values);
    }

    private void checkNonEmpty(final String message) {
        if (values.length == 0) {
            throw noSuchElement(message);
        }
    }

    private static <T extends C, C> Shallow<T, C> fromPossiblyEmptyArray(
        final Tag<T, C> tag,
        final long size,
        final T[] array
    ) {
        return (array.length != 0) ? make(tag, size, array) : tag.emptySeq();
    }

    private static <T extends C, C> Shallow<T, C> makeEmpty(final Tag<T, C> tag) {
        return make(tag, 0, tag.emptyArray());
    }

    final T[] values;

    private static final int maxLength = Chunk.maxLength;
    private static final Shallow<?, Object> emptyUnit = makeEmpty(Tag.unit());
    private static final Shallow<? extends Chunk<?>, Chunk<?>> emptyChunk = makeEmpty(Tag.chunk());

    static final class Itr<T> extends Seq.Itr<T> {
        private Itr(final T[] values) {
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
        public T peek() {
            if (index >= values.length) {
                throw noMoreElements();
            }
            return values[index];
        }

        @Override
        public long nextIndex() {
            return index;
        }

        @Override
        void forEachRemainingImpl(final Consumer<? super T> action) {
            ArrayOps.forEachFrom(values, index, action);
            index = values.length;
        }

        @Override
        SeqImpl<T, @Nullable Object> restImpl() {
            return (index < values.length) ? ofUnits(ArrayOps.drop(values, index)) : emptyUnit();
        }

        private int index = 0;
        private final T[] values;
    }
}
