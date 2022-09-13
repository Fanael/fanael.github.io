// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Function;
import org.checkerframework.checker.nullness.qual.Nullable;

// NB: the class hierarchy is admittedly a bit backwards here: ideally, SeqImpl would be the base class, with Seq<T>
// extending SeqImpl<T, Object>, but that would unfortunately require code duplication of large swathes of Shallow and
// Deep. Because of that, public methods are expected to operate on and return instances of SeqImpl<T, Object>, see
// asUnitSeq(). Sequences of chunks are allowed to implement degenerate behavior, such as throwing an exception, for
// methods not used recursively, such as returning an iterator, because they're never supposed to be called in the first
// place.
abstract sealed class SeqImpl<T extends C, C> extends Seq<T> permits Shallow, Deep {
    SeqImpl(final long subtreeSize) {
        super(subtreeSize);
    }

    static NoSuchElementException noSuchElement(final String message) {
        throw new NoSuchElementException(message);
    }

    static <T> GetResult<T> getFromArray(final Tag<T, ? super T> tag, final T[] array, final long index) {
        final var splitPoint = tag.findSplitPoint(array, index);
        return new GetResult<>(array[splitPoint.index()], splitPoint.remainder());
    }

    static <T> T[] updatedArray(
        final Tag<T, ? super T> tag,
        final T[] array,
        final long index,
        final Updater<T> updater
    ) {
        final var splitPoint = tag.findSplitPoint(array, index);
        final var arrayIndex = splitPoint.index();
        final var oldValue = array[arrayIndex];
        final var newValue = updater.update(splitPoint.remainder(), oldValue);
        assert tag.sizeOf(oldValue) == tag.sizeOf(newValue);
        return ArrayOps.updated(array, arrayIndex, newValue);
    }

    @SuppressWarnings("unchecked")
    static <T extends C, U extends C, C> U[] mapArray(
        final Tag<T, C> ignored, // Only here to provide C, so the compiler can't just infer C = Object
        final T[] array,
        final Function<? super T, ? extends U> function
    ) {
        final var length = array.length;
        final @Nullable C[] newArray = ArrayOps.newArray(array, length);
        for (int i = 0; i < length; i += 1) {
            newArray[i] = function.apply(array[i]);
        }
        return (U[]) newArray;
    }

    static <T> Updater<Chunk<T>> makeChunkUpdater(final Tag<T, ? super T> tag, final Updater<T> updater) {
        return (index, chunk) -> Chunk.make(tag, chunk.subtreeSize, updatedArray(tag, chunk.values, index, updater));
    }

    static <T extends C, U extends C, C> Chunk<U> mapChunk(
        final Tag<T, C> tag,
        final Chunk<T> chunk,
        final Function<? super T, ? extends U> function
    ) {
        return Chunk.make(tag.cast(), chunk.subtreeSize, mapArray(tag, chunk.values, function));
    }

    @Override
    public final <U> Seq<U> map(final Function<? super T, ? extends U> function) {
        return asUnitSeq().map(Tag.unit(), function);
    }

    @Override
    public final Seq<T> prepended(final T value) {
        return asUnitSeq().prepended(Tag.unit(), value);
    }

    @Override
    public final Seq<T> appended(final T value) {
        return asUnitSeq().appended(Tag.unit(), value);
    }

    @Override
    public final Seq<T> updatedFirst(final T value) {
        return asUnitSeq().updatedFirst(Tag.unit(), value);
    }

    @Override
    public final Seq<T> updatedLast(final T value) {
        return asUnitSeq().updatedLast(Tag.unit(), value);
    }

    @Override
    public final Seq<T> withoutFirst() {
        return asUnitSeq().withoutFirst(Tag.unit());
    }

    @Override
    public final Seq<T> withoutLast() {
        return asUnitSeq().withoutLast(Tag.unit());
    }

    @Override
    public final Seq<T> concat(final Seq<? extends T> other) {
        return asUnitSeq().concat(Tag.unit(), baseToUnitSeq(other));
    }

    @Override
    public final T get(final long index) {
        Objects.checkIndex(index, subtreeSize); // Check once here, let implementations assume it's in bounds.
        return asUnitSeq().get(Tag.unit(), index).value;
    }

    @Override
    public final Seq<T> updated(final long index, final T newValue) {
        Objects.checkIndex(index, subtreeSize); // Check once here, let implementations assume it's in bounds.
        return asUnitSeq().updated(Tag.unit(), index, (idx, oldValue) -> newValue);
    }

    @Override
    public final Split<T> splitAt(final long index) {
        // Check once here, let implementations assume it's in bounds.
        Objects.checkFromToIndex(index, subtreeSize, subtreeSize);
        return asUnitSeq().splitAtImpl(Tag.unit(), index);
    }

    abstract <U extends C> SeqImpl<U, C> map(Tag<T, C> tag, Function<? super T, ? extends U> function);

    abstract SeqImpl<T, C> prepended(Tag<T, C> tag, T value);

    abstract SeqImpl<T, C> appended(Tag<T, C> tag, T value);

    abstract SeqImpl<T, C> updatedFirst(Tag<T, C> tag, T value);

    abstract SeqImpl<T, C> updatedLast(Tag<T, C> tag, T value);

    abstract SeqImpl<T, C> withoutFirst(Tag<T, C> tag);

    abstract SeqImpl<T, C> withoutLast(Tag<T, C> tag);

    abstract SeqImpl<T, C> concat(Tag<T, C> tag, SeqImpl<T, C> other);

    abstract GetResult<T> get(Tag<T, C> tag, long index);

    abstract SeqImpl<T, C> updated(Tag<T, C> tag, long index, Updater<T> updater);

    abstract TreeSplit<T, C> splitTree(Tag<T, C> tag, long index);

    abstract Class<? extends Object[]> getArrayType();

    final long addToSize(final Tag<T, C> tag, final T value) {
        return computeNewSize(tag.sizeOf(value));
    }

    final long subtractFromSize(final Tag<T, C> tag, final T value) {
        return computeNewSize(-tag.sizeOf(value));
    }

    final long updateSize(final Tag<T, C> tag, final T newValue, final T oldValue) {
        return computeNewSize(tag.sizeOf(newValue) - tag.sizeOf(oldValue));
    }

    private Split<T> splitAtImpl(final Tag<T, C> tag, final long index) {
        if (index >= exactSize()) {
            return new Split<>(this, empty());
        }
        final var treeSplit = splitTree(tag, index);
        return new Split<>(treeSplit.front, treeSplit.back.prepended(tag, treeSplit.middle));
    }

    @SuppressWarnings("unchecked")
    private SeqImpl<T, @Nullable Object> asUnitSeq() {
        assert Tag.unit().arrayTypeMatches(getArrayType());
        return (SeqImpl<T, @Nullable Object>) this;
    }

    private static <T> SeqImpl<T, @Nullable Object> baseToUnitSeq(final Seq<? extends T> seq) {
        // Since Seq is immutable, casting <? extends T> to <T> is fine.
        @SuppressWarnings("unchecked") final var impl = (SeqImpl<T, ? super T>) seq;
        return impl.asUnitSeq();
    }

    @FunctionalInterface
    interface Updater<T> {
        T update(long index, T oldValue);
    }

    record GetResult<T>(T value, long remainder) {
    }

    record TreeSplit<T extends C, C>(SeqImpl<T, C> front, T middle, SeqImpl<T, C> back) {
    }
}
