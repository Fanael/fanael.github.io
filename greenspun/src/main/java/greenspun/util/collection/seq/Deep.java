// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

final class Deep<T extends C, C> extends SeqImpl<T, C> {
    private Deep(
        final long subtreeSize,
        final long prefixSize,
        final T[] prefix,
        final SeqImpl<Chunk<T>, Chunk<?>> middle,
        final T[] suffix
    ) {
        super(subtreeSize);
        assert prefix.length >= 1 && prefix.length <= maxAffixLength;
        assert suffix.length >= 1 && suffix.length <= maxAffixLength;
        assert prefix.getClass() == suffix.getClass();
        assert subtreeSize > 0;
        this.prefixSize = prefixSize;
        this.prefix = prefix;
        this.middle = middle;
        this.suffix = suffix;
    }

    static <T extends C, C> Deep<T, C> make(
        final Tag<T, C> tag,
        final long subtreeSize,
        final long prefixSize,
        final T[] prefix,
        final SeqImpl<Chunk<T>, Chunk<?>> middle,
        final T[] suffix
    ) {
        assert tag.arrayTypeMatches(prefix.getClass());
        assert prefixSize == tag.sumOfSizes(prefix);
        assert subtreeSize == prefixSize + middle.subtreeSize + tag.sumOfSizes(suffix);
        return new Deep<>(subtreeSize, prefixSize, prefix, middle, suffix);
    }

    static <T extends C, C> SeqImpl<T, C> fromSingleArray(
        final Tag<T, C> tag,
        final long newSize,
        final T[] array,
        final int shallowLimit
    ) {
        if (array.length <= shallowLimit) {
            return (array.length != 0) ? Shallow.make(tag, newSize, array) : tag.emptySeq();
        }
        final var split = ArrayOps.split(array);
        final var prefix = split.front();
        return make(tag, newSize, tag.sumOfSizes(prefix), prefix, Shallow.emptyChunk(), split.back());
    }

    @Override
    public Seq.@NonNull Itr<T> iterator() {
        return new Itr<>(this);
    }

    @Override
    public boolean anySatisfies(final Predicate<? super T> predicate) {
        return ArrayOps.anySatisfies(prefix, predicate)
            || ArrayOps.anySatisfies(suffix, predicate)
            || middle.anySatisfies(chunk -> ArrayOps.anySatisfies(chunk.values, predicate));
    }

    @Override
    public T first() {
        return prefix[0];
    }

    @Override
    public T last() {
        return suffix[suffix.length - 1];
    }

    @Override
    void forEachArray(final ArrayConsumer<? super T> action) {
        action.accept(prefix);
        middle.forEachArray(chunks -> ArrayOps.forEach(chunks, chunk -> action.accept(chunk.values)));
        action.accept(suffix);
    }

    @Override
    <U extends C> SeqImpl<U, C> map(final Tag<T, C> tag, final Function<? super T, ? extends U> function) {
        final var newPrefix = mapArray(tag, prefix, function);
        final var newMiddle = middle.map(Tag.chunk(), chunk -> SeqImpl.<T, U, C>mapChunk(tag, chunk, function));
        final var newSuffix = mapArray(tag, suffix, function);
        return make(tag.cast(), subtreeSize, prefixSize, newPrefix, newMiddle, newSuffix);
    }

    @Override
    SeqImpl<T, C> prepended(final Tag<T, C> tag, final T value) {
        return (prefix.length < maxAffixLength) ? prependedSimple(tag, value) : prependedMiddle(tag, value);
    }

    @Override
    SeqImpl<T, C> appended(final Tag<T, C> tag, final T value) {
        return (suffix.length < maxAffixLength) ? appendedSimple(tag, value) : appendedMiddle(tag, value);
    }

    @Override
    SeqImpl<T, C> updatedFirst(final Tag<T, C> tag, final T value) {
        final var delta = tag.sizeOf(value) - tag.sizeOf(first());
        return withNewFront(tag, computeNewSize(delta), prefixSize + delta, ArrayOps.updated(prefix, 0, value));
    }

    @Override
    SeqImpl<T, C> updatedLast(final Tag<T, C> tag, final T value) {
        final var newSize = updateSize(tag, value, last());
        return withNewBack(tag, newSize, middle, ArrayOps.updated(suffix, suffix.length - 1, value));
    }

    @Override
    SeqImpl<T, C> withoutFirst(final Tag<T, C> tag) {
        return (prefix.length > 1) ? withoutFirstSimple(tag) : withoutFirstNoPrefix(tag);
    }

    @Override
    SeqImpl<T, C> withoutLast(final Tag<T, C> tag) {
        return (suffix.length > 1) ? withoutLastSimple(tag) : withoutLastNoSuffix(tag);
    }

    @Override
    SeqImpl<T, C> concat(final Tag<T, C> tag, final SeqImpl<T, C> other) {
        return switch (other) {
            case Shallow<T, C> shallow -> appendedShallow(tag, shallow);
            case Deep<T, C> deep -> concatDeep(tag, deep);
        };
    }

    @Override
    GetResult<T> get(final Tag<T, C> tag, final long index) {
        if (index < prefixSize) {
            return getFromArray(tag, prefix, index);
        }
        final var frontSize = prefixSize + middle.subtreeSize;
        if (index < frontSize) {
            final var result = middle.get(Tag.chunk(), index - prefixSize);
            return getFromArray(tag, result.value().values, result.remainder());
        }
        return getFromArray(tag, suffix, index - frontSize);
    }

    @Override
    SeqImpl<T, C> updated(final Tag<T, C> tag, final long index, final Updater<T> updater) {
        if (index < prefixSize) {
            return withNewFront(tag, subtreeSize, prefixSize, updatedArray(tag, prefix, index, updater));
        }
        final var frontSize = prefixSize + middle.subtreeSize;
        if (index < frontSize) {
            final var newMiddle = middle.updated(Tag.chunk(), index - prefixSize, makeChunkUpdater(tag, updater));
            return withNewBack(tag, subtreeSize, newMiddle, suffix);
        }
        return withNewBack(tag, subtreeSize, middle, updatedArray(tag, suffix, index - frontSize, updater));
    }

    @Override
    TreeSplit<T, C> splitTree(final Tag<T, C> tag, final long index) {
        if (index < prefixSize) {
            return splitAtPrefix(tag, index);
        }
        final var frontSize = prefixSize + middle.subtreeSize;
        if (index < frontSize) {
            return splitAtMiddle(tag, index - prefixSize);
        }
        return splitAtSuffix(tag, index - frontSize);
    }

    @Override
    Class<? extends Object[]> getArrayType() {
        return prefix.getClass();
    }

    Deep<T, C> prependedShallow(final Tag<T, C> tag, final Shallow<T, C> other) {
        if (other.isEmpty()) {
            return this;
        }
        final var newSize = computeNewSize(other.subtreeSize);
        final var lengthAfterConcat = other.values.length + prefix.length;
        if (lengthAfterConcat <= maxAffixLength) {
            return withNewFront(tag, newSize, prefixSize + other.subtreeSize, ArrayOps.concat(other.values, prefix));
        }
        final var split = ArrayOps.concatSplitAt(other.values, prefix, lengthAfterConcat - Chunk.maxLength);
        final var newMiddle = middle.prepended(Tag.chunk(), Chunk.make(tag, split.back()));
        return withNewFront(tag, newSize, split.front(), newMiddle);
    }

    private Deep<T, C> prependedSimple(final Tag<T, C> tag, final T value) {
        final var valueSize = tag.sizeOf(value);
        return withNewFront(tag, computeNewSize(valueSize), prefixSize + valueSize, ArrayOps.prepended(prefix, value));
    }

    private Deep<T, C> prependedMiddle(final Tag<T, C> tag, final T value) {
        final var newSize = addToSize(tag, value);
        final var newPrefix = ArrayOps.prependedSlice(prefix, value);
        final var chunk = Chunk.make(tag, ArrayOps.drop(prefix, prefix.length - Chunk.maxLength));
        return withNewFront(tag, newSize, newPrefix, middle.prepended(Tag.chunk(), chunk));
    }

    private SeqImpl<T, C> appendedSimple(final Tag<T, C> tag, final T value) {
        final var newSize = addToSize(tag, value);
        return withNewBack(tag, newSize, middle, ArrayOps.appended(suffix, value));
    }

    private SeqImpl<T, C> appendedMiddle(final Tag<T, C> tag, final T value) {
        final var newSize = addToSize(tag, value);
        final var chunk = Chunk.make(tag, ArrayOps.take(suffix, Chunk.maxLength));
        final var newSuffix = ArrayOps.appendedSlice(suffix, value);
        return withNewBack(tag, newSize, middle.appended(Tag.chunk(), chunk), newSuffix);
    }

    private Deep<T, C> withoutFirstSimple(final Tag<T, C> tag) {
        final var size = tag.sizeOf(first());
        return withNewFront(tag, computeNewSize(-size), prefixSize - size, ArrayOps.drop(prefix, 1));
    }

    private SeqImpl<T, C> withoutFirstNoPrefix(final Tag<T, C> tag) {
        final var newSize = subtractFromSize(tag, first());
        return fromEmptyPrefix(tag, newSize, middle, suffix);
    }

    private Deep<T, C> withoutLastSimple(final Tag<T, C> tag) {
        final var newSize = subtractFromSize(tag, last());
        return withNewBack(tag, newSize, middle, ArrayOps.take(suffix, suffix.length - 1));
    }

    private SeqImpl<T, C> withoutLastNoSuffix(final Tag<T, C> tag) {
        final var newSize = subtractFromSize(tag, last());
        return fromEmptySuffix(tag, newSize, prefixSize, prefix, middle);
    }

    private Deep<T, C> appendedShallow(final Tag<T, C> tag, final Shallow<T, C> other) {
        if (other.isEmpty()) {
            return this;
        }
        final var newSize = computeNewSize(other.subtreeSize);
        if (suffix.length + other.values.length <= maxAffixLength) {
            return withNewBack(tag, newSize, middle, ArrayOps.concat(suffix, other.values));
        }
        final var split = ArrayOps.concatSplitAt(suffix, other.values, Chunk.maxLength);
        final var newMiddle = middle.appended(Tag.chunk(), Chunk.make(tag, split.front()));
        return withNewBack(tag, newSize, newMiddle, split.back());
    }

    private Deep<T, C> concatDeep(final Tag<T, C> tag, final Deep<T, C> other) {
        return (suffix.length + other.prefix.length >= Chunk.minLength)
            ? concatInsertingInfix(tag, other)
            : concatPartial(tag, other);
    }

    private Deep<T, C> concatInsertingInfix(final Tag<T, C> tag, final Deep<T, C> other) {
        // The infix is long enough that it can be turned into at least one chunk.
        final var newSize = computeNewSize(other.subtreeSize);
        final var chunkTag = Tag.<T>chunk();
        final var infixLength = suffix.length + other.prefix.length;
        if (infixLength <= Chunk.maxLength) {
            // The infix is short enough that it forms one chunk.
            final var chunk = Chunk.make(tag, ArrayOps.concat(suffix, other.prefix));
            final var newMiddle = middle.appended(chunkTag, chunk).concat(chunkTag, other.middle);
            return withNewBack(tag, newSize, newMiddle, other.suffix);
        }
        if (infixLength <= 2 * Chunk.maxLength) {
            // The infix is too long for a single chunk, but fits in two.
            final var infix = ArrayOps.concatSplitAt(suffix, other.prefix, infixLength / 2);
            final var frontMiddle = middle.appended(chunkTag, Chunk.make(tag, infix.front()));
            final var backMiddle = other.middle.prepended(chunkTag, Chunk.make(tag, infix.back()));
            return withNewBack(tag, newSize, frontMiddle.concat(chunkTag, backMiddle), other.suffix);
        }
        // The infix is so long it needs three chunks; can happen if at least one of the affixes is longer than
        // Chunk.maxLength.
        final var firstSplit = ArrayOps.concatSplitAt(suffix, other.prefix, infixLength / 3);
        final var secondSplit = ArrayOps.split(firstSplit.back());
        final var frontMiddle = middle.appended(chunkTag, Chunk.make(tag, firstSplit.front()))
            .appended(chunkTag, Chunk.make(tag, secondSplit.front()));
        final var backMiddle = other.middle.prepended(chunkTag, Chunk.make(tag, secondSplit.back()));
        return withNewBack(tag, newSize, frontMiddle.concat(chunkTag, backMiddle), other.suffix);
    }

    private Deep<T, C> concatPartial(final Tag<T, C> tag, final Deep<T, C> other) {
        // The infix is too short to form a chunk, need to merge it with the middles, or if those are empty, the outer
        // affixes.
        final var newSize = computeNewSize(other.subtreeSize);
        final var front = mergeShortSuffix(tag);
        final var back = other.mergeShortPrefix(tag);
        final var newMiddle = front.middle.concat(Tag.chunk(), back.middle);
        return make(tag, newSize, front.prefixSize, front.prefix, newMiddle, back.suffix);
    }

    private PartialBack<T> mergeShortPrefix(final Tag<T, C> tag) {
        return middle.isEmpty() ? mergeShortPrefixWithSuffix(tag) : mergeShortPrefixWithMiddle(tag);
    }

    private PartialFront<T> mergeShortSuffix(final Tag<T, C> tag) {
        return middle.isEmpty() ? mergeShortSuffixWithPrefix(tag) : mergeShortSuffixWithMiddle(tag);
    }

    private PartialBack<T> mergeShortPrefixWithSuffix(final Tag<T, C> tag) {
        assert middle.isEmpty();
        final var totalLength = prefix.length + suffix.length;
        if (totalLength <= maxAffixLength) {
            return new PartialBack<>(Shallow.emptyChunk(), ArrayOps.concat(prefix, suffix));
        }
        final var split = ArrayOps.concatSplitAt(prefix, suffix, totalLength / 2);
        return new PartialBack<>(Shallow.ofChunk(tag, split.front()), split.back());
    }

    private PartialFront<T> mergeShortSuffixWithPrefix(final Tag<T, C> tag) {
        assert middle.isEmpty();
        final var totalLength = prefix.length + suffix.length;
        if (totalLength <= maxAffixLength) {
            return new PartialFront<>(subtreeSize, ArrayOps.concat(prefix, suffix), Shallow.emptyChunk());
        }
        final var split = ArrayOps.concatSplitAt(prefix, suffix, totalLength / 2);
        return new PartialFront<>(tag.sumOfSizes(split.front()), split.front(), Shallow.ofChunk(tag, split.back()));
    }

    private PartialBack<T> mergeShortPrefixWithMiddle(final Tag<T, C> tag) {
        final var chunkTag = Tag.<T>chunk();
        final var firstChunk = middle.first();
        final var totalLength = prefix.length + firstChunk.values.length;
        if (totalLength <= Chunk.maxLength) {
            final var newChunkSize = prefixSize + firstChunk.subtreeSize;
            final var newFirstChunk = Chunk.make(tag, newChunkSize, ArrayOps.concat(prefix, firstChunk.values));
            return new PartialBack<>(middle.updatedFirst(chunkTag, newFirstChunk), suffix);
        }
        final var split = ArrayOps.concatSplitAt(prefix, firstChunk.values, totalLength / 2);
        final var newFirstChunk = Chunk.make(tag, split.front());
        final var newSecondChunk = Chunk.make(tag, split.back());
        final var newMiddle = middle.updatedFirst(chunkTag, newSecondChunk).prepended(chunkTag, newFirstChunk);
        return new PartialBack<>(newMiddle, suffix);
    }

    private PartialFront<T> mergeShortSuffixWithMiddle(final Tag<T, C> tag) {
        final var chunkTag = Tag.<T>chunk();
        final var lastChunk = middle.last();
        final var totalLength = lastChunk.values.length + suffix.length;
        if (totalLength <= Chunk.maxLength) {
            final var newChunkSize =
                lastChunk.subtreeSize + (subtreeSize - prefixSize - middle.subtreeSize);
            final var newLastChunk = Chunk.make(tag, newChunkSize, ArrayOps.concat(lastChunk.values, suffix));
            return new PartialFront<>(prefixSize, prefix, middle.updatedLast(chunkTag, newLastChunk));
        }
        final var split = ArrayOps.concatSplitAt(lastChunk.values, suffix, totalLength / 2);
        final var newPenultimateChunk = Chunk.make(tag, split.front());
        final var newLastChunk = Chunk.make(tag, split.back());
        final var newMiddle = middle.updatedLast(chunkTag, newPenultimateChunk).appended(chunkTag, newLastChunk);
        return new PartialFront<>(prefixSize, prefix, newMiddle);
    }

    private TreeSplit<T, C> splitAtPrefix(final Tag<T, C> tag, final long index) {
        final var point = tag.findSplitPoint(prefix, index);
        final var split = tag.splitArray(prefix, point.index());
        final var middle = split.middle();
        final var middleSize = tag.sizeOf(middle);
        final var frontSize = point.prefixSize();
        final var front = fromSingleArray(tag, frontSize, split.front());
        final var backPrefixSize = prefixSize - frontSize - middleSize;
        final var backSize = subtreeSize - frontSize - middleSize;
        final var back = fromUnknownPrefix(tag, backSize, backPrefixSize, split.back(), this.middle, suffix);
        return new TreeSplit<>(front, middle, back);
    }

    private TreeSplit<T, C> splitAtMiddle(final Tag<T, C> tag, final long index) {
        final var treeSplit = middle.splitTree(Tag.chunk(), index);
        final var frontSeq = treeSplit.front();
        final var chunk = treeSplit.middle();
        final var point = tag.findSplitPoint(chunk.values, index - frontSeq.subtreeSize);
        final var split = tag.splitArray(chunk.values, point.index());
        final var middle = split.middle();
        final var middleSize = tag.sizeOf(middle);
        final var frontSize = prefixSize + frontSeq.subtreeSize + point.prefixSize();
        final var front = fromUnknownSuffix(tag, frontSize, prefixSize, prefix, frontSeq, split.front());
        final var backPrefixSize = chunk.subtreeSize - point.prefixSize() - middleSize;
        final var backSize = subtreeSize - frontSize - middleSize;
        final var back = fromUnknownPrefix(tag, backSize, backPrefixSize, split.back(), treeSplit.back(), suffix);
        return new TreeSplit<>(front, middle, back);
    }

    private TreeSplit<T, C> splitAtSuffix(final Tag<T, C> tag, final long index) {
        final var point = tag.findSplitPoint(suffix, index);
        final var split = tag.splitArray(suffix, point.index());
        final var middle = split.middle();
        final var frontSize = prefixSize + this.middle.subtreeSize + point.prefixSize();
        final var front = fromUnknownSuffix(tag, frontSize, prefixSize, prefix, this.middle, split.front());
        final var back = fromSingleArray(tag, subtreeSize - frontSize - tag.sizeOf(middle), split.back());
        return new TreeSplit<>(front, middle, back);
    }

    private static <T extends C, C> SeqImpl<T, C> fromUnknownPrefix(
        final Tag<T, C> tag,
        final long size,
        final long prefixSize,
        final T[] prefix,
        final SeqImpl<Chunk<T>, Chunk<?>> middle,
        final T[] suffix
    ) {
        return (prefix.length == 0)
            ? fromEmptyPrefix(tag, size, middle, suffix)
            : make(tag, size, prefixSize, prefix, middle, suffix);
    }

    private static <T extends C, C> SeqImpl<T, C> fromUnknownSuffix(
        final Tag<T, C> tag,
        final long size,
        final long prefixSize,
        final T[] prefix,
        final SeqImpl<Chunk<T>, Chunk<?>> middle,
        final T[] suffix
    ) {
        return (suffix.length == 0)
            ? fromEmptySuffix(tag, size, prefixSize, prefix, middle)
            : make(tag, size, prefixSize, prefix, middle, suffix);
    }

    private static <T extends C, C> SeqImpl<T, C> fromEmptyPrefix(
        final Tag<T, C> tag,
        final long newSize,
        final SeqImpl<Chunk<T>, Chunk<?>> middle,
        final T[] suffix
    ) {
        if (middle.isEmpty()) {
            return fromSingleArray(tag, newSize, suffix);
        }
        final var chunk = middle.first();
        final var newMiddle = middle.withoutFirst(Tag.chunk());
        return make(tag, newSize, chunk.subtreeSize, chunk.values, newMiddle, suffix);
    }

    private static <T extends C, C> SeqImpl<T, C> fromEmptySuffix(
        final Tag<T, C> tag,
        final long newSize,
        final long prefixSize,
        final T[] prefix,
        final SeqImpl<Chunk<T>, Chunk<?>> middle
    ) {
        if (middle.isEmpty()) {
            return fromSingleArray(tag, newSize, prefix);
        }
        final var suffix = middle.last().values;
        final var newMiddle = middle.withoutLast(Tag.chunk());
        return make(tag, newSize, prefixSize, prefix, newMiddle, suffix);
    }

    private Deep<T, C> withNewFront(
        final Tag<T, C> tag,
        final long newSize,
        final long newPrefixSize,
        final T[] newPrefix
    ) {
        return make(tag, newSize, newPrefixSize, newPrefix, middle, suffix);
    }

    private Deep<T, C> withNewFront(
        final Tag<T, C> tag,
        final long newSize,
        final T[] newPrefix,
        final SeqImpl<Chunk<T>, Chunk<?>> newMiddle
    ) {
        return make(tag, newSize, tag.sumOfSizes(newPrefix), newPrefix, newMiddle, suffix);
    }

    private Deep<T, C> withNewBack(
        final Tag<T, C> tag,
        final long newSize,
        final SeqImpl<Chunk<T>, Chunk<?>> newMiddle,
        final T[] newSuffix
    ) {
        return make(tag, newSize, prefixSize, prefix, newMiddle, newSuffix);
    }

    private static <T extends C, C> SeqImpl<T, C> fromSingleArray(
        final Tag<T, C> tag,
        final long newSize,
        final T[] array
    ) {
        return fromSingleArray(tag, newSize, array, maxElementsToShrink);
    }

    // Allow 4 more elements in affixes to ensure that almost all arrays in a sequence built by repeatedly
    // appending/prepending are maxChunkLength elements long, and that the complexity requirements are met.
    private static final int maxAffixLength = Chunk.maxLength + 4;
    // The number of elements at which removals are allowed to turn deep sequences into shallow ones.
    private static final int maxElementsToShrink = Chunk.minLength;

    private final long prefixSize;
    private final T[] prefix;
    private final SeqImpl<Chunk<T>, Chunk<?>> middle;
    private final T[] suffix;

    static final class Itr<T> extends Seq.Itr<T> {
        private Itr(final Deep<T, ? super T> parent) {
            assert Tag.unit().arrayTypeMatches(parent.prefix.getClass());
            array = parent.prefix;
            remainingMiddle = parent.middle;
            remainingSuffix = parent.suffix;
        }

        @Override
        public boolean hasNext() {
            return index < array.length || remainingSuffix != null;
        }

        @Override
        @SuppressWarnings("IteratorNextCanNotThrowNoSuchElementException")
        public T next() {
            nextArrayIfNeeded();
            final var idx = index;
            index = idx + 1;
            sequenceIndex += 1;
            return array[idx];
        }

        @Override
        public T peek() {
            nextArrayIfNeeded();
            return array[index];
        }

        @Override
        public long nextIndex() {
            return sequenceIndex;
        }

        @Override
        void forEachRemainingImpl(final Consumer<? super T> action) {
            while (true) {
                ArrayOps.forEachFrom(array, index, action);
                sequenceIndex += array.length - index;
                if (remainingSuffix == null) {
                    break;
                }
                nextArray();
            }
        }

        @Override
        @SuppressWarnings("VariableNotUsedInsideIf")
        SeqImpl<T, @Nullable Object> restImpl() {
            return (remainingSuffix == null) ? makeRestFromSingle() : makeRestFromMany();
        }

        private void nextArrayIfNeeded() {
            if (index >= array.length) {
                nextArray();
            }
        }

        private void nextArray() {
            if (remainingMiddle.isEmpty()) {
                nextArrayFromSuffix();
            } else {
                nextArrayFromMiddle();
            }
            index = 0;
        }

        private void nextArrayFromMiddle() {
            array = remainingMiddle.first().values;
            remainingMiddle = remainingMiddle.withoutFirst(Tag.chunk());
        }

        private void nextArrayFromSuffix() {
            if (remainingSuffix == null) {
                throw noMoreElements();
            }
            array = remainingSuffix;
            remainingSuffix = null;
        }

        private SeqImpl<T, @Nullable Object> makeRestFromSingle() {
            if (index >= array.length) {
                return Shallow.emptyUnit();
            }
            final Tag<T, @Nullable Object> tag = Tag.unit();
            final var newArray = ArrayOps.drop(array, index);
            return fromSingleArray(tag, tag.sumOfSizes(newArray), newArray, Chunk.maxLength);
        }

        @RequiresNonNull("remainingSuffix")
        private SeqImpl<T, @Nullable Object> makeRestFromMany() {
            final var suffix = remainingSuffix;
            final Tag<T, @Nullable Object> tag = Tag.unit();
            final var backSize = remainingMiddle.subtreeSize + tag.sumOfSizes(suffix);
            if (index >= array.length) {
                return fromEmptyPrefix(tag, backSize, remainingMiddle, suffix);
            }
            final var prefix = ArrayOps.drop(array, index);
            final var prefixSize = tag.sumOfSizes(prefix);
            return make(tag, prefixSize + backSize, prefixSize, prefix, remainingMiddle, suffix);
        }

        private int index = 0;
        private T[] array;
        private long sequenceIndex = 0;
        private SeqImpl<Chunk<T>, Chunk<?>> remainingMiddle;
        private T @Nullable [] remainingSuffix;
    }

    private record PartialFront<T>(long prefixSize, T[] prefix, SeqImpl<Chunk<T>, Chunk<?>> middle) {
    }

    private record PartialBack<T>(SeqImpl<Chunk<T>, Chunk<?>> middle, T[] suffix) {
    }
}
