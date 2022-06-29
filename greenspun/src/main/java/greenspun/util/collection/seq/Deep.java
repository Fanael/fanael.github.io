// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class Deep<T, Phantom> extends TaggedSeq<T, Phantom> {
    Deep(
        final @NotNull Tag<T, Phantom> tag,
        final long subtreeSize,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle,
        final T @NotNull [] suffix
    ) {
        super(tag, subtreeSize);
        this.prefix = prefix;
        this.middle = middle;
        this.suffix = suffix;
        assert checkInvariants();
    }

    @Override
    public @NotNull Itr<T> iterator() {
        return new Itr<>(this);
    }

    @Override
    public boolean anySatisfies(final @NotNull Predicate<? super T> predicate) {
        return ArrayOps.anySatisfies(prefix, predicate)
            || ArrayOps.anySatisfies(suffix, predicate)
            || middle.anySatisfies(chunk -> ArrayOps.anySatisfies(chunk.values, predicate));
    }

    @Override
    public <U> @NotNull TaggedSeq<U, Phantom> map(final @NotNull Function<? super T, ? extends U> function) {
        final var newTag = tag.<U>cast();
        final var newPrefix = ArrayOps.map(prefix, function);
        final var newMiddle = middle.map(chunk -> chunk.map(newTag, function));
        final var newSuffix = ArrayOps.map(suffix, function);
        return new Deep<>(newTag, subtreeSize, newPrefix, newMiddle, newSuffix);
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
    public @NotNull TaggedSeq<T, Phantom> updatedFirst(final T object) {
        final var newSize = updateSize(object, first());
        return withNewFront(newSize, ArrayOps.updated(prefix, 0, object), middle);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedLast(final T object) {
        final var newSize = updateSize(object, last());
        return withNewBack(newSize, middle, ArrayOps.updated(suffix, suffix.length - 1, object));
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> prepended(final T object) {
        final var size = addToSize(object);
        return (prefix.length < maxAffixLength) ? prependedSimple(size, object) : prependedRecursive(size, object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> appended(final T object) {
        final var size = addToSize(object);
        return (suffix.length < maxAffixLength) ? appendedSimple(size, object) : appendedRecursive(size, object);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutFirst() {
        final var newSize = subtractFromSize(first());
        return (prefix.length > 1) ? withoutFirstSimple(newSize) : withoutFirstNoPrefix(newSize);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutLast() {
        final var newSize = subtractFromSize(last());
        return (suffix.length > 1) ? withoutLastSimple(newSize) : withoutLastNoSuffix(newSize);
    }

    @Override
    void forEachArray(final @NotNull ArrayConsumer<T> action) {
        action.accept(prefix);
        middle.forEachArray(chunks -> ArrayOps.forEach(chunks, chunk -> action.accept(chunk.values)));
        action.accept(suffix);
    }

    @Override
    @NotNull GetResult<T> getImpl(final long index, final long accumulator) {
        final var prefixSplitPoint = tag.findSplitPoint(prefix, index, accumulator);
        final var prefixSize = prefixSplitPoint.accumulator();
        if (index < prefixSize) {
            return getFromArray(prefix, prefixSplitPoint);
        }
        final var frontSize = prefixSize + middle.subtreeSize;
        if (index < frontSize) {
            final var result = middle.getImpl(index, prefixSize);
            final var array = result.element().values;
            final var splitPoint = tag.findSplitPoint(array, index, result.accumulator());
            return getFromArray(array, splitPoint);
        }
        final var suffixSplitPoint = tag.findSplitPoint(suffix, index, frontSize);
        return getFromArray(suffix, suffixSplitPoint);
    }

    @Override
    boolean eligibleForInsertionSortImpl() {
        return middle.isEmpty();
    }

    @Override
    T @NotNull [] toSmallArray() {
        assert eligibleForInsertionSort();
        return ArrayOps.concat(prefix, suffix);
    }

    @Override
    @NotNull TaggedSeq<T, Phantom> concatImpl(final @NotNull TaggedSeq<T, Phantom> other) {
        return switch (other) {
            case Shallow<T, Phantom> shallow -> appendedShallow(shallow);
            case Deep<T, Phantom> deep -> appendedDeep(deep);
        };
    }

    @Override
    @NotNull TreeSplit<T, Phantom> splitTree(final long index, final long accumulator) {
        final var prefixSplitPoint = tag.findSplitPoint(prefix, index, accumulator);
        final var prefixSize = prefixSplitPoint.accumulator();
        if (index < prefixSize) {
            return splitTreeAtPrefix(accumulator, prefixSplitPoint);
        }
        final var frontSize = prefixSize + middle.subtreeSize;
        if (index < frontSize) {
            return splitTreeRecursively(accumulator, index, prefixSize);
        }
        return splitTreeAtSuffix(accumulator, index, frontSize);
    }

    @NotNull Deep<T, Phantom> prependedShallow(final @NotNull Shallow<T, Phantom> other) {
        if (other.isEmpty()) {
            return this;
        }
        final var newSize = computeNewSize(other.subtreeSize);
        final var lengthAfterConcat = other.values.length + prefix.length;
        if (lengthAfterConcat <= maxAffixLength) {
            return withNewFront(newSize, ArrayOps.concat(other.values, prefix), middle);
        } else {
            final var split = ArrayOps.concatSplitAt(other.values, prefix, lengthAfterConcat - maxChunkLength);
            final var newMiddle = middle.prepended(makeChunk(split.back()));
            return withNewFront(newSize, split.front(), newMiddle);
        }
    }

    private @NotNull Deep<T, Phantom> prependedSimple(final long newSize, final T object) {
        return withNewFront(newSize, ArrayOps.prepended(prefix, object), middle);
    }

    private @NotNull Deep<T, Phantom> prependedRecursive(final long newSize, final T object) {
        final var length = prefix.length;
        final var newPrefix = ArrayOps.prependedSlice(prefix, object);
        final var newChunk = makeChunk(Arrays.copyOfRange(prefix, length - maxChunkLength, length));
        return withNewFront(newSize, newPrefix, middle.prepended(newChunk));
    }

    private @NotNull Deep<T, Phantom> appendedSimple(final long newSize, final T object) {
        return withNewBack(newSize, middle, ArrayOps.appended(suffix, object));
    }

    private @NotNull Deep<T, Phantom> appendedRecursive(final long newSize, final T object) {
        final var newSuffix = ArrayOps.appendedSlice(suffix, object);
        final var newChunk = makeChunk(Arrays.copyOf(suffix, maxChunkLength));
        return withNewBack(newSize, middle.appended(newChunk), newSuffix);
    }

    private @NotNull Deep<T, Phantom> withoutFirstSimple(final long newSize) {
        return withNewFront(newSize, Arrays.copyOfRange(prefix, 1, prefix.length), middle);
    }

    private @NotNull TaggedSeq<T, Phantom> withoutFirstNoPrefix(final long newSize) {
        return fromEmptyPrefix(tag, newSize, middle, suffix);
    }

    private @NotNull Deep<T, Phantom> withoutLastSimple(final long newSize) {
        return withNewBack(newSize, middle, Arrays.copyOf(suffix, suffix.length - 1));
    }

    private @NotNull TaggedSeq<T, Phantom> withoutLastNoSuffix(final long newSize) {
        return fromEmptySuffix(tag, newSize, prefix, middle);
    }

    private @NotNull Deep<T, Phantom> appendedShallow(final @NotNull Shallow<T, Phantom> other) {
        if (other.isEmpty()) {
            return this;
        }
        final var newSize = computeNewSize(other.subtreeSize);
        if (suffix.length + other.values.length <= maxAffixLength) {
            return withNewBack(newSize, middle, ArrayOps.concat(suffix, other.values));
        } else {
            final var split = ArrayOps.concatSplitAt(suffix, other.values, maxChunkLength);
            final var newMiddle = middle.appended(makeChunk(split.front()));
            return withNewBack(newSize, newMiddle, split.back());
        }
    }

    private @NotNull Deep<T, Phantom> appendedDeep(final @NotNull Deep<T, Phantom> other) {
        final var newSize = computeNewSize(other.subtreeSize);
        return (suffix.length + other.prefix.length >= minChunkLength)
            ? concatAddingInfix(newSize, other)
            : concatPartial(newSize, other);
    }

    private @NotNull Deep<T, Phantom> concatAddingInfix(final long newSize, final @NotNull Deep<T, Phantom> other) {
        // The infix is long enough that it can be turned into one to three chunks.
        final var infixLength = suffix.length + other.prefix.length;
        if (infixLength <= maxChunkLength) {
            // The infix is short enough that it only forms one chunk.
            final var chunk = makeChunk(ArrayOps.concat(suffix, other.prefix));
            final var newMiddle = middle.appended(chunk).concat(other.middle);
            return withNewBack(newSize, newMiddle, other.suffix);
        }
        if (infixLength <= 2 * maxChunkLength) {
            // The infix is too long for a single chunk, but still fits in two.
            final var infix = ArrayOps.concatSplitAt(suffix, other.prefix, infixLength / 2);
            final var newOurMiddle = middle.appended(makeChunk(infix.front()));
            final var newTheirMiddle = other.middle.prepended(makeChunk(infix.back()));
            return withNewBack(newSize, newOurMiddle.concat(newTheirMiddle), other.suffix);
        }
        // The infix is so long it has to be split into three chunks; can happen if at least one of the infix arrays
        // is overlong, i.e. maxChunkLength < its length <= maxAffixLength.
        final var firstSplit = ArrayOps.concatSplitAt(suffix, other.prefix, infixLength / 3);
        final var secondSplit = ArrayOps.split(firstSplit.back());
        final var newOurMiddle =
            middle.appended(makeChunk(firstSplit.front())).appended(makeChunk(secondSplit.front()));
        final var newTheirMiddle = other.middle.prepended(makeChunk(secondSplit.back()));
        return withNewBack(newSize, newOurMiddle.concat(newTheirMiddle), other.suffix);
    }

    private @NotNull Deep<T, Phantom> concatPartial(final long newSize, final @NotNull Deep<T, Phantom> other) {
        final var front = mergeShortSuffix();
        final var back = other.mergeShortPrefix();
        final var newMiddle = front.middle.concat(back.middle);
        return new Deep<>(tag, newSize, front.prefix, newMiddle, back.suffix);
    }

    private @NotNull PartialBack<T> mergeShortPrefix() {
        return middle.isEmpty() ? mergeShortPrefixWithSuffix() : mergeShortPrefixWithMiddle();
    }

    private @NotNull PartialFront<T> mergeShortSuffix() {
        return middle.isEmpty() ? mergeShortSuffixWithPrefix() : mergeShortSuffixWithMiddle();
    }

    private @NotNull PartialBack<T> mergeShortPrefixWithSuffix() {
        assert middle.isEmpty();
        final var totalLength = prefix.length + suffix.length;
        if (totalLength <= maxChunkLength) {
            return new PartialBack<>(Shallow.emptyChunk(), ArrayOps.concat(prefix, suffix));
        }
        final var split = ArrayOps.concatSplitAt(prefix, suffix, totalLength / 2);
        return new PartialBack<>(Shallow.ofChunk(makeChunk(split.front())), split.back());
    }

    private @NotNull PartialFront<T> mergeShortSuffixWithPrefix() {
        assert middle.isEmpty();
        final var totalLength = prefix.length + suffix.length;
        if (totalLength <= maxChunkLength) {
            return new PartialFront<>(ArrayOps.concat(prefix, suffix), Shallow.emptyChunk());
        }
        final var split = ArrayOps.concatSplitAt(prefix, suffix, totalLength / 2);
        return new PartialFront<>(split.front(), Shallow.ofChunk(makeChunk(split.back())));
    }

    private @NotNull PartialBack<T> mergeShortPrefixWithMiddle() {
        final var firstChunk = middle.first().values;
        final var totalLength = prefix.length + firstChunk.length;
        if (totalLength <= maxChunkLength) {
            final var newFirstChunk = makeChunk(ArrayOps.concat(prefix, firstChunk));
            return new PartialBack<>(middle.updatedFirst(newFirstChunk), suffix);
        }
        final var split = ArrayOps.concatSplitAt(prefix, firstChunk, totalLength / 2);
        final var newFirstChunk = makeChunk(split.front());
        final var newSecondChunk = makeChunk(split.back());
        final var newMiddle = middle.updatedFirst(newSecondChunk).prepended(newFirstChunk);
        return new PartialBack<>(newMiddle, suffix);
    }

    private @NotNull PartialFront<T> mergeShortSuffixWithMiddle() {
        final var lastChunk = middle.last().values;
        final var totalLength = lastChunk.length + suffix.length;
        if (totalLength <= maxChunkLength) {
            final var newLastChunk = makeChunk(ArrayOps.concat(lastChunk, suffix));
            return new PartialFront<>(prefix, middle.updatedLast(newLastChunk));
        }
        final var split = ArrayOps.concatSplitAt(lastChunk, suffix, totalLength / 2);
        final var newPenultimateChunk = makeChunk(split.front());
        final var newLastChunk = makeChunk(split.back());
        final var newMiddle = middle.updatedLast(newPenultimateChunk).appended(newLastChunk);
        return new PartialFront<>(prefix, newMiddle);
    }

    private @NotNull TreeSplit<T, Phantom> splitTreeAtPrefix(
        final long initialAccumulator,
        final @NotNull Tag.SplitPoint splitPoint
    ) {
        final var split = tag.splitArray(prefix, splitPoint.index());
        final var middle = split.middle();
        final var middleSize = tag.measureSingle(middle);
        final var frontSize = splitPoint.accumulator() - initialAccumulator - middleSize;
        final var front = fromSingleArray(tag, frontSize, split.front());
        final var backSize = subtreeSize - frontSize - middleSize;
        final var back = fromUnknownPrefix(tag, backSize, split.back(), this.middle, suffix);
        return new TreeSplit<>(front, middle, back);
    }

    private @NotNull TreeSplit<T, Phantom> splitTreeRecursively(
        final long initialAccumulator,
        final long index,
        final long prefixSize
    ) {
        final var split = middle.splitTree(index, prefixSize);
        final var array = split.middle().values;
        final var splitPoint = tag.findSplitPoint(array, index, prefixSize + split.front().subtreeSize);
        final var arraySplit = tag.splitArray(array, splitPoint.index());
        final var middle = arraySplit.middle();
        final var middleSize = tag.measureSingle(middle);
        final var frontSize = splitPoint.accumulator() - initialAccumulator - middleSize;
        final var front = fromUnknownSuffix(tag, frontSize, prefix, split.front(), arraySplit.front());
        final var backSize = subtreeSize - frontSize - middleSize;
        final var back = fromUnknownPrefix(tag, backSize, arraySplit.back(), split.back(), suffix);
        return new TreeSplit<>(front, middle, back);
    }

    private @NotNull TreeSplit<T, Phantom> splitTreeAtSuffix(
        final long initialAccumulator,
        final long index,
        final long originalFrontSize
    ) {
        final var splitPoint = tag.findSplitPoint(suffix, index, originalFrontSize);
        final var split = tag.splitArray(suffix, splitPoint.index());
        final var middle = split.middle();
        final var middleSize = tag.measureSingle(middle);
        final var frontSize = splitPoint.accumulator() - initialAccumulator - middleSize;
        final var front = fromUnknownSuffix(tag, frontSize, prefix, this.middle, split.front());
        final var backSize = subtreeSize - frontSize - middleSize;
        final var back = fromSingleArray(tag, backSize, split.back());
        return new TreeSplit<>(front, middle, back);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromEmptyPrefix(
        final @NotNull Tag<T, Phantom> tag,
        final long newSize,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle,
        final T @NotNull [] suffix
    ) {
        if (middle.isEmpty()) {
            return fromSingleArray(tag, newSize, suffix);
        }
        final var newPrefix = middle.first().values;
        final var newMiddle = middle.withoutFirst();
        return new Deep<>(tag, newSize, newPrefix, newMiddle, suffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromEmptySuffix(
        final @NotNull Tag<T, Phantom> tag,
        final long newSize,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle
    ) {
        if (middle.isEmpty()) {
            return fromSingleArray(tag, newSize, prefix);
        }
        final var newSuffix = middle.last().values;
        final var newMiddle = middle.withoutLast();
        return new Deep<>(tag, newSize, prefix, newMiddle, newSuffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromUnknownPrefix(
        final @NotNull Tag<T, Phantom> tag,
        final long newSize,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle,
        final T @NotNull [] suffix
    ) {
        return (prefix.length == 0)
            ? fromEmptyPrefix(tag, newSize, middle, suffix)
            : new Deep<>(tag, newSize, prefix, middle, suffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromUnknownSuffix(
        final @NotNull Tag<T, Phantom> tag,
        final long newSize,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle,
        final T @NotNull [] suffix
    ) {
        return (suffix.length == 0)
            ? fromEmptySuffix(tag, newSize, prefix, middle)
            : new Deep<>(tag, newSize, prefix, middle, suffix);
    }

    private @NotNull Deep<T, Phantom> withNewFront(
        final long newSize,
        final T @NotNull [] newPrefix,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> newMiddle
    ) {
        return new Deep<>(tag, newSize, newPrefix, newMiddle, suffix);
    }

    private @NotNull Deep<T, Phantom> withNewBack(
        final long newSize,
        final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> newMiddle,
        final T @NotNull [] newSuffix
    ) {
        return new Deep<>(tag, newSize, prefix, newMiddle, newSuffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromSingleArray(
        final @NotNull Tag<T, Phantom> tag,
        final long newSize,
        final T @NotNull [] array
    ) {
        if (array.length <= maxElementsToShrink) {
            return (array.length != 0) ? new Shallow<>(tag, newSize, array) : tag.emptySeq();
        }
        final var split = ArrayOps.split(array);
        return new Deep<>(tag, newSize, split.front(), Shallow.emptyChunk(), split.back());
    }

    private @NotNull Chunk<T> makeChunk(final T @NotNull [] array) {
        return new Chunk<>(tag, array);
    }

    // Should normally be only used in an assert statement.
    @SuppressWarnings("SameReturnValue")
    private boolean checkInvariants() {
        assert prefix.length != 0 && prefix.length <= maxAffixLength;
        assert suffix.length != 0 && suffix.length <= maxAffixLength;
        assert subtreeSize > 0;
        final var actualSize = tag.measureArray(prefix) + middle.subtreeSize + tag.measureArray(suffix);
        assert subtreeSize == actualSize;
        return true;
    }

    // Allow 4 more elements in affixes to ensure that almost all arrays in a sequence built by repeatedly
    // appending/prepending are maxChunkLength elements long, and that the complexity requirements are met.
    private static final int maxAffixLength = maxChunkLength + 4;
    // The number of elements below which removals are allowed to turn deep sequences into shallow ones.
    private static final int maxElementsToShrink = minChunkLength;

    private final T @NotNull [] prefix;
    private final @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle;
    private final T @NotNull [] suffix;

    static final class Itr<T> extends Seq.Itr<T> {
        private Itr(final @NotNull Deep<T, ?> parent) {
            array = parent.prefix;
            this.parent = parent;
        }

        @Override
        public boolean hasNext() {
            return index < array.length || currentStage != Stage.SUFFIX;
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
        public long nextIndex() {
            return sequenceIndex;
        }

        @Override
        void forEachRemainingImpl(final @NotNull Consumer<? super T> action) {
            while (true) {
                ArrayOps.forEachFrom(array, index, action);
                sequenceIndex += array.length - index;
                if (currentStage == Stage.SUFFIX) {
                    break;
                }
                nextArray();
            }
        }

        private void nextArrayIfNeeded() {
            if (index >= array.length) {
                nextArray();
            }
        }

        private void nextArray() {
            switch (currentStage) {
                case PREFIX -> tryFirstChunk();
                case MIDDLE -> tryNextChunk();
                case SUFFIX -> throw noMoreElements();
            }
        }

        private void tryFirstChunk() {
            final var middle = parent.middle;
            if (middle.isEmpty()) {
                changeStage(Stage.SUFFIX, parent.suffix);
            } else {
                chunkIterator = middle.iterator();
                changeStage(Stage.MIDDLE, chunkIterator.next().values);
            }
        }

        private void tryNextChunk() {
            assert chunkIterator != null;
            if (chunkIterator.hasNext()) {
                setArray(chunkIterator.next().values);
            } else {
                chunkIterator = null;
                changeStage(Stage.SUFFIX, parent.suffix);
            }
        }

        private void changeStage(final @NotNull Stage newStage, final T @NotNull [] newArray) {
            currentStage = newStage;
            setArray(newArray);
        }

        private void setArray(final T @NotNull [] newArray) {
            array = newArray;
            index = 0;
        }

        private int index = 0;
        private T @NotNull [] array;
        private long sequenceIndex = 0;
        private @NotNull Stage currentStage = Stage.PREFIX;
        private @Nullable Seq.Itr<@NotNull Chunk<T>> chunkIterator = null;
        private final @NotNull Deep<T, ?> parent;

        private enum Stage {
            PREFIX,
            MIDDLE,
            SUFFIX,
        }
    }

    private record PartialFront<T>(T @NotNull [] prefix, @NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle) {
    }

    private record PartialBack<T>(@NotNull TaggedSeq<@NotNull Chunk<T>, Chunk<?>> middle, T @NotNull [] suffix) {
    }
}
