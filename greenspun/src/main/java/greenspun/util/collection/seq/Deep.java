// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class Deep<T, Phantom> extends TaggedSeq<T, Phantom> {
    Deep(
        final @NotNull TypeTag<T, Phantom> tag,
        final long size,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle,
        final T @NotNull [] suffix
    ) {
        super(tag, size);
        assert prefix.length > 0 && prefix.length <= maxAffixLength;
        assert suffix.length > 0 && suffix.length <= maxAffixLength;
        assert size == tag.measureArray(prefix) + middle.exactSize() + tag.measureArray(suffix);
        this.prefix = prefix;
        this.middle = middle;
        this.suffix = suffix;
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Deep<T, Object> ofUnits(final T e1, final T e2) {
        return new Deep<>(TypeTag.unit(), 2, (T[]) new Object[]{e1}, Empty.node(), (T[]) new Object[]{e2});
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Deep<T, Object> ofUnits(final T e1, final T e2, final T e3) {
        return new Deep<>(TypeTag.unit(), 3, (T[]) new Object[]{e1, e2}, Empty.node(), (T[]) new Object[]{e3});
    }

    @SuppressWarnings("unchecked")
    static <T> @NotNull Deep<T, Object> ofUnits(final T e1, final T e2, final T e3, final T e4) {
        return new Deep<>(TypeTag.unit(), 4, (T[]) new Object[]{e1, e2}, Empty.node(), (T[]) new Object[]{e3, e4});
    }

    @Override
    public @NotNull Seq.Itr<T> iterator() {
        return new Itr();
    }

    @Override
    public boolean anySatisfies(final @NotNull Predicate<? super T> predicate) {
        return ArrayOps.anySatisfies(prefix, predicate)
            || ArrayOps.anySatisfies(suffix, predicate)
            || middle.anySatisfies(node -> ArrayOps.anySatisfies(node.values(), predicate));
    }

    @Override
    public @NotNull <U> TaggedSeq<U, Phantom> map(final @NotNull Function<? super T, ? extends U> function) {
        final var tag = tag().<U>cast();
        final var newPrefix = ArrayOps.map(tag, prefix, function);
        final var newMiddle = middle.map(node -> node.map(tag, function));
        final var newSuffix = ArrayOps.map(tag, suffix, function);
        return new Deep<>(tag.cast(), exactSize(), newPrefix, newMiddle, newSuffix);
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
        final var tag = tag();
        final var newSize = computeNewSize(tag.measureSingle(object) - tag.measureSingle(prefix[0]));
        final var newPrefix = prefix.clone();
        newPrefix[0] = object;
        return new Deep<>(tag(), newSize, newPrefix, middle, suffix);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> updatedLast(final T object) {
        final var tag = tag();
        final var lastIndex = suffix.length - 1;
        final var newSize = computeNewSize(tag.measureSingle(object) - tag.measureSingle(suffix[lastIndex]));
        final var newSuffix = suffix.clone();
        newSuffix[lastIndex] = object;
        return new Deep<>(tag(), newSize, prefix, middle, newSuffix);
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
        final var size = subtractFromSize(first());
        return (prefix.length > 1) ? withoutFirstSimple(size) : withoutFirstNoPrefix(size);
    }

    @Override
    public @NotNull TaggedSeq<T, Phantom> withoutLast() {
        final var size = subtractFromSize(last());
        return (suffix.length > 1) ? withoutLastSimple(size) : withoutLastNoSuffix(size);
    }

    @Override
    void forEachChunk(final @NotNull ChunkConsumer<T> action) {
        action.acceptArray(prefix);
        middle.forEachChunk(action.lift());
        action.acceptArray(suffix);
    }

    @Override
    T getImpl(final long index) {
        // We're storing units, not nodes, so we can use direct indexing of affixes.
        assert tag() == TypeTag.unit();
        final var prefixSize = prefix.length;
        if (index < prefixSize) {
            return prefix[(int) index];
        }
        var remaining = index - prefixSize;
        final var middleSize = middle.exactSize();
        if (remaining < middleSize) {
            final var accumulator = new IndexAccumulator(remaining);
            final var node = getRecursive(middle, accumulator);
            return node.values()[(int) accumulator.get()];
        }
        remaining -= middleSize;
        assert remaining <= Integer.MAX_VALUE;
        return suffix[(int) remaining];
    }

    @Override
    @NotNull TaggedSeq<T, Phantom> concatImpl(final @NotNull TaggedSeq<T, Phantom> other) {
        return switch (other) {
            case Empty<T, Phantom> ignored -> this;
            case Single<T, Phantom> single -> appended(single.first());
            case Deep<T, Phantom> deep -> concatDeep(deep);
        };
    }

    @Override
    @NotNull TreeSplit<T, Phantom> splitTree(final long index, final long accumulator) {
        final var tag = tag();
        final var prefixSize = accumulator + tag.measureArray(prefix);
        if (index < prefixSize) {
            final var split = splitArray(tag, index, accumulator, prefix);
            final var left = ArrayOps.toSeq(tag, split.left);
            final var right = fromUnknownPrefix(tag, split.right, middle, suffix);
            return new TreeSplit<>(left, split.middle, right);
        }
        final var middleSize = prefixSize + middle.exactSize();
        if (index < middleSize) {
            final var split = middle.splitTree(index, prefixSize);
            final var nodeSplit =
                splitArray(tag, index, prefixSize + split.left().exactSize(), split.middle().values());
            final var left = fromUnknownSuffix(tag, prefix, split.left(), nodeSplit.left);
            final var right = fromUnknownPrefix(tag, nodeSplit.right, split.right(), suffix);
            return new TreeSplit<>(left, nodeSplit.middle, right);
        }
        final var split = splitArray(tag, index, middleSize, suffix);
        final var left = fromUnknownSuffix(tag, prefix, middle, split.left);
        final var right = ArrayOps.toSeq(tag, split.right);
        return new TreeSplit<>(left, split.middle, right);
    }

    boolean eligibleForInsertionSort() {
        return exactSize() <= Node.maxLength && middle.isEmpty();
    }

    T @NotNull [] toSmallArray() {
        assert eligibleForInsertionSort();
        return ArrayOps.concat(tag(), prefix, suffix);
    }

    private @NotNull Deep<T, Phantom> prependedSimple(final long newSize, final T object) {
        final var tag = tag();
        return new Deep<>(tag, newSize, ArrayOps.prepended(tag, prefix, object), middle, suffix);
    }

    private @NotNull Deep<T, Phantom> prependedRecursive(final long newSize, final T object) {
        final var length = prefix.length;
        final var splitPoint = length / 4;
        final var tag = tag();
        final var newPrefix = ArrayOps.prependedSlice(tag, prefix, splitPoint, object);
        final var newNode = new Node<>(tag, Arrays.copyOfRange(prefix, splitPoint, length));
        return new Deep<>(tag, newSize, newPrefix, middle.prepended(newNode), suffix);
    }

    private @NotNull Deep<T, Phantom> appendedSimple(final long newSize, final T object) {
        return new Deep<>(tag(), newSize, prefix, middle, ArrayOps.appended(suffix, object));
    }

    private @NotNull Deep<T, Phantom> appendedRecursive(final long newSize, final T object) {
        final var length = suffix.length;
        final var splitPoint = 3 * length / 4;
        final var tag = tag();
        final var newSuffix = ArrayOps.appendedSlice(tag, suffix, splitPoint, object);
        final var newNode = new Node<>(tag, Arrays.copyOf(suffix, splitPoint));
        return new Deep<>(tag, newSize, prefix, middle.appended(newNode), newSuffix);
    }

    private @NotNull Deep<T, Phantom> withoutFirstSimple(final long newSize) {
        return new Deep<>(tag(), newSize, Arrays.copyOfRange(prefix, 1, prefix.length), middle, suffix);
    }

    private @NotNull TaggedSeq<T, Phantom> withoutFirstNoPrefix(final long newSize) {
        return fromEmptyPrefix(tag(), newSize, middle, suffix);
    }

    private @NotNull Deep<T, Phantom> withoutLastSimple(final long newSize) {
        return new Deep<>(tag(), newSize, prefix, middle, Arrays.copyOf(suffix, suffix.length - 1));
    }

    private @NotNull TaggedSeq<T, Phantom> withoutLastNoSuffix(final long newSize) {
        return fromEmptySuffix(tag(), newSize, prefix, middle);
    }

    private T getRecursive(final @NotNull IndexAccumulator accumulator) {
        // We're storing nodes, need to iterate the affixes.
        assert tag() == TypeTag.node();
        return getAccumulatingSingle(accumulator, prefix, () -> {
            final var middleSize = middle.exactSize();
            final Supplier<T> fellThrough = () -> {
                throw fellThroughTryingToIndex();
            };
            if (accumulator.get() < middleSize) {
                final var node = getRecursive(middle, accumulator);
                return getAccumulatingSingle(accumulator, node.values(), fellThrough);
            }
            accumulator.subtract(middleSize);
            return getAccumulatingSingle(accumulator, suffix, fellThrough);
        });
    }

    private T getAccumulatingSingle(
        final @NotNull IndexAccumulator accumulator,
        final T @NotNull [] array,
        final @NotNull Supplier<? extends T> fallback
    ) {
        final var tag = tag();
        for (final var item : array) {
            if (accumulator.subtract(tag.measureSingle(item))) {
                return item;
            }
        }
        return fallback.get();
    }

    private @NotNull Deep<T, Phantom> concatDeep(final @NotNull Deep<T, Phantom> other) {
        final var newSize = computeNewSize(other.exactSize());
        return (suffix.length + other.prefix.length >= Node.minLength)
            ? concatAddingInfix(newSize, other)
            : concatBalancing(newSize, other);
    }

    private @NotNull Deep<T, Phantom> concatAddingInfix(final long newSize, final @NotNull Deep<T, Phantom> other) {
        // We can turn the infix into one or two nodes, adding them to either middle.
        final var tag = tag();
        final var infix = ArrayOps.concat(tag, suffix, other.prefix);
        final var infixLength = infix.length;
        if (infixLength <= Node.maxLength) {
            // Just one node will do.
            final var newMiddle = middle.appended(new Node<>(tag, infix)).concat(other.middle);
            return new Deep<>(tag, newSize, prefix, newMiddle, other.suffix);
        }
        // The infix is too big, need to split it into two.
        final var splitPoint = infixLength / 2;
        final var leftInfix = new Node<>(tag, Arrays.copyOf(infix, splitPoint));
        final var rightInfix = new Node<>(tag, Arrays.copyOfRange(infix, splitPoint, infixLength));
        final var newMiddle = middle.appended(leftInfix).concat(other.middle.prepended(rightInfix));
        return new Deep<>(tag, newSize, prefix, newMiddle, other.suffix);
    }

    private @NotNull Deep<T, Phantom> concatBalancing(final long newSize, final @NotNull Deep<T, Phantom> other) {
        // The infix is not large enough to turn it into a node.
        final var left = balanceSuffixForConcat();
        final var right = other.balancePrefixForConcat();
        final var newMiddle = left.middle.concat(right.middle);
        return new Deep<>(tag(), newSize, left.affix, newMiddle, right.affix);
    }

    private @NotNull PartialDeep<T, Phantom> balanceSuffixForConcat() {
        return middle.isEmpty() ? balanceSuffixUsingPrefix() : balanceSuffixUsingMiddle();
    }

    private @NotNull PartialDeep<T, Phantom> balancePrefixForConcat() {
        return middle.isEmpty() ? balancePrefixUsingSuffix() : balancePrefixUsingMiddle();
    }

    private @NotNull PartialDeep<T, Phantom> balanceSuffixUsingPrefix() {
        // We know the middle is empty, time to join the prefix and suffix and see what we can do with that array.
        assert middle.isEmpty();
        final var tag = tag();
        final var joined = ArrayOps.concat(tag, prefix, suffix);
        final var length = joined.length;
        if (length <= maxAffixLength) {
            return new PartialDeep<>(middle, joined);
        }
        final var splitPoint = length / 2;
        final var newPrefix = Arrays.copyOf(joined, splitPoint);
        final var node = new Node<>(tag, Arrays.copyOfRange(joined, splitPoint, length));
        final var newMiddle = middle.appended(node);
        return new PartialDeep<>(newMiddle, newPrefix);
    }

    private @NotNull PartialDeep<T, Phantom> balanceSuffixUsingMiddle() {
        // We can get a node from the middle and either steal some of its elements so that the suffix can become
        // a node, or move the suffix to the node so that the suffix becomes empty.
        final var nodeValues = middle.last().values();
        final var tag = tag();
        final var joined = ArrayOps.concat(tag, nodeValues, suffix);
        final var length = joined.length;
        if (length <= Node.maxLength) {
            return new PartialDeep<>(middle.updatedLast(new Node<>(tag, joined)), prefix);
        }
        final var splitPoint = length / 2;
        final var leftNode = new Node<>(tag, Arrays.copyOf(joined, splitPoint));
        final var rightNode = new Node<>(tag, Arrays.copyOfRange(joined, splitPoint, length));
        final var newMiddle = middle.updatedLast(leftNode).appended(rightNode);
        return new PartialDeep<>(newMiddle, prefix);
    }

    private @NotNull PartialDeep<T, Phantom> balancePrefixUsingSuffix() {
        // We know the middle is empty, time to join the prefix and suffix and see what we can do with that array.
        assert middle.isEmpty();
        final var tag = tag();
        final var joined = ArrayOps.concat(tag, prefix, suffix);
        final var length = joined.length;
        if (length <= maxAffixLength) {
            return new PartialDeep<>(middle, joined);
        }
        final var splitPoint = length / 2;
        final var node = new Node<>(tag, Arrays.copyOf(joined, splitPoint));
        final var newSuffix = Arrays.copyOfRange(joined, splitPoint, length);
        final var newMiddle = middle.prepended(node);
        return new PartialDeep<>(newMiddle, newSuffix);
    }

    private @NotNull PartialDeep<T, Phantom> balancePrefixUsingMiddle() {
        // We can get a node from the middle and either steal some of its elements so that the prefix can become
        // a node, or move the prefix to the node so that the prefix becomes empty.
        final var nodeValues = middle.first().values();
        final var tag = tag();
        final var joined = ArrayOps.concat(tag, prefix, nodeValues);
        final var length = joined.length;
        if (length <= Node.maxLength) {
            return new PartialDeep<>(middle.updatedFirst(new Node<>(tag, joined)), suffix);
        }
        final var splitPoint = length / 2;
        final var leftNode = new Node<>(tag, Arrays.copyOf(joined, splitPoint));
        final var rightNode = new Node<>(tag, Arrays.copyOfRange(joined, splitPoint, length));
        final var newMiddle = middle.updatedFirst(rightNode).prepended(leftNode);
        return new PartialDeep<>(newMiddle, suffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromEmptyPrefix(
        final @NotNull TypeTag<T, Phantom> tag,
        final long newSize,
        final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle,
        final T @NotNull [] suffix
    ) {
        if (middle.isEmpty()) {
            // All that remains non-empty is the suffix, need to turn it into a sequence.
            return ArrayOps.toSeq(tag, newSize, suffix);
        }
        final var node = middle.first();
        final var newMiddle = middle.withoutFirst();
        return new Deep<>(tag, newSize, node.values(), newMiddle, suffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromUnknownPrefix(
        final @NotNull TypeTag<T, Phantom> tag,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle,
        final T @NotNull [] suffix
    ) {
        assert suffix.length > 0;
        final var rightSideSize = middle.exactSize() + tag.measureArray(suffix);
        if (prefix.length == 0) {
            return fromEmptyPrefix(tag, rightSideSize, middle, suffix);
        }
        final var size = tag.measureArray(prefix) + rightSideSize;
        return new Deep<>(tag, size, prefix, middle, suffix);
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromEmptySuffix(
        final @NotNull TypeTag<T, Phantom> tag,
        final long newSize,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle
    ) {
        if (middle.isEmpty()) {
            // All that remains non-empty is the prefix, need to turn it into a sequence.
            return ArrayOps.toSeq(tag, newSize, prefix);
        }
        final var node = middle.last();
        final var newMiddle = middle.withoutLast();
        return new Deep<>(tag, newSize, prefix, newMiddle, node.values());
    }

    private static <T, Phantom> @NotNull TaggedSeq<T, Phantom> fromUnknownSuffix(
        final @NotNull TypeTag<T, Phantom> tag,
        final T @NotNull [] prefix,
        final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle,
        final T @NotNull [] suffix
    ) {
        assert prefix.length > 0;
        final var leftSideSize = tag.measureArray(prefix) + middle.exactSize();
        if (suffix.length == 0) {
            return fromEmptySuffix(tag, leftSideSize, prefix, middle);
        }
        final var size = leftSideSize + tag.measureArray(suffix);
        return new Deep<>(tag, size, prefix, middle, suffix);
    }

    private static <T, Phantom> @NotNull Node<T> getRecursive(
        final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> seq,
        final @NotNull IndexAccumulator accumulator
    ) {
        return (seq instanceof Deep<Node<T>, Node<Phantom>> deep)
            ? deep.getRecursive(accumulator)
            : seq.getImpl(accumulator.get());
    }

    private static <T> @NotNull ArraySplit<T> splitArray(
        final @NotNull TypeTag<T, ?> tag,
        final long index,
        final long accumulator,
        final T @NotNull [] array
    ) {
        // TODO: consider specializing for TypeTag.unit()
        final var length = array.length;
        final var lastIndex = length - 1;
        var acc = accumulator;
        for (int i = 0; i < lastIndex; i += 1) {
            final var item = array[i];
            acc += tag.measureSingle(item);
            if (index < acc) {
                return new ArraySplit<>(Arrays.copyOf(array, i), item, Arrays.copyOfRange(array, i + 1, length));
            }
        }
        return new ArraySplit<>(Arrays.copyOf(array, lastIndex), array[lastIndex], tag.emptyArray());
    }

    private final T @NotNull [] prefix;
    private final @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle;
    private final T @NotNull [] suffix;

    final class Itr extends Seq.Itr<T> {
        private Itr() {
        }

        @Override
        public boolean hasNext() {
            return index < array.length || stage < suffixStage;
        }

        @Override
        public T next() {
            if (index >= array.length) {
                getNextArray();
            }
            final var result = array[index];
            index += 1;
            return result;
        }

        @Override
        void forEachRemainingImpl(final @NotNull Consumer<? super T> action) {
            while (true) {
                ArrayOps.forEachFrom(array, index, action);
                if (stage == suffixStage) {
                    break;
                }
                getNextArray();
            }
        }

        private void getNextArray() {
            switch (stage) {
                case middleStage -> {
                    assert nodeIterator != null;
                    if (nodeIterator.hasNext()) {
                        array = nodeIterator.next().values();
                    } else {
                        nodeIterator = null;
                        setSuffixStage();
                    }
                }
                case prefixStage -> {
                    if (middle.isEmpty()) {
                        setSuffixStage();
                    } else {
                        stage = middleStage;
                        nodeIterator = middle.iterator();
                        array = nodeIterator.next().values();
                    }
                }
                case suffixStage -> throw noMoreElements();
            }
            index = 0;
        }

        private void setSuffixStage() {
            stage = suffixStage;
            array = suffix;
        }

        private static final int prefixStage = 0;
        private static final int middleStage = 1;
        private static final int suffixStage = 2;

        private int index = 0;
        private T @NotNull [] array = prefix;
        private int stage = prefixStage;
        private @Nullable Seq.Itr<@NotNull Node<T>> nodeIterator = null;
    }

    private record PartialDeep<T, Phantom>(
        @NotNull TaggedSeq<@NotNull Node<T>, Node<Phantom>> middle,
        T @NotNull [] affix
    ) {
    }

    private record ArraySplit<T>(T @NotNull [] left, T middle, T @NotNull [] right) {
    }
}
