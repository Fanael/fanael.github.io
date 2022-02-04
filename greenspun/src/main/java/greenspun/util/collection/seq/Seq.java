// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import edu.umd.cs.findbugs.annotations.CheckReturnValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A general purpose persistent (immutable with efficient copy-and-update semantics) double-ended queue. It is intended
 * to serve as the preferred persistent data structure wherever efficient access to either end is required, or as a
 * general-purpose immutable "list" if logarithmic time random access is tolerable.
 * <p>
 * There are no restrictions on what elements are permitted. {@code null} is allowed, just like any other possible
 * element.
 * <p>
 * Pushing and popping elements to/from either end executes in amortized constant time, worst-case logarithmic time.
 * <p>
 * Sequences allow up to {@link Long#MAX_VALUE} elements. When handling large sequences, the {@link #size()} method is
 * not reliable, prefer {@link #exactSize()} instead.
 * <p>
 * For exact complexity guarantees of each operation, see the corresponding method. Unless noted otherwise, all
 * guarantees are worst-case. Space complexity of updating operations is the same as the time complexity, unless noted
 * otherwise. For methods that invoke non-{@code Seq} code, like element equality, hash code or arbitrary predicates,
 * the stated complexity assumes those operations don't depend on the size of the {@code Seq}.
 * <p>
 * Immutability is shallow: it doesn't extend to the elements themselves. Prefer element types which are immutable
 * themselves.
 * <p>
 * Mutable methods of {@link Collection} are unsupported, as befits an immutable data structure: they always immediately
 * throw {@link UnsupportedOperationException}. They're marked as deprecated, to allow accidental uses to be reported
 * as a warning by the compiler when the static type is known to be {@code Seq}.
 * <p>
 * Despite representing a double-ended queue, this class doesn't implement {@link java.util.Deque}, because that
 * interface's nature is very ephemeral (mutable in-place), so useful implementation cannot be provided.
 * <p>
 * Internally, the sequence is implemented as a <i>finger tree</i>, using path copying for persistence. The maximum size
 * of a node is 32, to strike a balance between tree depth and cache friendliness, and copying costs of update
 * operations.
 */
public abstract sealed class Seq<T> implements Collection<T> permits TaggedSeq {
    Seq(final long exactSize) {
        assert exactSize > 0 || this instanceof Empty<T, ?>;
        this.exactSize = exactSize;
    }

    /**
     * Returns an empty persistent sequence pretending to contain objects of the given type.
     * <p>
     * There is no guarantee regarding the identity of the returned sequence, so relying on identity-based operations
     * such as synchronization or reference equality is discouraged.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> empty() {
        return Empty.unit();
    }

    /**
     * Returns a new persistent sequence containing only the given element.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T element) {
        return Single.ofUnit(element);
    }

    /**
     * Returns a new persistent sequence containing only the given two elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T e1, final T e2) {
        return Deep.ofUnits(e1, e2);
    }

    /**
     * Returns a new persistent sequence containing only the given three elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T e1, final T e2, final T e3) {
        return Deep.ofUnits(e1, e2, e3);
    }

    /**
     * Returns a new persistent sequence containing only the given four elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T e1, final T e2, final T e3, final T e4) {
        return Deep.ofUnits(e1, e2, e3, e4);
    }

    /**
     * Returns a new persistent sequence containing the elements of the given iterable in iteration order.
     * <p>
     * Complexity: constant time if the iterable is already a {@code Seq}, linear time otherwise.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull Seq<T> fromIterable(final @NotNull Iterable<? extends T> iterable) {
        if (iterable instanceof Seq<? extends T> seq) {
            return (Seq<T>) seq; // Seq is immutable, this is fine.
        }
        @NotNull Seq<T> result = empty();
        for (final var item : iterable) {
            result = result.appended(item);
        }
        return result;
    }

    /**
     * Returns a new persistent sequence containing the results of applying the given function to the elements of
     * the given iterable in iteration order.
     * <p>
     * Complexity: linear time.
     */
    public static <T, R> @NotNull Seq<R> mapIterable(
        final @NotNull Iterable<? extends T> iterable,
        final @NotNull Function<? super T, ? extends R> function
    ) {
        if (iterable instanceof Seq<? extends T> seq) {
            return seq.map(function);
        }
        @NotNull Seq<R> result = empty();
        for (final var item : iterable) {
            result = result.appended(function.apply(item));
        }
        return result;
    }

    /**
     * Returns the hash code of this sequence. The algorithm used to compute the hash code is unspecified.
     * <p>
     * Complexity: linear time.
     */
    @Override
    @SuppressWarnings("ObjectInstantiationInEqualsHashCode")
    public final int hashCode() {
        final var accumulator = new HashCodeAccumulator<T>();
        forEachChunk(accumulator);
        return accumulator.hashCode;
    }

    /**
     * Returns {@code true} iff the given object is a sequence of the same size, containing equal elements in the same
     * order.
     * <p>
     * Element equality is determined according to {@link Objects#equals(Object, Object)}.
     * <p>
     * Complexity: constant time if the sizes differ, linear time otherwise.
     */
    @Override
    public final boolean equals(final @Nullable Object object) {
        return object instanceof Seq<?> other && equalsImpl(other);
    }

    /**
     * Returns a string representation of this sequence.
     * <p>
     * The string representation of a sequence consists of the concatenation of the string representations of its
     * elements, separated by the string {@code ", "}, enclosed in square brackets.
     * <p>
     * Complexity: linear time.
     */
    @Override
    public final @NotNull String toString() {
        if (isEmpty()) {
            return "[]";
        }

        final var builder = new StringBuilder();
        builder.append('[');
        forEach(item -> builder.append(item).append(", "));
        builder.setLength(builder.length() - 2); // Remove final separator.
        builder.append(']');
        return builder.toString();
    }

    /**
     * Returns a new iterator over the elements of this sequence, from the first element to the last.
     * <p>
     * Complexity: constant time.
     */
    @Override
    public abstract @NotNull Itr<T> iterator();

    /**
     * Performs the given action on each element of this sequence, in iteration order.
     * <p>
     * Exceptions thrown by the action are passed to the caller.
     * <p>
     * Complexity: linear time.
     *
     * @throws NullPointerException if the action is {@code null}
     */
    @Override
    public final void forEach(final @NotNull Consumer<? super T> action) {
        Objects.requireNonNull(action); // Check eagerly in case we're empty.
        forEachChunk(ChunkConsumer.fromConsumer(action));
    }

    /**
     * Returns a new spliterator over the elements of this sequence.
     * <p>
     * The returned spliterator always reports at least {@link Spliterator#SIZED}, {@link Spliterator#SUBSIZED},
     * {@link Spliterator#ORDERED} and {@link Spliterator#IMMUTABLE} characteristics.
     * <p>
     * Complexity: constant time.
     */
    @Override
    public final @NotNull Spliterator<T> spliterator() {
        return Spliterators.spliterator(iterator(), exactSize, Spliterator.ORDERED | Spliterator.IMMUTABLE);
    }

    /**
     * Returns the number of elements contained in this sequence.
     * <p>
     * If this sequence contains more than {@link Integer#MAX_VALUE} elements, {@link Integer#MAX_VALUE} is returned.
     * <p>
     * Complexity: constant time.
     *
     * @see #exactSize()
     * @deprecated Provided only because it's required by {@link Collection}. Prefer {@link #exactSize()} instead.
     */
    @Deprecated
    @Override
    public final int size() {
        return (int) Long.min(exactSize, Integer.MAX_VALUE);
    }

    /**
     * Returns {@code true} iff this sequence contains no elements.
     * <p>
     * Complexity: constant time.
     */
    @Override
    public final boolean isEmpty() {
        return exactSize == 0;
    }

    /**
     * Returns {@code true} iff this sequence contains an element equal to the given object.
     * <p>
     * Element equality is determined according to {@link Objects#equals(Object, Object)}.
     * <p>
     * The order in which elements of the sequence are compared against the given object is unspecified.
     * <p>
     * Complexity: constant time if a matching element is close to either end, linear time otherwise.
     */
    @Override
    public final boolean contains(final @Nullable Object object) {
        return anySatisfies(makeEqualityPredicate(object));
    }

    /**
     * Returns a new array containing all the elements of this sequence, in iteration order.
     * <p>
     * The runtime component type of the returned array is {@link Object}.
     * <p>
     * Complexity: linear time.
     *
     * @throws IllegalStateException if this sequence is too large to fit in an array
     */
    @Override
    public final Object @NotNull [] toArray() {
        return toArray(Object[]::new);
    }

    /**
     * Returns an array containing all the elements of this sequence, in iteration order.
     * If the sequence fits in the given array, it is returned therein. Otherwise, a new array of the same size as this
     * sequence is allocated and returned.
     * <p>
     * If the sequence fits with room to spare, the element immediately following the end of the sequence is set to
     * {@code null}.
     * <p>
     * The runtime component type of the returned array is the same as that of the given array.
     * <p>
     * Complexity: linear time.
     *
     * @throws IllegalStateException if this sequence is too large to fit in an array
     */
    @Override
    @SuppressWarnings("unchecked")
    public final <U> U @NotNull [] toArray(final U @NotNull [] array) {
        final var size = getEffectiveArraySize();
        final var usedArray = (array.length >= size)
            ? array
            : (U[]) Array.newInstance(array.getClass().getComponentType(), size);
        forEachChunk(new ArrayFiller<>(usedArray));
        if (usedArray.length > size) {
            usedArray[size] = null;
        }
        return usedArray;
    }

    /**
     * Returns an array containing all the elements of this sequence, in iteration order.
     * <p>
     * The given {@code generator} function is used to create an array of appropriate size.
     * <p>
     * Complexity: linear time.
     *
     * @throws IllegalStateException if this sequence is too large to fit in an array
     */
    @Override
    public final <U> U @NotNull [] toArray(final @NotNull IntFunction<U @NotNull []> generator) {
        final var array = generator.apply(getEffectiveArraySize());
        forEachChunk(new ArrayFiller<>(array));
        return array;
    }

    /**
     * Returns {@code true} iff this sequence contains all the elements of the given collection.
     * If the given collection is empty, {@code true} is returned.
     * <p>
     * Complexity: O(mn), where {@code m} is the size of the collection and {@code n} is the size of this sequence.
     *
     * @see #contains(Object)
     */
    @Override
    public final boolean containsAll(final @NotNull Collection<?> collection) {
        for (final var element : collection) {
            if (!contains(element)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns {@code true} iff this sequence contains an element that satisfies the given predicate.
     * <p>
     * The order in which elements of the sequence are tested.
     * <p>
     * Complexity: constant time if a matching element is close to either end, linear time otherwise.
     */
    public abstract boolean anySatisfies(@NotNull Predicate<? super T> predicate);

    /**
     * Returns a sequence containing the results of applying the given function to the elements of this sequence, in
     * iteration order.
     * <p>
     * Exceptions thrown by the given function are passed through to the caller.
     * <p>
     * Complexity: linear time.
     */
    @CheckReturnValue
    public abstract <U> @NotNull Seq<U> map(@NotNull Function<? super T, ? extends U> function);

    /**
     * Returns the number of elements contained in this sequence, as a {@code long}.
     * <p>
     * Complexity: constant time.
     */
    public final long exactSize() {
        return exactSize;
    }

    /**
     * Returns the first element of this sequence.
     * <p>
     * Complexity: constant time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    public abstract T first();

    /**
     * Returns the last element of this sequence.
     * <p>
     * Complexity: constant time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    public abstract T last();

    /**
     * Returns a copy of this sequence with the first element replaced with the given object.
     * <p>
     * Complexity: constant time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> updatedFirst(T object);

    /**
     * Returns a copy of this sequence with the last element replaced with the given object.
     * <p>
     * Complexity: constant time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> updatedLast(T object);

    /**
     * Returns a copy of this sequence with the given element prepended, that is, added as the new first element.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws IllegalStateException if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     **/
    @CheckReturnValue
    public abstract @NotNull Seq<T> prepended(T object);

    /**
     * Returns a copy of this sequence with the given element appended, that is, added as the new last element.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws IllegalStateException if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> appended(T object);

    /**
     * Returns a copy of this sequence with the first element removed.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> withoutFirst();

    /**
     * Returns a copy of this sequence with the last element removed.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> withoutLast();

    /**
     * Returns a sequence containing the elements of this sequence followed by the elements of the given sequence.
     * <p>
     * Complexity: logarithmic time.
     *
     * @throws IllegalStateException if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> concat(@NotNull Seq<? extends T> other);

    /**
     * Returns the element at the specified index in this sequence.
     * <p>
     * Complexity: constant time if the index is close to either end, logarithmic time otherwise.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    public final T get(final long index) {
        // Check the index here and let implementations assume it's always valid.
        return getImpl(Objects.checkIndex(index, exactSize));
    }

    /**
     * Returns a copy of this sequence with the element at the given index replaced with the given new value.
     * <p>
     * Complexity: logarithmic time.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> updated(final long index, final T newValue);

    /**
     * Splits this sequence into two at the given index.
     * <p>
     * The left part of the result contains the elements of this sequence stored at indices smaller than the given
     * index, the right part contains the elements stored at indices greater than or equal to the given index.
     * <p>
     * Complexity: logarithmic time.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    @CheckReturnValue
    public final @NotNull Split<T> splitAt(final long index) {
        // Check the index here and let implementations assume it's always valid.
        return splitAtImpl(Objects.checkFromToIndex(index, exactSize, exactSize));
    }

    /**
     * Returns a copy of this sequence with elements sorted according to the given comparator.
     * <p>
     * This operation is stable: the relative order of elements considered equal by the comparator is preserved.
     * <p>
     * Complexity: linear time if this sequence is already sorted or nearly sorted, O(n log n) otherwise.
     */
    @CheckReturnValue
    public final @NotNull Seq<T> sorted(final @NotNull Comparator<? super T> comparator) {
        return sortedImpl(this, comparator);
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean add(final T element) {
        throw TaggedSeq.unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean remove(final @Nullable Object object) {
        throw TaggedSeq.unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean addAll(final @NotNull Collection<? extends T> collection) {
        throw TaggedSeq.unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean removeAll(final @NotNull Collection<?> collection) {
        throw TaggedSeq.unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean retainAll(final @NotNull Collection<?> collection) {
        throw TaggedSeq.unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final void clear() {
        throw TaggedSeq.unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean removeIf(final @NotNull Predicate<? super T> filter) {
        throw TaggedSeq.unsupportedModification();
    }

    abstract void forEachChunk(@NotNull ChunkConsumer<T> action);

    abstract T getImpl(long index);

    abstract @NotNull Split<T> splitAtImpl(long index);

    private boolean equalsImpl(final @NotNull Seq<?> other) {
        if (this == other) {
            return true;
        }
        if (exactSize != other.exactSize) {
            return false;
        }
        // We can safely assume that the sizes are equal, so no need to check otherIterator.hasNext().
        final var thisIterator = iterator();
        final var otherIterator = other.iterator();
        while (thisIterator.hasNext()) {
            if (!Objects.equals(thisIterator.next(), otherIterator.next())) {
                return false;
            }
        }
        return true;
    }

    private int getEffectiveArraySize() {
        if (exactSize > maxArraySize) {
            throw new IllegalStateException("Sequence too large to convert to an array");
        }
        return (int) exactSize;
    }

    private static @NotNull Predicate<Object> makeEqualityPredicate(final @Nullable Object object) {
        return (object != null) ? object::equals : Objects::isNull;
    }

    private static <T> @NotNull Seq<T> sortedImpl(
        final @NotNull Seq<T> seq,
        final @NotNull Comparator<? super T> comparator
    ) {
        final var size = seq.exactSize;
        if (size <= 1) {
            return seq;
        }
        if (seq instanceof Deep<T, ?> deep && deep.eligibleForInsertionSort()) {
            // No point in splitting any further.
            final var array = deep.toSmallArray();
            insertionSort(array, comparator);
            return ArrayOps.toSeq(deep.tag(), size, array);
        }
        final var split = seq.splitAt(size / 2);
        final var left = sortedImpl(split.left, comparator);
        final var right = sortedImpl(split.right, comparator);
        return merge(left, right, comparator);
    }

    private static <T> @NotNull Seq<T> merge(
        final @NotNull Seq<T> left,
        final @NotNull Seq<T> right,
        final @NotNull Comparator<? super T> comparator
    ) {
        if (left.isEmpty()) {
            return right;
        }
        if (right.isEmpty()) {
            return left;
        }
        if (comparator.compare(left.last(), right.first()) <= 0) {
            return left.concat(right);
        }
        final var l = left.iterator();
        final var r = right.iterator();
        var itemL = l.next();
        var itemR = r.next();
        final var builder = new Builder<T>(left.exactSize + right.exactSize);
        while (true) {
            if (comparator.compare(itemL, itemR) <= 0) {
                builder.append(itemL);
                if (!l.hasNext()) {
                    return builder.toSeq().concat(right.splitAt(r.nextIndex() - 1).right);
                }
                itemL = l.next();
            } else {
                builder.append(itemR);
                if (!r.hasNext()) {
                    return builder.toSeq().concat(left.splitAt(l.nextIndex() - 1).right);
                }
                itemR = r.next();
            }
        }
    }

    private static <T> void insertionSort(final T @NotNull [] array, final @NotNull Comparator<? super T> comparator) {
        final var length = array.length;
        for (int i = 1; i < length; i += 1) {
            for (int k = i; k > 0 && comparator.compare(array[k - 1], array[k]) > 0; k -= 1) {
                final var temp = array[k];
                array[k] = array[k - 1];
                array[k - 1] = temp;
            }
        }
    }

    // Some JVMs are unable to create arrays of exactly Integer.MAX_VALUE elements, so give them some slack.
    private static final int maxArraySize = Integer.MAX_VALUE - 8;

    private final long exactSize;

    /**
     * Self-explanatory record for storing the result of {@link #splitAt(long)}.
     */
    public record Split<T>(@NotNull Seq<T> left, @NotNull Seq<T> right) {
    }

    /**
     * The iterator over a sequence.
     * <p>
     * See the documentation of each method for its complexity guarantees.
     */
    public abstract static sealed class Itr<T> implements Iterator<T> permits Empty.Itr, Single.Itr, Deep.Itr {
        Itr() {
        }

        static @NotNull NoSuchElementException noMoreElements() {
            throw new NoSuchElementException("No more elements");
        }

        /**
         * Returns {@code true} iff this iterator has any remaining elements.
         * <p>
         * Complexity: constant time.
         */
        @Override
        public abstract boolean hasNext();

        /**
         * Returns the next element, advancing the iterator by one position.
         * <p>
         * Complexity: amortized constant time, worst-case logarithmic time.
         *
         * @throws NoSuchElementException if the iterator has no next element
         */
        @Override
        public abstract T next();

        /**
         * Returns the index of the element that would be returned by a subsequent call to {@link #next()}.
         * <p>
         * If at the end of the sequence, returns the sequence size.
         * <p>
         * Complexity: constant time.
         */
        public abstract long nextIndex();

        /**
         * Performs the given action for all remaining elements.
         * <p>
         * If the action throws an exception, the exception is relayed to the caller; the state of the iterator becomes
         * unspecified, but valid.
         * <p>
         * Complexity: linear time.
         *
         * @throws NullPointerException if the action is null
         */
        @Override
        public final void forEachRemaining(final @NotNull Consumer<? super T> action) {
            Objects.requireNonNull(action); // Check for null just once, implementations can assume it's never null.
            forEachRemainingImpl(action);
        }

        /**
         * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
         */
        @Deprecated
        @Override
        public final void remove() {
            throw TaggedSeq.unsupportedModification();
        }

        abstract void forEachRemainingImpl(@NotNull Consumer<? super T> action);
    }

    private static final class HashCodeAccumulator<T> implements ChunkConsumer<T> {
        @Override
        public void acceptSingle(final T object) {
            hashCode = mixHash(hashCode, object);
        }

        @Override
        public void acceptArray(final T @NotNull [] array) {
            int h = hashCode;
            for (final var item : array) {
                h = mixHash(h, item);
            }
            hashCode = h;
        }

        private static int mixHash(final int hash, final @Nullable Object object) {
            return 31 * hash + Objects.hashCode(object);
        }

        private int hashCode = 1;
    }

    private static final class ArrayFiller<T extends U, U> implements ChunkConsumer<T> {
        private ArrayFiller(final U @NotNull [] array) {
            this.array = array;
        }

        @Override
        public void acceptSingle(final T object) {
            array[index] = object;
            index += 1;
        }

        @Override
        public void acceptArray(final T @NotNull [] array) {
            final var length = array.length;
            System.arraycopy(array, 0, this.array, index, length);
            index += length;
        }

        private final U @NotNull [] array;
        private int index = 0;
    }

    // A minimal mutable builder for use in sorting.
    private static final class Builder<T> {
        private Builder(final long sizeHint) {
            buffer = TypeTag.<T>unit().newArray((int) Math.min(sizeHint, maxBufferSize));
        }

        private void append(final T object) {
            if (bufferIndex >= buffer.length) {
                flushBuffer();
            }
            final var index = bufferIndex;
            buffer[index] = object;
            bufferIndex = index + 1;
        }

        private @NotNull Seq<T> toSeq() {
            flushBuffer();
            return sequence;
        }

        private void flushBuffer() {
            final var effectiveLength = bufferIndex;
            if (effectiveLength == 0) {
                return;
            }
            sequence = sequence.concat(ArrayOps.toSeq(TypeTag.unit(), effectiveLength, buffer, effectiveLength));
            bufferIndex = 0;
        }

        private static final int maxBufferSize = 2 * TaggedSeq.maxAffixLength;

        private final T @NotNull [] buffer;
        private int bufferIndex = 0;
        private @NotNull Seq<T> sequence = empty();
    }
}
