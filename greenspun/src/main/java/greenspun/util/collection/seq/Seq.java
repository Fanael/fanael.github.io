// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Arrays;
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
 * the stated complexity assumes those operations don't depend on the size of the {@code Seq}
 * <p>
 * Amortized complexity guarantees are provided under the sequential model: they hold true if all updating operations
 * since construction are accounted for in a strict linear order. They may or may not hold in fully persistent model.
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
 * Internally, the sequence is implemented as a <i>finger tree</i>, using path copying for persistence. The maximum
 * size of a node is 28, to strike a balance between tree depth and cache friendliness on one hand, and copying costs
 * of update operations on the other.
 */
public abstract sealed class Seq<T> implements Collection<T> permits TaggedSeq {
    Seq(final long subtreeSize) {
        assert subtreeSize >= 0;
        this.subtreeSize = subtreeSize;
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
        return Shallow.emptyUnit();
    }

    /**
     * Returns a new persistent sequence containing only the given element.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T element) {
        return Shallow.ofUnits(element);
    }

    /**
     * Returns a new persistent sequence containing only the given two elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T e1, final T e2) {
        return Shallow.ofUnits(e1, e2);
    }

    /**
     * Returns a new persistent sequence containing only the given three elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T e1, final T e2, final T e3) {
        return Shallow.ofUnits(e1, e2, e3);
    }

    /**
     * Returns a new persistent sequence containing only the given four elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> @NotNull Seq<T> of(final T e1, final T e2, final T e3, final T e4) {
        return Shallow.ofUnits(e1, e2, e3, e4);
    }

    /**
     * Returns a new persistent sequence containing the elements of the given iterable in iteration order.
     * <p>
     * Complexity: constant time if the iterable is already a {@code Seq}, linear time otherwise.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull Seq<T> fromIterable(final @NotNull Iterable<? extends T> iterable) {
        if (iterable instanceof Seq<? extends T> seq) {
            return (Seq<T>) seq; // This cast is fine because Seq is immutable.
        }
        final var builder = new Builder<T>();
        for (final var item : iterable) {
            builder.append(item);
        }
        return builder.toSeq();
    }

    /**
     * Returns a new persistent sequence containing the results of applying the given function to the elements of
     * the given iterable in iteration order.
     * <p>
     * Exceptions thrown by the given function are passed through to the caller.
     * <p>
     * Complexity: linear time.
     */
    public static <T, U> @NotNull Seq<U> mapIterable(
        final @NotNull Iterable<? extends T> iterable,
        final @NotNull Function<? super T, ? extends U> function
    ) {
        if (iterable instanceof Seq<? extends T> seq) {
            return seq.map(function);
        }
        final var builder = new Builder<U>();
        for (final var item : iterable) {
            builder.append(function.apply(item));
        }
        return builder.toSeq();
    }

    static @NotNull NoSuchElementException noSuchElement(final @NotNull String message) {
        throw new NoSuchElementException(message);
    }

    static @NotNull UnsupportedOperationException unsupportedModification() {
        throw new UnsupportedOperationException("Sequences don't support in-place mutation");
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
        forEachArray(accumulator);
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
        forEachArray(array -> ArrayOps.forEach(array, element -> builder.append(element).append(", ")));
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
        forEachArray(array -> ArrayOps.forEach(array, action));
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
        return Spliterators.spliterator(iterator(), subtreeSize, Spliterator.ORDERED | Spliterator.IMMUTABLE);
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
        return (int) Long.min(subtreeSize, Integer.MAX_VALUE);
    }

    /**
     * Returns {@code true} iff this sequence contains no elements.
     * <p>
     * Complexity: constant time.
     */
    @Override
    public final boolean isEmpty() {
        return subtreeSize == 0;
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
        return anySatisfies((object == null) ? Objects::isNull : object::equals);
    }

    /**
     * Returns a new array containing all the elements of this sequence, in iteration order.
     * <p>
     * The runtime component type of the returned array is {@link Object}.
     * <p>
     * Complexity: linear time.
     *
     * @throws OutOfMemoryError if this sequence is too large to fit in an array
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
     * @throws OutOfMemoryError if this sequence is too large to fit in an array
     */
    @Override
    public final <U> U @NotNull [] toArray(final U @NotNull [] array) {
        final var length = getEffectiveArraySize();
        final var usedArray = (array.length >= length) ? array : ArrayOps.newArray(array, length);
        forEachArray(new ArrayFiller<>(usedArray));
        if (usedArray.length > length) {
            usedArray[length] = null;
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
     * @throws OutOfMemoryError if this sequence is too large to fit in an array
     */
    @Override
    public final <U> U @NotNull [] toArray(final @NotNull IntFunction<U @NotNull []> generator) {
        final var array = generator.apply(getEffectiveArraySize());
        forEachArray(new ArrayFiller<>(array));
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
     * Returns the number of elements contained in this sequence, as a {@code long}.
     * <p>
     * Complexity: constant time.
     */
    public final long exactSize() {
        return subtreeSize;
    }

    /**
     * Returns {@code true} iff this sequence contains an element that satisfies the given predicate.
     * <p>
     * The order in which elements of the sequence are tested is unspecified.
     * <p>
     * Exceptions thrown by the given predicate are passed through to the caller.
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
     * @throws OutOfMemoryError if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     **/
    @CheckReturnValue
    public abstract @NotNull Seq<T> prepended(T object);

    /**
     * Returns a copy of this sequence with the given element appended, that is, added as the new last element.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws OutOfMemoryError if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
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
     * Complexity: amortized O(log(min(m,n))) time, worst-case O(log(m+n)) time.
     *
     * @throws IllegalStateException if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     */
    @CheckReturnValue
    public abstract @NotNull Seq<T> concat(@NotNull Seq<? extends T> other);

    /**
     * Returns the element at the specified index in this sequence.
     * <p>
     * Complexity: O(log(min(m,n))) time.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    public final T get(final long index) {
        // Check the index once here, let children assume it's always valid.
        return getImpl(Objects.checkIndex(index, subtreeSize), 0).element;
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
     * The front part of the result contains the elements of this sequence stored at indices smaller than the given
     * index, the back part contains the elements stored at indices greater than or equal to the given index.
     * <p>
     * Complexity: amortized O(log(min(m,n))) time, worst-case O(log(m+n)) time.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    @CheckReturnValue
    public final @NotNull Split<T> splitAt(final long index) {
        // Check the index once here, let children assume it's always valid.
        return splitAtImpl(Objects.checkFromToIndex(index, subtreeSize, subtreeSize));
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
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean remove(final @Nullable Object object) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean addAll(final @NotNull Collection<? extends T> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean removeAll(final @NotNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean retainAll(final @NotNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final void clear() {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean removeIf(final @NotNull Predicate<? super T> filter) {
        throw unsupportedModification();
    }

    abstract void forEachArray(@NotNull ArrayConsumer<T> action);

    abstract @NotNull GetResult<T> getImpl(long index, long accumulator);

    abstract @NotNull Split<T> splitAtImpl(long index);

    abstract boolean eligibleForInsertionSortImpl();

    abstract T @NotNull [] toSmallArray();

    final boolean eligibleForInsertionSort() {
        return subtreeSize <= maxChunkLength && eligibleForInsertionSortImpl();
    }

    final long computeNewSize(final long delta) {
        try {
            return Math.addExact(subtreeSize, delta);
        } catch (final ArithmeticException e) {
            throw new OutOfMemoryError("Sequence size too large");
        }
    }

    private boolean equalsImpl(final @NotNull Seq<?> other) {
        if (this == other) {
            return true;
        }
        if (subtreeSize != other.subtreeSize) {
            return false;
        }
        // We can safely assume that the sizes are equal, so no need to check theirs.hasNext().
        final var ours = iterator();
        final var theirs = other.iterator();
        while (ours.hasNext()) {
            if (!Objects.equals(ours.next(), theirs.next())) {
                return false;
            }
        }
        return true;
    }

    private int getEffectiveArraySize() {
        if (subtreeSize > maxArraySize) {
            throw new OutOfMemoryError("Sequence too large to convert to an array");
        }
        return (int) subtreeSize;
    }

    private static <T> @NotNull Seq<T> sortedImpl(
        final @NotNull Seq<T> seq,
        final @NotNull Comparator<? super T> comparator
    ) {
        final var size = seq.subtreeSize;
        if (size <= 1) {
            return seq;
        }
        if (seq.eligibleForInsertionSort()) {
            // No point in splitting any further.
            final var array = seq.toSmallArray();
            insertionSort(array, comparator);
            // NB: directly creating a shallow sequence from a freshly-created array here is fine, as we don't retain
            // any references to the array.
            return Shallow.ofUnits(array);
        }
        final var split = seq.splitAt(size / 2);
        final var left = sortedImpl(split.front, comparator);
        final var right = sortedImpl(split.back, comparator);
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
        final var builder = new Builder<T>();
        while (true) {
            if (comparator.compare(itemL, itemR) <= 0) {
                builder.append(itemL);
                if (!l.hasNext()) {
                    return builder.toSeq().concat(right.splitAt(r.nextIndex() - 1).back);
                }
                itemL = l.next();
            } else {
                builder.append(itemR);
                if (!r.hasNext()) {
                    return builder.toSeq().concat(left.splitAt(l.nextIndex() - 1).back);
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

    // We want array objects that are 32 elements big, leaving 4 references worth of space for the object header and
    // the array length. In current versions of the HotSpot VM, with 4 byte references, this makes max-sized arrays
    // exactly 128 bytes long, and with 8 byte references is just one element short of 256 bytes, which is roughly
    // 1 to 4 cache lines worth of data on common hardware.
    static final int maxChunkLength = 28;
    static final int minChunkLength = maxChunkLength / 2;

    final long subtreeSize;

    // Some JVMs are unable to create arrays of exactly Integer.MAX_VALUE elements, so give them some slack.
    private static final int maxArraySize = Integer.MAX_VALUE - 8;

    @FunctionalInterface
    interface ArrayConsumer<T> {
        void accept(T @NotNull [] array);
    }

    /**
     * The iterator over a sequence.
     * <p>
     * See the documentation of each method for its complexity guarantees.
     */
    public abstract static sealed class Itr<T> implements Iterator<T> permits Shallow.Itr, Deep.Itr {
        Itr() {
        }

        static @NotNull NoSuchElementException noMoreElements() {
            throw noSuchElement("No more elements");
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
            Objects.requireNonNull(action); // Check for null just once, children can assume it's never null.
            forEachRemainingImpl(action);
        }

        /**
         * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
         */
        @Deprecated
        @Override
        public final void remove() {
            throw unsupportedModification();
        }

        abstract void forEachRemainingImpl(@NotNull Consumer<? super T> action);
    }

    /**
     * A mutable builder for sequences.
     * <p>
     * Has the same time complexity as calling {@link Seq#appended(Object)} in a loop, but likely with better constant
     * factors.
     *
     * @param <T> the type of elements the built sequence will hold
     */
    public static final class Builder<T> {
        /**
         * Initializes a new, empty sequence builder.
         */
        @SuppressWarnings("unchecked")
        public Builder() {
            buffer = (T[]) new Object[maxChunkLength];
        }

        /**
         * Returns the sequence this builder represents.
         * <p>
         * The state of the builder remains unchanged.
         * <p>
         * Complexity: constant time.
         */
        public @NotNull Seq<T> toSeq() {
            flushBuffer();
            return sequence;
        }

        /**
         * Returns the sequence this builder represents and clears the builder.
         * <p>
         * Equivalent to calling {@link #toSeq()} followed by {@link #setSeq(Seq)} with an empty sequence.
         * <p>
         * Complexity: constant time.
         */
        public @NotNull Seq<T> take() {
            final var result = toSeq();
            setSeq(empty());
            return result;
        }

        /**
         * Replaces the sequence this builder represents with the given sequence.
         * <p>
         * Complexity: constant time.
         */
        public void setSeq(final @NotNull Seq<T> sequence) {
            this.sequence = sequence;
            bufferIndex = 0;
        }

        /**
         * Appends a new element to the sequence this builder represents.
         * <p>
         * Complexity: amortized constant time, worst-case logarithmic time.
         */
        public void append(final T object) {
            if (bufferIndex >= buffer.length) {
                flushBuffer();
            }
            final var index = bufferIndex;
            buffer[index] = object;
            bufferIndex = index + 1;
        }

        private void flushBuffer() {
            final var effectiveLength = bufferIndex;
            if (effectiveLength == 0) {
                return;
            }
            // NB: the copy done by copyOf is important, even if the effective length is equal to buffer length: since
            // Shallow.ofUnits creates a Shallow directly backed by the given array, we cannot pass our buffer there,
            // because our buffer is mutable.
            sequence = sequence.concat(Shallow.ofUnits(Arrays.copyOf(buffer, effectiveLength)));
            bufferIndex = 0;
        }

        private final T @NotNull [] buffer;
        private int bufferIndex = 0;
        private @NotNull Seq<T> sequence = empty();
    }

    /**
     * Self-explanatory record representing the result of {@link #splitAt(long)}.
     */
    public record Split<T>(@NotNull Seq<T> front, @NotNull Seq<T> back) {
    }

    record GetResult<T>(T element, long accumulator) {
    }

    private static final class HashCodeAccumulator<T> implements ArrayConsumer<T> {
        @Override
        public void accept(final T @NotNull [] array) {
            int hash = hashCode;
            for (final var item : array) {
                hash = 31 * hash + Objects.hashCode(item);
            }
            hashCode = hash;
        }

        private int hashCode = 1;
    }

    private static final class ArrayFiller<T extends U, U> implements ArrayConsumer<T> {
        private ArrayFiller(final U @NotNull [] array) {
            this.array = array;
        }

        @Override
        public void accept(final T @NotNull [] array) {
            final var length = array.length;
            System.arraycopy(array, 0, this.array, index, length);
            index += length;
        }

        private final U @NotNull [] array;
        private int index = 0;
    }
}
