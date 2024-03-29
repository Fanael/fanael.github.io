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
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import edu.umd.cs.findbugs.annotations.CheckReturnValue;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

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
public abstract sealed class Seq<T> implements Collection<T> permits SeqImpl {
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
    public static <T> Seq<T> empty() {
        return Shallow.emptyUnit();
    }

    /**
     * Returns a new persistent sequence containing only the given element.
     * <p>
     * Complexity: constant time.
     */
    public static <T> Seq<T> of(final T element) {
        return Shallow.ofUnits(element);
    }

    /**
     * Returns a new persistent sequence containing only the given two elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> Seq<T> of(final T e1, final T e2) {
        return Shallow.ofUnits(e1, e2);
    }

    /**
     * Returns a new persistent sequence containing only the given three elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> Seq<T> of(final T e1, final T e2, final T e3) {
        return Shallow.ofUnits(e1, e2, e3);
    }

    /**
     * Returns a new persistent sequence containing only the given four elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> Seq<T> of(final T e1, final T e2, final T e3, final T e4) {
        return Shallow.ofUnits(e1, e2, e3, e4);
    }

    /**
     * Returns a new persistent sequence containing only the given five elements, in order.
     * <p>
     * Complexity: constant time.
     */
    public static <T> Seq<T> of(final T e1, final T e2, final T e3, final T e4, final T e5) {
        return Shallow.ofUnits(e1, e2, e3, e4, e5);
    }

    /**
     * Returns a new persistent sequence containing the elements of the given iterable in iteration order.
     * <p>
     * Complexity: constant time if the iterable is already a {@code Seq}, linear time otherwise.
     */
    @SuppressWarnings("unchecked")
    public static <T> Seq<T> fromIterable(final Iterable<? extends T> iterable) {
        if (iterable instanceof final Seq<? extends T> seq) {
            return (Seq<T>) seq; // Since Seq is immutable, casting <? extends T> to <T> is fine.
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
    public static <T, U> Seq<U> mapIterable(
        final Iterable<? extends T> iterable,
        final Function<? super T, ? extends U> function
    ) {
        if (iterable instanceof final Seq<? extends T> seq) {
            return seq.map(function);
        }
        final var builder = new Builder<U>();
        for (final var item : iterable) {
            builder.append(function.apply(item));
        }
        return builder.toSeq();
    }

    /**
     * Returns the hash code of this sequence. The algorithm used to compute the hash code is unspecified.
     * <p>
     * Complexity: linear time.
     */
    @Override
    @SuppressWarnings("ObjectInstantiationInEqualsHashCode")
    public final int hashCode() {
        final var accumulator = new HashCodeAccumulator();
        forEachArray(accumulator);
        return accumulator.hash;
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
        return object instanceof final Seq<?> other && equalsImpl(other);
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
    public final String toString() {
        final var accumulator = new StringAccumulator();
        forEachArray(accumulator);
        return accumulator.finish();
    }

    /**
     * Returns a new iterator over the elements of this sequence, from the first element to the last.
     * <p>
     * Complexity: constant time.
     */
    @Override
    public abstract @NonNull Itr<T> iterator();

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
    public final void forEach(final Consumer<? super T> action) {
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
    public final Spliterator<T> spliterator() {
        return new SpliteratorImpl<>(this);
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
    public final Object @NonNull [] toArray() {
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
    // The semantics of this method are too complex to express in CF terms.
    @SuppressWarnings({"nullness:override.param", "nullness:conditional", "nullness:return", "keyfor:override.return"})
    public final <U> U @NonNull [] toArray(final U[] array) {
        final var length = getEffectiveArraySize();
        final @Nullable U[] usedArray = (array.length >= length) ? array : ArrayOps.newArray(array, length);
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
    @SuppressWarnings("keyfor:override.return") // CF incorrectly infers the return type.
    public final <U> U[] toArray(final IntFunction<U[]> generator) {
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
    public final boolean containsAll(final Collection<?> collection) {
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
    public abstract boolean anySatisfies(Predicate<? super T> predicate);

    /**
     * Returns a sequence containing the results of applying the given function to the elements of this sequence, in
     * iteration order.
     * <p>
     * Exceptions thrown by the given function are passed through to the caller.
     * <p>
     * Complexity: linear time.
     */
    @CheckReturnValue
    public abstract <U> Seq<U> map(Function<? super T, ? extends U> function);

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
    public abstract Seq<T> updatedFirst(T object);

    /**
     * Returns a copy of this sequence with the last element replaced with the given object.
     * <p>
     * Complexity: constant time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract Seq<T> updatedLast(T object);

    /**
     * Returns a copy of this sequence with the given element prepended, that is, added as the new first element.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws OutOfMemoryError if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     **/
    @CheckReturnValue
    public abstract Seq<T> prepended(T object);

    /**
     * Returns a copy of this sequence with the given element appended, that is, added as the new last element.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws OutOfMemoryError if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     */
    @CheckReturnValue
    public abstract Seq<T> appended(T object);

    /**
     * Returns a copy of this sequence with the first element removed.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract Seq<T> withoutFirst();

    /**
     * Returns a copy of this sequence with the last element removed.
     * <p>
     * Complexity: amortized constant time, worst-case logarithmic time.
     *
     * @throws NoSuchElementException if this sequence is empty
     */
    @CheckReturnValue
    public abstract Seq<T> withoutLast();

    /**
     * Returns a sequence containing the elements of this sequence followed by the elements of the given sequence.
     * <p>
     * Complexity: amortized O(log(min(m,n))) time, worst-case O(log(m+n)) time.
     *
     * @throws OutOfMemoryError if the resulting sequence would be larger than {@link Long#MAX_VALUE}.
     */
    @CheckReturnValue
    public abstract Seq<T> concat(Seq<? extends T> other);

    /**
     * Returns the element at the specified index in this sequence.
     * <p>
     * Complexity: O(log(min(k,n-k))) time.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    public abstract T get(final long index);

    /**
     * Returns a copy of this sequence with the element at the given index replaced with the given new value.
     * <p>
     * Complexity: O(log(min(k,n-k))) time.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    @CheckReturnValue
    public abstract Seq<T> updated(final long index, final T newValue);

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
    public abstract Split<T> splitAt(final long index);

    /**
     * Returns a copy of this sequence with elements sorted according to the given comparator.
     * <p>
     * This operation is stable: the relative order of elements considered equal by the comparator is preserved.
     * <p>
     * Complexity: linear time if this sequence is already sorted or nearly sorted, O(n log n) otherwise.
     */
    @CheckReturnValue
    public final Seq<T> sorted(final Comparator<? super T> comparator) {
        final var sequences = new Builder<Seq<T>>();
        forEachArray(array -> sequences.append(insertionSort(array, comparator)));
        return mergePairs(sequences.toSeq(), comparator);
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
    public final boolean addAll(final @NonNull Collection<? extends T> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean removeAll(final @NonNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public final boolean retainAll(final @NonNull Collection<?> collection) {
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
    public final boolean removeIf(final Predicate<? super T> filter) {
        throw unsupportedModification();
    }

    abstract void forEachArray(ArrayConsumer<? super T> action);

    final long computeNewSize(final long delta) {
        try {
            return Math.addExact(subtreeSize, delta);
        } catch (final ArithmeticException e) {
            throw new OutOfMemoryError("Sequence size too large");
        }
    }

    private boolean equalsImpl(final Seq<?> other) {
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

    private static <T> Seq<T> mergePairs(final Seq<Seq<T>> sequences, final Comparator<? super T> comparator) {
        var remaining = sequences;
        while (remaining.subtreeSize > 1) {
            final var builder = new Builder<Seq<T>>();
            for (final var it = remaining.iterator(); it.hasNext(); ) {
                final var first = it.next();
                final var merged = it.hasNext() ? merge(first, it.next(), comparator) : first;
                builder.append(merged);
            }
            remaining = builder.toSeq();
        }
        return remaining.first();
    }

    private static <T> Seq<T> merge(final Seq<T> left, final Seq<T> right, final Comparator<? super T> comparator) {
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
                    builder.append(itemR);
                    return builder.toSeq().concat(r.rest());
                }
                itemL = l.next();
            } else {
                builder.append(itemR);
                if (!r.hasNext()) {
                    builder.append(itemL);
                    return builder.toSeq().concat(l.rest());
                }
                itemR = r.next();
            }
        }
    }

    private static <T> Seq<T> insertionSort(final T[] array, final Comparator<? super T> comparator) {
        return switch (array.length) {
            case 0 -> empty();
            case 1 -> of(array[0]);
            default -> {
                final var newArray = array.clone();
                insertionSortInPlace(newArray, comparator);
                yield Deep.fromSingleArray(Tag.unit(), newArray.length, newArray, Chunk.maxLength);
            }
        };
    }

    private static <T> void insertionSortInPlace(final T[] array, final Comparator<? super T> comparator) {
        final var length = array.length;
        for (int i = 1; i < length; i += 1) {
            for (int k = i; k > 0 && comparator.compare(array[k - 1], array[k]) > 0; k -= 1) {
                final var temp = array[k];
                array[k] = array[k - 1];
                array[k - 1] = temp;
            }
        }
    }

    private static UnsupportedOperationException unsupportedModification() {
        throw new UnsupportedOperationException("Sequences don't support in-place mutation");
    }

    final long subtreeSize;

    // Some JVMs are unable to create arrays of exactly Integer.MAX_VALUE elements, so give them some slack.
    private static final int maxArraySize = Integer.MAX_VALUE - 8;

    /**
     * The iterator over a sequence.
     * <p>
     * See the documentation of each method for its complexity guarantees.
     */
    public abstract static sealed class Itr<T> implements Iterator<T> permits Shallow.Itr, Deep.Itr {
        Itr() {
        }

        static NoSuchElementException noMoreElements() {
            throw SeqImpl.noSuchElement("No more elements");
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
         * Returns the next element, without advancing the iterator.
         * <p>
         * Complexity: constant time.
         *
         * @throws NoSuchElementException if the iterator has no next element
         */
        public abstract T peek();

        /**
         * Returns a sequence containing all elements that would be returned by continuously calling {@link #next()}.
         * <p>
         * Complexity: constant time on average, worst-case logarithmic time.
         */
        public final Seq<T> rest() {
            return restImpl();
        }

        /**
         * Returns the index of the element that would be returned by a subsequent call to {@link #next()}.
         * <p>
         * If at the end of the sequence, returns the sequence size.
         * <p>
         * Complexity: constant time.
         */
        public abstract long nextIndex();

        /**
         * Returns the index of the element that was returned by the previous call to {@link #next()}.
         * <p>
         * If at the beginning of the sequence, returns {@code -1}.
         * <p>
         * Complexity: constant time.
         */
        public final long previousIndex() {
            return nextIndex() - 1;
        }

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
        public final void forEachRemaining(final Consumer<? super T> action) {
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

        abstract void forEachRemainingImpl(Consumer<? super T> action);

        abstract SeqImpl<T, @Nullable Object> restImpl();
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
         * <p>
         * Equivalent to calling {@link #Builder(Seq)} with {@link #empty()}.
         */
        public Builder() {
            this(empty());
        }

        /**
         * Initializes a new sequence builder with the given sequence as its initial contents.
         */
        @SuppressWarnings("unchecked")
        public Builder(final Seq<T> sequence) {
            buffer = (T[]) new Object[Chunk.maxLength];
            this.sequence = sequence;
        }

        /**
         * Returns the sequence this builder represents.
         * <p>
         * The state of the builder remains unchanged.
         * <p>
         * Complexity: constant time.
         */
        public Seq<T> toSeq() {
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
        public Seq<T> take() {
            final var result = toSeq();
            setSeq(empty());
            return result;
        }

        /**
         * Replaces the sequence this builder represents with the given sequence.
         * <p>
         * Complexity: constant time.
         */
        public void setSeq(final Seq<T> sequence) {
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

        private final T[] buffer;
        private int bufferIndex = 0;
        private Seq<T> sequence;
    }

    /**
     * Self-explanatory record representing the result of {@link #splitAt(long)}.
     */
    public record Split<T>(Seq<T> front, Seq<T> back) {
    }

    private static final class SpliteratorImpl<T> implements Spliterator<T> {
        private SpliteratorImpl(final Seq<T> sequence) {
            setSequence(sequence);
        }

        @Override
        public boolean tryAdvance(final Consumer<? super T> action) {
            Objects.requireNonNull(action); // Check in case there are no elements left.
            if (!iterator.hasNext()) {
                return false;
            }
            action.accept(iterator.next());
            return true;
        }

        @Override
        public void forEachRemaining(final Consumer<? super T> action) {
            iterator.forEachRemaining(action);
        }

        @Override
        public @Nullable Spliterator<T> trySplit() {
            final var size = remainingSize();
            if (size < 2) {
                return null;
            }
            final var split = iterator.rest().splitAt(size / 2);
            final var result = new SpliteratorImpl<>(split.front());
            setSequence(split.back());
            return result;
        }

        @Override
        public long estimateSize() {
            return remainingSize();
        }

        @Override
        public long getExactSizeIfKnown() {
            return remainingSize();
        }

        @Override
        public int characteristics() {
            return SIZED | SUBSIZED | ORDERED | IMMUTABLE;
        }

        private long remainingSize() {
            return sequenceSize - iterator.nextIndex();
        }

        @EnsuresNonNull("iterator")
        private void setSequence(@UnknownInitialization SpliteratorImpl<T> this, final Seq<T> sequence) {
            iterator = sequence.iterator();
            sequenceSize = sequence.subtreeSize;
        }

        private Itr<T> iterator;
        private long sequenceSize;
    }

    private static final class HashCodeAccumulator implements ArrayConsumer<@Nullable Object> {
        @Override
        public void accept(final @Nullable Object[] array) {
            int hash = this.hash;
            for (final var item : array) {
                hash = 31 * hash + Objects.hashCode(item);
            }
            this.hash = hash;
        }

        private int hash = 1;
    }

    private static final class StringAccumulator implements ArrayConsumer<@Nullable Object> {
        @Override
        public void accept(final @Nullable Object[] array) {
            boolean needsSeparator = this.needsSeparator;
            for (final var element : array) {
                if (needsSeparator) {
                    builder.append(", ");
                }
                needsSeparator = true;
                builder.append(element);
            }
            this.needsSeparator = needsSeparator;
        }

        private String finish() {
            builder.append(']');
            return builder.toString();
        }

        private final StringBuilder builder = new StringBuilder("[");
        private boolean needsSeparator = false;
    }

    private static final class ArrayFiller<T extends U, U> implements ArrayConsumer<T> {
        private ArrayFiller(final @Nullable U[] array) {
            this.array = array;
        }

        @Override
        public void accept(final T[] array) {
            final var length = array.length;
            System.arraycopy(array, 0, this.array, index, length);
            index += length;
        }

        private final @Nullable U[] array;
        private int index = 0;
    }
}
