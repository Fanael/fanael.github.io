// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.RandomAccess;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A {@link List} implementation that is guaranteed to be immutable.
 * <p>
 * The immutability is shallow: it does <em>not</em> extend to the contained objects, they can still be mutated if their
 * type allows it. For this reason, it is recommended that {@code ImmutableList} only be used immutable element types
 * that are immutable themselves.
 * <p>
 * All methods that potentially modify the list throw {@link UnsupportedOperationException}. They're marked as
 * deprecated, to allow accidental uses to be caught at compile time.
 * <p>
 * Immutable lists permit {@code null} elements.
 * <p>
 * This list is implemented using a single array, so its performance characteristics are very similar to
 * {@link java.util.ArrayList}.
 *
 * @param <T> the type of elements in this list
 */
public final class ImmutableList<T> implements List<T>, RandomAccess {
    private ImmutableList(final T @NotNull [] items) {
        assert items.getClass() == Object[].class;
        this.items = items;
    }

    /**
     * Returns an empty immutable list pretending to contain objects of the given type.
     * <p>
     * There is no guarantee regarding the identity of the returned list, so relying on identity-based operations
     * such as synchronization or reference equality is discouraged.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull ImmutableList<T> empty() {
        return (ImmutableList<T>) EmptyHolder.instance;
    }

    /**
     * Returns a new immutable list containing only the given element.
     */
    public static <T> @NotNull ImmutableList<T> of(final T element) {
        return ofArgs(element);
    }

    /**
     * Returns a new immutable list containing only the given two elements, in order.
     */
    public static <T> @NotNull ImmutableList<T> of(final T e1, final T e2) {
        return ofArgs(e1, e2);
    }

    /**
     * Returns a new immutable list containing only the given three elements, in order.
     */
    public static <T> @NotNull ImmutableList<T> of(final T e1, final T e2, final T e3) {
        return ofArgs(e1, e2, e3);
    }

    /**
     * Returns a new immutable list containing only the given four elements, in order.
     */
    public static <T> @NotNull ImmutableList<T> of(final T e1, final T e2, final T e3, final T e4) {
        return ofArgs(e1, e2, e3, e4);
    }

    /**
     * Returns an immutable list containing the results of applying the given function to the elements of the given
     * collection.
     * <p>
     * If the given collection makes any guarantees regarding the order of its elements as returned by its iterator,
     * the returned list will contain the elements in the same order; otherwise no particular order is guaranteed.
     * <p>
     * If the given collection is empty, has the same effect as {@link #empty()}.
     * <p>
     * Exceptions thrown by the given function are passed through.
     */
    public static <T, R> @NotNull ImmutableList<R> map(
        final @NotNull Collection<? extends T> collection,
        final @NotNull Function<? super T, ? extends R> function
    ) {
        final var builder = new Builder<R>(collection.size());
        for (final var item : collection) {
            builder.add(function.apply(item));
        }
        return builder.freeze();
    }

    /**
     * Returns the hash code of this list, following the algorithm specified by {@link List#hashCode()}.
     */
    @Override
    public int hashCode() {
        int hash = 1;
        for (final var item : items) {
            hash = 31 * hash + Objects.hashCode(item);
        }
        return hash;
    }

    /**
     * Returns {@code true} iff the given object is a {@link List} of the same size containing equal elements in the
     * same order.
     */
    @Override
    public boolean equals(final @Nullable Object object) {
        if (object instanceof ImmutableList<?> list) {
            return Arrays.equals(items, list.items);
        }
        if (object instanceof List<?> list) {
            return equalsFallback(list);
        }
        return false;
    }

    /**
     * Returns a string representation of this list.
     * <p>
     * The returned string follows the same format as {@link java.util.AbstractCollection#toString()}.
     */
    @Override
    public @NotNull String toString() {
        final var items = this.items;
        if (items.length == 0) {
            return "[]";
        }

        final var builder = new StringBuilder();
        builder.append('[');
        for (final T item : items) {
            builder.append(item).append(", ");
        }
        // Need to remove the final comma and space.
        builder.setLength(builder.length() - 2);
        builder.append(']');
        return builder.toString();
    }

    /**
     * Returns a new iterator over the elements of this list, from the first element to the last.
     */
    @Override
    public @NotNull Iterator<T> iterator() {
        return new Iter<>(items, 0);
    }

    /**
     * Returns a new {@link Spliterator} over the elements of this list.
     * <p>
     * The returned spliterator always reports at least {@link Spliterator#ORDERED}, {@link Spliterator#SIZED},
     * {@link Spliterator#IMMUTABLE} and {@link Spliterator#SUBSIZED} characteristics.
     */
    @Override
    public @NotNull Spliterator<T> spliterator() {
        return Spliterators.spliterator(items, Spliterator.ORDERED | Spliterator.IMMUTABLE);
    }

    /**
     * Performs the given action for each element of this list.
     * <p>
     * Exceptions thrown by the action are passed to the caller.
     */
    @Override
    public void forEach(final @NotNull Consumer<? super T> action) {
        Objects.requireNonNull(action);
        for (final var item : items) {
            action.accept(item);
        }
    }

    /**
     * Returns the number of elements in this list.
     */
    @Override
    public int size() {
        return items.length;
    }

    /**
     * Returns {@code true} iff this list contains no elements.
     */
    @Override
    public boolean isEmpty() {
        return items.length == 0;
    }

    /**
     * Returns {@code true} iff this list contains any elements equal to the given object.
     * <p>
     * Equality is determined using {@link Objects#equals(Object, Object)}.
     */
    @Override
    public boolean contains(final @Nullable Object object) {
        return anySatisfies(makeEqualityPredicate(object));
    }

    /**
     * Returns a new array containing all elements of this list in the same order.
     * <p>
     * The runtime component type of the returned array is {@link Object}.
     */
    @Override
    public Object @NotNull [] toArray() {
        return items.clone();
    }

    /**
     * Returns an array containing all the elements of this list in the same order.
     * If the list fits in the given array, it is returned therein. Otherwise, a new array of the same size as this list
     * is allocated and returned.
     * <p>
     * If the list fits with room to spare, the element immediately following the end of the sequence is set to
     * {@code null}.
     * <p>
     * The runtime component type of the returned array is the same as that of the given array.
     */
    @SuppressWarnings({"unchecked", "SuspiciousSystemArraycopy"})
    @Override
    public <U> U @NotNull [] toArray(final U @NotNull [] array) {
        final var size = items.length;
        if (array.length < size) {
            return (U[]) Arrays.copyOf(items, size, array.getClass());
        }
        System.arraycopy(items, 0, array, 0, size);
        if (array.length > size) {
            array[size] = null;
        }
        return array;
    }

    /**
     * Returns {@code true} iff this list contains all the elements of the given collection.
     * If the given collection is empty, {@code true} is returned.
     * <p>
     * Equality is determined using {@link Objects#equals(Object, Object)}.
     */
    @Override
    public boolean containsAll(final @NotNull Collection<?> collection) {
        for (final var item : collection) {
            if (!contains(item)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns the element at the specified index in this list.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    @Override
    public T get(final int index) {
        return items[Objects.checkIndex(index, items.length)];
    }

    /**
     * Returns the index of the <em>first</em> element equal to the given object, or {@code -1} if no element is equal.
     * <p>
     * Equality is determined using {@link Objects#equals(Object, Object)}.
     */
    @Override
    public int indexOf(final @Nullable Object object) {
        return findFirst(makeEqualityPredicate(object));
    }

    /**
     * Returns the index of the <em>last</em> element equal to the given object, or {@code -1} if no element is equal.
     * <p>
     * Equality is determined using {@link Objects#equals(Object, Object)}.
     */
    @Override
    public int lastIndexOf(final @Nullable Object object) {
        return findLast(makeEqualityPredicate(object));
    }

    /**
     * Returns a new list iterator over the elements of this list, from the first element to the last.
     */
    @Override
    public @NotNull ListIterator<T> listIterator() {
        return new Iter<>(items, 0);
    }

    /**
     * Returns a new list iterator over the elements of this list, starting from the given index.
     *
     * @throws IndexOutOfBoundsException if the index is out of range
     */
    @Override
    public @NotNull ListIterator<T> listIterator(final int index) {
        return new Iter<>(items, Objects.checkIndex(index, items.length + 1));
    }

    /**
     * Returns a copy of this list containing the elements in the given range.
     *
     * @throws IndexOutOfBoundsException if either endpoint is out of range
     */
    @Override
    public @NotNull ImmutableList<T> subList(final int fromIndex, final int toIndex) {
        Objects.checkFromToIndex(fromIndex, toIndex, items.length);
        return fromIndex == toIndex ? empty() : new ImmutableList<>(Arrays.copyOfRange(items, fromIndex, toIndex));
    }

    /**
     * Returns {@code true} iff this list contains any elements for which the given predicate returns {@code true}.
     */
    public boolean anySatisfies(final @NotNull Predicate<? super T> predicate) {
        return findFirst(predicate) != -1;
    }

    /**
     * Returns the index of the <em>first</em> element for which the given predicate returns {@code true}.
     * <p>
     * If no such element is found, {@code -1} is returned.
     */
    public int findFirst(final @NotNull Predicate<? super T> predicate) {
        final var items = this.items;
        final var length = items.length;
        for (int i = 0; i < length; i += 1) {
            if (predicate.test(items[i])) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Returns the index of the <em>last</em> element for which the given predicate returns {@code true}.
     * <p>
     * If no such element is found, {@code -1} is returned.
     */
    public int findLast(final @NotNull Predicate<? super T> predicate) {
        final var items = this.items;
        final var length = items.length;
        for (int i = length - 1; i >= 0; i -= 1) {
            if (predicate.test(items[i])) {
                return i;
            }
        }
        return -1;
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean add(final T element) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean remove(final @Nullable Object object) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean addAll(final @NotNull Collection<? extends T> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean addAll(final int index, final @NotNull Collection<? extends T> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean removeAll(final @NotNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean retainAll(final @NotNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public void clear() {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public T set(final int index, final T element) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public void add(final int index, final T element) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public T remove(final int index) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public void replaceAll(final @NotNull UnaryOperator<T> operator) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public void sort(final @NotNull Comparator<? super T> comparator) {
        throw unsupportedModification();
    }

    /**
     * @deprecated Mutable operation, always throws {@link UnsupportedOperationException}.
     */
    @Deprecated
    @Override
    public boolean removeIf(final @NotNull Predicate<? super T> filter) {
        throw unsupportedModification();
    }

    private boolean equalsFallback(final @NotNull List<?> list) {
        final var items = this.items;
        if (items.length != list.size()) {
            return false;
        }
        // The other object is some other list type of the same size, need to iterate it.
        final var iterator = list.iterator();
        for (int index = 0; iterator.hasNext(); index += 1) {
            final var thisItem = items[index];
            final var otherItem = iterator.next();
            if (!Objects.equals(thisItem, otherItem)) {
                return false;
            }
        }
        return true;
    }

    private static @NotNull UnsupportedOperationException unsupportedModification() {
        throw new UnsupportedOperationException("Cannot modify an immutable list");
    }

    private static @NotNull Predicate<Object> makeEqualityPredicate(final @Nullable Object object) {
        return (object != null) ? object::equals : Objects::isNull;
    }

    @SafeVarargs
    @SuppressWarnings("varargs")
    private static <T> @NotNull ImmutableList<T> ofArgs(final T... items) {
        return new ImmutableList<>(items);
    }

    private final T @NotNull [] items;

    /**
     * The builder for immutable lists.
     * <p>
     * Essentially just a stripped down {@link java.util.ArrayList} that can be {@link #freeze() frozen} to get an
     * immutable list out of it.
     * <p>
     * The internal array employs geometric growth on reallocation, so the {@link #add(Object)} method runs in amortized
     * constant time.
     *
     * @param <T> the type of elements the immutable list will hold
     */
    public static final class Builder<T> {
        /**
         * Initializes a new empty immutable list builder with initial capacity of 10.
         */
        public Builder() {
            array = defaultCapacityMarker;
            size = 0;
        }

        /**
         * Initializes a new empty immutable list builder with the given initial capacity.
         */
        public Builder(final int initialCapacity) {
            array = (initialCapacity == 0) ? EmptyHolder.emptyArray : new Object[initialCapacity];
            size = 0;
        }

        /**
         * Freezes the contents of this builder, returning a new immutable list.
         * The returned immutable list will represent the same array as this builder.
         * <p>
         * This method causes this builder to be cleared: all elements are removed and its capacity reverts to the
         * default 10.
         */
        @SuppressWarnings("unchecked")
        public @NotNull ImmutableList<T> freeze() {
            if (size == 0) {
                return empty();
            }
            final var result = (array.length == size)
                ? new ImmutableList<>((T[]) array)
                : new ImmutableList<>((T[]) Arrays.copyOf(array, size));
            array = defaultCapacityMarker;
            size = 0;
            return result;
        }

        /**
         * Returns the number of elements this builder currently holds.
         */
        public int size() {
            return size;
        }

        /**
         * Returns {@code true} iff this builder contains no elements.
         */
        public boolean isEmpty() {
            return size == 0;
        }

        /**
         * Returns the element at the specified index in this builder.
         *
         * @throws IndexOutOfBoundsException if the index is out of range
         */
        @SuppressWarnings("unchecked")
        public T get(final int index) {
            return (T) array[Objects.checkIndex(index, size)];
        }

        /**
         * Changes the element at the specified index in this list to the given new value.
         *
         * @throws IndexOutOfBoundsException if the index is out of range
         */
        public void set(final int index, final T newValue) {
            array[Objects.checkIndex(index, size)] = newValue;
        }

        /**
         * Adds the given element to the end of this builder.
         */
        public void add(final T item) {
            growIfNeeded(1);
            final var index = size;
            array[index] = item;
            size = index + 1;
        }

        /**
         * Adds all elements of the given collection to the end of this builder.
         */
        public void addAll(final @NotNull Collection<? extends T> collection) {
            final var source = (collection instanceof ImmutableList<?> list) ? list.items : collection.toArray();
            final var newItemCount = source.length;
            growIfNeeded(newItemCount);
            final var index = size;
            System.arraycopy(source, 0, array, size, newItemCount);
            size = index + newItemCount;
        }

        private void growIfNeeded(final int delta) {
            final var capacity = array.length;
            final var requiredCapacity = size + delta;
            if (requiredCapacity <= capacity) {
                return;
            }
            if (requiredCapacity < delta) {
                // Capacity has overflown.
                throw arrayTooBig();
            }
            grow(requiredCapacity);
        }

        @SuppressWarnings("ArrayEquality")
        private void grow(final int requiredCapacity) {
            final var oldCapacity = array.length;
            var preferredCapacity = (array == defaultCapacityMarker)
                ? defaultCapacity
                : (oldCapacity + oldCapacity / 2);
            if (preferredCapacity < oldCapacity) {
                preferredCapacity = maxArraySize;
            }
            final var newCapacity = Math.max(preferredCapacity, requiredCapacity);
            array = Arrays.copyOf(array, newCapacity);
        }

        private static @NotNull OutOfMemoryError arrayTooBig() {
            throw new OutOfMemoryError("Array size too big");
        }

        // Some JVMs are unable to create arrays of exactly Integer.MAX_VALUE elements, so give them some slack.
        private static final int maxArraySize = Integer.MAX_VALUE - 8;
        private static final int defaultCapacity = 10;
        private static final Object[] defaultCapacityMarker = new Object[0];

        private Object @NotNull [] array;
        private int size;
    }

    private static final class EmptyHolder {
        private static final Object[] emptyArray = new Object[0];
        private static final ImmutableList<?> instance = new ImmutableList<>(emptyArray);
    }

    private static final class Iter<T> implements ListIterator<T> {
        private Iter(final T @NotNull [] items, final int index) {
            this.items = items;
            this.index = index;
        }

        @Override
        public boolean hasNext() {
            return index < items.length;
        }

        @Override
        public boolean hasPrevious() {
            return index > 0;
        }

        @Override
        @SuppressFBWarnings(value = "IT_NO_SUCH_ELEMENT", justification = "It can, SpotBugs is confused")
        public T next() {
            final var index = this.index;
            if (index >= items.length) {
                throw noMoreElements();
            }
            final var item = items[index];
            this.index = index + 1;
            return item;
        }

        @Override
        public T previous() {
            final var index = this.index - 1;
            if (index < 0) {
                throw noMoreElements();
            }
            final var item = items[index];
            this.index = index;
            return item;
        }

        @Override
        public int nextIndex() {
            return index;
        }

        @Override
        public int previousIndex() {
            return index - 1;
        }

        @Override
        public void forEachRemaining(final @NotNull Consumer<? super T> action) {
            Objects.requireNonNull(action);
            final var items = this.items;
            final var length = items.length;
            for (int i = index; i < length; i += 1) {
                action.accept(items[i]);
            }
            index = length;
        }

        @Override
        public void remove() {
            throw unsupportedModification();
        }

        @Override
        public void set(final T t) {
            throw unsupportedModification();
        }

        @Override
        public void add(final T t) {
            throw unsupportedModification();
        }

        private static @NotNull NoSuchElementException noMoreElements() {
            throw new NoSuchElementException("No more elements");
        }

        private final T @NotNull [] items;
        private int index;
    }
}
