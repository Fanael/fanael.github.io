// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection;

import java.util.ArrayList;
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
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.util.function.ThrowingFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A {@link List} implementation that is guaranteed to be immutable.
 * <p>
 * The immutability is shallow: it does <em>not</em> extend to the contained objects, they can still be mutated if their
 * type allows it. For this reason, it is recommended that {@code ImmutableList} only be used immutable element types
 * that are immutable themselves.
 * <p>
 * All methods that potentially modify the list throw {@link UnsupportedOperationException}.
 * <p>
 * Immutable lists permit {@code null} elements.
 *
 * @param <T> the type of elements in this list
 */
public abstract sealed class ImmutableList<T> implements List<T>, RandomAccess {
    private ImmutableList() {
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
    @SuppressWarnings("unchecked")
    public static <T> @NotNull ImmutableList<T> of(final T element) {
        return new Simple<>((T[]) new Object[]{element});
    }

    /**
     * Returns a new immutable list containing only the given two elements, in order.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull ImmutableList<T> of(final T e1, final T e2) {
        return new Simple<>((T[]) new Object[]{e1, e2});
    }

    /**
     * Returns a new immutable list containing only the given three elements, in order.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull ImmutableList<T> of(final T e1, final T e2, final T e3) {
        return new Simple<>((T[]) new Object[]{e1, e2, e3});
    }

    /**
     * Returns a new immutable list containing only the given four elements, in order.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull ImmutableList<T> of(final T e1, final T e2, final T e3, final T e4) {
        return new Simple<>((T[]) new Object[]{e1, e2, e3, e4});
    }

    /**
     * Returns an immutable list containing all elements of the given {@link ArrayList} in the same order.
     * <p>
     * If the given {@link ArrayList} is empty, has the same effect as {@link #empty()}.
     */
    @SuppressWarnings("unchecked")
    public static <T> @NotNull ImmutableList<T> freeze(final @NotNull ArrayList<? extends T> list) {
        return list.isEmpty() ? empty() : new Simple<>((T[]) SafeArrayAccess.toSafeArray(list));
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
    @SuppressWarnings("unchecked")
    public static <T, R, E extends Throwable> @NotNull ImmutableList<R> map(
        final @NotNull Collection<? extends T> collection,
        final @NotNull ThrowingFunction<? super T, ? extends R, E> function
    ) throws E {
        final var array = SafeArrayAccess.toSafeArray(collection);
        assert array.getClass().getComponentType() == Object.class;
        final var size = array.length;
        if (size == 0) {
            return empty();
        }
        for (int index = 0; index < size; index += 1) {
            array[index] = function.apply((T) array[index]);
        }
        return new Simple<>((R[]) array);
    }

    /**
     * Returns the hash code of this list, following the algorithm specified by {@link List#hashCode()}.
     */
    @Override
    public abstract int hashCode();

    /**
     * Returns {@code true} iff the given object is a list of the same size containing equal elements in the same order.
     * This follows the requirements of {@link List#equals(Object)}.
     */
    @Override
    public abstract boolean equals(final @Nullable Object object);

    /**
     * Returns a string representation of this list.
     * <p>
     * The returned string follows the same format as {@link java.util.AbstractCollection#toString()}.
     */
    @Override
    public abstract @NotNull String toString();

    /**
     * Returns a new {@link Spliterator} over the elements of this list.
     * <p>
     * The returned spliterator always reports at least {@link Spliterator#ORDERED}, {@link Spliterator#SIZED},
     * {@link Spliterator#IMMUTABLE} and {@link Spliterator#SUBSIZED} characteristics.
     */
    @Override
    public abstract @NotNull Spliterator<T> spliterator();

    /**
     * Performs the given action for each element of this list.
     */
    @Override
    public abstract void forEach(final @NotNull Consumer<? super T> action);

    /**
     * Returns an immutable view of this list between the specified indices {@code from} (inclusive) and {@code to}
     * (exclusive).
     */
    @Override
    public abstract @NotNull ImmutableList<T> subList(final int from, final int to);

    /**
     * Returns {@code true} iff this list contains all of the elements of the given collection.
     * <p>
     * If the given collection is empty, {@code true} is returned.
     */
    @Override
    public final boolean containsAll(final @NotNull Collection<?> collection) {
        for (final var item : collection) {
            if (!contains(item)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns {@code} true iff this list contains an element equal to the given object.
     * <p>
     * Equality follows the semantics of {@link Objects#equals(Object, Object)}.
     */
    @Override
    public final boolean contains(final @Nullable Object object) {
        return indexOf(object) != -1;
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean add(final T element) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean remove(final @Nullable Object object) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean addAll(final @NotNull Collection<? extends T> collection) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean addAll(final int index, final @NotNull Collection<? extends T> collection) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean removeAll(final @NotNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean retainAll(final @NotNull Collection<?> collection) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final void clear() {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final T set(final int index, final T element) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final void add(final int index, final T element) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final T remove(final int index) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final void replaceAll(final @NotNull UnaryOperator<T> operator) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final void sort(final @NotNull Comparator<? super T> comparator) {
        throw unsupportedModification();
    }

    /**
     * Always throws {@link UnsupportedOperationException}, as immutable lists cannot be modified.
     */
    @Override
    public final boolean removeIf(final @NotNull Predicate<? super T> filter) {
        throw unsupportedModification();
    }

    private static @NotNull UnsupportedOperationException unsupportedModification() {
        throw new UnsupportedOperationException("Cannot modify an immutable list");
    }

    private abstract static sealed class Iter<T> implements ListIterator<T> {
        static @NotNull NoSuchElementException noMoreElements() {
            throw new NoSuchElementException("No more elements left");
        }

        @Override
        public final void remove() {
            throw unsupportedModification();
        }

        @Override
        public final void set(final T t) {
            throw unsupportedModification();
        }

        @Override
        public final void add(final T t) {
            throw unsupportedModification();
        }
    }

    private static final class EmptyHolder {
        private static final Simple<?> instance = new Simple<>(new Object[0]);
    }

    private static final class Simple<T> extends ImmutableList<T> {
        private Simple(final T @NotNull [] items) {
            this.items = items;
        }

        @Override
        public int hashCode() {
            return RangeUtils.hashCode(items, 0, items.length);
        }

        @Override
        public boolean equals(final @Nullable Object object) {
            if (!(object instanceof List<?> list)) {
                return false;
            }
            if (list instanceof Simple<?> simple) {
                return Arrays.equals(items, simple.items);
            }
            if (list instanceof Sublist<?> sublist) {
                return Arrays.equals(items, 0, items.length, sublist.items, sublist.fromIndex, sublist.toIndex());
            }
            return RangeUtils.equalsFallback(items, 0, items.length, list);
        }

        @Override
        public @NotNull String toString() {
            return RangeUtils.toString(items, 0, items.length);
        }

        @Override
        public void forEach(final @NotNull Consumer<? super T> action) {
            RangeUtils.forEach(items, 0, items.length, action);
        }

        @Override
        public int size() {
            return items.length;
        }

        @Override
        public boolean isEmpty() {
            return items.length == 0;
        }

        @Override
        public @NotNull Iterator<T> iterator() {
            return new Iter<>(items, 0);
        }

        @Override
        public Object @NotNull [] toArray() {
            return items.clone();
        }

        @Override
        @SuppressWarnings("unchecked")
        public <U> U @NotNull [] toArray(final U @NotNull [] array) {
            final var size = items.length;
            return (array.length < size)
                ? (U[]) Arrays.copyOf(items, size, array.getClass())
                : RangeUtils.copyAndNullTerminate(items, 0, array, size);
        }

        @Override
        public T get(final int index) {
            Objects.checkIndex(index, items.length);
            return items[index];
        }

        @Override
        public int indexOf(final @Nullable Object object) {
            return RangeUtils.indexOf(items, 0, items.length, object);
        }

        @Override
        public int lastIndexOf(final @Nullable Object object) {
            return RangeUtils.lastIndexOf(items, 0, items.length, object);
        }

        @Override
        public @NotNull ListIterator<T> listIterator() {
            return new Iter<>(items, 0);
        }

        @Override
        public @NotNull ListIterator<T> listIterator(final int index) {
            Objects.checkIndex(index, items.length + 1);
            return new Iter<>(items, index);
        }

        @Override
        public @NotNull ImmutableList<T> subList(final int from, final int to) {
            Objects.checkFromToIndex(from, to, items.length);
            return (from == to) ? ImmutableList.empty() : new Sublist<>(items, from, to - from);
        }

        @Override
        public @NotNull Spliterator<T> spliterator() {
            return Spliterators.spliterator(items, Spliterator.ORDERED | Spliterator.IMMUTABLE);
        }

        private final T @NotNull [] items;

        private static final class Iter<T> extends ImmutableList.Iter<T> {
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
            @SuppressFBWarnings(value = "IT_NO_SUCH_ELEMENT", justification = "It actually can, SpotBugs is confused")
            public T next() {
                final var i = index;
                if (i >= items.length) {
                    throw noMoreElements();
                }
                final var item = items[i];
                index = i + 1;
                return item;
            }

            @Override
            public T previous() {
                final var i = index - 1;
                if (i < 0) {
                    throw noMoreElements();
                }
                final var item = items[i];
                index = i;
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
                for (int i = index, size = items.length; i < size; i += 1) {
                    index = i + 1;
                    action.accept(items[i]);
                }
            }

            private final T @NotNull [] items;
            private int index;
        }
    }

    private static final class Sublist<T> extends ImmutableList<T> {
        private Sublist(final T @NotNull [] items, final int fromIndex, final int size) {
            // We assume that the array is never empty, the shared empty instance should be used instead for immutable
            // sub-lists.
            assert size > 0;
            this.items = items;
            this.fromIndex = fromIndex;
            this.size = size;
        }

        @Override
        public int hashCode() {
            return RangeUtils.hashCode(items, fromIndex, toIndex());
        }

        @Override
        public boolean equals(final @Nullable Object object) {
            if (!(object instanceof List<?> list)) {
                return false;
            }
            if (list instanceof Sublist<?> sublist) {
                return Arrays.equals(items, fromIndex, toIndex(), sublist.items, sublist.fromIndex, sublist.toIndex());
            }
            if (list instanceof Simple<?> simple) {
                return Arrays.equals(items, fromIndex, toIndex(), simple.items, 0, simple.items.length);
            }
            return RangeUtils.equalsFallback(items, fromIndex, toIndex(), list);
        }

        @Override
        public @NotNull String toString() {
            return RangeUtils.toString(items, fromIndex, toIndex());
        }

        @Override
        public @NotNull Spliterator<T> spliterator() {
            return Spliterators.spliterator(items, fromIndex, toIndex(), Spliterator.ORDERED | Spliterator.IMMUTABLE);
        }

        @Override
        public void forEach(final @NotNull Consumer<? super T> action) {
            RangeUtils.forEach(items, fromIndex, toIndex(), action);
        }

        @Override
        public @NotNull ImmutableList<T> subList(final int from, final int to) {
            Objects.checkFromToIndex(from, to, size);
            return (from == to) ? ImmutableList.empty() : new Sublist<>(items, fromIndex + from, to - from);
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public @NotNull Iterator<T> iterator() {
            return new Iter<>(items, fromIndex, fromIndex, toIndex());
        }

        @Override
        public Object @NotNull [] toArray() {
            return Arrays.copyOfRange(items, fromIndex, toIndex(), Object[].class);
        }

        @Override
        @SuppressWarnings("unchecked")
        public <U> U @NotNull [] toArray(final U @NotNull [] array) {
            return (array.length < size)
                ? (U[]) Arrays.copyOfRange(items, fromIndex, toIndex(), array.getClass())
                : RangeUtils.copyAndNullTerminate(items, fromIndex, array, size);
        }

        @Override
        public T get(final int index) {
            Objects.checkIndex(index, size);
            return items[fromIndex + index];
        }

        @Override
        public int indexOf(final @Nullable Object object) {
            return RangeUtils.indexOf(items, fromIndex, toIndex(), object);
        }

        @Override
        public int lastIndexOf(final @Nullable Object object) {
            return RangeUtils.lastIndexOf(items, fromIndex, toIndex(), object);
        }

        @NotNull
        @Override
        public ListIterator<T> listIterator() {
            return new Iter<>(items, fromIndex, fromIndex, toIndex());
        }

        @NotNull
        @Override
        public ListIterator<T> listIterator(final int index) {
            Objects.checkIndex(index, size + 1);
            return new Iter<>(items, fromIndex, fromIndex + index, toIndex());
        }

        private int toIndex() {
            return fromIndex + size;
        }

        private final T @NotNull [] items;
        private final int fromIndex;
        private final int size;

        private static final class Iter<T> extends ImmutableList.Iter<T> {
            private Iter(final T @NotNull [] items, final int from, final int index, final int to) {
                this.items = items;
                fromIndex = from;
                this.index = index;
                toIndex = to;
            }

            @Override
            public boolean hasNext() {
                return index < toIndex;
            }

            @Override
            public boolean hasPrevious() {
                return index > fromIndex;
            }

            @Override
            @SuppressFBWarnings(value = "IT_NO_SUCH_ELEMENT", justification = "It actually can, SpotBugs is confused")
            public T next() {
                final var i = index;
                if (i >= toIndex) {
                    throw noMoreElements();
                }
                final var item = items[i];
                index = i + 1;
                return item;
            }

            @Override
            public T previous() {
                final var i = index - 1;
                if (i < fromIndex) {
                    throw noMoreElements();
                }
                final var item = items[i];
                index = i;
                return item;
            }

            @Override
            public int nextIndex() {
                return index - fromIndex;
            }

            @Override
            public int previousIndex() {
                return index - fromIndex - 1;
            }

            @Override
            public void forEachRemaining(final @NotNull Consumer<? super T> action) {
                Objects.requireNonNull(action);
                for (int i = index; i < toIndex; i += 1) {
                    index = i + 1;
                    action.accept(items[i]);
                }
            }

            private final T @NotNull [] items;
            private final int fromIndex;
            private int index;
            private final int toIndex;
        }
    }

    private static final class RangeUtils {
        private static <T> int hashCode(final T @NotNull [] items, final int from, final int to) {
            int result = 1;
            for (int i = from; i < to; i += 1) {
                result = 31 * result + Objects.hashCode(items[i]);
            }
            return result;
        }

        private static <T> boolean equalsFallback(
            final T @NotNull [] items,
            final int from,
            final int to,
            final @NotNull List<?> list
        ) {
            final var size = to - from;
            if (size != list.size()) {
                return false;
            }
            // The other object is some other list type of the same size, need to iterate it.
            final var iterator = list.iterator();
            for (int index = from; iterator.hasNext(); index += 1) {
                final var thisItem = items[index];
                final var otherItem = iterator.next();
                if (!Objects.equals(thisItem, otherItem)) {
                    return false;
                }
            }
            return true;
        }

        private static <T> @NotNull String toString(final T @NotNull [] items, final int from, final int to) {
            if (from >= to) {
                return "[]";
            }

            final var builder = new StringBuilder();
            builder.append('[');
            for (int i = from; i < to; i += 1) {
                builder.append(items[i]).append(", ");
            }
            // Need to remove the final comma and space.
            builder.setLength(builder.length() - 2);
            builder.append(']');
            return builder.toString();
        }

        private static <T> void forEach(
            final T @NotNull [] items,
            final int from,
            final int to,
            final @NotNull Consumer<? super T> action
        ) {
            Objects.requireNonNull(action);
            for (int i = from; i < to; i += 1) {
                action.accept(items[i]);
            }
        }

        private static <T> int indexOf(
            final T @NotNull [] items,
            final int from,
            final int to,
            final @Nullable Object object
        ) {
            if (object == null) {
                for (int i = from; i < to; i += 1) {
                    if (items[i] == null) {
                        return i - from;
                    }
                }
            } else {
                for (int i = from; i < to; i += 1) {
                    if (object.equals(items[i])) {
                        return i - from;
                    }
                }
            }
            return -1;
        }

        private static <T> int lastIndexOf(
            final T @NotNull [] items,
            final int from,
            final int to,
            final @Nullable Object object
        ) {
            if (object == null) {
                for (int i = to - 1; i >= from; i -= 1) {
                    if (items[i] == null) {
                        return i - from;
                    }
                }
            } else {
                for (int i = to - 1; i >= from; i -= 1) {
                    if (object.equals(items[i])) {
                        return i - from;
                    }
                }
            }
            return -1;
        }

        @SuppressWarnings("SuspiciousSystemArraycopy")
        private static <T, U> U @NotNull [] copyAndNullTerminate(
            final T @NotNull [] source,
            final int sourceIndex,
            final U @NotNull [] destination,
            final int size
        ) {
            System.arraycopy(source, sourceIndex, destination, 0, size);
            if (destination.length > size) {
                destination[size] = null;
            }
            return destination;
        }
    }
}
