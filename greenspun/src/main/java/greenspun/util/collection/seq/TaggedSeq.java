// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.Objects;
import java.util.function.Function;

abstract sealed class TaggedSeq<T, Phantom> extends Seq<T> permits Shallow, Deep {
    TaggedSeq(final Tag<T, Phantom> tag, final long subtreeSize) {
        super(subtreeSize);
        this.tag = tag;
    }

    @Override
    public abstract <U> TaggedSeq<U, Phantom> map(Function<? super T, ? extends U> function);

    @Override
    public abstract TaggedSeq<T, Phantom> updatedFirst(T object);

    @Override
    public abstract TaggedSeq<T, Phantom> updatedLast(T object);

    @Override
    public abstract TaggedSeq<T, Phantom> prepended(T object);

    @Override
    public abstract TaggedSeq<T, Phantom> appended(T object);

    @Override
    public abstract TaggedSeq<T, Phantom> withoutFirst();

    @Override
    public abstract TaggedSeq<T, Phantom> withoutLast();

    @Override
    @SuppressWarnings("unchecked")
    public final TaggedSeq<T, Phantom> concat(final Seq<? extends T> other) {
        final var otherTagged = (TaggedSeq<? extends T, ?>) other;
        assert tag == otherTagged.tag;
        return concatImpl((TaggedSeq<T, Phantom>) otherTagged);
    }

    @Override
    public final Seq<T> updated(final long index, final T newValue) {
        // Check the index once here, let children assume it's always valid.
        return updatedImpl(Objects.checkIndex(index, subtreeSize), 0, (accumulator, oldValue) -> newValue);
    }

    @Override
    final Split<T> splitAtImpl(final long index) {
        if (index >= exactSize()) {
            return new Split<>(this, empty());
        }
        final var split = splitTree(index, 0);
        return new Split<>(split.front, split.back.prepended(split.middle));
    }

    abstract TaggedSeq<T, Phantom> updatedImpl(long index, long accumulator, Tag.Updater<T> updater);

    abstract TaggedSeq<T, Phantom> concatImpl(TaggedSeq<T, Phantom> other);

    abstract TreeSplit<T, Phantom> splitTree(long index, long accumulator);

    final long addToSize(final T object) {
        return computeNewSize(tag.measureSingle(object));
    }

    final long subtractFromSize(final T object) {
        return computeNewSize(-tag.measureSingle(object));
    }

    final long updateSize(final T newObject, final T oldObject) {
        return computeNewSize(tag.measureSingle(newObject) - tag.measureSingle(oldObject));
    }

    final Tag<T, Phantom> tag;

    record TreeSplit<T, Phantom>(TaggedSeq<T, Phantom> front, T middle, TaggedSeq<T, Phantom> back) {
    }
}
