// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.collection.seq;

import java.util.function.Function;
import greenspun.util.UnreachableCodeReachedError;
import org.jetbrains.annotations.NotNull;

abstract sealed class TaggedSeq<T, Phantom> extends Seq<T> permits Empty, Single, Deep {
    TaggedSeq(final @NotNull TypeTag<T, Phantom> tag, final long exactSize) {
        super(exactSize);
        this.tag = tag;
    }

    static @NotNull UnsupportedOperationException unsupportedModification() {
        throw new UnsupportedOperationException("Sequences don't support in-place mutation");
    }

    static @NotNull UnreachableCodeReachedError fellThroughTryingToIndex() {
        throw new UnreachableCodeReachedError("Fell through a sequence trying to index it");
    }

    @Override
    public abstract <U> @NotNull TaggedSeq<U, Phantom> map(@NotNull Function<? super T, ? extends U> function);

    @Override
    public abstract @NotNull TaggedSeq<T, Phantom> updatedFirst(T object);

    @Override
    public abstract @NotNull TaggedSeq<T, Phantom> updatedLast(T object);

    @Override
    public abstract @NotNull TaggedSeq<T, Phantom> prepended(T object);

    @Override
    public abstract @NotNull TaggedSeq<T, Phantom> appended(T object);

    @Override
    public abstract @NotNull TaggedSeq<T, Phantom> withoutFirst();

    @Override
    public abstract @NotNull TaggedSeq<T, Phantom> withoutLast();

    @Override
    @SuppressWarnings("unchecked")
    public final @NotNull TaggedSeq<T, Phantom> concat(final @NotNull Seq<? extends T> other) {
        final var otherTagged = (TaggedSeq<? extends T, ?>) other;
        assert tag == otherTagged.tag;
        return concatImpl((TaggedSeq<T, Phantom>) otherTagged);
    }

    @Override
    final @NotNull Split<T> splitAtImpl(final long index) {
        if (index >= exactSize()) {
            return new Split<>(this, empty());
        }
        final var split = splitTree(index, 0);
        return new Split<>(split.left, split.right.prepended(split.middle));
    }

    abstract @NotNull TaggedSeq<T, Phantom> concatImpl(@NotNull TaggedSeq<T, Phantom> other);

    abstract @NotNull TreeSplit<T, Phantom> splitTree(final long index, final long accumulator);

    final @NotNull TypeTag<T, Phantom> tag() {
        return tag;
    }

    final long computeNewSize(final long delta) {
        try {
            return Math.addExact(exactSize(), delta);
        } catch (final ArithmeticException e) {
            throw new IllegalStateException("Sequence size too large");
        }
    }

    final long addToSize(final T object) {
        return computeNewSize(tag().measureSingle(object));
    }

    final long subtractFromSize(final T object) {
        return computeNewSize(-tag().measureSingle(object));
    }

    static final int maxAffixLength = 32;

    private final @NotNull TypeTag<T, Phantom> tag;

    record TreeSplit<T, Phantom>(@NotNull TaggedSeq<T, Phantom> left, T middle, @NotNull TaggedSeq<T, Phantom> right) {
    }
}
