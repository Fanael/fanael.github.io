// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Objects;
import java.util.function.IntConsumer;
import java.util.stream.LongStream;
import greenspun.util.collection.seq.Seq;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

final class SeqTest {
    static LongStream provideSeeds() {
        return LongStream.generate(RandomUtils::generateRandomSeed).limit(8);
    }

    @Test
    void toStringWorksSimple() {
        assertThat(Seq.empty()).asString().isEqualTo("[]");
        assertThat(Seq.of(new IntPair(0, 5))).asString().isEqualTo("[(0, 5)]");
        assertThat(Seq.of(0, 1, 2, 3)).asString().isEqualTo("[0, 1, 2, 3]");
        assertThat(Seq.of("abc", "def", "")).asString().isEqualTo("[abc, def, ]");
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void toStringWorks(final int size) {
        final var list = new ArrayList<Integer>();
        generateInts(size, list::add);
        final var seq = generateInts(size);
        assertThat(seq).asString().isEqualTo(list.toString());
    }

    @Test
    void equalsWorks() {
        assertThat(Seq.empty()).isEqualTo(Seq.empty());
        assertThat(Seq.empty()).isNotEqualTo(Seq.of(5));
        assertThat(Seq.of(5)).isNotSameAs(Seq.of(5));
        assertThat(Seq.of(5)).isEqualTo(Seq.of(5));
        assertThat(Seq.of(5, 10)).isNotEqualTo(Seq.of(10, 5));
        assertThat(generateInts(1_000)).isEqualTo(generateInts(1_000));
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    @SuppressWarnings("UseBulkOperation")
    void iterationWorks(final int size) {
        final var seq = generateInts(size);
        final var expected = new ArrayList<Integer>(size);
        generateInts(size, expected::add);
        final var actual = new ArrayList<Integer>(size);
        for (final var element : seq) {
            actual.add(element);
        }
        assertThat(actual).isEqualTo(expected);
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void iteratorIndexIsCorrect(final int size) {
        final var seq = generateInts(size);
        final var iterator = seq.iterator();
        for (int i = -1; iterator.hasNext(); i += 1) {
            assertThat(iterator.previousIndex()).isEqualTo(i);
            assertThat(iterator.nextIndex()).isEqualTo(i + 1);
            iterator.next();
        }
        assertThat(iterator.previousIndex()).isEqualTo(size - 1);
        assertThat(iterator.nextIndex()).isEqualTo(size);
    }

    @Tag("quadratic")
    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {10, 100, 1_000, 10_000})
    void iteratorRestWorks(final int size) {
        final var seq = generateInts(size);
        final var iterator = seq.iterator();
        var expectedRest = seq;
        assertThat(iterator.rest()).isEqualTo(expectedRest);
        while (iterator.hasNext()) {
            iterator.next();
            expectedRest = expectedRest.withoutFirst();
            assertThat(iterator.rest()).isEqualTo(expectedRest);
        }
        final var actualRest = iterator.rest();
        assertThat(actualRest).isEmpty();
        assertThat(actualRest).isEqualTo(Seq.empty());
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    @SuppressWarnings("UseBulkOperation")
    void forEachWorks(final int size) {
        final var seq = generateInts(size);
        final var expected = new ArrayList<Integer>(size);
        generateInts(size, expected::add);
        final var actual = new ArrayList<Integer>(size);
        seq.forEach(actual::add);
        assertThat(actual).isEqualTo(expected);
    }

    @Tag("quadratic")
    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {5, 50, 500, 5_000})
    void containsWorks(final int size) {
        final var seq = generateInts(size);
        assertThat(seq).doesNotContain(-1);
        assertThat(seq).doesNotContain(size);
        for (int i = 0; i < size; i += 1) {
            assertThat(seq).contains(i);
        }
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void toArrayWorks(final int size) {
        final var seq = generateInts(size);
        final var list = new ArrayList<Integer>(size);
        generateInts(size, list::add);
        final var actual = seq.toArray(Integer[]::new);
        assertThat(actual).isExactlyInstanceOf(Integer[].class);
        assertThat(actual).hasSize(size);
        // NB: wrap the array in a list, because the iterable version of containsExactlyElementsOf is linear when
        // the elements match, the array version is quadratic.
        assertThat(Arrays.asList(actual)).containsExactlyElementsOf(list);
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void anySatisfiesWorks(final int size) {
        final var seq = generateInts(size);
        assertThat(seq.anySatisfies(x -> x == size - 1)).isTrue();
        assertThat(seq.anySatisfies(x -> x % 5 == 3)).isTrue();
        assertThat(seq.anySatisfies(Objects::isNull)).isFalse();
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void canBuildWithPrepends(final int size) {
        var seq = Seq.<Integer>empty();
        for (int i = size - 1; i >= 0; i -= 1) {
            seq = seq.prepended(i);
        }
        assertThat(seq).isEqualTo(generateInts(size));
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void canBuildWithAppends(final int size) {
        var seq = Seq.<Integer>empty();
        for (int i = 0; i < size; i += 1) {
            seq = seq.appended(i);
        }
        assertThat(seq).isEqualTo(generateInts(size));
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    @SuppressWarnings("CollectionAddedToSelf")
    void canBuildAtBothSides(final int size) {
        var seq = Seq.<Integer>empty();
        for (int i = 0; i < size; i += 1) {
            seq = seq.appended(i).prepended(size - i - 1);
        }
        final var expectedHalf = generateInts(size);
        assertThat(seq).isEqualTo(expectedHalf.concat(expectedHalf));
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void canDeconstructFromFront(final int size) {
        final var original = generateInts(size);
        var seq = original;
        for (int i = 0; !seq.isEmpty(); i += 1) {
            assertThat(seq.first()).isEqualTo(i);
            seq = seq.updatedFirst(0);
            assertThat(seq.first()).isEqualTo(0);
            seq = seq.withoutFirst();
        }
        // Did anything affect the original in any way?
        assertThat(original).isEqualTo(generateInts(size));
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void canDeconstructFromBack(final int size) {
        final var original = generateInts(size);
        var seq = original;
        for (int i = size - 1; !seq.isEmpty(); i -= 1) {
            assertThat(seq.last()).isEqualTo(i);
            seq = seq.updatedLast(0);
            assertThat(seq.last()).isEqualTo(0);
            seq = seq.withoutLast();
        }
        // Did anything affect the original in any way?
        assertThat(original).isEqualTo(generateInts(size));
    }

    @Tag("quadratic")
    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {5, 50, 500, 5_000})
    void concatWorks(final int size) {
        final var expected = generateInts(size);
        for (int i = 0; i <= size; i += 1) {
            final var first = generateInts(0, i);
            final var second = generateInts(i, size);
            assertThat(first.concat(second)).isEqualTo(expected);
        }
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {3, 250, 1_500})
    void exerciseConcatBalancingCases(final int baseSize) {
        final var base = generateInts(baseSize);
        final int limit = 128;
        for (int addedToFront = 0; addedToFront < limit; addedToFront += 1) {
            var front = base;
            for (int i = 0; i < addedToFront; i += 1) {
                front = front.appended(i);
            }
            for (int addedToBack = 0; addedToBack < limit; addedToBack += 1) {
                var back = base;
                for (int i = 0; i < addedToBack; i += 1) {
                    back = back.prepended(addedToBack);
                }
                assertThat(front.concat(back)).hasSize(2 * baseSize + addedToFront + addedToBack);
            }
        }
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void getWorks(final int size) {
        final var seq = generateInts(size);
        for (int i = 0; i < size; i += 1) {
            assertThat(seq.get(i)).isEqualTo(i);
        }
        assertThatExceptionOfType(IndexOutOfBoundsException.class).isThrownBy(() -> seq.get(-1));
        assertThatExceptionOfType(IndexOutOfBoundsException.class).isThrownBy(() -> seq.get(size));
    }

    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {25, 250, 2_500, 25_000, 250_000})
    void updatedWorks(final int size) {
        final var original = generatePairs(size);
        final var expected = original.map(pair -> new IntPair(pair.first, 1));
        var seq = original;
        for (int i = 0; i < size; i += 1) {
            final var pair = new IntPair(i, 1);
            final var newSeq = seq.updated(i, pair);
            assertThat(newSeq).isNotSameAs(seq);
            seq = newSeq;
            // Test if updates don't accidentally cause in-place mutations…
            assertThat(original.updated(i, pair)).isNotSameAs(original);
        }
        // …and if they occasionally do, this will likely trip.
        assertThat(original.map(pair -> new IntPair(pair.first, pair.second + 1))).isEqualTo(expected);
        assertThat(seq).isEqualTo(expected);
    }

    @Tag("quadratic")
    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {5, 50, 500, 5_000})
    void splitAtWorks(final int size) {
        final var original = generateInts(size);
        var front = Seq.<Integer>empty();
        var back = original;
        for (int i = 0; i < size; i += 1) {
            final var split = original.splitAt(i);
            assertThat(split.front()).isEqualTo(front);
            assertThat(split.back()).isEqualTo(back);
            front = front.appended(back.first());
            back = back.withoutFirst();
        }
    }

    @Tag("quadratic")
    @ParameterizedTest(name = sizedTestDisplayName)
    @ValueSource(ints = {5, 50, 500, 5_000})
    void concatSplitAtIsIdentity(final int size) {
        final var seq = generateInts(size);
        for (int i = 0; i <= size; i += 1) {
            final var split = seq.splitAt(i);
            final var joined = split.front().concat(split.back());
            assertThat(joined).hasSize(size);
            assertThat(joined).isEqualTo(seq);
        }
    }

    @ParameterizedTest(name = seededTestDisplayName)
    @MethodSource("provideSeeds")
    void sortingIsStable(final long seed) {
        final var original = generateRandomPairs(seed);
        final var sorted = original.sorted(Comparator.comparingInt(IntPair::first));
        assertThat(sorted.exactSize()).isEqualTo(original.exactSize());
        assertThat(sorted.toArray(IntPair[]::new))
            .isSortedAccordingTo(Comparator.comparingInt(IntPair::first).thenComparingInt(IntPair::second));
    }

    private static Seq<Integer> generateInts(final int size) {
        return generateInts(0, size);
    }

    private static Seq<Integer> generateInts(final int from, final int to) {
        final var builder = new Seq.Builder<Integer>();
        generateInts(from, to, builder::append);
        return checkSize(builder.toSeq(), Integer.max(0, to - from));
    }

    private static void generateInts(final int size, final IntConsumer action) {
        generateInts(0, size, action);
    }

    private static void generateInts(final int from, final int to, final IntConsumer action) {
        for (int i = from; i < to; i += 1) {
            action.accept(i);
        }
    }

    private static Seq<IntPair> generatePairs(final int size) {
        final var builder = new Seq.Builder<IntPair>();
        for (int i = 0; i < size; i += 1) {
            builder.append(new IntPair(i, 0));
        }
        return checkSize(builder.toSeq(), size);
    }

    private static Seq<IntPair> generateRandomPairs(final long seed) {
        final int size = 15_000;
        final var builder = new Seq.Builder<IntPair>();
        final var generator = RandomUtils.createGenerator(seed);
        for (int i = 0; i < size; i += 1) {
            builder.append(new IntPair(generator.nextInt(32), i));
        }
        return checkSize(builder.toSeq(), size);
    }

    private static <T> Seq<T> checkSize(final Seq<T> seq, final int expectedSize) {
        assertThat(seq).hasSize(expectedSize);
        return seq;
    }

    private static final String seededTestDisplayName = "{displayName} [{index}] seed = {0}";
    private static final String sizedTestDisplayName = "{displayName} size = {0}";

    private record IntPair(int first, int second) {
        @Override
        public String toString() {
            return "(" + first + ", " + second + ")";
        }
    }
}
