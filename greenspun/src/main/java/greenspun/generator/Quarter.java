// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import java.time.LocalDate;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("ComparableImplementedButEqualsNotOverridden")
record Quarter(int year, byte quarter) implements Comparable<Quarter> {
    Quarter {
        assert quarter >= 1 && quarter <= 4 : "Quarter out of range";
    }

    static @NotNull Quarter fromDate(final @NotNull LocalDate date) {
        return new Quarter(date.getYear(), (byte) ((date.getMonthValue() - 1) / 3 + 1));
    }

    @Override
    public int compareTo(final @NotNull Quarter other) {
        final var yearComparison = Integer.compare(year, other.year);
        return (yearComparison == 0) ? Byte.compare(quarter, other.quarter) : yearComparison;
    }

    @Override
    public @NotNull String toString() {
        return names[quarter - 1] + " quarter of " + year;
    }

    private static final String[] names = {"first", "second", "third", "fourth"};
}
