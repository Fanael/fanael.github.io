// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.time.LocalDate;

record Quarter(int year, byte quarter) implements Comparable<Quarter> {
    Quarter {
        assert quarter >= 1 && quarter <= 4 : "Quarter out of range";
    }

    static Quarter fromDate(final LocalDate date) {
        return new Quarter(date.getYear(), (byte) ((date.getMonthValue() - 1) / 3 + 1));
    }

    @Override
    public int compareTo(final Quarter other) {
        final var yearComparison = Integer.compare(year, other.year);
        return (yearComparison == 0) ? Byte.compare(quarter, other.quarter) : yearComparison;
    }

    @Override
    public String toString() {
        return names[quarter - 1] + " quarter of " + year;
    }

    private static final String[] names = {"first", "second", "third", "fourth"};
}
