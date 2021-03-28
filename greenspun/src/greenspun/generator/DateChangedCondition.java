// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import java.time.LocalDate;
import greenspun.util.condition.Condition;
import org.jetbrains.annotations.NotNull;

/**
 * A condition type representing the fact that the publication date of an article has change during reloading.
 * <p>
 * The article date <em>must</em> stay the same between reloads, as changes in the date may cause changes in
 * the chronological order of articles, which is not allowed without a full rebuild.
 */
public final class DateChangedCondition extends Condition {
    DateChangedCondition(final @NotNull LocalDate previous, final @NotNull LocalDate current) {
        super("The date of an article has changed");
        this.previous = previous;
        this.current = current;
    }

    @Override
    public @NotNull String detailedMessage() {
        return message() + "\nPrevious date: " + previous + "\nNew date: " + current;
    }

    private final @NotNull LocalDate previous;
    private final @NotNull LocalDate current;
}
