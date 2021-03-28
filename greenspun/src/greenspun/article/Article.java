// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.article;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;

/**
 * A parsed article.
 *
 * @param title                  The title of the article.
 * @param description            A short description of the article.
 * @param date                   The publication date of the article.
 * @param inhibitTableOfContents If {@code true}, the article will have no table of contents.
 * @param topics                 The list of topics this article belongs to.
 * @param rootSection            The root section of the article.
 */
@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
public record Article(
    @NotNull String title,
    @NotNull String description,
    @NotNull LocalDate date,
    boolean inhibitTableOfContents,
    @NotNull List<@NotNull String> topics,
    @NotNull Section rootSection
) {
    public @NotNull List<@NotNull String> topics() {
        return Collections.unmodifiableList(topics);
    }
}
