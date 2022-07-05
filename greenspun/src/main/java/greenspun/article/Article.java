// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import java.time.LocalDate;
import greenspun.util.collection.seq.Seq;

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
public record Article(
    String title,
    String description,
    LocalDate date,
    boolean inhibitTableOfContents,
    Seq<String> topics,
    Section rootSection
) {
}
