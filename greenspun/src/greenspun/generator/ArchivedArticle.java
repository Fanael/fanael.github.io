// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.article.Article;
import org.jetbrains.annotations.NotNull;

@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
record ArchivedArticle(@NotNull Article article, @NotNull String identifier, @NotNull String url) {
}
