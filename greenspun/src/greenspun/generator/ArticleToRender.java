// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.article.Article;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
record ArticleToRender(
    @NotNull Article article,
    @Nullable String predecessorUrl,
    @Nullable String successorUrl
) {
}
