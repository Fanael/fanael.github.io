// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import java.net.URI;
import greenspun.article.Article;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

record ArticleToRender(@NotNull Article article, @Nullable URI predecessorUri, @Nullable URI successorUri) {
}
