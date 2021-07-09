// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import greenspun.article.Article;
import org.jetbrains.annotations.NotNull;

record ArchivedArticle(@NotNull Article article, @NotNull String identifier, @NotNull DomainRelativeUri uri) {
}
