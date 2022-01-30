// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import greenspun.article.Article;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

record ArticleToRender(
    @NotNull Article article,
    @Nullable DomainRelativeUri predecessorUri,
    @Nullable DomainRelativeUri successorUri
) {
}
