// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import greenspun.article.Article;
import greenspun.util.annotation.Nullable;

record ArticleToRender(
    Article article,
    @Nullable DomainRelativeUri predecessorUri,
    @Nullable DomainRelativeUri successorUri
) {
}
