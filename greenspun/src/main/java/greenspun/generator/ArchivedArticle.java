// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import greenspun.article.Article;

record ArchivedArticle(Article article, String identifier, DomainRelativeUri uri) {
}
