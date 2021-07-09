// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import greenspun.dom.Node;
import greenspun.sexp.Sexp;
import greenspun.util.collection.ImmutableList;
import org.jetbrains.annotations.NotNull;

/**
 * A section of a parsed article.
 *
 * @param identifier The article-unique identifier of the section.
 * @param header     The text used for the section heading.
 * @param children   The list of subsections this section contains.
 * @param body       A list of DOM tree nodes representing the body text of the section.
 */
public record Section(
    @NotNull Sexp.Symbol identifier,
    @NotNull String header,
    @NotNull ImmutableList<@NotNull Section> children,
    @NotNull ImmutableList<@NotNull Node> body
) {
}
