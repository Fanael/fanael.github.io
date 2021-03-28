// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.article;

import java.util.Collections;
import java.util.List;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.dom.Node;
import greenspun.sexp.Sexp;
import org.jetbrains.annotations.NotNull;

/**
 * A section of a parsed article.
 *
 * @param identifier The article-unique identifier of the section.
 * @param header     The text used for the section heading.
 * @param children   The list of subsections this section contains.
 * @param body       A list of DOM tree nodes representing the body text of the section.
 */
@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
public record Section(
    @NotNull Sexp.Symbol identifier,
    @NotNull String header,
    @NotNull List<@NotNull Section> children,
    @NotNull List<@NotNull Node> body
) {
    public @NotNull List<@NotNull Section> children() {
        return Collections.unmodifiableList(children);
    }

    public @NotNull List<@NotNull Node> body() {
        return Collections.unmodifiableList(body);
    }
}
