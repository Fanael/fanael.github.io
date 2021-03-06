// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import java.util.Map;
import greenspun.dom.Node;
import greenspun.dom.Tag;
import greenspun.generator.Renderer;
import greenspun.pygments.PygmentsCache;
import greenspun.sexp.Sexp;
import greenspun.sexp.Sexps;
import greenspun.util.Trace;
import greenspun.util.collection.ImmutableList;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;
import greenspun.util.condition.Unwind;
import org.jetbrains.annotations.NotNull;

/**
 * The HTSL converter: the primary means of converting HTSL forms into DOM tree nodes.
 * <p>
 * <dfn>HTSL</dfn>, <dfn>Hypertext S-expression Language</dfn> is the HTML-as-sexps representation used in article
 * source files.
 */
public final class HtslConverter {
    /**
     * Initializes a new HTSL converter that will use the given Pygments cache for highlighting code snippets.
     */
    public HtslConverter(final @NotNull PygmentsCache pygmentsCache) {
        this.pygmentsCache = pygmentsCache;
    }

    /**
     * Converts the given list of S-expressions into a list of DOM tree nodes.
     * <p>
     * On error, a fatal condition is signaled:
     * <ul>
     * <li>{@link HtslConversionErrorCondition} is signaled if an S-expression could not be converted into a DOM tree
     * node.
     * <li>Any condition type that {@link PygmentsCache#highlightCode(String, String, String)} can signal, if an error
     * occurs in syntax highlighting during the macro-expansion of {@code highlighted-code} tag macro.
     * </ul>
     */
    public @NotNull ImmutableList<@NotNull Node> convert(
        final @NotNull ImmutableList<@NotNull Sexp> forms
    ) throws Unwind {
        return ImmutableList.map(forms, this::convertForm);
    }

    private @NotNull Node convertForm(final @NotNull Sexp form) throws Unwind {
        if (form instanceof Sexp.String string) {
            return new Node.Text(string.value());
        }
        final var list = Sexps.asList(form);
        if (list == null || list.size() < 1) {
            throw signalError("This doesn't appear to be a valid HTSL element: " + Sexps.prettyPrint(form));
        }
        final var tagHead = extractTagHead(list.get(0));
        final var tagName = tagHead.tagName;
        try (final var trace = new Trace(() -> "Converting HTSL element " + tagName + " into a DOM node")) {
            trace.use();
            final var children = list.subList(1, list.size());
            final var tagMacro = tagMacros.get(tagName);
            if (tagMacro != null) {
                return tagMacro.expand(this, tagHead.attributes, children);
            }
            return convertElement(tagName, tagHead.attributes, children);
        }
    }

    private @NotNull Node.Element convertElement(
        final @NotNull Sexp.Symbol tagName,
        final @NotNull ImmutableList<Sexp> attributes,
        final @NotNull ImmutableList<Sexp> children
    ) throws Unwind {
        final var tag = Tag.byHtmlName(tagName.symbolName());
        if (tag == null) {
            throw signalError("Unknown tag name " + tagName);
        }
        return Node.build(tag, builder -> {
            convertAttributes(builder, attributes);
            for (final var childForm : children) {
                builder.append(convertForm(childForm));
            }
        });
    }

    private static void convertAttributes(
        final @NotNull Node.ElementBuilder builder,
        final @NotNull ImmutableList<Sexp> attributes
    ) throws Unwind {
        for (int i = 0, size = attributes.size(); i < size; i += 2) {
            final var keyForm = attributes.get(i);
            final var key = Sexps.asKeyword(keyForm);
            if (key == null) {
                throw signalError("Attribute name doesn't appear to be a Lisp keyword: " + Sexps.prettyPrint(keyForm));
            }
            final var attributeName = key.symbolName().substring(1);
            final var value = (i + 1 < size) ? attributes.get(i + 1) : Sexp.KnownSymbol.NIL;
            if (value instanceof Sexp.String string) {
                builder.set(attributeName, string.value());
            } else if (value instanceof Sexp.Integer integer) {
                builder.set(attributeName, integer.value());
            } else if (Sexps.isNil(value)) {
                builder.set(attributeName, false);
            } else if (value == Sexp.KnownSymbol.T) {
                builder.set(attributeName, true);
            } else {
                throw signalError("Don't know what to do with this attribute value: " + Sexps.prettyPrint(value));
            }
        }
    }

    private static @NotNull TagHead extractTagHead(final @NotNull Sexp form) throws Unwind {
        final var symbol = Sexps.asSymbol(form);
        if (symbol != null) {
            return new TagHead(symbol, ImmutableList.empty());
        }
        final var list = Sexps.asList(form);
        if (list == null || list.isEmpty()) {
            throw signalError("This doesn't appear to be a valid HTSL tag head: " + Sexps.prettyPrint(form));
        }
        final var tagNameForm = list.get(0);
        final var tagName = Sexps.asSymbol(tagNameForm);
        if (tagName == null) {
            throw signalError("This doesn't appear to be a valid HTSL tag name: " + Sexps.prettyPrint(tagNameForm));
        }
        return new TagHead(tagName, list.subList(1, list.size()));
    }

    private @NotNull Node expandCodeBlock(
        final @NotNull ImmutableList<Sexp> attributes,
        final @NotNull ImmutableList<Sexp> children
    ) throws Unwind {
        if (attributes.size() != 2 || attributes.get(0) != Sexp.KnownSymbol.KW_LANGUAGE) {
            throw signalError("code-block accepts exactly one attribute, :language");
        }
        final var languageNameForm = attributes.get(1);
        if (!(languageNameForm instanceof Sexp.String languageName)) {
            throw signalError("This doesn't appear to be a string: " + Sexps.prettyPrint(languageNameForm));
        }
        return Renderer.wrapCodeBlock(ImmutableList.map(children, this::convertForm), languageName.value());
    }

    private @NotNull Node expandHighlightedCode(
        final @NotNull ImmutableList<Sexp> attributes,
        final @NotNull ImmutableList<Sexp> children
    ) throws Unwind {
        if (children.size() != 1 || !(children.get(0) instanceof Sexp.String codeNode)) {
            throw signalError("highlighted-code accepts exactly one string child");
        }
        if (attributes.size() != 2 || attributes.get(0) != Sexp.KnownSymbol.KW_LANGUAGE) {
            throw signalError("highlighted-code accepts exactly one attribute, :language");
        }
        final var languageTagForm = attributes.get(1);
        final var languageTag = Sexps.asKeyword(languageTagForm);
        if (languageTag == null) {
            throw signalError("This doesn't appear to be a language name: " + Sexps.prettyPrint(languageTagForm));
        }
        final var prettyLanguageName = syntaxHighlightingLanguages.get(languageTag);
        if (prettyLanguageName == null) {
            throw signalError("Unknown language for syntax highlighting: " + languageTag);
        }
        final var code = codeNode.value();
        final var pygmentsLanguageName = languageTag.symbolName().substring(1);
        return pygmentsCache.highlightCode(code, pygmentsLanguageName, prettyLanguageName);
    }

    private @NotNull Node expandImageFigure(
        final @NotNull ImmutableList<Sexp> attributes,
        final @NotNull ImmutableList<Sexp> children
    ) throws Unwind {
        return Node.build(Tag.FIGURE, figure -> {
            figure.appendBuild(Tag.FIGCAPTION, figcaption -> {
                for (final var child : children) {
                    figcaption.append(convertForm(child));
                }
            });
            figure.appendBuild(Tag.DIV, div -> {
                div.set("class", "holder");
                div.appendBuild(Tag.IMG, img -> convertAttributes(img, attributes));
            });
        });
    }

    private static @NotNull UnhandledErrorError signalError(final @NotNull String message) throws Unwind {
        return ConditionContext.error(new HtslConversionErrorCondition(message));
    }

    private static final Map<Sexp.Symbol, TagMacroExpander> tagMacros = Map.of(
        Sexp.KnownSymbol.CODE_BLOCK, HtslConverter::expandCodeBlock,
        Sexp.KnownSymbol.HIGHLIGHTED_CODE, HtslConverter::expandHighlightedCode,
        Sexp.KnownSymbol.IMAGE_FIGURE, HtslConverter::expandImageFigure
    );
    private static final Map<Sexp.Symbol, String> syntaxHighlightingLanguages = Map.of(
        Sexp.KnownSymbol.KW_CPLUSPLUS, "C++",
        Sexp.KnownSymbol.KW_COMMON_LISP, "Common Lisp",
        Sexp.KnownSymbol.KW_DIFF, "Unified diff",
        Sexp.KnownSymbol.KW_JAVA, "Java"
    );

    private final @NotNull PygmentsCache pygmentsCache;

    @FunctionalInterface
    private interface TagMacroExpander {
        @NotNull Node expand(
            @NotNull HtslConverter converter,
            @NotNull ImmutableList<Sexp> attributes,
            @NotNull ImmutableList<Sexp> children
        ) throws Unwind;
    }

    private record TagHead(@NotNull Sexp.Symbol tagName, @NotNull ImmutableList<Sexp> attributes) {
    }
}
