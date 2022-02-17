// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import java.util.Map;
import greenspun.dom.Attribute;
import greenspun.dom.Attributes;
import greenspun.dom.Node;
import greenspun.dom.Tag;
import greenspun.generator.Renderer;
import greenspun.pygments.PygmentsCache;
import greenspun.sexp.Sexp;
import greenspun.sexp.Sexps;
import greenspun.util.Trace;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;
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
    public @NotNull Seq<@NotNull Node> convert(final @NotNull Seq<@NotNull Sexp> forms) {
        return forms.map(this::convertForm);
    }

    private @NotNull Node convertForm(final @NotNull Sexp form) {
        if (form instanceof Sexp.String string) {
            return new Node.Text(string.value());
        }
        final var list = Sexps.asList(form);
        if (list == null || list.isEmpty()) {
            throw signalError("This doesn't appear to be a valid HTSL element: " + Sexps.prettyPrint(form));
        }
        final var tagHead = extractTagHead(list.first());
        final var tagName = tagHead.tagName;
        try (final var trace = new Trace(() -> "Converting HTSL element " + tagName + " into a DOM node")) {
            trace.use();
            final var children = list.withoutFirst();
            final var tagMacro = tagMacros.get(tagName);
            if (tagMacro != null) {
                return tagMacro.expand(this, tagHead.attributes, children);
            }
            return convertElement(tagName, tagHead.attributes, children);
        }
    }

    private @NotNull Node.Element convertElement(
        final @NotNull Sexp.Symbol tagName,
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        final var tag = Tag.byHtmlName(tagName.symbolName());
        if (tag == null) {
            throw signalError("Unknown tag name " + tagName);
        }
        final var convertedAttributes = convertAttributes(attributes);
        final var convertedChildren = children.map(this::convertForm);
        return new Node.Element(tag, convertedAttributes, convertedChildren);
    }

    private static @NotNull Seq<Attribute> convertAttributes(final @NotNull Seq<Sexp> attributes) {
        @NotNull Seq<Attribute> result = Seq.empty();
        for (final var it = attributes.iterator(); it.hasNext(); ) {
            final var keyForm = it.next();
            final var key = Sexps.asKeyword(keyForm);
            if (key == null) {
                throw signalError("Attribute name doesn't appear to be a Lisp keyword: " + Sexps.prettyPrint(keyForm));
            }
            final var name = key.symbolName().substring(1);
            final var value = it.hasNext() ? it.next() : Sexp.KnownSymbol.NIL;
            switch (value) {
                case Sexp.String string -> result = Attributes.updated(result, Attribute.of(name, string.value()));
                case Sexp.Integer integer -> result = Attributes.updated(result, Attribute.of(name, integer.value()));
                default -> {
                    if (Sexps.isNil(value)) {
                        result = Attributes.updated(result, Attribute.of(name, false));
                    } else if (value == Sexp.KnownSymbol.T) {
                        result = Attributes.updated(result, Attribute.of(name, true));
                    } else {
                        throw signalError(
                            "Don't know what to do with this attribute value: " + Sexps.prettyPrint(value));
                    }
                }
            }
        }
        return result;
    }

    private static @NotNull TagHead extractTagHead(final @NotNull Sexp form) {
        final var symbol = Sexps.asSymbol(form);
        if (symbol != null) {
            return new TagHead(symbol, Seq.empty());
        }
        final var list = Sexps.asList(form);
        if (list == null || list.isEmpty()) {
            throw signalError("This doesn't appear to be a valid HTSL tag head: " + Sexps.prettyPrint(form));
        }
        final var tagNameForm = list.first();
        final var tagName = Sexps.asSymbol(tagNameForm);
        if (tagName == null) {
            throw signalError("This doesn't appear to be a valid HTSL tag name: " + Sexps.prettyPrint(tagNameForm));
        }
        return new TagHead(tagName, list.withoutFirst());
    }

    private @NotNull Node expandCodeBlock(
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        if (attributes.exactSize() != 2 || attributes.first() != Sexp.KnownSymbol.KW_LANGUAGE) {
            throw signalError("code-block accepts exactly one attribute, :language");
        }
        final var languageNameForm = attributes.last();
        if (!(languageNameForm instanceof Sexp.String languageName)) {
            throw signalError("This doesn't appear to be a string: " + Sexps.prettyPrint(languageNameForm));
        }
        return Renderer.wrapCodeBlock(children.map(this::convertForm), languageName.value());
    }

    private @NotNull Node expandHighlightedCode(
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        if (children.exactSize() != 1 || !(children.first() instanceof Sexp.String codeNode)) {
            throw signalError("highlighted-code accepts exactly one string child");
        }
        if (attributes.exactSize() != 2 || attributes.first() != Sexp.KnownSymbol.KW_LANGUAGE) {
            throw signalError("highlighted-code accepts exactly one attribute, :language");
        }
        final var languageTagForm = attributes.last();
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

    private @NotNull Node.Element expandImageFigure(
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        return Node.simple(
            Tag.FIGURE,
            Seq.of(
                Node.simple(Tag.FIGCAPTION, children.map(this::convertForm)),
                makeFigureContent(
                    Seq.empty(),
                    Seq.of(Node.empty(Tag.IMG, convertAttributes(attributes))))));
    }

    private @NotNull Node.Element expandSidenote(
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        return new Node.Element(
            Tag.DIV,
            Attributes.updated(
                Attributes.addedClass(convertAttributes(attributes), "sidenote"),
                Attribute.of("role", "note")
            ),
            children.map(this::convertForm)
        );
    }

    private @NotNull Node.Element expandFigcontent(
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        return makeFigureContent(convertAttributes(attributes), children.map(this::convertForm));
    }

    private @NotNull Node.Element expandInfoBox(
        final @NotNull Seq<Sexp> attributes,
        final @NotNull Seq<Sexp> children
    ) {
        return new Node.Element(
            Tag.DIV,
            Attributes.addedClass(convertAttributes(attributes), "info"),
            children.map(this::convertForm)
        );
    }

    private static @NotNull Node.Element makeFigureContent(
        final @NotNull Seq<@NotNull Attribute> attributes,
        final @NotNull Seq<@NotNull Node> children
    ) {
        return new Node.Element(Tag.DIV, Attributes.addedClass(attributes, "holder"), children);
    }

    private static @NotNull UnhandledErrorError signalError(final @NotNull String message) {
        return ConditionContext.error(new HtslConversionErrorCondition(message));
    }

    private static final Map<Sexp.Symbol, TagMacroExpander> tagMacros = Map.of(
        Sexp.KnownSymbol.CODE_BLOCK, HtslConverter::expandCodeBlock,
        Sexp.KnownSymbol.HIGHLIGHTED_CODE, HtslConverter::expandHighlightedCode,
        Sexp.KnownSymbol.IMAGE_FIGURE, HtslConverter::expandImageFigure,
        Sexp.KnownSymbol.SIDENOTE, HtslConverter::expandSidenote,
        Sexp.KnownSymbol.FIGCONTENT, HtslConverter::expandFigcontent,
        Sexp.KnownSymbol.INFO_BOX, HtslConverter::expandInfoBox
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
            @NotNull Seq<Sexp> attributes,
            @NotNull Seq<Sexp> children
        );
    }

    private record TagHead(@NotNull Sexp.Symbol tagName, @NotNull Seq<Sexp> attributes) {
    }
}
