// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import java.util.Map;
import greenspun.dom.Attribute;
import greenspun.dom.Attributes;
import greenspun.dom.Node;
import greenspun.dom.Tag;
import greenspun.generator.Renderer;
import greenspun.pygments.Language;
import greenspun.pygments.PygmentsCache;
import greenspun.sexp.Sexp;
import greenspun.sexp.Sexps;
import greenspun.util.Trace;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;

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
    public HtslConverter(final PygmentsCache pygmentsCache) {
        this.pygmentsCache = pygmentsCache;
    }

    /**
     * Converts the given list of S-expressions into a list of DOM tree nodes.
     * <p>
     * On error, a fatal condition is signaled:
     * <ul>
     * <li>{@link HtslConversionErrorCondition} is signaled if an S-expression could not be converted into a DOM tree
     * node.
     * <li>Any condition type that {@link PygmentsCache#highlightCode(String, Language)} can signal, if an error occurs
     * in syntax highlighting during the macro-expansion of {@code highlighted-code} tag macro.
     * </ul>
     */
    public Seq<Node> convert(final Seq<Sexp> forms) {
        return forms.map(this::convertForm);
    }

    private Node convertForm(final Sexp form) {
        if (form instanceof final Sexp.String string) {
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

    private Node.Element convertElement(
        final Sexp.Symbol tagName,
        final Seq<Sexp> attributes,
        final Seq<Sexp> children
    ) {
        final var tag = Tag.byHtmlName(tagName.symbolName());
        if (tag == null) {
            throw signalError("Unknown tag name " + tagName);
        }
        final var convertedAttributes = convertAttributes(attributes);
        final var convertedChildren = children.map(this::convertForm);
        return new Node.Element(tag, convertedAttributes, convertedChildren);
    }

    private static Seq<Attribute> convertAttributes(final Seq<Sexp> attributes) {
        Seq<Attribute> result = Seq.empty();
        for (final var it = attributes.iterator(); it.hasNext(); ) {
            final var keyForm = it.next();
            final var key = Sexps.asKeyword(keyForm);
            if (key == null) {
                throw signalError("Attribute name doesn't appear to be a Lisp keyword: " + Sexps.prettyPrint(keyForm));
            }
            final var name = key.symbolName().substring(1);
            final var value = it.hasNext() ? it.next() : Sexp.KnownSymbol.NIL;
            switch (value) {
                case final Sexp.String s -> result = Attributes.updated(result, Attribute.of(name, s.value()));
                case final Sexp.Integer i -> result = Attributes.updated(result, Attribute.of(name, i.value()));
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

    private static TagHead extractTagHead(final Sexp form) {
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

    private Node expandCodeBlock(final Seq<Sexp> attributes, final Seq<Sexp> children) {
        if (attributes.exactSize() != 2 || attributes.first() != Sexp.KnownSymbol.KW_LANGUAGE) {
            throw signalError("code-block accepts exactly one attribute, :language");
        }
        final var languageNameForm = attributes.last();
        if (!(languageNameForm instanceof final Sexp.String languageName)) {
            throw signalError("This doesn't appear to be a string: " + Sexps.prettyPrint(languageNameForm));
        }
        return Renderer.wrapCodeBlock(children.map(this::convertForm), languageName.value());
    }

    private Node expandHighlightedCode(final Seq<Sexp> attributes, final Seq<Sexp> children) {
        if (children.exactSize() != 1 || !(children.first() instanceof final Sexp.String codeNode)) {
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
        final var pygmentsLanguageName = languageTag.symbolName().substring(1);
        final var language = Language.byPygmentsName(pygmentsLanguageName);
        if (language == null) {
            throw signalError("Unknown language for syntax highlighting: " + languageTag);
        }
        return pygmentsCache.highlightCode(codeNode.value(), language);
    }

    private Node.Element expandImageFigure(final Seq<Sexp> attributes, final Seq<Sexp> children) {
        return Node.simple(
            Tag.FIGURE,
            Seq.of(
                Node.simple(Tag.FIGCAPTION, children.map(this::convertForm)),
                makeFigureContent(
                    Seq.empty(),
                    Seq.of(Node.empty(Tag.IMG, convertAttributes(attributes))))));
    }

    private Node.Element expandSidenote(final Seq<Sexp> attributes, final Seq<Sexp> children) {
        return new Node.Element(
            Tag.DIV,
            Attributes.updated(
                Attributes.addedClass(convertAttributes(attributes), "sidenote"),
                Attribute.of("role", "note")
            ),
            children.map(this::convertForm)
        );
    }

    private Node.Element expandFigcontent(final Seq<Sexp> attributes, final Seq<Sexp> children) {
        return makeFigureContent(convertAttributes(attributes), children.map(this::convertForm));
    }

    private Node.Element expandInfoBox(final Seq<Sexp> attributes, final Seq<Sexp> children) {
        return new Node.Element(
            Tag.DIV,
            Attributes.addedClass(convertAttributes(attributes), "info"),
            children.map(this::convertForm)
        );
    }

    private static Node.Element makeFigureContent(final Seq<Attribute> attributes, final Seq<Node> children) {
        return new Node.Element(Tag.DIV, Attributes.addedClass(attributes, "holder"), children);
    }

    private static UnhandledErrorError signalError(final String message) {
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

    private final PygmentsCache pygmentsCache;

    @FunctionalInterface
    private interface TagMacroExpander {
        Node expand(HtslConverter converter, Seq<Sexp> attributes, Seq<Sexp> children);
    }

    private record TagHead(Sexp.Symbol tagName, Seq<Sexp> attributes) {
    }
}
