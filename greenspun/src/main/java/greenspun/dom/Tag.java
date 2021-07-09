// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import greenspun.util.collection.ImmutableList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * HTML elements known to the generator.
 * <p>
 * This is intentionally incomplete: not all elements that are part of HTML are desired here. Additionally, certain
 * elements, such as {@code <meta>} were split into multiple pseudo-elements.
 */
public enum Tag {
    HTML(build(Context.ROOT, Context.HEAD_AND_BODY)),
    HEAD(build(Context.HEAD_AND_BODY, Context.METADATA)),
    /**
     * A pseudo-element representing the UTF-8 encoding declaration, that is {@code <meta charset="UTF-8">}.
     */
    META_CHARSET_UTF8(build(Context.METADATA, ChildContext.none())
        .setOmitClosingTag()
        .setElementSerializer((final @NotNull Serializer serializer, final @NotNull Node.Element element) ->
            serializer.serializePseudoElement(
                "meta",
                ImmutableList.of(new Attribute.String("charset", "UTF-8")),
                element.children(),
                true
            ))
    ),
    /**
     * A pseudo-element representing {@code <meta name="…" content="…">}.
     */
    META_NAMED(build(Context.METADATA, ChildContext.none())
        .setOmitClosingTag()
        .setAllowedAttributes(Map.of(
            "name", Verifier.attributeIsString,
            "content", Verifier.attributeIsString
        ))
        .setRequiredAttributes(ImmutableList.of("name", "content"))
        .setElementSerializer((final @NotNull Serializer serializer, final @NotNull Node.Element element) ->
            serializer.serializePseudoElement("meta", element.attributes(), element.children(), true))
    ),
    LINK(build(Context.METADATA, ChildContext.none())
        .setOmitClosingTag()
        .setAllowedAttributes(Map.of(
            "href", Verifier.attributeIsString,
            "rel", new StringSetVerifier(Set.of("alternate", "license", "stylesheet")),
            "title", Verifier.attributeIsString,
            "type", Verifier.attributeIsString
        ))
        .setRequiredAttributes(ImmutableList.of("href", "rel"))
    ),
    TITLE(build(Context.METADATA, Context.TEXT_ONLY)),
    BODY(build(Context.HEAD_AND_BODY, Context.FLOW)),
    MAIN(build(Context.FLOW, Context.FLOW)),
    ARTICLE(build(Context.FLOW, Context.FLOW)),
    SECTION(build(Context.FLOW, Context.FLOW)),
    NAV(build(Context.FLOW, Context.FLOW)),
    HEADER(build(Context.FLOW, Context.FLOW)),
    FOOTER(build(Context.FLOW, Context.FLOW)),
    ASIDE(build(Context.FLOW, Context.FLOW)),
    H1(build(Context.FLOW, Context.FLOW)),
    H2(build(Context.FLOW, Context.FLOW)),
    H3(build(Context.FLOW, Context.FLOW)),
    H4(build(Context.FLOW, Context.FLOW)),
    H5(build(Context.FLOW, Context.FLOW)),
    H6(build(Context.FLOW, Context.FLOW)),
    BLOCKQUOTE(build(Context.FLOW, Context.FLOW)),
    DIV(build(EnumSet.of(Context.FLOW, Context.FIGURE), ChildContext.transparent())),
    P(build(Context.FLOW, Context.PHRASING)),
    BR(build(EnumSet.of(Context.FLOW, Context.PHRASING), ChildContext.none())
        .setOmitClosingTag()
    ),
    PRE(build(Context.FLOW, Context.PHRASING)),
    OL(build(Context.FLOW, Context.LIST_ELEMENT)
        .setAllowedAttributes(Map.of(
            "reversed", Verifier.attributeIsBoolean,
            "start", Verifier.attributeIsInteger
        ))
    ),
    UL(build(Context.FLOW, Context.LIST_ELEMENT)),
    LI(build(Context.LIST_ELEMENT, Context.FLOW)),
    A(build(EnumSet.of(Context.FLOW, Context.PHRASING), ChildContext.transparent())
        .setAllowedAttributes(Map.of(
            "href", Verifier.attributeIsString,
            "rel", new StringSetVerifier(Set.of("author", "license", "next", "prev"))
        ))
        .setRequiredAttributes(ImmutableList.of("href"))
    ),
    EM(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    STRONG(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    CODE(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    VAR(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    SAMP(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    KBD(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    SPAN(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    B(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    I(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    SUP(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    SUB(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    CITE(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    DFN(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)),
    TIME(build(EnumSet.of(Context.FLOW, Context.PHRASING), Context.PHRASING)
        .setAllowedAttributes(Map.of("datetime", new DatetimeVerifier()))
        .setRequiredAttributes(ImmutableList.of("datetime"))
    ),
    FIGURE(build(Context.FLOW, Context.FIGURE)),
    FIGCAPTION(build(Context.FIGURE, Context.FLOW)),
    IMG(build(Context.FIGURE, ChildContext.none())
        .setOmitClosingTag()
        .setAllowedAttributes(Map.of(
            "src", Verifier.attributeIsString,
            "alt", Verifier.attributeIsString,
            "width", Verifier.attributeIsInteger,
            "height", Verifier.attributeIsInteger
        ))
        .setRequiredAttributes(ImmutableList.of("src", "alt", "width", "height"))
    ),
    TABLE(build(Context.FIGURE, Context.TABLE_SECTION)),
    THEAD(build(Context.TABLE_SECTION, Context.TABLE_ROW)),
    TBODY(build(Context.TABLE_SECTION, Context.TABLE_ROW)),
    TFOOT(build(Context.TABLE_SECTION, Context.TABLE_ROW)),
    TR(build(Context.TABLE_ROW, Context.TABLE_CELL)),
    TH(build(Context.TABLE_CELL, Context.FLOW)
        .setOmitClosingTag()
    ),
    TD(build(Context.TABLE_CELL, Context.FLOW)
        .setOmitClosingTag()
    );

    Tag(final @NotNull Builder builder) {
        htmlName = name().toLowerCase(Locale.ROOT).replace('_', '-');
        allowedContexts = builder.allowedContexts;
        childContext = builder.childContext;
        omitClosingTag = builder.omitClosingTag;
        elementSerializer = builder.elementSerializer;
        allowedAttributes = builder.allowedAttributes;
        requiredAttributes = builder.requiredAttributes;
    }

    /**
     * Retrieves the tag with the given HTML name, or {@code null} if one doesn't exist.
     * <p>
     * The HTML name of a tag is a lowercase string, with words separated with the ASCII dash symbol "-".
     */
    public static @Nullable Tag byHtmlName(final @NotNull String htmlName) {
        return tagsByHtmlName.get(htmlName);
    }

    /**
     * Retrieves the HTML name of the tag.
     */
    public @NotNull String htmlName() {
        return htmlName;
    }

    boolean allowedIn(final @NotNull Context context) {
        return allowedContexts.contains(context);
    }

    @NotNull String allowedContextsString() {
        return allowedContexts.toString();
    }

    @NotNull ChildContext childContext() {
        return childContext;
    }

    boolean omitClosingTag() {
        return omitClosingTag;
    }

    @Nullable Serializer.ForElement elementSerializer() {
        return elementSerializer;
    }

    @NotNull Map<String, Verifier.AttributeVerifier> allowedAttributes() {
        return allowedAttributes;
    }

    @NotNull ImmutableList<String> requiredAttributes() {
        return requiredAttributes;
    }

    private static @NotNull Builder build(
        final @NotNull Context allowedContext,
        final @NotNull ChildContext childContext
    ) {
        return new Builder(EnumSet.of(allowedContext), childContext);
    }

    private static @NotNull Builder build(
        final @NotNull EnumSet<Context> allowedContexts,
        final @NotNull ChildContext childContext
    ) {
        return new Builder(allowedContexts, childContext);
    }

    private static final Map<String, Tag> tagsByHtmlName =
        Arrays.stream(values()).collect(Collectors.toUnmodifiableMap(Tag::htmlName, Function.identity()));

    private final @NotNull String htmlName;
    private final @NotNull EnumSet<Context> allowedContexts;
    private final @NotNull ChildContext childContext;
    private final boolean omitClosingTag;
    private final @Nullable Serializer.ForElement elementSerializer;
    private final @NotNull Map<String, Verifier.AttributeVerifier> allowedAttributes;
    private final @NotNull ImmutableList<String> requiredAttributes;

    private record StringSetVerifier(@NotNull Set<String> allowed) implements Verifier.AttributeVerifier {
        @Override
        public void verify(final @NotNull Verifier.AttributeVerificationContext context) {
            Verifier.attributeIsString.verify(context);
            if (context.attribute() instanceof Attribute.String string && !allowed.contains(string.value())) {
                context.recordError("invalid value, expected one of " + allowed);
            }
        }
    }

    private static final class DatetimeVerifier implements Verifier.AttributeVerifier {
        @Override
        public void verify(final @NotNull Verifier.AttributeVerificationContext context) {
            Verifier.attributeIsString.verify(context);
            if (context.attribute() instanceof Attribute.String string &&
                !datetimePattern.matcher(string.value()).matches()) {
                context.recordError("invalid date format, YYYY-MM-DD expected");
            }
        }

        private static final Pattern datetimePattern = Pattern.compile("[0-9]{4,}-[0-9]{2}-[0-9]{2}");
    }

    private static final class Builder {
        private Builder(final @NotNull EnumSet<Context> allowedContexts, final @NotNull ChildContext childContext) {
            this.allowedContexts = allowedContexts;
            this.childContext = childContext;
        }

        private Builder setOmitClosingTag() {
            omitClosingTag = true;
            return this;
        }

        private Builder setElementSerializer(final @NotNull Serializer.ForElement elementSerializer) {
            this.elementSerializer = elementSerializer;
            return this;
        }

        private Builder setAllowedAttributes(final @NotNull Map<String, Verifier.AttributeVerifier> allowedAttributes) {
            this.allowedAttributes = allowedAttributes;
            return this;
        }

        private Builder setRequiredAttributes(final @NotNull ImmutableList<String> requiredAttributes) {
            this.requiredAttributes = requiredAttributes;
            return this;
        }

        private final @NotNull EnumSet<Context> allowedContexts;
        private final @NotNull ChildContext childContext;
        private boolean omitClosingTag = false;
        private @Nullable Serializer.ForElement elementSerializer = null;
        private @NotNull Map<String, Verifier.AttributeVerifier> allowedAttributes = Collections.emptyMap();
        private @NotNull ImmutableList<String> requiredAttributes = ImmutableList.empty();
    }
}
