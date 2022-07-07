// Copyright © 2021-2022  Fanael Linithien
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
import greenspun.util.collection.seq.Seq;
import org.checkerframework.checker.nullness.qual.Nullable;

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
        .setElementSerializer((final Serializer serializer, final Node.Element element) ->
            serializer.serializePseudoElement(
                "meta",
                Seq.of(new Attribute.String("charset", "UTF-8")),
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
        .setRequiredAttributes(Seq.of("name", "content"))
        .setElementSerializer((final Serializer serializer, final Node.Element element) ->
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
        .setRequiredAttributes(Seq.of("href", "rel"))
    ),
    TITLE(build(Context.METADATA, Context.TEXT_ONLY)),
    BODY(build(Context.HEAD_AND_BODY, Context.FLOW)),
    MAIN(build(Context.FLOW, Context.FLOW)),
    ARTICLE(build(Context.FLOW, Context.FLOW)),
    SECTION(build(Context.FLOW, Context.FLOW)),
    NAV(build(Context.FLOW, Context.FLOW)),
    HEADER(build(Context.FLOW, Context.FLOW)),
    FOOTER(build(Context.FLOW, Context.FLOW)),
    H1(build(Context.FLOW, Context.FLOW)),
    H2(build(Context.FLOW, Context.FLOW)),
    H3(build(Context.FLOW, Context.FLOW)),
    H4(build(Context.FLOW, Context.FLOW)),
    H5(build(Context.FLOW, Context.FLOW)),
    H6(build(Context.FLOW, Context.FLOW)),
    BLOCKQUOTE(build(Context.FLOW, Context.FLOW)),
    DIV(build(EnumSet.of(Context.FLOW, Context.FIGURE), ChildContext.transparent())
        .setAllowedAttributes(Map.of("role", new StringSetVerifier(Set.of("note"))))
    ),
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
        .setRequiredAttributes(Seq.of("href"))
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
        .setRequiredAttributes(Seq.of("datetime"))
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
        .setRequiredAttributes(Seq.of("src", "alt", "width", "height"))
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
    ),
    SCRIPT(build(Context.METADATA, ChildContext.none())
        .setAllowedAttributes(Map.of(
            "src", Verifier.attributeIsString,
            "defer", Verifier.attributeIsBoolean
        ))
        .setRequiredAttributes(Seq.of("src"))
    );

    Tag(final Builder builder) {
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
    public static @Nullable Tag byHtmlName(final String htmlName) {
        return tagsByHtmlName.get(htmlName);
    }

    /**
     * Retrieves the HTML name of the tag.
     */
    public String htmlName() {
        return htmlName;
    }

    boolean allowedIn(final Context context) {
        return allowedContexts.contains(context);
    }

    String allowedContextsString() {
        return allowedContexts.toString();
    }

    ChildContext childContext() {
        return childContext;
    }

    boolean omitClosingTag() {
        return omitClosingTag;
    }

    Serializer.@Nullable ForElement elementSerializer() {
        return elementSerializer;
    }

    Map<String, Verifier.AttributeVerifier> allowedAttributes() {
        return allowedAttributes;
    }

    Seq<String> requiredAttributes() {
        return requiredAttributes;
    }

    private static Builder build(
        final Context allowedContext,
        final ChildContext childContext
    ) {
        return new Builder(EnumSet.of(allowedContext), childContext);
    }

    private static Builder build(
        final EnumSet<Context> allowedContexts,
        final ChildContext childContext
    ) {
        return new Builder(allowedContexts, childContext);
    }

    private static final Map<String, Tag> tagsByHtmlName =
        Arrays.stream(values()).collect(Collectors.toUnmodifiableMap(Tag::htmlName, Function.identity()));

    private final String htmlName;
    private final EnumSet<Context> allowedContexts;
    private final ChildContext childContext;
    private final boolean omitClosingTag;
    private final Serializer.@Nullable ForElement elementSerializer;
    private final Map<String, Verifier.AttributeVerifier> allowedAttributes;
    private final Seq<String> requiredAttributes;

    private record StringSetVerifier(Set<String> allowed) implements Verifier.AttributeVerifier {
        @Override
        public void verify(final Verifier.AttributeVerificationContext context) {
            Verifier.attributeIsString.verify(context);
            if (context.attribute() instanceof Attribute.String string && !allowed.contains(string.value())) {
                context.recordError("invalid value, expected one of " + allowed);
            }
        }
    }

    private static final class DatetimeVerifier implements Verifier.AttributeVerifier {
        @Override
        public void verify(final Verifier.AttributeVerificationContext context) {
            Verifier.attributeIsString.verify(context);
            if (context.attribute() instanceof Attribute.String string &&
                !datetimePattern.matcher(string.value()).matches()) {
                context.recordError("invalid date format, YYYY-MM-DD expected");
            }
        }

        private static final Pattern datetimePattern = Pattern.compile("\\d{4,}-\\d{2}-\\d{2}");
    }

    private static final class Builder {
        private Builder(final EnumSet<Context> allowedContexts, final ChildContext childContext) {
            this.allowedContexts = allowedContexts;
            this.childContext = childContext;
        }

        private Builder setOmitClosingTag() {
            omitClosingTag = true;
            return this;
        }

        private Builder setElementSerializer(final Serializer.ForElement elementSerializer) {
            this.elementSerializer = elementSerializer;
            return this;
        }

        private Builder setAllowedAttributes(final Map<String, Verifier.AttributeVerifier> allowedAttributes) {
            this.allowedAttributes = allowedAttributes;
            return this;
        }

        private Builder setRequiredAttributes(final Seq<String> requiredAttributes) {
            this.requiredAttributes = requiredAttributes;
            return this;
        }

        private final EnumSet<Context> allowedContexts;
        private final ChildContext childContext;
        private boolean omitClosingTag = false;
        private Serializer.@Nullable ForElement elementSerializer = null;
        private Map<String, Verifier.AttributeVerifier> allowedAttributes = Collections.emptyMap();
        private Seq<String> requiredAttributes = Seq.empty();
    }
}
