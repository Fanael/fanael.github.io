// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.io.IOException;
import java.io.Writer;
import greenspun.util.collection.seq.Seq;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The DOM-to-HTML serializer.
 */
public final class Serializer {
    private Serializer(final Writer writer) {
        this.writer = writer;
    }

    /**
     * Serializes the DOM tree rooted at {@code rootNode} to HTML, writing the output to the given {@link Writer}.
     * <p>
     * Any {@link IOException}s thrown by the writer are allowed to propagate.
     */
    public static void serialize(final Writer writer, final Node rootNode) throws IOException {
        final var serializer = new Serializer(writer);
        serializer.serializeNode(rootNode);
    }

    void serializePseudoElement(
        final String elementName,
        final Seq<Attribute> attributes,
        final Seq<Node> children,
        final boolean omitClosingTag
    ) throws IOException {
        writer.write('<');
        writer.write(elementName);
        serializeAttributes(attributes);
        writer.write('>');
        for (final var child : children) {
            serializeNode(child);
        }
        if (!omitClosingTag) {
            writer.write("</");
            writer.write(elementName);
            writer.write('>');
        }
    }

    private void serializeNode(final Node node) throws IOException {
        switch (node) {
            case final Node.Text text -> serializeString(text.text(), TextEscaper.instance);
            case final Node.Element element -> {
                final var tag = element.tag();
                final var tagSerializer = tag.elementSerializer();
                if (tagSerializer == null) {
                    serializeDefault(element);
                } else {
                    tagSerializer.serialize(this, element);
                }
            }
        }
    }

    private void serializeDefault(final Node.Element element) throws IOException {
        final var tag = element.tag();
        serializePseudoElement(tag.htmlName(), element.attributes(), element.children(), tag.omitClosingTag());
    }

    private void serializeAttributes(final Seq<Attribute> attributes) throws IOException {
        for (final var attribute : attributes) {
            switch (attribute) {
                case final Attribute.Boolean booleanAttr -> {
                    if (booleanAttr.value()) {
                        serializeAttributeName(booleanAttr.name());
                    }
                }
                case final Attribute.Integer integerAttr -> {
                    serializeAttributeName(integerAttr.name());
                    writer.write("=\"");
                    writer.write(integerAttr.value().toString());
                    writer.write('"');
                }
                case final Attribute.String stringAttr -> {
                    serializeAttributeName(stringAttr.name());
                    writer.write("=\"");
                    serializeString(stringAttr.value(), AttributeEscaper.instance);
                    writer.write('"');
                }
            }
        }
    }

    private void serializeString(final String string, final Escaper escaper) throws IOException {
        int index = 0;
        int indexToEscape;
        while ((indexToEscape = findCharacterToEscape(string, index, escaper)) >= 0) {
            writer.write(string, index, indexToEscape - index);
            final var escaped = escaper.escape(string.charAt(indexToEscape));
            assert escaped != null : "Non-deterministic character escaper? @AssumeAssertion(nullness)";
            writer.write(escaped);
            index = indexToEscape + 1;
        }
        if (index < string.length()) {
            writer.write(string, index, string.length() - index);
        }
    }

    private void serializeAttributeName(final String name) throws IOException {
        writer.write(' ');
        writer.write(name);
    }

    private static int findCharacterToEscape(final String string, final int startIndex, final Escaper escaper) {
        final var length = string.length();
        for (int i = startIndex; i < length; i += 1) {
            if (escaper.escape(string.charAt(i)) != null) {
                return i;
            }
        }
        return -1;
    }

    private final Writer writer;

    @FunctionalInterface
    interface ForElement {
        void serialize(final Serializer serializer, final Node.Element element) throws IOException;
    }

    private sealed interface Escaper {
        @Nullable String escape(char character);
    }

    private static final class TextEscaper implements Escaper {
        @Override
        public @Nullable String escape(final char character) {
            return switch (character) {
                case '<' -> "&lt;";
                case '>' -> "&gt;";
                case '&' -> "&amp;";
                default -> null;
            };
        }

        private static final TextEscaper instance = new TextEscaper();
    }

    private static final class AttributeEscaper implements Escaper {
        @Override
        public @Nullable String escape(final char character) {
            return (character == '"') ? "&quot;" : TextEscaper.instance.escape(character);
        }

        private static final AttributeEscaper instance = new AttributeEscaper();
    }
}
