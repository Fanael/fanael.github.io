// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import java.io.IOException;
import java.io.Writer;
import java.util.Objects;
import greenspun.util.UnreachableCodeReachedError;
import greenspun.util.collection.ImmutableList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The DOM-to-HTML serializer.
 */
public final class Serializer {
    private Serializer(final @NotNull Writer writer) {
        this.writer = writer;
    }

    /**
     * Serializes the DOM tree rooted at {@code rootNode} to HTML, writing the output to the given {@link Writer}.
     * <p>
     * Any {@link IOException}s thrown by the writer are allowed to propagate.
     */
    public static void serialize(
        final @NotNull Writer writer,
        final @NotNull Node rootNode
    ) throws IOException {
        final var serializer = new Serializer(writer);
        serializer.serializeNode(rootNode);
    }

    void serializePseudoElement(
        final @NotNull String elementName,
        final @NotNull ImmutableList<@NotNull Attribute> attributes,
        final @NotNull ImmutableList<@NotNull Node> children,
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

    private void serializeNode(final @NotNull Node node) throws IOException {
        if (node instanceof Node.Text text) {
            serializeString(text.text(), TextEscaper.instance);
        } else if (node instanceof Node.Element element) {
            final var tag = element.tag();
            final var tagSerializer = tag.elementSerializer();
            if (tagSerializer == null) {
                serializeDefault(element);
            } else {
                tagSerializer.serialize(this, element);
            }
        } else {
            throw new UnreachableCodeReachedError();
        }
    }

    private void serializeDefault(final @NotNull Node.Element element) throws IOException {
        final var tag = element.tag();
        serializePseudoElement(tag.htmlName(), element.attributes(), element.children(), tag.omitClosingTag());
    }

    private void serializeAttributes(final @NotNull ImmutableList<@NotNull Attribute> attributes) throws IOException {
        for (final var attribute : attributes) {
            if (attribute instanceof Attribute.Boolean booleanAttr) {
                if (booleanAttr.value()) {
                    serializeAttributeName(booleanAttr.name());
                }
            } else if (attribute instanceof Attribute.Integer integerAttr) {
                serializeAttributeName(integerAttr.name());
                writer.write("=\"");
                writer.write(integerAttr.value().toString());
                writer.write('"');
            } else if (attribute instanceof Attribute.String stringAttr) {
                serializeAttributeName(stringAttr.name());
                writer.write("=\"");
                serializeString(stringAttr.value(), AttributeEscaper.instance);
                writer.write('"');
            } else {
                throw new UnreachableCodeReachedError();
            }
        }
    }

    private void serializeString(final @NotNull String string, final @NotNull Escaper escaper) throws IOException {
        int index = 0;
        int indexToEscape;
        while ((indexToEscape = findCharacterToEscape(string, index, escaper)) >= 0) {
            writer.write(string, index, indexToEscape - index);
            writer.write(Objects.requireNonNull(escaper.escape(string.charAt(indexToEscape))));
            index = indexToEscape + 1;
        }
        if (index < string.length()) {
            writer.write(string, index, string.length() - index);
        }
    }

    private void serializeAttributeName(final @NotNull String name) throws IOException {
        writer.write(' ');
        writer.write(name);
    }

    private static int findCharacterToEscape(
        final @NotNull String string,
        final int startIndex,
        final @NotNull Escaper escaper
    ) {
        final var length = string.length();
        for (int i = startIndex; i < length; i += 1) {
            if (escaper.escape(string.charAt(i)) != null) {
                return i;
            }
        }
        return -1;
    }

    private final @NotNull Writer writer;

    @FunctionalInterface
    interface ForElement {
        void serialize(final @NotNull Serializer serializer, final @NotNull Node.Element element) throws IOException;
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
