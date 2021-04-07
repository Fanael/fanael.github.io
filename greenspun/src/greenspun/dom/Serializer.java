// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import java.io.IOException;
import java.util.List;
import greenspun.util.UnreachableCodeReachedError;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The DOM-to-HTML serializer.
 */
public final class Serializer {
    private Serializer(final @NotNull Appendable appendable) {
        this.appendable = appendable;
    }

    /**
     * Serializes the DOM tree rooted at {@code rootNode} to HTML, appending the output to the given {@link Appendable}.
     * <p>
     * Any {@link IOException}s thrown by the appendable are allowed to propagate.
     */
    public static void serialize(
        final @NotNull Appendable appendable,
        final @NotNull Node rootNode
    ) throws IOException {
        final var serializer = new Serializer(appendable);
        serializer.serializeNode(rootNode);
    }

    void serializePseudoElement(
        final @NotNull String elementName,
        final @NotNull List<@NotNull Attribute> attributes,
        final @NotNull List<@NotNull Node> children,
        final boolean omitClosingTag
    ) throws IOException {
        appendable.append('<');
        appendable.append(elementName);
        serializeAttributes(attributes);
        appendable.append('>');
        for (final var child : children) {
            serializeNode(child);
        }
        if (!omitClosingTag) {
            appendable.append("</");
            appendable.append(elementName);
            appendable.append('>');
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

    private void serializeAttributes(final @NotNull List<@NotNull Attribute> attributes) throws IOException {
        for (final var attribute : attributes) {
            if (attribute instanceof Attribute.Boolean booleanAttr) {
                if (booleanAttr.value()) {
                    serializeAttributeName(booleanAttr.name());
                }
            } else if (attribute instanceof Attribute.Integer integerAttr) {
                serializeAttributeName(integerAttr.name());
                appendable.append("=\"");
                appendable.append(integerAttr.value().toString());
                appendable.append('"');
            } else if (attribute instanceof Attribute.String stringAttr) {
                serializeAttributeName(stringAttr.name());
                appendable.append("=\"");
                serializeString(stringAttr.value(), AttributeEscaper.instance);
                appendable.append('"');
            } else {
                throw new UnreachableCodeReachedError();
            }
        }
    }

    private void serializeString(final @NotNull String string, final @NotNull Escaper escaper) throws IOException {
        int index = 0;
        int indexToEscape;
        while ((indexToEscape = findCharacterToEscape(string, index, escaper)) >= 0) {
            appendable.append(string, index, indexToEscape);
            appendable.append(escaper.escape(string.charAt(indexToEscape)));
            index = indexToEscape + 1;
        }
        if (index < string.length()) {
            appendable.append(string, index, string.length());
        }
    }

    private void serializeAttributeName(final @NotNull String name) throws IOException {
        appendable.append(' ');
        appendable.append(name);
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

    private final @NotNull Appendable appendable;

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
