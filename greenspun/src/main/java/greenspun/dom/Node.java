// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import greenspun.util.collection.seq.Seq;
import org.jetbrains.annotations.NotNull;

/**
 * The base class for DOM tree nodes.
 * <p>
 * DOM tree nodes are guaranteed to be immutable.
 * <p>
 * Currently, only text and element nodes are supported. This is unlikely to change.
 */
public abstract sealed class Node {
    /**
     * Returns a new DOM element node representing the given tag, with given attributes and no children.
     */
    public static @NotNull Element empty(
        final @NotNull Tag tag,
        final @NotNull Seq<@NotNull Attribute> attributes
    ) {
        return new Element(tag, attributes, Seq.empty());
    }

    /**
     * Returns a new DOM element node representing the given tag, with given attribute and no children.
     */
    public static @NotNull Element empty(final @NotNull Tag tag, final @NotNull Attribute attribute) {
        return new Element(tag, Seq.of(attribute), Seq.empty());
    }

    /**
     * Returns a new DOM element node representing the given tag, with given children and no attributes.
     */
    public static @NotNull Element simple(final @NotNull Tag tag, final @NotNull Seq<@NotNull Node> children) {
        return new Element(tag, Seq.empty(), children);
    }

    /**
     * Returns a new DOM element node representing the given tag, with a single child and no attributes.
     */
    public static @NotNull Element simple(final @NotNull Tag tag, final @NotNull Node child) {
        return new Element(tag, Seq.empty(), Seq.of(child));
    }

    /**
     * DOM node representing bare text.
     */
    public static final class Text extends Node {
        /**
         * Initializes a new text node representing the given text.
         */
        public Text(final @NotNull String text) {
            this.text = text;
        }

        /**
         * Retrieves the represented text.
         */
        public @NotNull String text() {
            return text;
        }

        private final @NotNull String text;
    }

    /**
     * DOM node representing an HTML element, with optional attributes and optional children.
     */
    public static final class Element extends Node {
        /**
         * Creates a new DOM element node representing the given tag, with given attributes and children.
         */
        public Element(
            final @NotNull Tag tag,
            final @NotNull Seq<@NotNull Attribute> attributes,
            final @NotNull Seq<@NotNull Node> children
        ) {
            this.tag = tag;
            this.attributes = attributes;
            this.children = children;
        }

        /**
         * Returns the HTML tag this element represents.
         */
        public @NotNull Tag tag() {
            return tag;
        }

        /**
         * Retrieves the immutable sequence of all attributes of this element.
         */
        public @NotNull Seq<@NotNull Attribute> attributes() {
            return attributes;
        }

        /**
         * Retrieves the immutable sequence of all children of this element.
         */
        public @NotNull Seq<@NotNull Node> children() {
            return children;
        }

        private final @NotNull Tag tag;
        private final @NotNull Seq<@NotNull Attribute> attributes;
        private final @NotNull Seq<@NotNull Node> children;
    }
}
