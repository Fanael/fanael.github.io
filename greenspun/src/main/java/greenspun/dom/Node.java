// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.math.BigInteger;
import greenspun.util.collection.seq.Seq;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The base class for DOM tree nodes.
 * <p>
 * DOM tree nodes are guaranteed to be immutable.
 * <p>
 * Currently only text and element nodes are supported. This is unlikely to change.
 */
public abstract sealed class Node {
    /**
     * Builds a new DOM node representing the given HTML tag.
     * <p>
     * The given build function is passed a new element builder, which it's expected to modify. The state of the builder
     * is unspecified after this method returns.
     * <p>
     * Exceptions thrown by the build function are passed through.
     */
    public static @NotNull Element build(final @NotNull Tag tag, final @NotNull BuildFunction buildFunction) {
        final var builder = new ElementBuilder(tag);
        buildFunction.build(builder);
        return builder.toElement();
    }

    /**
     * Returns a new element object of the given tag with no attributes or children.
     */
    public static @NotNull Element makeEmptyElement(final @NotNull Tag tag) {
        return new Element(tag, Seq.empty(), Seq.empty());
    }

    /**
     * A functional interface for functions that build a DOM element.
     *
     * @see Node#build(Tag, BuildFunction)
     */
    @FunctionalInterface
    public interface BuildFunction {
        /**
         * Builds a DOM element by modifying the given element builder.
         */
        void build(@NotNull ElementBuilder builder);
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
        private Element(
            final @NotNull Tag tag,
            final @NotNull Seq<@NotNull Attribute> attributes,
            final @NotNull Seq<@NotNull Node> children
        ) {
            this.tag = tag;
            this.attributes = attributes;
            this.children = children;
        }

        /**
         * Builds a new DOM node by creating a clone of this element.
         * <p>
         * The clone represents the same tag and has the same attributes as this element, but starts with no children.
         * <p>
         * The given build function is passed a new element builder, which it's expected to modify. The state of
         * the builder is unspecified after this method returns.
         * <p>
         * Exceptions thrown by the build function are passed through.
         */
        public @NotNull Element buildClone(final @NotNull BuildFunction buildFunction) {
            final var builder = new ElementBuilder(tag);
            builder.attributes = attributes;
            buildFunction.build(builder);
            return builder.toElement();
        }

        /**
         * Retrieves the immutable list of all attributes of this element.
         */
        public @NotNull Seq<@NotNull Attribute> attributes() {
            return attributes;
        }

        /**
         * Retrieves the immutable list of all children of this element.
         */
        public @NotNull Seq<@NotNull Node> children() {
            return children;
        }

        /**
         * Retrieves the value of the attribute with the give name, or {@code null} if no such attribute is present.
         */
        public @Nullable Attribute getAttribute(final @NotNull String name) {
            for (final var attribute : attributes) {
                if (name.equals(attribute.name())) {
                    return attribute;
                }
            }
            return null;
        }

        @NotNull Tag tag() {
            return tag;
        }

        private final @NotNull Tag tag;
        private final Seq<@NotNull Attribute> attributes;
        private final Seq<@NotNull Node> children;
    }

    /**
     * A builder class that allows easy and convenient creation of {@link Element} objects with specified attributes
     * and children.
     * <p>
     * Instances of this class cannot be created directly, use {@link Node#build(Tag, BuildFunction)}} instead.
     */
    public static final class ElementBuilder {
        private ElementBuilder(final @NotNull Tag tag) {
            this.tag = tag;
        }

        /**
         * Appends the given node to the list of children.
         * <p>
         * This method doesn't perform any checking: it can be used to create cycles in the DOM "tree" or to insert
         * nodes into contexts they don't belong in. This is not checked here, but it can be detected by the DOM
         * {@link Verifier}.
         */
        public void append(final @NotNull Node child) {
            children = children.appended(child);
        }

        /**
         * Appends the given sequence of children to the list of children.
         * <p>
         * This method doesn't perform any checking: it can be used to create cycles in the DOM "tree" or to insert
         * nodes into contexts they don't belong in. This is not checked here, but it can be detected by the DOM
         * {@link Verifier}.
         */
        public void append(final @NotNull Seq<? extends @NotNull Node> newChildren) {
            children = children.concat(newChildren);
        }

        /**
         * A convenience method that appends a new text node representing the given string to the list of children.
         *
         * @see #append(Node)
         */
        public void appendText(final @NotNull String string) {
            append(new Text(string));
        }

        /**
         * A convenience method that appends a newly-built element node to the list of children.
         * <p>
         * Equivalent to {@code this.append(Node.build(tag, buildFunction))}.
         *
         * @see #append(Node)
         * @see Node#build(Tag, BuildFunction)
         */
        public void appendBuild(final @NotNull Tag tag, final @NotNull BuildFunction buildFunction) {
            append(build(tag, buildFunction));
        }

        /**
         * Sets the given attribute to a new value.
         * <p>
         * If the element being built already has an attribute with the same name, the value is overwritten.
         * <p>
         * This method doesn't perform any checking: it can be used to set unrecognized attributes or give attributes
         * bogus values. This is not checked here, but it can be detected by the DOM {@link Verifier}.
         */
        public void set(final @NotNull Attribute attribute) {
            final var name = attribute.name();
            long index = 0;
            for (final var attr : attributes) {
                if (name.equals(attr.name())) {
                    attributes = attributes.updated(index, attribute);
                    return;
                }
                index += 1;
            }
            attributes = attributes.appended(attribute);
        }

        /**
         * A convenience method that sets a string attribute to the given value.
         *
         * @see #set(Attribute)
         */
        public void set(final @NotNull String name, final @NotNull String value) {
            set(new Attribute.String(name, value));
        }

        /**
         * A convenience method that sets an integer attribute to the given value.
         *
         * @see #set(Attribute)
         */
        public void set(final @NotNull String name, final @NotNull BigInteger value) {
            set(new Attribute.Integer(name, value));
        }

        /**
         * A convenience method that sets a boolean attribute to the given value.
         *
         * @see #set(Attribute)
         */
        @SuppressWarnings("BooleanParameter")
        public void set(final @NotNull String name, final boolean value) {
            set(new Attribute.Boolean(name, value));
        }

        private @NotNull Element toElement() {
            return new Element(tag, attributes, children);
        }

        private final @NotNull Tag tag;
        private Seq<@NotNull Attribute> attributes = Seq.empty();
        private Seq<@NotNull Node> children = Seq.empty();
    }
}
