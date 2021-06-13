// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import greenspun.util.collection.ImmutableList;
import greenspun.util.function.ThrowingConsumer;
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
     * Returns a new element builder that will create an element of the given tag.
     */
    public static <E extends Throwable> @NotNull Element build(
        final @NotNull Tag tag,
        final @NotNull ThrowingConsumer<? super ElementBuilder, E> buildFunction
    ) throws E {
        final var builder = new ElementBuilder(tag);
        buildFunction.accept(builder);
        return builder.toElement();
    }

    /**
     * Returns a new element object of the given tag with no attributes or children.
     */
    public static @NotNull Element makeEmptyElement(final @NotNull Tag tag) {
        return new Element(tag, ImmutableList.empty(), ImmutableList.empty());
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
            final @NotNull ImmutableList<@NotNull Attribute> attributes,
            final @NotNull ImmutableList<@NotNull Node> children
        ) {
            this.tag = tag;
            this.attributes = attributes;
            this.children = children;
        }

        /**
         * Retrieves the immutable list of all attributes of this element.
         */
        public @NotNull ImmutableList<@NotNull Attribute> attributes() {
            return attributes;
        }

        /**
         * Retrieves the immutable list of all children of this element.
         */
        public @NotNull ImmutableList<@NotNull Node> children() {
            return children;
        }

        @NotNull Tag tag() {
            return tag;
        }

        @Nullable Attribute getAttribute(final @NotNull String name) {
            final var index = ElementBuilder.findAttributeIndex(attributes, name);
            return (index == -1) ? null : attributes.get(index);
        }

        private final @NotNull Tag tag;
        private final ImmutableList<@NotNull Attribute> attributes;
        private final ImmutableList<@NotNull Node> children;
    }

    /**
     * A builder class that allows easy and convenient creation of {@link Element} objects with specified attributes
     * and children.
     * <p>
     * Instances of this class cannot be created directly, use {@link Node#build(Tag, ThrowingConsumer)}} instead.
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
            children.add(child);
        }

        /**
         * Appends the given collection of children to the list of children.
         * <p>
         * Nodes are added in the order returned by the collection's iterator.
         * <p>
         * This method doesn't perform any checking: it can be used to create cycles in the DOM "tree" or to insert
         * nodes into contexts they don't belong in. This is not checked here, but it can be detected by the DOM
         * {@link Verifier}.
         */
        public void append(final @NotNull Collection<? extends @NotNull Node> newChildren) {
            children.addAll(newChildren);
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
         * @see Node#build(Tag, ThrowingConsumer)
         */
        public <E extends Throwable> void appendBuild(
            final @NotNull Tag tag,
            final @NotNull ThrowingConsumer<? super ElementBuilder, E> buildFunction
        ) throws E {
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
            final var index = findAttributeIndex(attributes, attribute.name());
            if (index == -1) {
                attributes.add(attribute);
            } else {
                attributes.set(index, attribute);
            }
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
            return new Element(tag, ImmutableList.freeze(attributes), ImmutableList.freeze(children));
        }

        private static int findAttributeIndex(final @NotNull List<Attribute> attributes, final @NotNull String name) {
            for (int i = 0; i < attributes.size(); i += 1) {
                if (name.equals(attributes.get(i).name())) {
                    return i;
                }
            }
            return -1;
        }

        private final @NotNull Tag tag;
        private final ArrayList<@NotNull Attribute> attributes = new ArrayList<>();
        private final ArrayList<@NotNull Node> children = new ArrayList<>();
    }
}
