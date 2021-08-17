// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.math.BigInteger;
import java.util.Collection;
import greenspun.util.collection.ImmutableList;
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
    public static <E extends Throwable> @NotNull Element build(
        final @NotNull Tag tag,
        final @NotNull BuildFunction<E> buildFunction
    ) throws E {
        final var builder = new ElementBuilder(tag);
        buildFunction.build(builder);
        return builder.toElement();
    }

    /**
     * Returns a new element object of the given tag with no attributes or children.
     */
    public static @NotNull Element makeEmptyElement(final @NotNull Tag tag) {
        return new Element(tag, ImmutableList.empty(), ImmutableList.empty());
    }

    /**
     * A functional interface for functions that operate on an element builder.
     * <p>
     * The {@code build} method is allowed to throw arbitrary exceptions of type {@code E}.
     *
     * @see Node#build(Tag, BuildFunction)
     */
    @FunctionalInterface
    public interface BuildFunction<E extends Throwable> {
        /**
         * Builds a DOM element by modifying the given element builder.
         */
        void build(@NotNull ElementBuilder builder) throws E;
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
            final var index = attributes.findFirst(attribute -> name.equals(attribute.name()));
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
         * @see Node#build(Tag, BuildFunction)
         */
        public <E extends Throwable> void appendBuild(
            final @NotNull Tag tag,
            final @NotNull BuildFunction<E> buildFunction
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
            final var name = attribute.name();
            final var size = attributes.size();
            int index = 0;
            for (; index < size; index += 1) {
                if (name.equals(attributes.get(index).name())) {
                    break;
                }
            }
            if (index < size) {
                attributes.set(index, attribute);
            } else {
                attributes.add(attribute);
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
            return new Element(tag, attributes.freeze(), children.freeze());
        }

        private final @NotNull Tag tag;
        private final ImmutableList.Builder<@NotNull Attribute> attributes = new ImmutableList.Builder<>();
        private final ImmutableList.Builder<@NotNull Node> children = new ImmutableList.Builder<>();
    }
}
