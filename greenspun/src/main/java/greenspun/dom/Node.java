// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import greenspun.util.collection.seq.Seq;

/**
 * The base interface for DOM tree nodes.
 * <p>
 * DOM tree nodes are guaranteed to be immutable.
 * <p>
 * Currently, only text and element nodes are supported. This is unlikely to change.
 */
public sealed interface Node {
    /**
     * Returns a new DOM element node representing the given tag, with given attributes and no children.
     */
    static Element empty(final Tag tag, final Seq<Attribute> attributes) {
        return new Element(tag, attributes, Seq.empty());
    }

    /**
     * Returns a new DOM element node representing the given tag, with given children and no attributes.
     */
    static Element simple(final Tag tag, final Seq<Node> children) {
        return new Element(tag, Seq.empty(), children);
    }

    /**
     * Returns a new DOM element node representing the given tag, with a single child and no attributes.
     */
    static Element simple(final Tag tag, final Node child) {
        return new Element(tag, Seq.empty(), Seq.of(child));
    }

    /**
     * DOM node representing bare text.
     */
    record Text(String text) implements Node {
    }

    /**
     * DOM node representing an HTML element, with optional attributes and optional children.
     */
    record Element(Tag tag, Seq<Attribute> attributes, Seq<Node> children) implements Node {
    }
}
