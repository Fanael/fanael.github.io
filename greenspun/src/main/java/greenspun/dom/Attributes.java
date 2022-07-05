// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import edu.umd.cs.findbugs.annotations.CheckReturnValue;
import greenspun.util.annotation.Nullable;
import greenspun.util.collection.seq.Seq;

/**
 * A utility class containing operations on sequences of attributes.
 */
public final class Attributes {
    private Attributes() {
    }

    /**
     * Retrieves the value of the attribute with the given name, or {@code null} if no such attribute is present.
     */
    public static @Nullable Attribute get(final Seq<Attribute> attributes, final String name) {
        for (final var attribute : attributes) {
            if (name.equals(attribute.name())) {
                return attribute;
            }
        }
        return null;
    }

    /**
     * Returns a new sequence of attributes with the given attribute set to a new value.
     * <p>
     * If the sequence already contains attribute with the same name, the value is replaced. Otherwise, the attribute
     * is appended.
     */
    @CheckReturnValue
    public static Seq<Attribute> updated(final Seq<Attribute> attributes, final Attribute attribute) {
        final var name = attribute.name();
        for (final var it = attributes.iterator(); it.hasNext(); ) {
            final var attr = it.next();
            if (name.equals(attr.name())) {
                return attributes.updated(it.previousIndex(), attribute);
            }
        }
        return attributes.appended(attribute);
    }

    /**
     * Returns a new sequence of attribute with the given class name added.
     * <p>
     * If the sequence already contains a string attribute named "class", the given class name is appended. Otherwise,
     * a new class attribute is created and added to the sequence.
     */
    @CheckReturnValue
    public static Seq<Attribute> addedClass(final Seq<Attribute> attributes, final String className) {
        for (final var it = attributes.iterator(); it.hasNext(); ) {
            final var attr = it.next();
            if ("class".equals(attr.name())) {
                final var index = it.previousIndex();
                if (attr instanceof Attribute.String string) {
                    return attributes.updated(index, Attribute.of("class", string.value() + ' ' + className));
                } else {
                    // Non-string class attribute are most likely a mistake, just ignore them.
                    return attributes.updated(index, Attribute.of("class", className));
                }
            }
        }
        return attributes.appended(Attribute.of("class", className));
    }
}
