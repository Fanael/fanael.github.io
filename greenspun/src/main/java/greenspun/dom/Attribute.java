// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.math.BigInteger;
import org.jetbrains.annotations.NotNull;

/**
 * A typed representation of a DOM element's attribute.
 */
public abstract sealed class Attribute {
    private Attribute(final @NotNull java.lang.String name) {
        this.name = name;
    }

    /**
     * Retrieves the name of this attribute.
     */
    public final @NotNull java.lang.String name() {
        return name;
    }

    private final @NotNull java.lang.String name;

    /**
     * A DOM attribute whose value is a boolean.
     * <p>
     * Boolean attributes don't have values in serialized form, they're either present or not.
     */
    public static final class Boolean extends Attribute {
        /**
         * Initializes a new boolean attribute with the given name and value.
         */
        @SuppressWarnings("BooleanParameter")
        public Boolean(final @NotNull java.lang.String name, final boolean value) {
            super(name);
            this.value = value;
        }

        /**
         * Retrieves the value of this attribute.
         */
        public boolean value() {
            return value;
        }

        private final boolean value;
    }

    /**
     * A DOM attribute whose value is an integer.
     */
    public static final class Integer extends Attribute {
        /**
         * Initializes a new integer attribute with the given name and value.
         */
        public Integer(final @NotNull java.lang.String name, final @NotNull BigInteger value) {
            super(name);
            this.value = value;
        }

        /**
         * Retrieves the value of this attribute.
         */
        public @NotNull BigInteger value() {
            return value;
        }

        private final @NotNull BigInteger value;
    }

    /**
     * A DOM attribute whose value is a string.
     */
    public static final class String extends Attribute {
        /**
         * Initializes a new string attribute with the given name and value.
         */
        public String(final @NotNull java.lang.String name, final @NotNull java.lang.String value) {
            super(name);
            this.value = value;
        }

        /**
         * Retrieves the value of this attribute.
         */
        public @NotNull java.lang.String value() {
            return value;
        }

        private final @NotNull java.lang.String value;
    }
}
