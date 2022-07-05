// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.math.BigInteger;

/**
 * A typed representation of a DOM element's attribute.
 */
public abstract sealed class Attribute {
    private Attribute(final java.lang.String name) {
        this.name = name;
    }

    /**
     * Returns a new instance of a boolean attribute.
     */
    @SuppressWarnings("BooleanParameter")
    public static Boolean of(final java.lang.String name, final boolean value) {
        return new Boolean(name, value);
    }

    /**
     * Returns a new instance of an integer attribute.
     */
    public static Integer of(final java.lang.String name, final BigInteger value) {
        return new Integer(name, value);
    }

    /**
     * Returns a new instance of a string attribute.
     */
    public static String of(final java.lang.String name, final java.lang.String value) {
        return new String(name, value);
    }

    /**
     * Retrieves the name of this attribute.
     */
    public final java.lang.String name() {
        return name;
    }

    private final java.lang.String name;

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
        public Boolean(final java.lang.String name, final boolean value) {
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
        public Integer(final java.lang.String name, final BigInteger value) {
            super(name);
            this.value = value;
        }

        /**
         * Retrieves the value of this attribute.
         */
        public BigInteger value() {
            return value;
        }

        private final BigInteger value;
    }

    /**
     * A DOM attribute whose value is a string.
     */
    public static final class String extends Attribute {
        /**
         * Initializes a new string attribute with the given name and value.
         */
        public String(final java.lang.String name, final java.lang.String value) {
            super(name);
            this.value = value;
        }

        /**
         * Retrieves the value of this attribute.
         */
        public java.lang.String value() {
            return value;
        }

        private final java.lang.String value;
    }
}
