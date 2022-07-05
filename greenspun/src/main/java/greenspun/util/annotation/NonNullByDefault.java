// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.annotation.Nonnull;
import javax.annotation.meta.TypeQualifierDefault;

/**
 * A common annotation to declare that all elements should be considered non-nullable by default in the given package
 * or class.
 * <p>
 * Can be locally overridden with a {@link Nullable} annotation.
 * <p>
 * Preferably all packages would be annotated with this annotation, but sometimes its effects are <em>too</em>
 * far-reaching, typically in conjunction with generics. In such cases, use explicit {@link NonNull} and
 * {@link Nullable} annotations where applicable instead.
 */
@Documented
@Retention(RetentionPolicy.CLASS)
@Target({ElementType.PACKAGE, ElementType.TYPE})
@TypeQualifierDefault({
    ElementType.FIELD,
    ElementType.METHOD,
    ElementType.PARAMETER,
    ElementType.RECORD_COMPONENT,
    ElementType.TYPE_USE,
})
@Nonnull
public @interface NonNullByDefault {
}
