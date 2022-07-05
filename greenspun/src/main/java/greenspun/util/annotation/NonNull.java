// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.annotation.Nonnull;
import javax.annotation.meta.TypeQualifierNickname;

/**
 * A common annotation to declare that the annotated element can never {@code null}.
 * <p>
 * Should <em>only</em> be used in packages or classes which are not {@link NonNullByDefault}.
 */
@Documented
@Retention(RetentionPolicy.CLASS)
@Target({
    ElementType.FIELD,
    ElementType.LOCAL_VARIABLE,
    ElementType.METHOD,
    ElementType.PARAMETER,
    ElementType.RECORD_COMPONENT,
    ElementType.TYPE_USE,
})
@TypeQualifierNickname
@Nonnull
public @interface NonNull {
}
