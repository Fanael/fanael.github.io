// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.auki.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that a class is intended for subtyping or that a method is intended for overriding in subtypes.
 * <p>
 * Most classes are not designed with subtyping in mind, so our annotation processor enforces that classes and methods
 * are either {@code final} or explicitly open, either through being abstract or through an {@code @Open} annotation.
 * <p>
 * Abstract classes and methods could be annotated as {@code @Open}, but it's entirely redundant.
 */
@Documented
@Retention(RetentionPolicy.SOURCE)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Open {
}
