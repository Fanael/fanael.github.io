// Copyright © 2022  Fanael Linithien
// SPDX-License-Identifier: CC0-1.0
package javax.annotation.meta;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Target(ElementType.ANNOTATION_TYPE)
@Retention(RetentionPolicy.CLASS)
public @interface TypeQualifierDefault {
    ElementType[] value() default {};
}
