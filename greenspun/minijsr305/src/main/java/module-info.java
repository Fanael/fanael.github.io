// Copyright © 2022  Fanael Linithien
// SPDX-License-Identifier: CC0-1.0

/**
 * Clean room minimal reimplementation of just enough of JSR 305 annotations to support greenspun's nullity annotations.
 */
@SuppressWarnings({"module", "JavaModuleNaming"})
module greenspun.minijsr305 {
    exports javax.annotation;
    exports javax.annotation.meta;
}
