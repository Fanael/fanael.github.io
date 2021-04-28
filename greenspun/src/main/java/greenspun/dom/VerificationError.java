// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import greenspun.util.collections.ImmutableList;
import org.jetbrains.annotations.NotNull;

record VerificationError(@NotNull String message, @NotNull ImmutableList<@NotNull Tag> ancestorTags) {
}
