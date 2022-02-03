// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import greenspun.util.collection.seq.Seq;
import org.jetbrains.annotations.NotNull;

record VerificationError(@NotNull String message, @NotNull Seq<@NotNull Tag> ancestorTags) {
}
