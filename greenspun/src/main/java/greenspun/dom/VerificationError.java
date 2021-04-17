// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.util.collections.ImmutableList;
import org.jetbrains.annotations.NotNull;

@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
record VerificationError(@NotNull String message, @NotNull ImmutableList<@NotNull Tag> ancestorTags) {
}
