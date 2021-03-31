// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import java.util.List;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;

@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
record VerificationError(@NotNull String message, @NotNull List<@NotNull Tag> ancestorTags) {
}
