// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.dom;

import java.util.List;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.jetbrains.annotations.NotNull;

/**
 * A record representing one DOM verification error.
 *
 * @param message      User-readable message explaining the error
 * @param ancestorTags List of ancestor tags of the place where the error occurred.
 *                     Ancestors are ordered starting from the outermost one.
 */
@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
public record VerificationError(@NotNull String message, @NotNull List<@NotNull Tag> ancestorTags) {
}
