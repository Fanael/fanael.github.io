// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.article.PygmentsCache;
import greenspun.pygments.PygmentsServer;
import org.jetbrains.annotations.NotNull;

/**
 * A recording holding the state that can be safely shared between multiple worker threads.
 * <p>
 * Only add new fields to this record if they can be safely used from multiple threads at the same time.
 */
@SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
public record SharedState(@NotNull PygmentsServer pygmentsServer, @NotNull PygmentsCache pygmentsCache) {
}
