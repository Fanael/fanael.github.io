// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import org.jetbrains.annotations.NotNull;

record ArchivedQuarter(@NotNull Quarter quarter, @NotNull DomainRelativeUri uri) {
}
