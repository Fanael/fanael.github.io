// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util.condition;

import org.jetbrains.annotations.NotNull;

/**
 * An interface representing a lazily-evaluated trace message.
 */
@FunctionalInterface
public interface MessageSupplier {
    @NotNull String get();
}
