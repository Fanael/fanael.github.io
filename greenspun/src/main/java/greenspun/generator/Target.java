// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.nio.file.Path;
import org.jetbrains.annotations.NotNull;

record Target(@NotNull Path sourcePath, @NotNull Path destinationPath) {
}
