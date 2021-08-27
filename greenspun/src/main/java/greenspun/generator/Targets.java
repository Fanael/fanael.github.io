// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;


import java.nio.file.Path;
import java.util.ArrayList;
import org.jetbrains.annotations.NotNull;

record Targets(
    @NotNull ArrayList<@NotNull Path> filesToUnlink,
    @NotNull ArrayList<@NotNull Path> directoriesToUnlink,
    @NotNull ArrayList<@NotNull Path> directoriesToCreate,
    @NotNull ArrayList<@NotNull Target> staticTargets,
    @NotNull ArrayList<@NotNull Target> pageTargets,
    @NotNull ArrayList<@NotNull Target> articleTargets
) {
}
