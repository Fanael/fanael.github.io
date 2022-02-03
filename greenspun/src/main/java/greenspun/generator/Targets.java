// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;


import java.nio.file.Path;
import greenspun.util.collection.seq.Seq;
import org.jetbrains.annotations.NotNull;

record Targets(
    @NotNull Seq<@NotNull Path> filesToUnlink,
    @NotNull Seq<@NotNull Path> directoriesToUnlink,
    @NotNull Seq<@NotNull Path> directoriesToCreate,
    @NotNull Seq<@NotNull Target> staticTargets,
    @NotNull Seq<@NotNull Target> pageTargets,
    @NotNull Seq<@NotNull Target> articleTargets
) {
}
