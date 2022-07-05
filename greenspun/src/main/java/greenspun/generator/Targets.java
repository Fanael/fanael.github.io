// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;


import java.nio.file.Path;
import greenspun.util.collection.seq.Seq;

record Targets(
    Seq<Path> filesToUnlink,
    Seq<Path> directoriesToUnlink,
    Seq<Path> directoriesToCreate,
    Seq<Target> staticTargets,
    Seq<Target> pageTargets,
    Seq<Target> articleTargets
) {
}
