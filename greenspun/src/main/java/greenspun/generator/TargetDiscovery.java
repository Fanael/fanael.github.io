// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashSet;
import greenspun.util.PathUtils;
import greenspun.util.Trace;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class TargetDiscovery {
    TargetDiscovery(final @NotNull Path sourceDirectory, final @NotNull Path destinationDirectory) {
        this.sourceDirectory = sourceDirectory;
        this.destinationDirectory = destinationDirectory;
        knownDestinationPaths.add(Path.of(""));
        knownDestinationPaths.add(Path.of("index.html"));
        knownDestinationPaths.add(Path.of(RenderConstants.feedFileName));
        knownDestinationPaths.add(Path.of(Generator.staticSubdirectoryName));
        knownDestinationPaths.add(Path.of(Generator.pagesSubdirectoryName));
        knownDestinationPaths.add(Path.of(Generator.archivesSubdirectoryName));
    }

    @NotNull Targets discover() {
        final var staticTargets = discoverStaticFiles();
        final var pages = discoverPages();
        final var articles = discoverArticles();
        final var unlinkTargets = discoverFilesToUnlink();
        return new Targets(
            unlinkTargets.files,
            unlinkTargets.directories,
            staticTargets.directoriesToCreate,
            staticTargets.targets,
            pages,
            articles
        );
    }

    private @NotNull StaticTargets discoverStaticFiles() {
        try (final var trace = new Trace("Discovering static files")) {
            trace.use();
            try {
                final var path = sourceDirectory.resolve(Generator.staticSubdirectoryName);
                final var result = new StaticTargets();
                Files.walkFileTree(path, new SimpleFileVisitor<>() {
                    @Override
                    public @NotNull FileVisitResult preVisitDirectory(
                        final @NotNull Path directory,
                        final @NotNull BasicFileAttributes attributes
                    ) {
                        final var relativePath = sourceDirectory.relativize(directory);
                        result.directoriesToCreate = result.directoriesToCreate.appended(relativePath);
                        knownDestinationPaths.add(relativePath);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public @NotNull FileVisitResult visitFile(
                        final @NotNull Path file,
                        final @NotNull BasicFileAttributes attributes
                    ) {
                        final var relativePath = sourceDirectory.relativize(file);
                        result.targets = result.targets.appended(new Target(relativePath, relativePath));
                        knownDestinationPaths.add(relativePath);
                        return FileVisitResult.CONTINUE;
                    }
                });
                return result;
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private @NotNull Seq<@NotNull Target> discoverArticles() {
        try (final var trace = new Trace("Discovering article files")) {
            trace.use();
            return discoverHtmlTargetsInDirectory(sourceDirectory);
        }
    }

    private @NotNull Seq<@NotNull Target> discoverPages() {
        try (final var trace = new Trace("Discovering non-article page files")) {
            trace.use();
            return discoverHtmlTargetsInDirectory(sourceDirectory.resolve(Generator.pagesSubdirectoryName));
        }
    }

    private @NotNull UnlinkTargets discoverFilesToUnlink() {
        try (final var trace = new Trace("Discovering files to remove from destination directory")) {
            trace.use();
            try {
                final var result = new UnlinkTargets();
                Files.walkFileTree(destinationDirectory, new SimpleFileVisitor<>() {
                    @Override
                    public @NotNull FileVisitResult preVisitDirectory(
                        final @NotNull Path directory,
                        final @NotNull BasicFileAttributes attributes
                    ) {
                        return isDotFile(directory) ? FileVisitResult.SKIP_SUBTREE : FileVisitResult.CONTINUE;
                    }

                    @Override
                    public @NotNull FileVisitResult visitFile(
                        final @NotNull Path file,
                        final @NotNull BasicFileAttributes attributes
                    ) {
                        final var relativePath = destinationDirectory.relativize(file);
                        if (!isDotFile(file) && !knownDestinationPaths.contains(relativePath)) {
                            result.files = result.files.appended(relativePath);
                        }
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public @NotNull FileVisitResult postVisitDirectory(
                        final @NotNull Path directory, final @Nullable IOException exception
                    ) throws IOException {
                        if (exception != null) {
                            throw exception;
                        }
                        final var relativePath = destinationDirectory.relativize(directory);
                        if (!knownDestinationPaths.contains(relativePath)) {
                            result.directories = result.directories.appended(relativePath);
                        }
                        return FileVisitResult.CONTINUE;
                    }

                    private static boolean isDotFile(final @NotNull Path path) {
                        final var name = path.getFileName();
                        return name != null && name.toString().startsWith(".");
                    }
                });
                return result;
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private @NotNull Seq<@NotNull Target> discoverHtmlTargetsInDirectory(final @NotNull Path directory) {
        try (final var stream = Files.newDirectoryStream(directory)) {
            var targets = Seq.<@NotNull Target>empty();
            for (final var path : stream) {
                if (Files.isDirectory(path)) {
                    continue;
                }
                final var sourcePath = sourceDirectory.relativize(path);
                final var destinationPath = PathUtils.changeExtension(sourcePath, "html");
                targets = targets.appended(new Target(sourcePath, destinationPath));
                knownDestinationPaths.add(destinationPath);
            }
            return targets;
        } catch (final DirectoryIteratorException e) {
            throw ConditionContext.error(new IOExceptionCondition(e.getCause()));
        } catch (final IOException e) {
            throw ConditionContext.error(new IOExceptionCondition(e));
        }
    }

    private final @NotNull Path sourceDirectory;
    private final @NotNull Path destinationDirectory;
    private final HashSet<Path> knownDestinationPaths = new HashSet<>();

    private static final class StaticTargets {
        private @NotNull Seq<@NotNull Path> directoriesToCreate = Seq.empty();
        private @NotNull Seq<@NotNull Target> targets = Seq.empty();
    }

    private static final class UnlinkTargets {
        private @NotNull Seq<@NotNull Path> files = Seq.empty();
        private @NotNull Seq<@NotNull Path> directories = Seq.empty();
    }
}
