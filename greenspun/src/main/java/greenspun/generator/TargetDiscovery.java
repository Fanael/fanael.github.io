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
import org.checkerframework.checker.nullness.qual.Nullable;

final class TargetDiscovery {
    TargetDiscovery(final Path sourceDirectory, final Path destinationDirectory) {
        this.sourceDirectory = sourceDirectory;
        this.destinationDirectory = destinationDirectory;
        knownDestinationPaths.add(Path.of(""));
        knownDestinationPaths.add(Path.of("index.html"));
        knownDestinationPaths.add(Path.of(RenderConstants.feedFileName));
        knownDestinationPaths.add(Path.of(Generator.staticSubdirectoryName));
        knownDestinationPaths.add(Path.of(Generator.pagesSubdirectoryName));
        knownDestinationPaths.add(Path.of(Generator.archivesSubdirectoryName));
    }

    Targets discover() {
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

    private StaticTargets discoverStaticFiles() {
        try (final var trace = new Trace("Discovering static files")) {
            trace.use();
            try {
                final var path = sourceDirectory.resolve(Generator.staticSubdirectoryName);
                final var directoriesToCreate = new Seq.Builder<Path>();
                final var targets = new Seq.Builder<Target>();
                Files.walkFileTree(path, new SimpleFileVisitor<>() {
                    @Override
                    public FileVisitResult preVisitDirectory(
                        final Path directory,
                        final BasicFileAttributes attributes
                    ) {
                        final var relativePath = sourceDirectory.relativize(directory);
                        directoriesToCreate.append(relativePath);
                        knownDestinationPaths.add(relativePath);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult visitFile(final Path file, final BasicFileAttributes attributes) {
                        final var relativePath = sourceDirectory.relativize(file);
                        targets.append(new Target(relativePath, relativePath));
                        knownDestinationPaths.add(relativePath);
                        return FileVisitResult.CONTINUE;
                    }
                });
                return new StaticTargets(directoriesToCreate.toSeq(), targets.toSeq());
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private Seq<Target> discoverArticles() {
        try (final var trace = new Trace("Discovering article files")) {
            trace.use();
            return discoverHtmlTargetsInDirectory(sourceDirectory);
        }
    }

    private Seq<Target> discoverPages() {
        try (final var trace = new Trace("Discovering non-article page files")) {
            trace.use();
            return discoverHtmlTargetsInDirectory(sourceDirectory.resolve(Generator.pagesSubdirectoryName));
        }
    }

    private UnlinkTargets discoverFilesToUnlink() {
        try (final var trace = new Trace("Discovering files to remove from destination directory")) {
            trace.use();
            try {
                final var files = new Seq.Builder<Path>();
                final var directories = new Seq.Builder<Path>();
                Files.walkFileTree(destinationDirectory, new SimpleFileVisitor<>() {
                    @Override
                    public FileVisitResult preVisitDirectory(
                        final Path directory,
                        final BasicFileAttributes attributes
                    ) {
                        return isDotFile(directory) ? FileVisitResult.SKIP_SUBTREE : FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult visitFile(final Path file, final BasicFileAttributes attributes) {
                        final var relativePath = destinationDirectory.relativize(file);
                        if (!isDotFile(file) && !knownDestinationPaths.contains(relativePath)) {
                            files.append(relativePath);
                        }
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult postVisitDirectory(
                        final Path directory,
                        final @Nullable IOException exception
                    ) throws IOException {
                        if (exception != null) {
                            throw exception;
                        }
                        final var relativePath = destinationDirectory.relativize(directory);
                        if (!knownDestinationPaths.contains(relativePath)) {
                            directories.append(relativePath);
                        }
                        return FileVisitResult.CONTINUE;
                    }

                    private static boolean isDotFile(final Path path) {
                        final var name = path.getFileName();
                        return name != null && name.toString().startsWith(".");
                    }
                });
                return new UnlinkTargets(files.toSeq(), directories.toSeq());
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private Seq<Target> discoverHtmlTargetsInDirectory(final Path directory) {
        try (final var stream = Files.newDirectoryStream(directory)) {
            final var targets = new Seq.Builder<Target>();
            for (final var path : stream) {
                if (Files.isDirectory(path)) {
                    continue;
                }
                final var sourcePath = sourceDirectory.relativize(path);
                final var destinationPath = PathUtils.changeExtension(sourcePath, "html");
                targets.append(new Target(sourcePath, destinationPath));
                knownDestinationPaths.add(destinationPath);
            }
            return targets.toSeq();
        } catch (final DirectoryIteratorException e) {
            throw ConditionContext.error(new IOExceptionCondition(e.getCause()));
        } catch (final IOException e) {
            throw ConditionContext.error(new IOExceptionCondition(e));
        }
    }

    private final Path sourceDirectory;
    private final Path destinationDirectory;
    private final HashSet<Path> knownDestinationPaths = new HashSet<>();

    private record StaticTargets(Seq<Path> directoriesToCreate, Seq<Target> targets) {
    }

    private record UnlinkTargets(Seq<Path> files, Seq<Path> directories) {
    }
}
