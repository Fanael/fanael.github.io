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
import java.util.ArrayList;
import java.util.HashSet;
import greenspun.util.PathUtils;
import greenspun.util.Trace;
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
        final var directoriesToCreate = new ArrayList<@NotNull Path>();
        final var staticFiles = new ArrayList<@NotNull Target>();
        discoverStaticFiles(directoriesToCreate, staticFiles);
        final var pages = discoverPages();
        final var articles = discoverArticles();
        final var filesToUnlink = new ArrayList<@NotNull Path>();
        final var directoriesToUnlink = new ArrayList<@NotNull Path>();
        discoverFilesToUnlink(filesToUnlink, directoriesToUnlink);
        return new Targets(filesToUnlink, directoriesToUnlink, directoriesToCreate, staticFiles, pages, articles);
    }

    private void discoverStaticFiles(
        final @NotNull ArrayList<@NotNull Path> directoriesToCreate,
        final @NotNull ArrayList<@NotNull Target> staticTargets
    ) {
        try (final var trace = new Trace("Discovering static files")) {
            trace.use();
            try {
                final var path = sourceDirectory.resolve(Generator.staticSubdirectoryName);
                Files.walkFileTree(path, new SimpleFileVisitor<>() {
                    @Override
                    public @NotNull FileVisitResult preVisitDirectory(
                        final @NotNull Path directory,
                        final @NotNull BasicFileAttributes attributes
                    ) {
                        final var relativePath = sourceDirectory.relativize(directory);
                        directoriesToCreate.add(relativePath);
                        knownDestinationPaths.add(relativePath);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public @NotNull FileVisitResult visitFile(
                        final @NotNull Path file,
                        final @NotNull BasicFileAttributes attributes
                    ) {
                        final var relativePath = sourceDirectory.relativize(file);
                        staticTargets.add(new Target(relativePath, relativePath));
                        knownDestinationPaths.add(relativePath);
                        return FileVisitResult.CONTINUE;
                    }
                });
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private @NotNull ArrayList<@NotNull Target> discoverArticles() {
        try (final var trace = new Trace("Discovering article files")) {
            trace.use();
            return discoverHtmlTargetsInDirectory(sourceDirectory);
        }
    }

    private @NotNull ArrayList<@NotNull Target> discoverPages() {
        try (final var trace = new Trace("Discovering non-article page files")) {
            trace.use();
            return discoverHtmlTargetsInDirectory(sourceDirectory.resolve(Generator.pagesSubdirectoryName));
        }
    }

    private void discoverFilesToUnlink(
        final @NotNull ArrayList<@NotNull Path> filesToUnlink,
        final @NotNull ArrayList<@NotNull Path> directoriesToUnlink
    ) {
        try (final var trace = new Trace("Discovering files to remove from destination directory")) {
            trace.use();
            try {
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
                            filesToUnlink.add(relativePath);
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
                            directoriesToUnlink.add(relativePath);
                        }
                        return FileVisitResult.CONTINUE;
                    }

                    private static boolean isDotFile(final @NotNull Path path) {
                        final var name = path.getFileName();
                        return name != null && name.toString().startsWith(".");
                    }
                });
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private @NotNull ArrayList<@NotNull Target> discoverHtmlTargetsInDirectory(final @NotNull Path directory) {
        try (final var stream = Files.newDirectoryStream(directory)) {
            final var targets = new ArrayList<@NotNull Target>();
            for (final var path : stream) {
                if (Files.isDirectory(path)) {
                    continue;
                }
                final var sourcePath = sourceDirectory.relativize(path);
                final var destinationPath = PathUtils.changeExtension(sourcePath, "html");
                targets.add(new Target(sourcePath, destinationPath));
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
}
