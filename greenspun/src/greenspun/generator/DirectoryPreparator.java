// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import greenspun.util.Trace;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@SuppressWarnings("ClassCanBeRecord")
final class DirectoryPreparator {
    DirectoryPreparator(final @NotNull Path sourceDirectory, final @NotNull Path destinationDirectory) {
        this.sourceDirectory = sourceDirectory;
        this.destinationDirectory = destinationDirectory;
    }

    @NotNull List<Path> getArticleSourcePaths() throws Unwind {
        try (final var trace = new Trace("Collecting source paths of articles")) {
            trace.use();
            return getSourceDirectoryRelativePaths(sourceDirectory);
        }
    }

    @NotNull List<Path> getPageSourcePaths() throws Unwind {
        try (final var trace = new Trace("Collecting source paths of non-article pages")) {
            trace.use();
            return getSourceDirectoryRelativePaths(sourceDirectory.resolve(Generator.pagesSubdirectoryName));
        }
    }

    void prepareDestinationDirectory() throws Unwind {
        try (final var trace = new Trace(() -> "Preparing destination directory " + destinationDirectory)) {
            trace.use();
            createDestinationDirectory();
            cleanUpDestinationDirectory();
            copyStaticFilesToDestinationDirectory();
            createPageDirectory();
            createArchiveDirectory();
        }
    }

    private @NotNull List<Path> getSourceDirectoryRelativePaths(final @NotNull Path directoryPath) throws Unwind {
        try (final var directory = Files.newDirectoryStream(directoryPath)) {
            final var paths = new ArrayList<@NotNull Path>();
            for (final var path : directory) {
                if (Files.isDirectory(path)) {
                    continue;
                }
                paths.add(sourceDirectory.relativize(path));
            }
            return paths;
        } catch (final DirectoryIteratorException e) {
            throw ConditionContext.error(new IOExceptionCondition(e.getCause()));
        } catch (final IOException e) {
            throw ConditionContext.error(new IOExceptionCondition(e));
        }
    }

    private void createDestinationDirectory() throws Unwind {
        try (final var trace = new Trace(() -> "Creating destination directory " + destinationDirectory)) {
            trace.use();
            createDirectory(destinationDirectory);
        }
    }

    private void cleanUpDestinationDirectory() throws Unwind {
        try (final var trace = new Trace(() -> "Cleaning up destination directory " + destinationDirectory)) {
            trace.use();
            try {
                Files.walkFileTree(destinationDirectory, new CleanUpFileVisitor());
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }

    }

    private void copyStaticFilesToDestinationDirectory() throws Unwind {
        try (final var trace = new Trace(() ->
            "Copying static files to destination directory " + destinationDirectory)
        ) {
            trace.use();
            try {
                Files.walkFileTree(sourceDirectory.resolve("static"), new CopyStaticFilesFileVisitor());
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private void createPageDirectory() throws Unwind {
        try (final var trace = new Trace(() -> "Creating pages directory in " + destinationDirectory)) {
            trace.use();
            createDirectory(destinationDirectory.resolve(Generator.pagesSubdirectoryName));
        }
    }

    private void createArchiveDirectory() throws Unwind {
        try (final var trace = new Trace(() -> "Create archive directory in " + destinationDirectory)) {
            trace.use();
            createDirectory(destinationDirectory.resolve(Generator.archivesSubdirectoryName));
        }
    }

    private static void createDirectory(final @NotNull Path path) throws Unwind {
        try (final var trace = new Trace(() -> "Creating directory " + path)) {
            trace.use();
            try {
                Files.createDirectory(path);
            } catch (final FileAlreadyExistsException e) {
                // No need to do anything if it already exists.
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private final @NotNull Path sourceDirectory;
    private final @NotNull Path destinationDirectory;

    private final class CleanUpFileVisitor extends SimpleFileVisitor<Path> {
        @Override
        public @NotNull FileVisitResult preVisitDirectory(
            final @NotNull Path directoryPath,
            final @NotNull BasicFileAttributes attributes
        ) {
            return isDotFile(directoryPath)
                ? FileVisitResult.SKIP_SUBTREE
                : FileVisitResult.CONTINUE;
        }

        @Override
        public @NotNull FileVisitResult visitFile(
            final @NotNull Path filePath,
            final @NotNull BasicFileAttributes attributes
        ) throws IOException {
            if (!isDotFile(filePath)) {
                Files.delete(filePath);
            }
            return FileVisitResult.CONTINUE;
        }

        @Override
        public @NotNull FileVisitResult postVisitDirectory(
            final @NotNull Path directoryPath,
            final @Nullable IOException exception
        ) throws IOException {
            if (exception != null) {
                throw exception;
            }
            if (!isDotFile(directoryPath) && !directoryPath.equals(destinationDirectory)) {
                Files.delete(directoryPath);
            }
            return FileVisitResult.CONTINUE;
        }

        private static boolean isDotFile(final @NotNull Path path) {
            final var name = path.getFileName();
            return name != null && name.toString().startsWith(".");
        }
    }

    private final class CopyStaticFilesFileVisitor extends SimpleFileVisitor<Path> {
        @Override
        public @NotNull FileVisitResult preVisitDirectory(
            final @NotNull Path directoryPath,
            final @NotNull BasicFileAttributes attributes
        ) throws IOException {
            try {
                Files.createDirectory(mirrorPath(directoryPath));
            } catch (final FileAlreadyExistsException e) {
                // No need to do anything if it already exists.
            }
            return FileVisitResult.CONTINUE;
        }

        @Override
        public @NotNull FileVisitResult visitFile(
            final @NotNull Path filePath,
            final @NotNull BasicFileAttributes attributes
        ) throws IOException {
            Files.copy(filePath, mirrorPath(filePath));
            return FileVisitResult.CONTINUE;
        }

        private @NotNull Path mirrorPath(final @NotNull Path path) {
            return destinationDirectory.resolve(sourceDirectory.relativize(path));
        }
    }
}
