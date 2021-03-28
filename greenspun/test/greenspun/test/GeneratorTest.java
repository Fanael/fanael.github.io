// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import greenspun.article.PygmentsCache;
import greenspun.generator.Generator;
import greenspun.generator.SharedState;
import greenspun.pygments.PygmentsServer;
import greenspun.pygments.ServerCodeTemporaryFile;
import greenspun.util.condition.Unwind;
import org.assertj.core.api.Assertions;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

final class GeneratorTest {
    @Test
    void matchesReference(
        final @NotNull @TempDir Path destinationDirectory
    ) throws Unwind, InterruptedException, IOException {
        runGenerator(destinationDirectory);
        final var referenceFiles = getFileList(referenceDirectoryPath);
        final var actualFiles = getFileList(destinationDirectory);
        Assertions.assertThat(actualFiles).isEqualTo(referenceFiles);
        for (final var path : referenceFiles) {
            compareFile(referenceDirectoryPath.resolve(path), destinationDirectory.resolve(path));
        }
    }

    private static void runGenerator(final @NotNull Path destinationDirectory) throws Unwind, InterruptedException {
        try (
            final var serverCode = ServerCodeTemporaryFile.save();
            final var server = new PygmentsServer(serverCode.path())
        ) {
            final var sharedState = new SharedState(server, new PygmentsCache());
            final var generator = new Generator(
                testResourcesPath.resolve("source"),
                destinationDirectory,
                new TestExecutorService(),
                sharedState
            );
            generator.generate();
        }
    }

    private static void compareFile(final @NotNull Path referenceFilePath, final @NotNull Path actualFilePath) {
        if (Files.isDirectory(referenceFilePath)) {
            Assertions.assertThat(actualFilePath).isDirectory();
        } else {
            Assertions.assertThat(actualFilePath)
                .usingCharset(StandardCharsets.UTF_8)
                .hasSameTextualContentAs(referenceFilePath);
        }
    }

    private static @NotNull List<Path> getFileList(final @NotNull Path directory) throws IOException {
        try (final var stream = Files.walk(directory)) {
            return stream.map(directory::relativize).sorted().toList();
        }
    }

    private static final Path testResourcesPath = Path.of("test-resources");
    private static final Path referenceDirectoryPath = testResourcesPath.resolve("reference");
}
