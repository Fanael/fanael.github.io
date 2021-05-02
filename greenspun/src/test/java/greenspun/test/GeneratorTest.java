// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.Executors;
import greenspun.generator.Generator;
import greenspun.pygments.PygmentsCache;
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
        @TempDir final @NotNull Path destinationDirectory
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
        final var executorService = Executors.newSingleThreadExecutor();
        try (
            final var serverCode = ServerCodeTemporaryFile.save();
            final var server = new PygmentsServer(serverCode)
        ) {
            final var generator = new Generator(
                testResourcesPath.resolve("source"),
                destinationDirectory,
                executorService,
                new PygmentsCache(server)
            );
            generator.generate(Instant.EPOCH);
        } finally {
            executorService.shutdownNow();
        }
    }

    private static void compareFile(final @NotNull Path referenceFilePath, final @NotNull Path actualFilePath) {
        if (Files.isDirectory(referenceFilePath)) {
            Assertions.assertThat(actualFilePath).isDirectory();
        } else {
            Assertions.assertThat(actualFilePath)
                .usingCharset(StandardCharsets.UTF_8)
                .hasSameTextualContentAs(referenceFilePath, StandardCharsets.UTF_8);
        }
    }

    private static @NotNull List<Path> getFileList(final @NotNull Path directory) throws IOException {
        try (final var stream = Files.walk(directory)) {
            return stream.map(directory::relativize).sorted().toList();
        }
    }

    private static final Path testResourcesPath = Path.of("src/test/resources");
    private static final Path referenceDirectoryPath = testResourcesPath.resolve("reference");
}
