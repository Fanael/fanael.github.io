// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.util;

import java.nio.file.Path;
import org.jetbrains.annotations.NotNull;

/**
 * A utility class containing miscellaneous operations on paths.
 */
public final class PathUtils {
    private PathUtils() {
    }

    /**
     * Replace the file extension of the given path's file name with the given new extensions.
     * <p>
     * If the path has no file name, it is returned unchanged.
     * <p>
     * If the file name has no extension, the new extension is appended.
     */
    public static @NotNull Path changeExtension(final @NotNull Path path, final @NotNull String newExtension) {
        final var fileName = path.getFileName();
        if (fileName == null) {
            return path;
        }
        final var nameString = fileName.toString();
        final var dotIndex = nameString.lastIndexOf('.');
        final var newNameString = (dotIndex == -1)
            ? (nameString + '.' + newExtension)
            : (nameString.substring(0, dotIndex + 1) + newExtension);
        final var parent = path.getParent();
        return (parent == null)
            ? path.getFileSystem().getPath(newNameString)
            : parent.resolve(newNameString);
    }
}
