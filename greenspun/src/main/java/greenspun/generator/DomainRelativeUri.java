// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.io.File;
import java.nio.file.Path;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class DomainRelativeUri {
    DomainRelativeUri(final @NotNull Path path) {
        assert !path.isAbsolute();
        string = '/' + path.toString().replace(File.separatorChar, '/');
    }

    @Override
    public int hashCode() {
        return string.hashCode();
    }

    @Override
    public boolean equals(final @Nullable Object object) {
        return object instanceof DomainRelativeUri other && string.equals(other.string);
    }

    @Override
    public @NotNull String toString() {
        return string;
    }

    private final @NotNull String string;
}
