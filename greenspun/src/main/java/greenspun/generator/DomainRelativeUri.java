// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.io.File;
import java.nio.file.Path;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.checkerframework.checker.nullness.qual.Nullable;

final class DomainRelativeUri {
    DomainRelativeUri(final Path path) {
        assert !path.isAbsolute();
        string = '/' + path.toString().replace(File.separatorChar, '/');
    }

    @Override
    public int hashCode() {
        return string.hashCode();
    }

    @Override
    @SuppressFBWarnings(value = "NP_METHOD_PARAMETER_TIGHTENS_ANNOTATION", justification = "False positive")
    public boolean equals(final @Nullable Object object) {
        return object instanceof DomainRelativeUri other && string.equals(other.string);
    }

    @Override
    public String toString() {
        return string;
    }

    private final String string;
}
