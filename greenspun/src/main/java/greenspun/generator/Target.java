// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.nio.file.Path;

record Target(Path sourcePath, Path destinationPath) {
    DomainRelativeUri destinationUri() {
        return new DomainRelativeUri(destinationPath);
    }
}
