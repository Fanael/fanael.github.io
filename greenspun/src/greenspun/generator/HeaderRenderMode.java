// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import greenspun.util.UnreachableCodeReachedError;
import org.jetbrains.annotations.NotNull;

interface HeaderRenderMode {
    boolean shouldRender();

    @NotNull String getTopicArchiveUri(@NotNull String topicName);

    final class Skip implements HeaderRenderMode {
        private Skip() {
        }

        static @NotNull Skip instance() {
            return instance;
        }

        @Override
        public boolean shouldRender() {
            return false;
        }

        @Override
        public @NotNull String getTopicArchiveUri(final @NotNull String topicName) {
            throw new UnreachableCodeReachedError("Article topics aren't supposed to be rendered at all");
        }

        private static final Skip instance = new Skip();
    }
}
