// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import greenspun.util.UnreachableCodeReachedError;

interface HeaderRenderMode {
    boolean shouldRender();

    String getTopicArchiveUri(String topicName);

    final class Skip implements HeaderRenderMode {
        private Skip() {
        }

        static Skip instance() {
            return instance;
        }

        @Override
        public boolean shouldRender() {
            return false;
        }

        @Override
        public String getTopicArchiveUri(final String topicName) {
            throw new UnreachableCodeReachedError("Article topics aren't supposed to be rendered at all");
        }

        private static final Skip instance = new Skip();
    }
}
