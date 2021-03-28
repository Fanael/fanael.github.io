// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

enum HeaderRenderMode {
    SKIP(false),
    RENDER(true);

    HeaderRenderMode(final boolean shouldRender) {
        this.shouldRender = shouldRender;
    }

    public boolean shouldRender() {
        return shouldRender;
    }

    private final boolean shouldRender;
}
