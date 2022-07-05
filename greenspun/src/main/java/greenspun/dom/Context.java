// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

enum Context implements ChildContext {
    ROOT("root"),
    HEAD_AND_BODY("head and body"),
    METADATA("metadata"),
    TEXT_ONLY("text only"),
    FLOW("flow"),
    PHRASING("phrasing"),
    LIST_ELEMENT("list element"),
    FIGURE("figure"),
    TABLE_SECTION("table section"),
    TABLE_ROW("table row"),
    TABLE_CELL("table cell");

    Context(final String readableName) {
        this.readableName = readableName;
    }

    @Override
    public String toString() {
        return readableName;
    }

    private final String readableName;
}
