// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import greenspun.util.collection.seq.Seq;

record VerificationError(String message, Seq<Tag> ancestorTags) {
}
