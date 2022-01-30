// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later

module greenspun.auki {
    requires java.compiler;
    exports greenspun.auki.annotations;
    provides javax.annotation.processing.Processor with greenspun.auki.Processor;
}
