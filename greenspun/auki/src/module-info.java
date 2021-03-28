// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later

module greenspun.auki {
    requires java.compiler;
    exports greenspun.auki.annotations;
    provides javax.annotation.processing.Processor with greenspun.auki.Processor;
}
