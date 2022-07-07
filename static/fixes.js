// Copyright © 2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
'use strict';

function updateTabindex() {
    var list = document.querySelectorAll('.code-block, figure .holder');
    Array.prototype.forEach.call(list, function (elt) {
        if(elt.scrollWidth > elt.clientWidth) {
            elt.tabIndex = 0;
        } else {
            elt.removeAttribute('tabindex');
        }
    });
}

function debounce(func, delay) {
    var timeoutId = 0;
    return function() {
        clearTimeout(timeoutId);
        timeoutId = setTimeout(func, delay);
    };
}

function addListeners() {
    document.addEventListener('DOMContentLoaded', updateTabindex);
    window.addEventListener('load', updateTabindex);
    window.addEventListener('resize', debounce(updateTabindex, 100));
}

function isFirefox() {
    return ('CSS' in window)
        ? CSS.supports('-moz-appearance', 'none')
        : (typeof InstallTrigger !== 'undefined');
}

if(!isFirefox()) {
    addListeners();
}
