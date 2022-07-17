// Copyright © 2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
'use strict';

const statusBar = document.getElementById('status-bar');
const workerToggle = document.getElementById('worker-toggle');
const fetchAll = document.getElementById('fetch-all');
const clearCache = document.getElementById('clear-cache');
const fetchAllMessage = 'FETCH_ALL';

const state = (() => {
    let isActive = false;
    return {
        get value() {
            return isActive;
        },
        enable() {
            isActive = true;
            workerToggle.textContent = 'Disable offline mode';
        },
        disable() {
            isActive = false;
            workerToggle.textContent = 'Enable offline mode';
        },
    };
})();

function setStatus(newStatus) {
    statusBar.textContent = newStatus;
}

function toggleServiceWorker() {
    async function registerServiceWorker() {
        try {
            await navigator.serviceWorker.register('/sw.js', {scope: '/', updateViaCache: 'all'});
        } catch(e) {
            setStatus('An error occurred while trying to enable offline mode.');
            throw e;
        }
        setStatus('Service worker registered, offline mode enabled successfully.');
        state.enable();
    }

    async function unregisterServiceWorker() {
        const registration = await navigator.serviceWorker.getRegistration();
        try {
            if(registration) {
                await registration.unregister();
            }
        } catch(e) {
            setStatus('An error occurred while trying to disable offline mode.');
            throw e;
        }
        setStatus('Offline mode disabled successfully.');
        state.disable();
    }

    if(state.value) {
        unregisterServiceWorker();
    } else {
        registerServiceWorker();
    }
}

function sendFetchAllMessage() {
    function updateStatus(event) {
        const response = event.data;
        if(response === null) {
            setStatus('Fetching all pages failed: could not retrieve the list of files.');
            return;
        }

        const results = [];
        function addResult(key, tail) {
            const value = response.get(key) || 0;
            if(value > 0) {
                results.push(value + ' ' + tail);
            }
        }

        addResult('ok', 'succeeded');
        addResult('network-error', 'failed with a network error');
        addResult('server-error', 'failed with a server error');
        addResult('http-error', 'failed with an HTTP error');
        const total = response.get('total') || 0;
        const statusStart = 'Attempted to fetch ' + total + ' file' + (total === 1 ? '' : 's') + ': ';
        setStatus(statusStart + results.join(', ') + '.');
    }

    const worker = navigator.serviceWorker;
    if(!worker.controller) {
        setStatus('Offline mode is disabled, cannot fetch all pages.');
        state.disable();
        return;
    }
    worker.addEventListener('message', updateStatus, {once: true});
    worker.controller.postMessage({type: fetchAllMessage});
}

async function clearCacheImpl() {
    function deleteCacheEntry(key) {
        // NB: intentionally ignore errors.
        return caches.delete(key).catch(() => {});
    }

    try {
        const keys = await caches.keys();
        await Promise.all(keys.map(deleteCacheEntry));
        setStatus('Offline mode cache cleared successfully.');
    } catch(e) {
        setStatus('An error occurred while trying to clear the cache.');
        throw e;
    }
}

function initialize() {
    if(!('serviceWorker' in navigator)) {
        setStatus('Service workers not supported, are you in private browsing mode?');
        return;
    }
    const serviceWorker = navigator.serviceWorker;
    if(!serviceWorker.controller) {
        setStatus('Offline mode is currently disabled.');
    }
    serviceWorker.ready.then(() => {
        setStatus('Offline mode is currently enabled, service worker is active.');
        state.enable();
    });
    workerToggle.addEventListener('click', toggleServiceWorker);
    fetchAll.addEventListener('click', sendFetchAllMessage);
    clearCache.addEventListener('click', clearCacheImpl);
    workerToggle.disabled = false;
    fetchAll.disabled = false;
    clearCache.disabled = false;
}

document.addEventListener('DOMContentLoaded', initialize);
