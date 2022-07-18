// Copyright © 2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
'use strict';

const statusBar = document.getElementById('status-bar');
const workerToggle = document.getElementById('worker-toggle');
const fetchAll = document.getElementById('fetch-all');
const clearCache = document.getElementById('clear-cache');
const fetchFileListMessage = 'fetch-file-list';
const updateFileMessage = 'update-file';

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

async function toggleServiceWorker() {
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
        await unregisterServiceWorker();
    } else {
        await registerServiceWorker();
    }
}

class CacheUpdater {
    constructor(worker) {
        this.worker = worker;
        this.files = null;
        this.promises = [];
        this.deferredByPath = new Map();
        this.statusCounters = new Map();
        this.statusThrottled = false;
        this.filesProcessed = 0;
        this.progressSpan = document.createElement('span');
        this.progressSpan.ariaLive = 'polite';
    }

    async updateCache() {
        setStatus('Fetching the list of files…');
        this.files = await this.loadFileList();
        if(this.files === null) {
            setStatus('Updating all pages failed, could not fetch the list of files.');
            return;
        }
        this.createFilePromises();
        await this.updateFiles();
        this.setFinalStatus();
    }

    async updateFiles() {
        setStatus('Update in progress…');
        statusBar.insertBefore(this.progressSpan, statusBar.childNodes[0]);
        const handler = this.processResponse.bind(this);
        this.worker.addEventListener('message', handler);
        try {
            for(const file of this.files) {
                this.worker.controller.postMessage({type: updateFileMessage, file});
            }
            await Promise.all(this.promises);
        } finally {
            this.worker.removeEventListener('message', handler);
        }
    }

    processResponse(event) {
        const response = event.data;
        const file = response.file;
        const resolve = this.deferredByPath.get(file);
        if(!resolve) {
            return;
        }
        this.deferredByPath.delete(file);
        this.filesProcessed += 1;
        this.updateProgress();
        this.countResponseStatus(response.status);
        resolve();
    }

    updateProgress() {
        if(this.statusThrottled) {
            return;
        }
        this.progressSpan.textContent = this.filesProcessed + ' of ' + this.files.length + ' done. ';
        const currentProcessed = this.filesProcessed;
        this.statusThrottled = true;
        const stopThrottling = () => {
            this.statusThrottled = false;
            if(this.filesProcessed !== currentProcessed) {
                this.updateProgress();
            }
        };
        setTimeout(stopThrottling, 1350);
    }

    setFinalStatus() {
        const results = [];
        const addResult = (status, tail) => {
            const value = this.statusCounters.get(status) || 0;
            if(value > 0) {
                results.push(value + ' ' + tail);
            }
        };

        addResult('ok', 'succeeded');
        addResult('network-error', 'failed with a network error');
        addResult('server-error', 'failed with a server error');
        addResult('http-error', 'failed with an HTTP error');
        const total = this.files.length;
        const statusStart = 'Updating ' + total + ' file' + (total === 1 ? '' : 's') + ' complete: ';
        setStatus(statusStart + results.join(', ') + '.');
    }

    countResponseStatus(status) {
        const previous = this.statusCounters.get(status) || 0;
        this.statusCounters.set(status, previous + 1);
    }

    createFilePromises() {
        for(const file of this.files) {
            const [promise, resolve] = this.makeDeferred();
            this.promises.push(promise);
            this.deferredByPath.set(file, resolve);
        }
    }

    loadFileList() {
        const [promise, resolve] = this.makeDeferred();
        this.worker.addEventListener('message', event => resolve(event.data), {once: true});
        this.worker.controller.postMessage({type: fetchFileListMessage});
        return promise;
    }

    makeDeferred() {
        let resolve;
        const promise = new Promise(r => resolve = r);
        return [promise, resolve];
    }
}

async function updateCache() {
    const worker = navigator.serviceWorker;
    if(!worker.controller) {
        setStatus('Offline mode is disabled, cannot fetch all pages.');
        state.disable();
        return;
    }
    const updater = new CacheUpdater(worker);
    await updater.updateCache();
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
    fetchAll.addEventListener('click', updateCache);
    clearCache.addEventListener('click', clearCacheImpl);
    workerToggle.disabled = false;
    fetchAll.disabled = false;
    clearCache.disabled = false;
}

document.addEventListener('DOMContentLoaded', initialize);
