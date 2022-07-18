// Copyright © 2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
'use strict';

const scope = self.location.origin + '/';
const currentCacheName = 'v1';
const fetchFileListMessage = 'fetch-file-list';
const updateFileMessage = 'update-file';
const fileListPath = '/files.txt';
const fetchTimeout = 666;

async function deleteOldCaches() {
    function deleteCacheByName(name) {
        return (name === currentCacheName)
            ? Promise.resolve()
            : self.caches.delete(name);
    }

    const cacheNames = await self.caches.keys();
    await Promise.all(cacheNames.map(deleteCacheByName));
}

async function tryUpdateCache(cache, request, response) {
    const status = response.status;
    if(status >= 500 && status < 600) {
        return (await cache.match(request)) || response;
    }
    if(!response.ok) {
        return response;
    }
    await cache.put(request, response.clone());
    return response;
}

async function fetchImpl(request, event) {
    function delay(milliseconds) {
        return new Promise(resolve => setTimeout(resolve, milliseconds));
    }

    let promise = fetch(request.clone());
    const race = Promise.race([promise, delay(fetchTimeout)]);
    const cache = await self.caches.open(currentCacheName);
    let response;
    try {
        response = await race;
    } catch(e) {
        return (await cache.match(request)) || (() => { throw e; })();
    }
    if(response) {
        // The network won the race, use the network response.
        return tryUpdateCache(cache, request, response);
    }
    // The timeout won the race, try the cache if present, otherwise fall back
    // to the network.
    promise = promise.then(response => tryUpdateCache(cache, request, response));
    event.waitUntil(promise);
    return (await cache.match(request)) || promise;
}

async function fetchFileList() {
    let response;
    try {
        response = await fetch(fileListPath);
    } catch(e) {
        return null;
    }
    if(!response.ok) {
        return null;
    }
    return (await response.text()).split('\0');
}

async function updateFile(file) {
    const makeResult = status => ({file, status});

    const request = new Request(file);
    let response;
    try {
        response = await fetch(request.clone());
    } catch(e) {
        return makeResult('network-error');
    }
    const status = response.status;
    if(status >= 500 && status < 600) {
        return makeResult('server-error');
    }
    if(!response.ok) {
        return makeResult('http-error');
    }
    const cache = await self.caches.open(currentCacheName);
    await cache.put(request, response);
    return makeResult('ok');
}

self.addEventListener('install', () => {
    self.skipWaiting();
});

self.addEventListener('activate', event => {
    event.waitUntil(self.clients.claim());
    event.waitUntil(deleteOldCaches());
});

self.addEventListener('fetch', event => {
    const request = event.request;
    if(request.method !== 'GET' || !request.url.startsWith(scope)) {
        return;
    }
    event.respondWith(fetchImpl(request, event));
});

self.addEventListener('message', event => {
    const source = event.source;
    const message = event.data;
    switch(message.type) {
    case fetchFileListMessage:
        event.waitUntil(fetchFileList().then(result => source.postMessage(result)));
        break;
    case updateFileMessage:
        event.waitUntil(updateFile(message.file).then(result => source.postMessage(result)));
        break;
    }
});
