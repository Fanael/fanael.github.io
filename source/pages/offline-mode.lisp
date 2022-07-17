;; Copyright © 2022  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Offline mode"
  :description ""
  :date (1970 01 01) ; ignored anyway
  :children (what-is-this how-to-use)
  ((script :defer t :src "/static/offline-control.js"))
  (p ((output :id "status-bar" :aria-live "polite") "JavaScript did not load."))
  (ul
   (li ((button :type "button" :disabled t :id "worker-toggle") "Enable offline mode"))
   (li ((button :type "button" :disabled t :id "fetch-all") "Update all pages"))
   (li ((button :type "button" :disabled t :id "clear-cache") "Clear offline mode cache"))))

(defsection what-is-this
  :header "What is this?"
  (p "Since this blog is just a static website, it should be easy for it to be fully readable even with no internet connection. After all, only approximately 0% of the population of this planet have access to cheap, reliable, fast, always-active internet connection. This has always been " (em "possible") " by cloning the git repository and running an " ((a :href "https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol") "HTTP") " server locally, but that hardly qualifies as " (em  "easy") ".")
  (p "However, certain features of modern browsers, introduced primarily for use with " ((a :href "https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps") "progressive web applications") ", make it possible with just a tiny bit of JavaScript: by registering a " ((a :href "https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API") "service worker") " it is possible for a site to manually performing any caching it desires using the " ((a :href "https://developer.mozilla.org/en-US/docs/Web/API/Cache") "cache interface") ".")
  (p "If you want to be able to read this blog while offline, because, for instance, you have access to fast internet only sporadically, then this feature is something made for you.")
  (info-box
   (p "Naturally, this feature requires storing the cached pages on your device. None of it ever " (em "leaves") " your device, it is only used locally by the service worker running locally in the browser, unless an undocumented \"feature\" of the browser causes it to be sent somewhere for some reason? For that, I can only recommend avoiding proprietary software and other forms of malware.")
   (p "The "
      ((a :href "/static/offline-control.js") " source code of the script controlling interaction on this page")
      " and the "
      ((a :href "/sw.js") " source code of the service worker itself")
      " are of course available, if you want to audit them before executing either, or if you want to know how it's done. It's just a small amount of vanilla JavaScript, with no fancy frameworks; it's not even " ((a :href "https://en.wikipedia.org/wiki/Minification_(programming)") "minified") " — they're so short that minification wouldn't save much after " ((a :href "https://en.wikipedia.org/wiki/HTTP_compression") "transfer compression") ".")))

(defsection how-to-use
  :header "How do I use it?"
  (p "Just enable it. After enabling, all visited pages from now on will be saved into the offline mode cache for later viewing. Requests for saved pages will be served from the cache if no network connection is present, if a server error occurs or if the network takes too long.")
  (p "If connected to a slow or intermittent network, requests are still made, in hope of acquiring the up-to-date version of the page. Even if the network ends up being too slow and the cached version is served, the request continues running in the background so that the cache can be updated with the up-to-date version of the page once it finishes.")
  (p "The " ((a :href "#fetch-all") "\"update all pages\" button") " simply downloads the current versions of all pages on the site and stores them in the cache, which is very handy for allowing all contents of the site to be viewed offline.")
  (p "The " ((a :href "#clear-cache") "\"clear offline mode cache\" button") " removes all pages stored by this site in the cache storage. This is roughly equivalent to removing all stored data for this site in browser settings.")
  (p "Note that all network requests performed by the offline mode service worker " (em "are still subject to the browser's own cache") ": the browser can simply return cached data for a \"network\" request, without ever involving the network proper, and it can ask the server if its cached version is still current, for example using the " ((a :href "https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since") (code "If-Modified-Since")) " header. The amount of data transferred should therefore be similar whether the offline mode is enabled or not."))
