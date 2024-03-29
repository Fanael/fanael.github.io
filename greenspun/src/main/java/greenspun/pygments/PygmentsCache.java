// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.pygments;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import greenspun.dom.Node;
import greenspun.generator.Renderer;

/**
 * A concurrent cache of syntax-highlighted code snippets in DOM node form.
 * <p>
 * Since most syntax-highlighted code snippets don't change from one build to another, the entire DOM subtree
 * representing the code block is cached, to minimize the number of calls to the Pygments server.
 * <p>
 * As an invalidation strategy, this class uses explicit generations: the owner of this cache should call
 * {@link #nextGeneration()} every so often, to clean up entries that weren't referenced since the previous call to
 * that method.
 * <p>
 * This class is thread-safe: multiple threads can safely read and write to the same cache object at the same time with
 * no external synchronization.
 */
public final class PygmentsCache {
    /**
     * Initializes a new, empty Pygments cache that will use the given Pygments server for highlighting.
     */
    public PygmentsCache(final PygmentsServer server) {
        this.server = server;
    }

    /**
     * Advances the cache into the next generation, removing all entries that weren't referenced since the last call.
     */
    public void nextGeneration() {
        final var previousGeneration = currentGeneration.getAndAdd(1);
        final var currentGeneration = previousGeneration + 1;
        map.entrySet().removeIf(entry -> {
            final var entryGeneration = entry.getValue().generation;
            // Allow the current generation as well, in case there are concurrent writers.
            return entryGeneration != previousGeneration && entryGeneration != currentGeneration;
        });
    }

    /**
     * Highlights the syntax of the given code using the given language for determining syntactic rules.
     * The pretty name of the language will be included in the DOM.
     * <p>
     * If successful, returns a DOM {@link Node} representing the highlighted code.
     * <p>
     * On error, a fatal condition is signaled, following the same contract as
     * {@link PygmentsServer#highlightCode(String, Language)}.
     */
    public Node highlightCode(final String code, final Language language) {
        final var key = new CacheKey(code, language);
        final var cachedNode = map.get(key);
        if (cachedNode != null) {
            cachedNode.generation = currentGeneration.get();
            return cachedNode.node;
        }
        final var node = Renderer.wrapHighlightedCode(server.highlightCode(code, language), language.prettyName());
        // If multiple threads try to add an entry with the same key at the same time, just let the first one win
        // and use its DOM subtree, as DOM nodes are immutable anyway.
        final var newCachedNode = map.putIfAbsent(key, new CacheValue(node, currentGeneration.get()));
        return (newCachedNode != null) ? newCachedNode.node : node;
    }

    private final PygmentsServer server;
    private final ConcurrentHashMap<CacheKey, CacheValue> map = new ConcurrentHashMap<>();
    private final AtomicInteger currentGeneration = new AtomicInteger(0);

    private record CacheKey(String code, Language language) {
    }

    private static final class CacheValue {
        private CacheValue(final Node node, final int generation) {
            this.node = node;
            this.generation = generation;
        }

        private final Node node;
        private volatile int generation;
    }
}
