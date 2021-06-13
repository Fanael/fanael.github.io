// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.pygments;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import greenspun.dom.Node;
import greenspun.generator.Renderer;
import greenspun.util.condition.Unwind;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
    public PygmentsCache(final @NotNull PygmentsServer server) {
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
     * Highlights the syntax of the given code using the given language name for determining syntactic rules.
     * The given pretty name will be included in the DOM.
     * <p>
     * If successful, returns a DOM {@link Node} representing the highlighted code.
     * <p>
     * On error, a fatal condition is signaled, following the same contract as
     * {@link PygmentsServer#highlightCode(String, String)}.
     */
    public @NotNull Node highlightCode(
        final @NotNull String code,
        final @NotNull String languageName,
        final @NotNull String prettyName
    ) throws Unwind {
        final var digest = computeDigest(code, languageName, prettyName);
        final var cachedNode = map.get(digest);
        if (cachedNode != null) {
            cachedNode.generation = currentGeneration.get();
            return cachedNode.node;
        }
        final var node = Renderer.wrapHighlightedCode(server.highlightCode(code, languageName), prettyName);
        // If multiple threads try to add an entry with the same digest at the same time, just let the first one win
        // and use its DOM subtree, as DOM nodes are immutable anyway.
        final var newCachedNode = map.putIfAbsent(digest, new CachedNode(node, currentGeneration.get()));
        return (newCachedNode != null) ? newCachedNode.node : node;
    }

    private static @NotNull Digest computeDigest(
        final @NotNull String code,
        final @NotNull String languageName,
        final @NotNull String prettyName
    ) {
        final var digest = createSha256Digest();
        updateDigest(digest, languageName);
        updateDigest(digest, prettyName);
        updateDigest(digest, code);
        return new Digest(digest.digest());
    }

    private static @NotNull MessageDigest createSha256Digest() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (final NoSuchAlgorithmException e) {
            throw new AssertionError("SHA-256 not found despite being guaranteed by spec", e);
        }
    }

    private static void updateDigest(final @NotNull MessageDigest digest, final @NotNull String string) {
        final var bytes = string.getBytes(StandardCharsets.UTF_8);
        digest.update(toByteArray(bytes.length));
        digest.update(bytes);
    }

    private static byte[] toByteArray(final int integer) {
        final var result = new byte[4];
        asIntArray.set(result, 0, integer);
        return result;
    }

    private static final VarHandle asIntArray =
        MethodHandles.byteArrayViewVarHandle(int[].class, ByteOrder.LITTLE_ENDIAN).withInvokeExactBehavior();

    private final @NotNull PygmentsServer server;
    private final ConcurrentHashMap<Digest, CachedNode> map = new ConcurrentHashMap<>();
    private final AtomicInteger currentGeneration = new AtomicInteger(0);

    private static final class CachedNode {
        private CachedNode(final @NotNull Node node, final int generation) {
            this.node = node;
            this.generation = generation;
        }

        private final @NotNull Node node;
        private volatile int generation;
    }

    // We only keep a cryptographic hash of the (code, languageName, prettyName) tuple to avoid holding a reference to
    // the entire code string. Collisions are extremely unlikely: the probability of two digests colliding is about
    // 2^-128.
    private static final class Digest {
        private Digest(final byte[] bytes) {
            assert bytes.length == 32;
            this.bytes = bytes;
        }

        @Override
        public boolean equals(final @Nullable Object object) {
            return object instanceof Digest other && Arrays.equals(bytes, other.bytes);
        }

        @Override
        // VarHandle#get is a signature-polymorphic intrinsic, it doesn't actually allocate an array for varargs.
        @SuppressWarnings("ObjectInstantiationInEqualsHashCode")
        public int hashCode() {
            // Since what we have is the output of a cryptographic hash function, just take any four bytes.
            return (int) asIntArray.get(bytes, 0);
        }

        private final byte[] bytes;
    }
}
