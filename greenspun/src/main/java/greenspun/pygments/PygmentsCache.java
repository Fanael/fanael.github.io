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
import java.util.concurrent.locks.ReentrantReadWriteLock;
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
        final var lock = readWriteLock.writeLock();
        lock.lock();
        try {
            previousGeneration = currentGeneration;
            currentGeneration = new ConcurrentHashMap<>();
        } finally {
            lock.unlock();
        }
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
        final var cachedNode = getCached(digest);
        if (cachedNode != null) {
            return cachedNode;
        }
        final var node = Renderer.wrapHighlightedCode(server.highlightCode(code, languageName), prettyName);
        return putCached(digest, node);
    }

    private @Nullable Node getCached(final @NotNull Digest digest) {
        final var lock = readWriteLock.readLock();
        lock.lock();
        try {
            final var node = currentGeneration.get(digest);
            if (node != null) {
                return node;
            }
            // The value may be present in the old generation. If it is, we need to promote it to the current generation
            // so that the next call to nextGeneration preserves it.
            final var oldNode = previousGeneration.get(digest);
            return (oldNode != null) ? putToCurrent(digest, oldNode) : null;
        } finally {
            lock.unlock();
        }
    }

    private @NotNull Node putCached(final @NotNull Digest digest, final @NotNull Node node) {
        final var lock = readWriteLock.readLock();
        lock.lock();
        try {
            return putToCurrent(digest, node);
        } finally {
            lock.unlock();
        }
    }

    private @NotNull Node putToCurrent(final @NotNull Digest digest, final @NotNull Node node) {
        // If multiple threads can try to add an entry with the same digest at the same time, just let the first one
        // win and use its DOM subtree, as DOM nodes are immutable anyway.
        final var oldValue = currentGeneration.putIfAbsent(digest, node);
        return (oldValue != null) ? oldValue : node;
    }

    private static @NotNull Digest computeDigest(
        final @NotNull String code,
        final @NotNull String languageName,
        final @NotNull String prettyName
    ) {
        try {
            final var digest = MessageDigest.getInstance("SHA-256");
            updateDigest(digest, languageName);
            updateDigest(digest, prettyName);
            updateDigest(digest, code);
            return new Digest(digest.digest());
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
    // NB: this only protects map *references*, so only nextGeneration needs exclusive access.
    private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    private ConcurrentHashMap<Digest, @NotNull Node> previousGeneration = new ConcurrentHashMap<>();
    private ConcurrentHashMap<Digest, @NotNull Node> currentGeneration = new ConcurrentHashMap<>();

    // We only keep a cryptographic hash of the (code, languageName, prettyName) tuple to avoid holding a reference to
    // the entire code string. Collisions are extremely unlikely: the probability of two digests colliding is about
    // 2^-128.
    private static final class Digest {
        private Digest(final byte[] bytes) {
            assert bytes.length == 32;
            this.bytes = bytes;
        }

        @Override
        public boolean equals(final @Nullable Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            final var other = (Digest) o;
            return Arrays.equals(bytes, other.bytes);
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
