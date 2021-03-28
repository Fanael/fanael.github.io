// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.article;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import greenspun.dom.Node;
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
    static @NotNull Digest computeDigest(final @NotNull String code, final @NotNull String languageName) {
        try {
            final var digest = MessageDigest.getInstance("SHA-256");
            digest.update(toByteArray(languageName.length()));
            digest.update(languageName.getBytes(StandardCharsets.UTF_8));
            digest.update(code.getBytes(StandardCharsets.UTF_8));
            return new Digest(digest.digest());
        } catch (final NoSuchAlgorithmException e) {
            throw new AssertionError("SHA-256 not found despite being guaranteed by spec", e);
        }
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

    @Nullable Node get(final @NotNull Digest digest) {
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

    @NotNull Node put(final @NotNull Digest digest, final @NotNull Node node) {
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

    private static byte[] toByteArray(final int integer) {
        return new byte[]{
            (byte) integer,
            (byte) (integer >> 8),
            (byte) (integer >> 16),
            (byte) (integer >> 24)
        };
    }

    private ConcurrentHashMap<Digest, @NotNull Node> previousGeneration = new ConcurrentHashMap<>();
    private ConcurrentHashMap<Digest, @NotNull Node> currentGeneration = new ConcurrentHashMap<>();
    // NB: this only protects map *references*, so only nextGeneration needs exclusive access.
    private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();

    // Keep only a cryptographic hash of the (code, languageName) pair to avoid holding a reference to the entire code
    // string. Collisions are extremely unlikely: the probability of two digests colliding is about 2^-128.
    @SuppressWarnings("ClassCanBeRecord") // It cannot, the constructor and bytes field need to be private.
    static final class Digest {
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
        public int hashCode() {
            // Since what we have is the output of a cryptographic hash function, just take any four bytes.
            return (bytes[0] & 0xFF) | ((bytes[1] & 0xFF) << 8) | ((bytes[2] & 0xFF) << 16) | ((bytes[3] & 0xFF) << 24);
        }

        private final byte[] bytes;
    }
}
