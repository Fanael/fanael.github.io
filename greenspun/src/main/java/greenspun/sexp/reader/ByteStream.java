// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.sexp.reader;

import java.io.IOException;
import java.io.InputStream;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * An input stream of bytes with single-byte lookahead.
 * <p>
 * This class provides the lookahead necessary for the S-expression {@link Reader} to work.
 */
public final class ByteStream {
    /**
     * Initializes a new byte stream that will read bytes from the given input stream.
     * <p>
     * The stream is buffered internally in order to provide lookahead, so the passed stream does not need to be
     * buffered for performance.
     */
    public ByteStream(final @NotNull InputStream stream) {
        this.stream = stream;
        buffer = new byte[streamBufferCapacity];
        position = 0;
        bufferSize = 0;
    }

    /**
     * Returns {@code true} iff this byte stream has reached the end.
     * <p>
     * If {@code false} is returned, it becomes safe to peek at the current byte by calling {@link #peek()} and
     * to discard it by calling {@link #discardPeek()}. After discarding, this method needs to be called again.
     * <p>
     * This method may perform read calls on the associated input stream to refill the internal buffer and to determine
     * if the end of input has actually been reached. If an I/O error occurs, the {@link IOException} is caught
     * and signaled as a fatal {@link IOExceptionCondition}.
     */
    public boolean reachedEnd() {
        return position >= bufferSize && refill().hitEof();
    }

    /**
     * Returns the current byte of this stream, without advancing the current position.
     * <p>
     * Calling this method without checking if the stream is at the end by calling {@link #reachedEnd()} first is
     * an error. This is not checked in any way.
     */
    public byte peek() {
        assert position < bufferSize;
        return buffer[position];
    }

    /**
     * Discards the current byte, advancing to the next one.
     * <p>
     * Calling this method without checking if the stream is at the end by calling {@link #reachedEnd()} first is
     * an error. This is not checked in any way.
     */
    public void discardPeek() {
        position += 1;
    }

    @NotNull HitEof skipToLineFeed() {
        while (true) {
            if (position < bufferSize) {
                final var lineFeedPosition = findLineFeed();
                if (lineFeedPosition != -1) {
                    position = lineFeedPosition;
                    return HitEof.NO;
                }
            }
            if (refill().hitEof()) {
                return HitEof.YES;
            }
        }
    }

    private int findLineFeed() {
        for (int i = position; i < bufferSize; i += 1) {
            if (buffer[i] == '\n') {
                return i;
            }
        }
        return -1;
    }

    @SuppressWarnings("ArrayEquality")
    private @NotNull HitEof refill() {
        // Don't try to refill if we've reached end of input already.
        if (buffer == endOfInputMarker) {
            return HitEof.YES;
        }
        if (stream != null) {
            final int numberOfBytesRead;
            try {
                numberOfBytesRead = stream.read(buffer);
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
            if (numberOfBytesRead > 0) {
                assert numberOfBytesRead <= buffer.length;
                position = 0;
                bufferSize = numberOfBytesRead;
                return HitEof.NO;
            }
        }
        // We've reached the end of input, drop the stream and the buffer so that the GC can collect them, and so that
        // further calls to this method will not attempt further refills.
        stream = null;
        buffer = endOfInputMarker;
        position = 0;
        bufferSize = 0;
        return HitEof.YES;
    }

    private static final int streamBufferCapacity = 8192;
    private static final byte[] endOfInputMarker = new byte[0];

    private @Nullable InputStream stream;
    private byte @NotNull [] buffer;
    private int position;
    private int bufferSize;
}
