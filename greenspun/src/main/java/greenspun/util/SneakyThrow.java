// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.util;

import org.jetbrains.annotations.NotNull;

/**
 * Facilities for bypassing the checked exception mechanism.
 */
public final class SneakyThrow {
    private SneakyThrow() {
    }

    /**
     * Throws the given throwable as an <em>unchecked exception</em>, no matter its static nor dynamic type.
     * <p>
     * <strong>Use sparingly and with caution</strong>. Since the use of this method completely bypasses the checked
     * exception mechanism of Java, pervasive use has the potential to create unmaintainable mess. Prefer to limit the
     * use to exception types that either should arguably be unchecked in the first place, are so pervasive that
     * declaring them everywhere just adds noise because nigh-everything can throw that exception, or preferably both.
     * <p>
     * Examples of throwable types for which sneaky throws make sense are {@link InterruptedException} and
     * {@link greenspun.util.condition.Unwind}.
     * <p>
     * Since this method never returns normally, it's declared to return {@link UnreachableCodeReachedError} that can
     * be "thrown" at call sites to help the compiler's control flow analysis.
     */
    public static @NotNull UnreachableCodeReachedError doThrow(final @NotNull Throwable throwable) {
        throw doThrowImpl(throwable);
    }

    /**
     * Does nothing, pretending to throw exceptions of type indicated by the type parameter, allowing sneaky checked
     * exceptions to be caught at the call site.
     * <p>
     * Effectively the dual of {@link #doThrow(Throwable)}.
     */
    @SuppressWarnings({"RedundantThrows", "EmptyMethod"})
    public static <E extends Throwable> void pretendThrows() throws E {
    }

    // This works thanks to how type inference and generic type erasure work: E is erased to Throwable, so as far as
    // the JVM is concerned, the cast is completely redundant, and in fact doesn't exist at all in bytecode; but because
    // E is used in the throws clause, it's inferred to RuntimeException by the Java compiler absent any other way to
    // guide type inference, so callers that don't explicitly specify E effectively get the signature:
    // UnreachableCodeReachedError doThrowImpl(Throwable) throws RuntimeException.
    @SuppressWarnings("unchecked")
    private static <E extends Throwable> @NotNull UnreachableCodeReachedError doThrowImpl(
        final @NotNull Throwable throwable
    ) throws E {
        throw (E) throwable;
    }
}
