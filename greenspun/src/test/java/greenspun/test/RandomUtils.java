// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.test;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;
import java.security.SecureRandom;
import java.util.random.RandomGenerator;
import java.util.random.RandomGeneratorFactory;

final class RandomUtils {
    private RandomUtils() {
    }

    static RandomGenerator createGenerator(final long seed) {
        return factory.create(seed);
    }

    static long generateRandomSeed() {
        return SeedGenerator.generateSeed();
    }

    private static final RandomGeneratorFactory<?> factory = RandomGeneratorFactory.of("L32X64MixRandom");

    private static final class SeedGenerator {
        private static long generateSeed() {
            final var bytes = new byte[Long.BYTES];
            secureRandom.nextBytes(bytes);
            return (long) longView.get(bytes, 0);
        }

        private static final SecureRandom secureRandom = new SecureRandom();
        private static final VarHandle longView =
            MethodHandles.byteArrayViewVarHandle(long[].class, ByteOrder.nativeOrder()).withInvokeExactBehavior();
    }
}
