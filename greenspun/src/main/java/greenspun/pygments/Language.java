// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.pygments;

import java.util.HashMap;
import java.util.Locale;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A language that can be highlighted with Pygments.
 * <p>
 * Instances of this class cannot be created directly, {@link #byPygmentsName(String)} must be used instead.
 */
public final class Language {
    private Language(final String pygmentsName, final String prettyName) {
        this.pygmentsName = pygmentsName;
        this.prettyName = prettyName;
    }

    /**
     * Returns a {@code Language} object with the given Pygments name, or {@code null} if no supported language matches.
     */
    public static @Nullable Language byPygmentsName(final String pygmentsName) {
        return languagesByName.get(toLowerCase(pygmentsName));
    }

    /**
     * Returns the internal Pygments name of this language.
     */
    public String pygmentsName() {
        return pygmentsName;
    }

    /**
     * Returns the pretty, human-readable name of this language.
     */
    public String prettyName() {
        return prettyName;
    }

    private static void addLanguage(final Language language) {
        assert language.pygmentsName.equals(toLowerCase(language.pygmentsName)) : "Non-lowercase language name";
        final var result = languagesByName.put(language.pygmentsName, language);
        assert result == null : "Duplicate language name";
    }

    private static String toLowerCase(final String string) {
        return string.toLowerCase(Locale.ROOT);
    }

    private static final HashMap<String, Language> languagesByName = new HashMap<>();

    private final String pygmentsName;
    private final String prettyName;

    static {
        addLanguage(new Language("c++", "C++"));
        addLanguage(new Language("common-lisp", "Common Lisp"));
        addLanguage(new Language("diff", "Unified diff"));
        addLanguage(new Language("java", "Java"));
        addLanguage(new Language("nasm", "x86 assembly"));
    }
}
