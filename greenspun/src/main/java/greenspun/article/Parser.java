// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.article;

import java.math.BigInteger;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import greenspun.dom.Node;
import greenspun.sexp.Sexp;
import greenspun.sexp.Sexps;
import greenspun.sexp.reader.Reader;
import greenspun.util.Trace;
import greenspun.util.collection.ImmutableList;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;
import greenspun.util.condition.Unwind;
import greenspun.util.function.ThrowingFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The article parser: the primary means of turning S-expressions into parsed {@link Article} objects.
 */
public final class Parser {
    private Parser(final @NotNull Reader reader, final @NotNull HtslConverter htslConverter) {
        this.reader = reader;
        this.htslConverter = htslConverter;
    }

    /**
     * Parses the forms coming from the given S-expression {@code reader} into an {@link Article}.
     * The given {@code htslConverter} is used to convert HTSL forms into DOM tree {@link Node}s.
     * <p>
     * On error, a fatal condition is signaled:
     * <ul>
     * <li>{@link ArticleParseErrorCondition} is signaled if the article metadata couldn't be parsed.
     * <li>{@link SectionLinkingErrorCondition} is signaled if the article was parsed successfully, but contained
     * section linking errors, such as unreferenced sections or sections with multiple parents.
     * <li>Any condition type that {@link Reader} can signal, if there's a problem with parsing S-expressions
     * themselves.
     * <li>Any condition type that {@link HtslConverter} can signal, if there's a problem with converting HTSL forms
     * into DOM tree nodes.
     * </ul>
     *
     * @return The freshly parsed article.
     */
    public static @NotNull Article parseArticleForms(
        final @NotNull Reader reader,
        final @NotNull HtslConverter htslConverter
    ) throws Unwind {
        return new Parser(reader, htslConverter).parse();
    }

    private @NotNull Article parse() throws Unwind {
        final var article = parseDefarticle();
        parseDefsections();
        verifySectionGraph();
        return linkSections(article);
    }

    private @NotNull PartialArticle parseDefarticle() throws Unwind {
        try (final var trace = new Trace("Parsing the defarticle form")) {
            trace.use();
            return parseDefarticleImpl();
        }
    }

    private @NotNull PartialArticle parseDefarticleImpl() throws Unwind {
        final var form = reader.readTopLevelForm();
        if (form == null) {
            throw signalError("Cannot magically turn empty input into an article");
        }
        final var list = Sexps.asList(form);
        if (list == null || list.isEmpty() || list.get(0) != Sexp.KnownSymbol.DEFARTICLE) {
            throw signalError("This doesn't appear to be a valid defarticle form: " + Sexps.prettyPrint(form));
        }
        final var properties = extractProperties(list, 1, allowedDefarticleKeys);
        final var title = parseString(properties, Sexp.KnownSymbol.KW_TITLE);
        final var description = parseString(properties, Sexp.KnownSymbol.KW_DESCRIPTION);
        final var date = parseDate(properties);
        final var childIds = parseChildIds(properties);
        final var topics = parseTopics(properties);
        final var inhibitTableOfContents = parseBoolean(properties, Sexp.KnownSymbol.KW_INHIBIT_TABLE_OF_CONTENTS);
        final var rootSectionDom = htslConverter.convert(list.subList(properties.remainderOffset, list.size()));
        final var rootSection = new PartialSection(rootSectionId, "", childIds, rootSectionDom);
        sectionsById.put(rootSection.identifier, rootSection);
        return new PartialArticle(
            title,
            description,
            date,
            inhibitTableOfContents,
            topics,
            rootSection
        );
    }

    private void parseDefsections() throws Unwind {
        try (final var outerTrace = new Trace("Parsing defsection forms")) {
            outerTrace.use();
            int sectionCounter = 1; // Count sections for improved trace readability.
            for (@Nullable Sexp form; (form = reader.readTopLevelForm()) != null; sectionCounter += 1) {
                final var sectionIndex = sectionCounter;
                try (final var innerTrace = new Trace(() -> "Parsing defsection form #" + sectionIndex)) {
                    innerTrace.use();
                    parseDefsection(form);
                }
            }
        }
    }

    private void parseDefsection(final @NotNull Sexp form) throws Unwind {
        final var list = Sexps.asList(form);
        if (list == null || list.isEmpty() || list.get(0) != Sexp.KnownSymbol.DEFSECTION) {
            throw signalError("This doesn't appear to be a valid defsection form: " + Sexps.prettyPrint(form));
        }
        final var sectionIdSexp = list.get(1);
        final var sectionId = Sexps.asSymbol(sectionIdSexp);
        if (sectionId == null) {
            throw signalError(
                "This doesn't appear to be a valid section identifier: " + Sexps.prettyPrint(sectionIdSexp));
        }
        try (final var trace = new Trace(() -> "Parsing article section " + sectionId)) {
            trace.use();
            final var properties = extractProperties(list, 2, allowedDefsectionKeys);
            final var header = parseString(properties, Sexp.KnownSymbol.KW_HEADER);
            final var childIds = parseChildIds(properties);
            final var bodyDom = htslConverter.convert(list.subList(properties.remainderOffset, list.size()));
            final var section = new PartialSection(sectionId, header, childIds, bodyDom);
            if (sectionsById.put(sectionId, section) != null) {
                throw signalLinkingError("Duplicate section identifier found: " + sectionId);
            }
        }
    }

    private void verifySectionGraph() throws Unwind {
        try (final var trace = new Trace("Verifying section graph")) {
            trace.use();
            verifySectionGraphImpl();
        }
    }

    private void verifySectionGraphImpl() throws Unwind {
        final var references = new HashMap<Sexp.Symbol, Set<Sexp.Symbol>>();
        for (final var sectionId : sectionsById.keySet()) {
            references.put(sectionId, new HashSet<>());
        }
        // Use depth-first search to walk the section graph to compute the referred-from set of each section.
        final var seenSections = new HashSet<Sexp.Symbol>();
        final var stack = new ArrayList<SectionParent>();
        stack.add(new SectionParent(rootSectionId, null));
        while (!stack.isEmpty()) {
            final var reference = stack.remove(stack.size() - 1);
            final var sectionId = reference.section;
            final var parentId = reference.parent;
            if (parentId != null) {
                references.get(sectionId).add(parentId);
            }
            if (!seenSections.add(sectionId)) {
                continue;
            }
            for (final var childId : sectionsById.get(sectionId).childIds) {
                if (!sectionsById.containsKey(childId)) {
                    throw signalLinkingError("Reference to undefined section " + childId + " found in " + sectionId);
                }
                stack.add(new SectionParent(childId, sectionId));
            }
        }
        // Now we can look for potential errors.
        for (final var sectionReferences : references.entrySet()) {
            final var sectionId = sectionReferences.getKey();
            final var referredFromSet = sectionReferences.getValue();
            if (sectionId == rootSectionId) {
                if (!referredFromSet.isEmpty()) {
                    throw signalLinkingError("References to the root section found in sections " + referredFromSet);
                }
                continue;
            }
            final var referenceCount = referredFromSet.size();
            if (referenceCount == 0) {
                throw signalLinkingError("Section " + sectionId + " is unreachable from the root section");
            } else if (referenceCount != 1) {
                throw signalLinkingError(
                    "Multiple references to section " + sectionId + " in sections" + referredFromSet);
            }
        }
    }

    private @NotNull Article linkSections(final @NotNull PartialArticle article) {
        final var linkedRoot = linkSection(article.rootSection);
        return new Article(
            article.title,
            article.description,
            article.date,
            article.inhibitTableOfContents,
            article.topics,
            linkedRoot
        );
    }

    private @NotNull Section linkSection(final @NotNull PartialSection section) {
        final var linkedChildren = ImmutableList.map(section.childIds, childId -> {
            final var child = sectionsById.get(childId);
            assert child != null : "Reference to unknown section not found by graph verification?";
            return linkSection(child);
        });
        return new Section(section.identifier, section.header, linkedChildren, section.body);
    }

    private static @NotNull LocalDate parseDate(final @NotNull ExtractedProperties properties) throws Unwind {
        final var value = properties.get(Sexp.KnownSymbol.KW_DATE);
        final var list = Sexps.asList(value);
        if (list == null || list.size() != 3) {
            throw signalError("Property :date doesn't appear to be a 3-element list: " + Sexps.prettyPrint(value));
        }
        final var bigYear = parseInteger(list.get(0), "Year");
        final var bigMonth = parseInteger(list.get(1), "Month");
        final var bigDay = parseInteger(list.get(2), "Day");
        final var year = intValueExact(bigYear, "Year");
        final var month = intValueExact(bigMonth, "Month");
        final var day = intValueExact(bigDay, "Day");
        try {
            return LocalDate.of(year, month, day);
        } catch (final DateTimeException e) {
            throw signalError(e.getMessage());
        }
    }

    private static @NotNull ImmutableList<String> parseTopics(
        final @NotNull ExtractedProperties properties
    ) throws Unwind {
        return parseList(properties, Sexp.KnownSymbol.KW_TOPICS, (final @NotNull Sexp sexp) -> {
            final var topicName = Sexps.asString(sexp);
            if (topicName == null) {
                throw signalError("This doesn't appear to be a topic name: " + Sexps.prettyPrint(sexp));
            }
            return topicName;
        });
    }

    private static @NotNull ImmutableList<Sexp.Symbol> parseChildIds(
        final @NotNull ExtractedProperties properties
    ) throws Unwind {
        return parseList(properties, Sexp.KnownSymbol.KW_CHILDREN, (final @NotNull Sexp sexp) -> {
            final var childId = Sexps.asSymbol(sexp);
            if (childId == null) {
                throw signalError("This doesn't appear to be a child section identifier: " + Sexps.prettyPrint(sexp));
            }
            return childId;
        });
    }

    private static <T> @NotNull ImmutableList<T> parseList(
        final @NotNull ExtractedProperties properties,
        final @NotNull Sexp.KnownSymbol key,
        final @NotNull ThrowingFunction<Sexp, T, Unwind> function
    ) throws Unwind {
        final var value = properties.get(key);
        final var list = Sexps.asList(value);
        if (list == null) {
            throw signalError("Property " + key + " doesn't appear to be a list: " + Sexps.prettyPrint(value));
        }
        return ImmutableList.map(list, function);
    }

    private static @NotNull String parseString(
        final @NotNull ExtractedProperties properties,
        final @NotNull Sexp.KnownSymbol key
    ) throws Unwind {
        final var value = properties.get(key);
        final var string = Sexps.asString(value);
        if (string == null) {
            throw signalError("Property " + key + " doesn't appear to be a string: " + Sexps.prettyPrint(value));
        }
        return string;
    }

    @SuppressWarnings("SameParameterValue")
    private static boolean parseBoolean(
        final @NotNull ExtractedProperties properties,
        final @NotNull Sexp.KnownSymbol key
    ) throws Unwind {
        final var value = properties.get(key);
        if (Sexps.isNil(value)) {
            return false;
        } else if (value == Sexp.KnownSymbol.T) {
            return true;
        } else {
            throw signalError("Property " + key + " doesn't appear to be a boolean: " + Sexps.prettyPrint(value));
        }
    }

    private static @NotNull BigInteger parseInteger(
        final @NotNull Sexp sexp,
        final @NotNull String fieldName
    ) throws Unwind {
        final var integer = Sexps.asInteger(sexp);
        if (integer == null) {
            throw signalError(fieldName + " doesn't appear to be an integer: " + Sexps.prettyPrint(sexp));
        }
        return integer;
    }

    private static int intValueExact(final @NotNull BigInteger integer, final @NotNull String fieldName) throws Unwind {
        try {
            return integer.intValueExact();
        } catch (final ArithmeticException e) {
            throw signalError(fieldName + " number is too big to be a valid date component: " + integer);
        }
    }

    private static @NotNull ExtractedProperties extractProperties(
        final @NotNull ImmutableList<Sexp> list,
        final int startIndex,
        final @NotNull Set<? extends Sexp.Symbol> allowedKeys
    ) throws Unwind {
        final var properties = new HashMap<Sexp.Symbol, @NotNull Sexp>();
        int i = startIndex;
        for (final var size = list.size(); i < size; i += 2) {
            final var keyword = Sexps.asKeyword(list.get(i));
            if (keyword == null) {
                break;
            }
            if (!allowedKeys.contains(keyword)) {
                throw signalError("Property key " + keyword + " not allowed in this context");
            }
            final var value = (i + 1 < size) ? list.get(i + 1) : Sexp.KnownSymbol.NIL;
            if (properties.put(keyword, value) != null) {
                throw signalError("Duplicate value for property " + keyword);
            }
        }
        return new ExtractedProperties(properties, i);
    }

    private static @NotNull UnhandledErrorError signalError(final @NotNull String message) throws Unwind {
        return ConditionContext.error(new ArticleParseErrorCondition(message));
    }

    private static @NotNull UnhandledErrorError signalLinkingError(final @NotNull String message) throws Unwind {
        return ConditionContext.error(new SectionLinkingErrorCondition(message));
    }

    private static final EnumSet<Sexp.KnownSymbol> allowedDefarticleKeys = EnumSet.of(
        Sexp.KnownSymbol.KW_TITLE,
        Sexp.KnownSymbol.KW_DESCRIPTION,
        Sexp.KnownSymbol.KW_DATE,
        Sexp.KnownSymbol.KW_CHILDREN,
        Sexp.KnownSymbol.KW_INHIBIT_TABLE_OF_CONTENTS,
        Sexp.KnownSymbol.KW_TOPICS
    );
    private static final EnumSet<Sexp.KnownSymbol> allowedDefsectionKeys = EnumSet.of(
        Sexp.KnownSymbol.KW_HEADER,
        Sexp.KnownSymbol.KW_CHILDREN
    );
    private static final Sexp.RegularSymbol rootSectionId = new Sexp.RegularSymbol("#:root-section-id");

    private final @NotNull Reader reader;
    private final @NotNull HtslConverter htslConverter;
    private final HashMap<Sexp.Symbol, PartialSection> sectionsById = new HashMap<>();

    private record ExtractedProperties(@NotNull Map<Sexp.Symbol, Sexp> properties, int remainderOffset) {
        private @NotNull Sexp get(final @NotNull Sexp.Symbol symbol) {
            final var sexp = properties.get(symbol);
            return (sexp != null) ? sexp : Sexp.KnownSymbol.NIL;
        }
    }

    private record SectionParent(@NotNull Sexp.Symbol section, @Nullable Sexp.Symbol parent) {
    }

    private record PartialArticle(
        @NotNull String title,
        @NotNull String description,
        @NotNull LocalDate date,
        boolean inhibitTableOfContents,
        @NotNull ImmutableList<String> topics,
        @NotNull PartialSection rootSection
    ) {
    }

    private record PartialSection(
        @NotNull Sexp.Symbol identifier,
        @NotNull String header,
        @NotNull ImmutableList<Sexp.Symbol> childIds,
        @NotNull ImmutableList<Node> body
    ) {
    }
}
