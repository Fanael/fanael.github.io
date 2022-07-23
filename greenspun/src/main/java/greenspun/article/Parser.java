// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.article;

import java.math.BigInteger;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import greenspun.dom.Node;
import greenspun.sexp.Sexp;
import greenspun.sexp.Sexps;
import greenspun.sexp.reader.Reader;
import greenspun.util.Trace;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.UnhandledErrorError;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The article parser: the primary means of turning S-expressions into parsed {@link Article} objects.
 */
public final class Parser {
    private Parser(final Reader reader, final HtslConverter htslConverter) {
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
    public static Article parseArticleForms(final Reader reader, final HtslConverter htslConverter) {
        return new Parser(reader, htslConverter).parse();
    }

    private Article parse() {
        final var article = parseDefarticle();
        parseDefsections();
        verifySectionGraph();
        return linkSections(article);
    }

    private PartialArticle parseDefarticle() {
        try (final var trace = new Trace("Parsing the defarticle form")) {
            trace.use();
            return parseDefarticleImpl();
        }
    }

    private PartialArticle parseDefarticleImpl() {
        final var form = reader.readTopLevelForm();
        if (form == null) {
            throw signalError("Cannot magically turn empty input into an article");
        }
        final var list = Sexps.asList(form);
        if (list == null || list.isEmpty() || list.first() != Sexp.KnownSymbol.DEFARTICLE) {
            throw signalError("This doesn't appear to be a valid defarticle form: " + Sexps.prettyPrint(form));
        }
        final var properties = extractProperties(list.withoutFirst(), allowedDefarticleKeys);
        final var title = parseString(properties, Sexp.KnownSymbol.KW_TITLE);
        final var description = parseString(properties, Sexp.KnownSymbol.KW_DESCRIPTION);
        final var date = parseDate(properties);
        final var childIds = parseChildIds(properties);
        final var topics = parseTopics(properties);
        final var inhibitTableOfContents = parseBoolean(properties, Sexp.KnownSymbol.KW_INHIBIT_TABLE_OF_CONTENTS);
        final var rootSectionDom = htslConverter.convert(properties.tail);
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

    private void parseDefsections() {
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

    private void parseDefsection(final Sexp form) {
        final var list = Sexps.asList(form);
        if (list == null || list.exactSize() < 2 || list.first() != Sexp.KnownSymbol.DEFSECTION) {
            throw signalError("This doesn't appear to be a valid defsection form: " + Sexps.prettyPrint(form));
        }
        final var tail = list.withoutFirst();
        final var sectionIdSexp = tail.first();
        final var sectionId = Sexps.asSymbol(sectionIdSexp);
        if (sectionId == null) {
            throw signalError(
                "This doesn't appear to be a valid section identifier: " + Sexps.prettyPrint(sectionIdSexp));
        }
        try (final var trace = new Trace(() -> "Parsing article section " + sectionId)) {
            trace.use();
            final var properties = extractProperties(tail.withoutFirst(), allowedDefsectionKeys);
            final var header = parseString(properties, Sexp.KnownSymbol.KW_HEADER);
            final var childIds = parseChildIds(properties);
            final var bodyDom = htslConverter.convert(properties.tail);
            final var section = new PartialSection(sectionId, header, childIds, bodyDom);
            if (sectionsById.put(sectionId, section) != null) {
                throw signalLinkingError("Duplicate section identifier found: " + sectionId);
            }
        }
    }

    private void verifySectionGraph() {
        try (final var trace = new Trace("Verifying section graph")) {
            trace.use();
            SectionGraphVerifier.verify(sectionsById);
        }
    }

    private Article linkSections(final PartialArticle article) {
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

    private Section linkSection(final PartialSection section) {
        final var linkedChildren = section.childIds.map(childId -> {
            final var child = sectionsById.get(childId);
            assert child != null : "Undetected reference to unknown section? @AssumeAssertion(nullness)";
            return linkSection(child);
        });
        return new Section(section.identifier, section.header, linkedChildren, section.body);
    }

    private static LocalDate parseDate(final ExtractedProperties properties) {
        final var value = properties.get(Sexp.KnownSymbol.KW_DATE);
        final var list = Sexps.asList(value);
        if (list == null || list.exactSize() != 3) {
            throw signalError("Property :date doesn't appear to be a 3-element list: " + Sexps.prettyPrint(value));
        }
        final var bigYear = parseInteger(list.first(), "Year");
        final var bigMonth = parseInteger(list.get(1), "Month");
        final var bigDay = parseInteger(list.get(2), "Day");
        final var year = intValueExact(bigYear, "Year");
        final var month = intValueExact(bigMonth, "Month");
        final var day = intValueExact(bigDay, "Day");
        try {
            return LocalDate.of(year, month, day);
        } catch (final DateTimeException e) {
            final var originalMessage = e.getMessage();
            throw signalError((originalMessage != null)
                ? originalMessage
                : ("Property :date couldn't be parsed: " + Sexps.prettyPrint(value)));
        }
    }

    private static Seq<String> parseTopics(final ExtractedProperties properties) {
        return parseList(properties, Sexp.KnownSymbol.KW_TOPICS, sexp -> {
            final var topicName = Sexps.asString(sexp);
            if (topicName == null) {
                throw signalError("This doesn't appear to be a topic name: " + Sexps.prettyPrint(sexp));
            }
            return topicName;
        });
    }

    private static Seq<Sexp.Symbol> parseChildIds(final ExtractedProperties properties) {
        return parseList(properties, Sexp.KnownSymbol.KW_CHILDREN, sexp -> {
            final var childId = Sexps.asSymbol(sexp);
            if (childId == null) {
                throw signalError("This doesn't appear to be a child section identifier: " + Sexps.prettyPrint(sexp));
            }
            return childId;
        });
    }

    private static <T> Seq<T> parseList(
        final ExtractedProperties properties,
        final Sexp.KnownSymbol key,
        final Function<Sexp, T> function
    ) {
        final var value = properties.get(key);
        final var list = Sexps.asList(value);
        if (list == null) {
            throw signalError("Property " + key + " doesn't appear to be a list: " + Sexps.prettyPrint(value));
        }
        return list.map(function);
    }

    private static String parseString(final ExtractedProperties properties, final Sexp.KnownSymbol key) {
        final var value = properties.get(key);
        final var string = Sexps.asString(value);
        if (string == null) {
            throw signalError("Property " + key + " doesn't appear to be a string: " + Sexps.prettyPrint(value));
        }
        return string;
    }

    @SuppressWarnings("SameParameterValue")
    private static boolean parseBoolean(final ExtractedProperties properties, final Sexp.KnownSymbol key) {
        final var value = properties.get(key);
        if (Sexps.isNil(value)) {
            return false;
        } else if (value == Sexp.KnownSymbol.T) {
            return true;
        } else {
            throw signalError("Property " + key + " doesn't appear to be a boolean: " + Sexps.prettyPrint(value));
        }
    }

    private static BigInteger parseInteger(final Sexp sexp, final String fieldName) {
        final var integer = Sexps.asInteger(sexp);
        if (integer == null) {
            throw signalError(fieldName + " doesn't appear to be an integer: " + Sexps.prettyPrint(sexp));
        }
        return integer;
    }

    private static int intValueExact(final BigInteger integer, final String fieldName) {
        try {
            return integer.intValueExact();
        } catch (final ArithmeticException e) {
            throw signalError(fieldName + " number is too big to be a valid date component: " + integer);
        }
    }

    private static ExtractedProperties extractProperties(
        final Seq<Sexp> list,
        final Set<? extends Sexp.Symbol> allowedKeys
    ) {
        final var properties = new HashMap<Sexp.Symbol, Sexp>();
        final var it = list.iterator();
        while (it.hasNext()) {
            final var keyword = Sexps.asKeyword(it.peek());
            if (keyword == null) {
                break;
            }
            it.next();
            if (!allowedKeys.contains(keyword)) {
                throw signalError("Property key " + keyword + " not allowed in this context");
            }
            final var value = it.hasNext() ? it.next() : Sexp.KnownSymbol.NIL;
            if (properties.put(keyword, value) != null) {
                throw signalError("Duplicate value for property " + keyword);
            }
        }
        return new ExtractedProperties(properties, it.rest());
    }

    private static UnhandledErrorError signalError(final String message) {
        return ConditionContext.error(new ArticleParseErrorCondition(message));
    }

    private static UnhandledErrorError signalLinkingError(final String message) {
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

    private final Reader reader;
    private final HtslConverter htslConverter;
    private final HashMap<Sexp.Symbol, PartialSection> sectionsById = new HashMap<>();

    private record ExtractedProperties(Map<Sexp.Symbol, Sexp> properties, Seq<Sexp> tail) {
        private Sexp get(final Sexp.Symbol symbol) {
            final var sexp = properties.get(symbol);
            return (sexp != null) ? sexp : Sexp.KnownSymbol.NIL;
        }
    }

    private record PartialArticle(
        String title,
        String description,
        LocalDate date,
        boolean inhibitTableOfContents,
        Seq<String> topics,
        PartialSection rootSection
    ) {
    }

    private record PartialSection(Sexp.Symbol identifier, String header, Seq<Sexp.Symbol> childIds, Seq<Node> body) {
    }

    private static final class SectionGraphVerifier {
        private SectionGraphVerifier(final Map<Sexp.Symbol, PartialSection> sectionsById) {
            this.sectionsById = sectionsById;
            for (final var sectionId : sectionsById.keySet()) {
                references.put(sectionId, new HashSet<>());
            }
        }

        private static void verify(final Map<Sexp.Symbol, PartialSection> sectionsById) {
            final var verifier = new SectionGraphVerifier(sectionsById);
            verifier.computeReferredFromSets();
            verifier.findReferenceErrors();
        }

        private void computeReferredFromSets() {
            final var seenSections = new HashSet<Sexp.Symbol>();
            var stack = Seq.of(new SectionParent(rootSectionId, null));
            while (!stack.isEmpty()) {
                final var reference = stack.last();
                stack = stack.withoutLast();
                @SuppressWarnings("keyfor") // We know it's a valid key in both maps, but CF doesn't.
                final Sexp.@KeyFor({"references", "sectionsById"}) Symbol sectionId = reference.section;
                final var parentId = reference.parent;
                if (parentId != null) {
                    references.get(sectionId).add(parentId);
                }
                if (!seenSections.add(sectionId)) {
                    continue;
                }
                for (final var childId : sectionsById.get(sectionId).childIds) {
                    if (!sectionsById.containsKey(childId)) {
                        throw signalLinkingError(
                            "Reference to undefined section " + childId + " found in " + sectionId);
                    }
                    stack = stack.appended(new SectionParent(childId, sectionId));
                }
            }
        }

        private void findReferenceErrors() {
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
                        "Multiple references to section " + sectionId + " in sections " + referredFromSet);
                }
            }
        }

        private final Map<Sexp.Symbol, PartialSection> sectionsById;
        private final HashMap<Sexp.Symbol, HashSet<Sexp.Symbol>> references = new HashMap<>();

        private record SectionParent(Sexp.Symbol section, Sexp.@Nullable Symbol parent) {
        }
    }
}
