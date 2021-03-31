// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later
package greenspun.generator;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import greenspun.article.Article;
import greenspun.article.HtslConverter;
import greenspun.article.Parser;
import greenspun.article.Section;
import greenspun.dom.Node;
import greenspun.dom.Serializer;
import greenspun.dom.Verifier;
import greenspun.sexp.SymbolTable;
import greenspun.sexp.reader.Reader;
import greenspun.util.PathUtils;
import greenspun.util.Trace;
import greenspun.util.UncheckedInterruptedException;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The entry point to the generation process proper.
 */
@SuppressWarnings("ClassCanBeRecord")
public final class Generator {
    /**
     * Initializes a new generator that will use the given source directory to produce files in the given destination
     * directory.
     * <p>
     * Parallelizable parts of the generation process will submit tasks to the given executor service, sharing the given
     * shared state.
     */
    public Generator(
        final @NotNull Path sourceDirectory,
        final @NotNull Path destinationDirectory,
        final @NotNull ExecutorService executorService,
        final @NotNull SharedState sharedState
    ) {
        this.sourceDirectory = sourceDirectory;
        this.destinationDirectory = destinationDirectory;
        this.executorService = executorService;
        this.sharedState = sharedState;
    }

    /**
     * Performs a full build of the entire site.
     * <p>
     * The given build time will be used wherever a build timestamp is needed in the output.
     * <p>
     * A full build consists of:
     * <ul>
     * <li>Cleaning up the destination directory,
     * <li>Copying static files,
     * <li>Linking articles in chronological order,
     * <li>Generating HTML files of all articles and non-article pages,
     * <li>Generating the front page,
     * <li>Generating the RSS feed,
     * <li>Generating archive pages.
     * </ul>
     * <p>
     * Some parts of this process are done in parallel using the generator's executor service. Worker threads inherit
     * condition context state from the caller thread.
     * <p>
     * Since this method basically does everything, it can signal just about any
     * {@link greenspun.util.condition.Condition}.
     */
    public void generate(final @NotNull Instant buildTime) throws Unwind, InterruptedException {
        final var preparator = new DirectoryPreparator(sourceDirectory, destinationDirectory);
        final var articleSourcePaths = preparator.getArticleSourcePaths();
        final var pageSourcePaths = preparator.getPageSourcePaths();
        preparator.prepareDestinationDirectory();
        generatePages(pageSourcePaths);
        final var articles = generateArticles(articleSourcePaths);
        generateFrontPage(articles.subList(0, Math.min(articles.size(), frontPageArticleCount)));
        generateFeed(articles.subList(0, Math.min(articles.size(), feedArticleCount)), buildTime);
        generateArchives(articles);
    }

    private void generatePages(final @NotNull List<Path> pageSourcePaths) throws Unwind, InterruptedException {
        final var renderer = makeNonArticleRenderer();
        mapUsingExecutor(pageSourcePaths, sourceRelativePath -> generatePage(sourceRelativePath, renderer));
    }

    @SuppressWarnings("SameReturnValue")
    private @Nullable Void generatePage(
        final @NotNull Path sourceRelativePath,
        final @NotNull Renderer renderer
    ) throws Unwind {
        try (final var trace = new Trace("Generating non-article page from " + sourceRelativePath)) {
            trace.use();
            // Non-articles are similar enough to articles that we can use the article methods.
            final var loadedPage = loadArticle(sourceRelativePath);
            final var orderedPage = new OrderedArticle(loadedPage.article, sourceRelativePath, null, null);
            generateArticle(orderedPage, renderer);
            return null;
        }
    }

    private @NotNull List<ArchivedArticle> generateArticles(
        final @NotNull List<Path> articleSourcePaths
    ) throws Unwind, InterruptedException {
        final var articles = mapUsingExecutor(articleSourcePaths, this::loadArticle);
        // Use the file name as a tie breaker to ensure we don't rely on the order the file system returned paths in.
        articles.sort(
            Comparator.comparing((final @NotNull LoadedArticle article) -> article.article.date())
                .thenComparing(LoadedArticle::sourceRelativePath)
                .reversed()
        );
        final var articleCount = articles.size();
        final var orderedArticles = new ArrayList<@NotNull OrderedArticle>(articleCount);
        for (int i = 0; i < articleCount; i += 1) {
            final var article = articles.get(i);
            final var predecessorUrl = (i + 1 < articleCount) ? articles.get(i + 1).destinationUrl() : null;
            final var successorUrl = (i > 0) ? articles.get(i - 1).destinationUrl() : null;
            final var innerArticle = article.article;
            final var sourceRelativePath = article.sourceRelativePath;
            orderedArticles.add(new OrderedArticle(innerArticle, sourceRelativePath, predecessorUrl, successorUrl));
        }
        final var renderer = makeArticleRenderer();
        return mapUsingExecutor(orderedArticles, article -> generateArticle(article, renderer));
    }

    private @NotNull ArchivedArticle generateArticle(
        final @NotNull OrderedArticle orderedArticle,
        final @NotNull Renderer renderer
    ) throws Unwind {
        var outerArticle = orderedArticle;
        final var sourceRelativePath = orderedArticle.sourceRelativePath;
        final var destinationRelativePath = PathUtils.changeExtension(sourceRelativePath, "html");
        while (true) {
            final var article = outerArticle;
            final var result = ConditionContext.withRestart("reload-article-and-retry", restart -> {
                try (final var trace = new Trace(() -> "Generating article " + article.article.title())) {
                    trace.use();
                    final var articleToRender =
                        new ArticleToRender(article.article, article.predecessorUrl, article.successorUrl);
                    serializeDomTree(destinationRelativePath, renderer.renderArticle(articleToRender));
                    return stripSections(article);
                }
            });
            if (result != null) {
                return result;
            }
            outerArticle = reloadWithSameDate(outerArticle);
        }
    }

    private @NotNull OrderedArticle reloadWithSameDate(final @NotNull OrderedArticle originalArticle) throws Unwind {
        final var sourceRelativePath = originalArticle.sourceRelativePath;
        final var originalDate = originalArticle.article.date();
        try (final var trace = new Trace(() -> "Reloading article from " + sourceRelativePath)) {
            trace.use();
            while (true) {
                final var article = loadArticle(sourceRelativePath);
                final var newDate = article.article().date();
                if (newDate.equals(originalDate)) {
                    return new OrderedArticle(
                        article.article,
                        sourceRelativePath,
                        originalArticle.predecessorUrl,
                        originalArticle.successorUrl
                    );
                }
                ConditionContext.withRestart("reload-article-and-retry", restart -> {
                    throw ConditionContext.error(new DateChangedCondition(originalDate, newDate));
                });
            }
        }
    }

    private void serializeDomTree(
        final @NotNull Path destinationRelativePath,
        final @NotNull Node rootNode
    ) throws Unwind {
        Verifier.verify(rootNode);
        final var destinationPath = destinationDirectory.resolve(destinationRelativePath);
        try (final var trace = new Trace(() -> "Saving HTML to " + destinationPath)) {
            trace.use();
            try (final var writer = Files.newBufferedWriter(destinationPath)) {
                writer.write("<!DOCTYPE html>");
                Serializer.serialize(writer, rootNode);
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private @NotNull LoadedArticle loadArticle(final @NotNull Path sourceRelativePath) throws Unwind {
        final var fullSourcePath = sourceDirectory.resolve(sourceRelativePath);
        final var symbolTable = threadLocalSymbolTable.get();
        while (true) {
            final var article = ConditionContext.withRestart("reload-article-and-retry", restart -> {
                try (final var trace = new Trace(() -> "Loading article from " + fullSourcePath)) {
                    trace.use();
                    try (final var stream = Files.newInputStream(fullSourcePath)) {
                        final var reader = new Reader(stream, symbolTable);
                        final var htslConverter =
                            new HtslConverter(symbolTable, sharedState.pygmentsServer(), sharedState.pygmentsCache());
                        return Parser.parseArticleForms(reader, htslConverter);
                    } catch (final IOException e) {
                        throw ConditionContext.error(new IOExceptionCondition(e));
                    }
                }
            });
            if (article != null) {
                return new LoadedArticle(article, sourceRelativePath);
            }
        }
    }

    private static @NotNull ArchivedArticle stripSections(final @NotNull OrderedArticle orderedArticle) {
        final var innerArticle = orderedArticle.article;
        final var rootSection = innerArticle.rootSection();
        return new LoadedArticle(
            new Article(
                innerArticle.title(),
                innerArticle.description(),
                innerArticle.date(),
                innerArticle.inhibitTableOfContents(),
                innerArticle.topics(),
                new Section(rootSection.identifier(), rootSection.header(), Collections.emptyList(), rootSection.body())
            ),
            orderedArticle.sourceRelativePath
        ).toArchivedArticle();
    }

    private void generateArchives(final @NotNull List<ArchivedArticle> articles) throws Unwind, InterruptedException {
        try (final var outerTrace = new Trace("Generating blog archives")) {
            outerTrace.use();
            final var archivedQuarters = generateQuarterlyArchives(articles);
            final var archivedTopics = generateTopicArchives(articles);
            try (final var innerTrace = new Trace("Generating the archives index")) {
                innerTrace.use();
                final var destinationRelativePath = archivesSubdirectoryPath.resolve("index.html");
                final var domTree = Renderer.renderArchiveIndex(archivedQuarters, archivedTopics, articles);
                serializeDomTree(destinationRelativePath, domTree);
            }
        }
    }

    private @NotNull ArrayList<ArchivedTopic> generateTopicArchives(
        final @NotNull List<ArchivedArticle> articles
    ) throws Unwind, InterruptedException {
        try (final var outerTrace = new Trace("Generating per-topic blog archives")) {
            outerTrace.use();
            final var articlesByTopic = new HashMap<String, ArrayList<ArchivedArticle>>();
            for (final var article : articles) {
                for (final var topic : article.article().topics()) {
                    articlesByTopic.computeIfAbsent(topic, key -> new ArrayList<>()).add(article);
                }
            }
            final var topics = mapUsingExecutor(articlesByTopic.entrySet(), entry -> {
                final var topicName = entry.getKey();
                final var topicArticles = entry.getValue();
                try (final var innerTrace = new Trace(() -> "Generating archives of topic " + topicName)) {
                    innerTrace.use();
                    final var destinationRelativePath =
                        archivesSubdirectoryPath.resolve("topic-" + topicName + ".html");
                    final var domTree = makeArticleRenderer().renderTopicArchive(topicName, topicArticles);
                    serializeDomTree(destinationRelativePath, domTree);
                    return new ArchivedTopic(topicName, makeDomainRelativeUrl(destinationRelativePath));
                }
            });
            topics.sort(Comparator.comparing(ArchivedTopic::topic));
            return topics;
        }
    }

    private @NotNull ArrayList<ArchivedQuarter> generateQuarterlyArchives(
        final @NotNull List<ArchivedArticle> articles
    ) throws Unwind, InterruptedException {
        try (final var outerTrace = new Trace("Generating per-quarter blog archives")) {
            outerTrace.use();
            final var articlesByQuarter = new HashMap<Quarter, ArrayList<ArchivedArticle>>();
            for (final var article : articles) {
                final var quarter = Quarter.fromDate(article.article().date());
                articlesByQuarter.computeIfAbsent(quarter, key -> new ArrayList<>()).add(article);
            }
            final var quarters = mapUsingExecutor(articlesByQuarter.entrySet(), entry -> {
                final var quarter = entry.getKey();
                final var quarterArticles = entry.getValue();
                try (final var innerTrace = new Trace(() -> "Generating quarterly archives for the " + quarter)) {
                    innerTrace.use();
                    final var destinationRelativePath =
                        archivesSubdirectoryPath.resolve(quarter.year() + "-q" + quarter.quarter() + ".html");
                    final var domTree = makeArticleRenderer().renderQuarterlyArchive(quarter, quarterArticles);
                    serializeDomTree(destinationRelativePath, domTree);
                    return new ArchivedQuarter(quarter, makeDomainRelativeUrl(destinationRelativePath));
                }
            });
            quarters.sort(Comparator.comparing(ArchivedQuarter::quarter).reversed());
            return quarters;
        }
    }

    private void generateFrontPage(final @NotNull List<ArchivedArticle> articles) throws Unwind {
        try (final var trace = new Trace("Generating the front page")) {
            trace.use();
            final var domTree = makeArticleRenderer().renderFrontPage(articles);
            serializeDomTree(Path.of("index.html"), domTree);
        }
    }

    private void generateFeed(
        final @NotNull List<ArchivedArticle> articles,
        final @NotNull Instant buildTime
    ) throws Unwind {
        try (final var trace = new Trace("Generating RSS feed")) {
            trace.use();
            final var feedRenderer = new FeedRenderer();
            feedRenderer.createDom(articles, buildTime);
            final var destinationPath = destinationDirectory.resolve(RenderConstants.feedFileName);
            try (final var writer = Files.newBufferedWriter(destinationPath)) {
                feedRenderer.serialize(writer);
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private static @NotNull String makeDomainRelativeUrl(final @NotNull Path path) {
        assert !path.isAbsolute();
        return '/' + path.toString().replace(File.separatorChar, '/');
    }

    private static @NotNull Renderer makeArticleRenderer() {
        return new Renderer(HeaderRenderMode.RENDER, topic -> "/archives/topic-" + topic + ".html");
    }

    private static @NotNull Renderer makeNonArticleRenderer() {
        return new Renderer(HeaderRenderMode.SKIP, topic -> null);
    }

    private <T, R> @NotNull ArrayList<R> mapUsingExecutor(
        final @NotNull Collection<? extends T> collection,
        final @NotNull UnwindingFunction<? super T, ? extends R> function
    ) throws Unwind, InterruptedException {
        final var futures = new ArrayList<@NotNull Future<R>>(collection.size());
        final var inheritedState = ConditionContext.saveInheritableState();
        for (final var element : collection) {
            futures.add(executorService.submit(() -> {
                final var previousState = ConditionContext.inheritState(inheritedState);
                try (final var t = new Trace(() -> "Executing a task in thread " + Thread.currentThread().getName())) {
                    t.use();
                    return function.apply(element);
                } catch (final Unwind u) {
                    // Need to repackage the unwind because it's checked and it's not an exception.
                    throw new UnwindException(u);
                } finally {
                    ConditionContext.restoreState(previousState);
                }
            }));
        }
        return new AwaitAllImpl<>(futures).awaitAll();
    }

    static final String pagesSubdirectoryName = "pages";
    static final String archivesSubdirectoryName = "archives";
    private static final Path archivesSubdirectoryPath = Path.of(archivesSubdirectoryName);
    private static final int frontPageArticleCount = 5;
    private static final int feedArticleCount = 10;

    private final @NotNull Path sourceDirectory;
    private final @NotNull Path destinationDirectory;
    private final @NotNull ExecutorService executorService;
    private final @NotNull SharedState sharedState;
    private static final @NotNull ThreadLocal<SymbolTable> threadLocalSymbolTable =
        ThreadLocal.withInitial(SymbolTable::new);

    @FunctionalInterface
    private interface UnwindingFunction<T, R> {
        R apply(T object) throws Unwind;
    }

    @SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
    private static record LoadedArticle(@NotNull Article article, @NotNull Path sourceRelativePath) {
        private @NotNull String destinationUrl() {
            return makeDomainRelativeUrl(PathUtils.changeExtension(sourceRelativePath, "html"));
        }

        private @NotNull String identifier() {
            final var fileName = sourceRelativePath.getFileName();
            assert fileName != null;
            final var string = fileName.toString();
            final var dotIndex = string.lastIndexOf('.');
            return (dotIndex == -1) ? string : string.substring(0, dotIndex);
        }

        private @NotNull ArchivedArticle toArchivedArticle() {
            return new ArchivedArticle(article, identifier(), destinationUrl());
        }
    }

    @SuppressFBWarnings(value = "EQ_UNUSUAL", justification = "SpotBugs doesn't understand equals() of records yet")
    private static record OrderedArticle(
        @NotNull Article article,
        @NotNull Path sourceRelativePath,
        @Nullable String predecessorUrl,
        @Nullable String successorUrl
    ) {
    }

    private static final class AwaitAllImpl<T> {
        private AwaitAllImpl(final @NotNull Collection<? extends Future<T>> futures) {
            iterator = futures.iterator();
            results = new ArrayList<>(futures.size());
        }

        private @NotNull ArrayList<T> awaitAll() throws Unwind, InterruptedException {
            try {
                while (iterator.hasNext()) {
                    awaitOne(iterator.next());
                }
                if (foundInterrupt) {
                    throw new AssertionError("A task was interrupted, but no other task threw anything concrete");
                }
                return results;
            } finally {
                if (needsCancellation) {
                    while (iterator.hasNext()) {
                        iterator.next().cancel(true);
                    }
                }
            }
        }

        private void awaitOne(final @NotNull Future<T> future) throws Unwind, InterruptedException {
            try {
                results.add(future.get());
            } catch (final ExecutionException e) {
                needsCancellation = true;
                final var cause = e.getCause();
                if (cause instanceof UnwindException u) {
                    // Cross-thread unwind to a restart found, rethrow it to continue unwinding in the parent thread.
                    throw u.unwind;
                } else if (cause instanceof InterruptedException || cause instanceof UncheckedInterruptedException) {
                    // Continue looking, some other future will likely have a more concrete throwable.
                    foundInterrupt = true;
                } else if (cause instanceof Error error) {
                    // Errors should be passed through directly.
                    throw error;
                } else if (cause instanceof RuntimeException runtimeException) {
                    // As should runtime exceptions.
                    throw runtimeException;
                } else {
                    throw new AssertionError("An exception escaped from a worker through a future", cause);
                }
            }
        }

        private final @NotNull Iterator<? extends Future<T>> iterator;
        private final @NotNull ArrayList<T> results;
        private boolean foundInterrupt = false;
        private boolean needsCancellation = false;
    }

    private static final class UnwindException extends Exception {
        private UnwindException(final @NotNull Unwind unwind) {
            this.unwind = unwind;
        }

        private final @NotNull Unwind unwind;
    }
}
