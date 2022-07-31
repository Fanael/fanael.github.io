// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.generator;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Instant;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.ExecutorService;
import greenspun.article.Article;
import greenspun.article.HtslConverter;
import greenspun.article.Parser;
import greenspun.article.Section;
import greenspun.dom.Node;
import greenspun.dom.Serializer;
import greenspun.dom.Verifier;
import greenspun.pygments.PygmentsCache;
import greenspun.sexp.SymbolTable;
import greenspun.sexp.reader.ByteStream;
import greenspun.sexp.reader.Reader;
import greenspun.util.CollectionExecutorService;
import greenspun.util.Trace;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.exception.IOExceptionCondition;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The entry point to the generation process proper.
 */
public final class Generator {
    /**
     * Initializes a new generator that will use the given source directory to produce files in the given destination
     * directory.
     * <p>
     * Parallelizable parts of the generation process will submit tasks to the given executor service, sharing the given
     * Pygments cache.
     */
    public Generator(
        final Path sourceDirectory,
        final Path destinationDirectory,
        final ExecutorService executorService,
        final PygmentsCache pygmentsCache
    ) {
        this.sourceDirectory = sourceDirectory;
        this.destinationDirectory = destinationDirectory;
        executor = new CollectionExecutorService(executorService);
        this.pygmentsCache = pygmentsCache;
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
    public void generate(final Instant buildTime) {
        initializeDestinationDirectory();
        final var targets = new TargetDiscovery(sourceDirectory, destinationDirectory).discover();
        // NB: IO is not parallelized here, because it happens to not be beneficial; directories cannot be processed in
        // parallel anyway, because child directories have to be removed before parents, and parent directories have to
        // be created before children; discovery returns the lists in proper order.
        targets.filesToUnlink().forEach(this::unlinkFile);
        targets.directoriesToUnlink().forEach(this::unlinkFile);
        targets.directoriesToCreate().forEach(path -> ensureDirectoryExists(destinationDirectory.resolve(path)));
        copyFiles(targets.staticTargets());
        generatePages(targets.pageTargets());
        final var articles = generateArticles(targets.articleTargets());
        generateFrontPage(articles.splitAt(Math.min(articles.exactSize(), frontPageArticleCount)).front());
        generateFeed(articles, buildTime);
        final var archiveUris = generateArchives(articles);
        generateFileList(targets, archiveUris);
    }

    private void initializeDestinationDirectory() {
        try (final var trace = new Trace(() -> "Initializing destination directory: " + destinationDirectory)) {
            trace.use();
            ensureDirectoryExists(destinationDirectory);
            ensureDirectoryExists(destinationDirectory.resolve(staticSubdirectoryName));
            ensureDirectoryExists(destinationDirectory.resolve(pagesSubdirectoryName));
            ensureDirectoryExists(destinationDirectory.resolve(archivesSubdirectoryName));
        }
    }

    private void unlinkFile(final Path destinationRelativePath) {
        try (final var trace = new Trace(() -> "Unlinking unneeded file: " + destinationRelativePath)) {
            trace.use();
            Files.delete(destinationDirectory.resolve(destinationRelativePath));
        } catch (final IOException e) {
            throw ConditionContext.error(new IOExceptionCondition(e));
        }
    }

    private void copyFiles(final Seq<Target> targets) {
        for (final var target : targets) {
            final var sourcePath = target.sourcePath();
            try (final var trace = new Trace(() -> "Copying static file: " + sourcePath)) {
                trace.use();
                Files.copy(
                    sourceDirectory.resolve(sourcePath),
                    destinationDirectory.resolve(target.destinationPath()),
                    StandardCopyOption.REPLACE_EXISTING
                );
            } catch (final IOException e) {
                throw ConditionContext.error(new IOExceptionCondition(e));
            }
        }
    }

    private void generatePages(final Seq<Target> targets) {
        final var renderer = makeNonArticleRenderer();
        executor.forEach(targets, target -> generatePage(target, renderer));
    }

    private void generatePage(final Target target, final Renderer renderer) {
        final var sourcePath = target.sourcePath();
        try (final var trace = new Trace("Generating non-article page from " + sourcePath)) {
            trace.use();
            // Non-articles are similar enough to articles that we can use the article methods.
            final var loadedPage = loadArticle(target);
            final var orderedPage = new OrderedArticle(loadedPage.article, target, null, null);
            generateArticle(orderedPage, renderer);
        }
    }

    private Seq<ArchivedArticle> generateArticles(final Seq<Target> targets) {
        // Use the file name as a tie-breaker to ensure we don't rely on the order the file system returned paths in.
        final var articles = executor.map(targets, this::loadArticle).sorted(
            Comparator.comparing((final LoadedArticle article) -> article.article.date())
                .thenComparing(article -> article.target().sourcePath())
                .reversed()
        );
        final var orderedArticles = new Seq.Builder<OrderedArticle>();
        @Nullable LoadedArticle successor = null;
        for (final var it = articles.iterator(); it.hasNext(); ) {
            final var article = it.next();
            final var predecessorUri = it.hasNext() ? it.peek().destinationUri() : null;
            final var successorUri = (successor == null) ? null : successor.destinationUri();
            final var innerArticle = article.article;
            final var target = article.target;
            final var ordered = new OrderedArticle(innerArticle, target, predecessorUri, successorUri);
            orderedArticles.append(ordered);
            successor = article;
        }
        final var renderer = makeArticleRenderer();
        return executor.map(orderedArticles.toSeq(), article -> generateArticle(article, renderer));
    }

    private ArchivedArticle generateArticle(final OrderedArticle orderedArticle, final Renderer renderer) {
        var outerArticle = orderedArticle;
        while (true) {
            final var article = outerArticle;
            final var result = ConditionContext.withRestart("reload-article-and-retry", restart -> {
                try (final var trace = new Trace(() -> "Generating article " + article.article.title())) {
                    trace.use();
                    final var articleToRender =
                        new ArticleToRender(article.article, article.predecessorUri, article.successorUri);
                    serializeDomTree(article.target.destinationPath(), renderer.renderArticle(articleToRender));
                    return stripSections(article);
                }
            });
            if (result != null) {
                return result;
            }
            outerArticle = reloadWithSameDate(outerArticle);
        }
    }

    private OrderedArticle reloadWithSameDate(final OrderedArticle originalArticle) {
        final var target = originalArticle.target;
        final var originalDate = originalArticle.article.date();
        try (final var trace = new Trace(() -> "Reloading article from " + target.sourcePath())) {
            trace.use();
            while (true) {
                final var article = loadArticle(target);
                final var newDate = article.article().date();
                if (newDate.equals(originalDate)) {
                    return new OrderedArticle(
                        article.article,
                        target,
                        originalArticle.predecessorUri,
                        originalArticle.successorUri
                    );
                }
                ConditionContext.withRestart("reload-article-and-retry", restart -> {
                    throw ConditionContext.error(new DateChangedCondition(originalDate, newDate));
                });
            }
        }
    }

    private void serializeDomTree(final Path destinationRelativePath, final Node rootNode) {
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

    private LoadedArticle loadArticle(final Target target) {
        final var fullSourcePath = sourceDirectory.resolve(target.sourcePath());
        while (true) {
            final var article = ConditionContext.withRestart("reload-article-and-retry", restart -> {
                try (final var trace = new Trace(() -> "Loading article from " + fullSourcePath)) {
                    trace.use();
                    try (final var stream = Files.newInputStream(fullSourcePath)) {
                        final var reader = new Reader(new ByteStream(stream), symbolTable);
                        final var converter = new HtslConverter(pygmentsCache);
                        return Parser.parseArticleForms(reader, converter);
                    } catch (final IOException e) {
                        throw ConditionContext.error(new IOExceptionCondition(e));
                    }
                }
            });
            if (article != null) {
                return new LoadedArticle(article, target);
            }
        }
    }

    private static ArchivedArticle stripSections(final OrderedArticle orderedArticle) {
        final var innerArticle = orderedArticle.article;
        final var rootSection = innerArticle.rootSection();
        return new LoadedArticle(
            new Article(
                innerArticle.title(),
                innerArticle.description(),
                innerArticle.date(),
                innerArticle.inhibitTableOfContents(),
                innerArticle.topics(),
                new Section(rootSection.identifier(), rootSection.header(), Seq.empty(), rootSection.body())
            ),
            orderedArticle.target
        ).toArchivedArticle();
    }

    private Seq<DomainRelativeUri> generateArchives(final Seq<ArchivedArticle> articles) {
        try (final var outerTrace = new Trace("Generating blog archives")) {
            outerTrace.use();
            final var archivedQuarters = generateQuarterlyArchives(articles);
            final var archivedTopics = generateTopicArchives(articles);
            try (final var innerTrace = new Trace("Generating the archives index")) {
                innerTrace.use();
                final var destinationRelativePath = archivesSubdirectoryPath.resolve(directoryRootFileName);
                final var domTree = Renderer.renderArchiveIndex(archivedQuarters, archivedTopics, articles);
                serializeDomTree(destinationRelativePath, domTree);
            }
            var result = archivedQuarters.map(ArchivedQuarter::uri);
            result = result.concat(archivedTopics.map(ArchivedTopic::uri));
            result = result.appended(DomainRelativeUri.ofDirectory(archivesSubdirectoryPath));
            return result;
        }
    }

    private Seq<ArchivedTopic> generateTopicArchives(final Seq<ArchivedArticle> articles) {
        try (final var outerTrace = new Trace("Generating per-topic blog archives")) {
            outerTrace.use();
            final var articlesByTopic = new HashMap<String, Seq<ArchivedArticle>>();
            for (final var article : articles) {
                for (final var topic : article.article().topics()) {
                    articlesByTopic.merge(topic, Seq.of(article), Seq::concat);
                }
            }
            final var topics = executor.map(articlesByTopic.entrySet(), entry -> {
                final var topicName = entry.getKey();
                final var topicArticles = entry.getValue();
                try (final var innerTrace = new Trace(() -> "Generating archives of topic " + topicName)) {
                    innerTrace.use();
                    final var destinationRelativePath =
                        archivesSubdirectoryPath.resolve("topic-" + topicName + ".html");
                    final var domTree = makeArticleRenderer().renderTopicArchive(topicName, topicArticles);
                    serializeDomTree(destinationRelativePath, domTree);
                    return new ArchivedTopic(topicName, new DomainRelativeUri(destinationRelativePath));
                }
            });
            return topics.sorted(Comparator.comparing(ArchivedTopic::topic));
        }
    }

    private Seq<ArchivedQuarter> generateQuarterlyArchives(final Seq<ArchivedArticle> articles) {
        try (final var outerTrace = new Trace("Generating per-quarter blog archives")) {
            outerTrace.use();
            final var articlesByQuarter = new HashMap<Quarter, Seq<ArchivedArticle>>();
            for (final var article : articles) {
                articlesByQuarter.merge(Quarter.fromDate(article.article().date()), Seq.of(article), Seq::concat);
            }
            final var quarters = executor.map(articlesByQuarter.entrySet(), entry -> {
                final var quarter = entry.getKey();
                final var quarterArticles = entry.getValue();
                try (final var innerTrace = new Trace(() -> "Generating quarterly archives for the " + quarter)) {
                    innerTrace.use();
                    final var destinationRelativePath =
                        archivesSubdirectoryPath.resolve(quarter.year() + "-q" + quarter.quarter() + ".html");
                    final var domTree = makeArticleRenderer().renderQuarterlyArchive(quarter, quarterArticles);
                    serializeDomTree(destinationRelativePath, domTree);
                    return new ArchivedQuarter(quarter, new DomainRelativeUri(destinationRelativePath));
                }
            });
            return quarters.sorted(Comparator.comparing(ArchivedQuarter::quarter).reversed());
        }
    }

    private void generateFrontPage(final Seq<ArchivedArticle> articles) {
        try (final var trace = new Trace("Generating the front page")) {
            trace.use();
            final var domTree = makeArticleRenderer().renderFrontPage(articles);
            serializeDomTree(Path.of(directoryRootFileName), domTree);
        }
    }

    private void generateFeed(final Seq<ArchivedArticle> articles, final Instant buildTime) {
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

    private void generateFileList(final Targets targets, final Seq<DomainRelativeUri> archiveUris) {
        try (final var trace = new Trace("Generating list of files")) {
            trace.use();
            var uris = targets.staticTargets().map(Target::destinationUri);
            uris = uris.concat(targets.pageTargets().map(Target::destinationUri));
            uris = uris.concat(targets.articleTargets().map(Target::destinationUri));
            uris = uris.concat(archiveUris);
            uris = uris.appended(DomainRelativeUri.ofRoot());
            uris = uris.sorted(Comparator.comparing(DomainRelativeUri::toString));
            writeFileList(uris);
        }
    }

    private void writeFileList(final Seq<DomainRelativeUri> uris) {
        final var destinationPath = destinationDirectory.resolve(fileListName);
        try (final var writer = Files.newBufferedWriter(destinationPath)) {
            boolean needsSeparator = false;
            for (final var uri : uris) {
                if (needsSeparator) {
                    writer.write('\0');
                }
                writer.write(uri.toString());
                needsSeparator = true;
            }
        } catch (final IOException e) {
            throw ConditionContext.error(new IOExceptionCondition(e));
        }
    }

    private static void ensureDirectoryExists(final Path path) {
        try (final var trace = new Trace(() -> "Ensuring directory exists: " + path)) {
            trace.use();
            Files.createDirectory(path);
        } catch (final FileAlreadyExistsException e) {
            // No need to do anything if it already exists.
        } catch (final IOException e) {
            throw ConditionContext.error(new IOExceptionCondition(e));
        }
    }

    private static Renderer makeArticleRenderer() {
        return new Renderer(HeaderRenderImpl.instance);
    }

    private static Renderer makeNonArticleRenderer() {
        return new Renderer(HeaderRenderMode.Skip.instance());
    }

    static final String directoryRootFileName = "index.html";
    static final String staticSubdirectoryName = "static";
    static final String pagesSubdirectoryName = "pages";
    static final String archivesSubdirectoryName = "archives";
    static final String fileListName = "files.txt";
    static final Path serviceWorkerSourcePath = Path.of(staticSubdirectoryName, "root-sw.js");
    static final Path serviceWorkerDestinationPath = Path.of("sw.js");
    private static final Path archivesSubdirectoryPath = Path.of(archivesSubdirectoryName);
    private static final int frontPageArticleCount = 5;

    private final Path sourceDirectory;
    private final Path destinationDirectory;
    private final CollectionExecutorService executor;
    private final PygmentsCache pygmentsCache;
    private final SymbolTable symbolTable = new SymbolTable();

    private static final class HeaderRenderImpl implements HeaderRenderMode {
        @Override
        public boolean shouldRender() {
            return true;
        }

        @Override
        public String getTopicArchiveUri(final String topicName) {
            return "/archives/topic-" + topicName + ".html";
        }

        private static final HeaderRenderImpl instance = new HeaderRenderImpl();
    }

    private record LoadedArticle(Article article, Target target) {
        private DomainRelativeUri destinationUri() {
            return target.destinationUri();
        }

        private String identifier() {
            final var fileName = target.sourcePath().getFileName();
            assert fileName != null : "Article path has no file name? @AssumeAssertion(nullness)";
            final var string = fileName.toString();
            final var dotIndex = string.lastIndexOf('.');
            return (dotIndex == -1) ? string : string.substring(0, dotIndex);
        }

        private ArchivedArticle toArchivedArticle() {
            return new ArchivedArticle(article, identifier(), destinationUri());
        }
    }

    private record OrderedArticle(
        Article article,
        Target target,
        @Nullable DomainRelativeUri predecessorUri,
        @Nullable DomainRelativeUri successorUri
    ) {
    }
}
