// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
package greenspun.generator;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;
import greenspun.article.Article;
import greenspun.article.Section;
import greenspun.dom.Attribute;
import greenspun.dom.Attributes;
import greenspun.dom.Node;
import greenspun.dom.Tag;
import greenspun.util.collection.seq.Seq;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The renderer: the primary means of turning article and archive page data structures into complete DOM trees.
 * <p>
 * Most of the interesting methods are package-private, because only {@link Generator} uses them.
 * <p>
 * This class is thread-safe: as it doesn't hold any mutable state whatsoever, multiple threads can access one instance
 * at the same time with no external synchronization.
 */
public final class Renderer {
    Renderer(final @NotNull HeaderRenderMode headerRenderMode) {
        this.headerRenderMode = headerRenderMode;
    }

    /**
     * Wraps the given list of DOM nodes representing highlighted code into a single node.
     */
    public static @NotNull Node wrapHighlightedCode(
        final @NotNull Seq<@NotNull Node> nodes,
        final @NotNull String prettyLanguageName
    ) {
        return wrapCodeBlock(Seq.of(Node.simple(Tag.CODE, nodes)), prettyLanguageName);
    }

    /**
     * Wraps the given list of DOM nodes representing a code block into a single node.
     */
    public static @NotNull Node wrapCodeBlock(
        final @NotNull Seq<@NotNull Node> nodes,
        final @NotNull String prettyLanguageName
    ) {
        final var language = new Node.Element(
            Tag.SPAN,
            Seq.of(Attribute.of("class", "cx-language")),
            Seq.of(new Node.Text(prettyLanguageName))
        );
        return new Node.Element(
            Tag.PRE,
            Seq.of(Attribute.of("class", "code-block")),
            Seq.of(language, Constants.newLine).concat(renderLineNumbers(nodes))
        );
    }

    static @NotNull Node.Element renderArchiveIndex(
        final @NotNull Seq<@NotNull ArchivedQuarter> quarters,
        final @NotNull Seq<@NotNull ArchivedTopic> topics,
        final @NotNull Seq<@NotNull ArchivedArticle> articles
    ) {
        return renderDocument(
            renderHead("Blog archive index", "Main page of blog archives"),
            Constants.simpleBottomNav,
            renderArchiveIndexBody(quarters, topics, articles)
        );
    }

    @NotNull Node.Element renderFrontPage(final @NotNull Seq<@NotNull ArchivedArticle> articles) {
        final var header = Node.simple(Tag.HEADER, Node.simple(Tag.H1, new Node.Text("Latest articles")));
        return renderDocument(
            renderHead(null, "Latest posts on " + RenderConstants.siteTitle),
            Constants.simpleBottomNav,
            articles.isEmpty()
                ? Seq.of(header, Node.simple(Tag.P, new Node.Text("There are no articles yet.")))
                : renderExcerpts(articles).prepended(header)
        );
    }

    @NotNull Node.Element renderTopicArchive(
        final @NotNull String topicName,
        final @NotNull Seq<@NotNull ArchivedArticle> articles
    ) {
        return renderArchive(
            "Blog archives for topic " + topicName,
            "Archives for topic " + topicName,
            articles
        );
    }

    @NotNull Node.Element renderQuarterlyArchive(
        final @NotNull Quarter quarter,
        final @NotNull Seq<@NotNull ArchivedArticle> articles
    ) {
        return renderArchive(
            "Blog archives for the " + quarter,
            "Archives for the " + quarter,
            articles
        );
    }

    @NotNull Node.Element renderArticle(final @NotNull ArticleToRender article) {
        return renderDocument(
            renderHead(article.article().title(), article.article().description()),
            renderBottomNav(article.predecessorUri(), article.successorUri()),
            Seq.of(renderArticleBody(article.article()))
        );
    }

    private static @NotNull Seq<@NotNull Node> renderLineNumbers(final @NotNull Seq<@NotNull Node> nodes) {
        return (nodes.exactSize() == 1 && nodes.first() instanceof Node.Element parent)
            ? Seq.of(new LineNumberRenderer(parent).render())
            : nodes;
    }

    private static @NotNull Seq<@NotNull Node> renderArchiveIndexBody(
        final @NotNull Seq<ArchivedQuarter> quarters,
        final @NotNull Seq<ArchivedTopic> topics,
        final @NotNull Seq<ArchivedArticle> articles
    ) {
        return Seq.of(
            Node.simple(Tag.HEADER, Node.simple(Tag.H1, new Node.Text("Blog archives"))),
            renderArchiveIndexSection("By date", quarters, quarter -> renderSimpleLink(
                quarter.uri().toString(), "The " + quarter.quarter())),
            renderArchiveIndexSection("By topic", topics, topic -> renderSimpleLink(
                topic.uri().toString(), topic.topic())),
            renderArchiveIndexSection("By article title", articles, article -> renderSimpleLink(
                article.uri().toString(),
                renderIsoDate(article.article().date()) + " — " + article.article().title()
            ))
        );
    }

    private static <T> Node.@NotNull Element renderArchiveIndexSection(
        final @NotNull String name,
        final @NotNull Seq<? extends T> elements,
        final @NotNull Function<? super T, ? extends Node> linkFormatter
    ) {
        return Node.simple(Tag.SECTION, Seq.of(
            Node.simple(Tag.H2, new Node.Text(name)),
            Node.simple(Tag.UL, elements.map(element -> Node.simple(Tag.LI, linkFormatter.apply(element))))
        ));
    }

    private @NotNull Node.Element renderArchive(
        final @NotNull String title,
        final @NotNull String header,
        final @NotNull Seq<ArchivedArticle> articles
    ) {
        return renderDocument(
            renderHead(title, title),
            Constants.simpleBottomNav,
            renderExcerpts(articles)
                .prepended(renderArchiveTableOfContents(articles))
                .prepended(Node.simple(Tag.HEADER, Node.simple(Tag.H1, new Node.Text(header))))
        );
    }

    private static @NotNull Node.Element renderDocument(
        final @NotNull Node.Element head,
        final @NotNull Node.Element bottomNav,
        final @NotNull Seq<@NotNull Node> mainContent
    ) {
        return new Node.Element(
            Tag.HTML,
            Seq.of(Constants.htmlLang),
            Seq.of(head, Node.simple(
                Tag.BODY,
                Constants.header
                    .appended(new Node.Element(Tag.MAIN, Seq.of(Constants.idMain), mainContent))
                    .appended(bottomNav)
                    .appended(Constants.footer)
            ))
        );
    }

    private static @NotNull Node.Element renderArchiveTableOfContents(final @NotNull Seq<ArchivedArticle> articles) {
        return new Node.Element(
            Tag.NAV,
            Seq.of(Attribute.of("class", "toc"), Attribute.of("aria-labelledby", "toc-label")),
            Seq.of(
                Constants.tocLabel,
                Node.simple(Tag.OL, articles.map(article ->
                    Node.simple(Tag.LI, renderSimpleLink('#' + article.identifier(), article.article().title()))))));
    }

    private @NotNull Seq<@NotNull Node> renderExcerpts(final @NotNull Seq<ArchivedArticle> articles) {
        @NotNull Seq<@NotNull Node> result = Seq.empty();
        for (final var article : articles) {
            final var innerArticle = article.article();
            final var title = innerArticle.title();
            var headerContents = Seq.<Node>of(
                Node.simple(Tag.H2, renderSimpleLink(article.uri().toString(), title)),
                renderPublicationDate(innerArticle.date())
            );
            final var topics = renderArticleTopics(innerArticle.topics());
            if (topics != null) {
                headerContents = headerContents.appended(topics);
            }
            result = result.appended(new Node.Element(
                Tag.ARTICLE,
                Seq.of(Attribute.of("id", article.identifier())),
                innerArticle.rootSection().body()
                    .prepended(Node.simple(Tag.HEADER, headerContents))
                    .appended(new Node.Element(
                        Tag.A,
                        Seq.of(
                            Attribute.of("class", "read-full"),
                            Attribute.of("href", article.uri().toString()),
                            Attribute.of("aria-label", "Read the full article: " + title)
                        ),
                        Seq.of(new Node.Text("Read the full article…"))
                    ))
            ));
        }
        return result;
    }

    private static @NotNull Node.Element renderHead(
        final @Nullable String title,
        final @NotNull String description
    ) {
        final var effectiveTitle =
            (title != null) ? (title + " - " + RenderConstants.siteTitle) : RenderConstants.siteTitle;
        return Node.simple(
            Tag.HEAD,
            Constants.headPrefix
                .appended(Node.empty(Tag.META_NAMED, Seq.of(
                    Attribute.of("name", "description"),
                    Attribute.of("content", description)
                )))
                .concat(Constants.headSuffix)
                .appended(Node.simple(Tag.TITLE, new Node.Text(effectiveTitle)))
        );
    }

    private @NotNull Node.Element renderArticleBody(final @NotNull Article article) {
        var contents = article.rootSection().body().prepended(renderArticleHeader(article));
        final var children = article.rootSection().children();
        if (!children.isEmpty() && !article.inhibitTableOfContents()) {
            contents = contents.appended(renderTableOfContents(children));
        }
        for (final var child : children) {
            contents = contents.appended(renderSubsection(child, 2));
        }
        return Node.simple(Tag.ARTICLE, contents);
    }

    private @NotNull Node.Element renderArticleHeader(final @NotNull Article article) {
        var contents = Seq.<Node>of(renderHeading(Tag.H1, "#main", article.title()));
        if (headerRenderMode.shouldRender()) {
            contents = contents.appended(renderPublicationDate(article.date()));
            final var topicsNode = renderArticleTopics(article.topics());
            if (topicsNode != null) {
                contents = contents.appended(topicsNode);
            }
        }
        return Node.simple(Tag.HEADER, contents);
    }

    private static @NotNull Node.Element renderPublicationDate(final @NotNull LocalDate date) {
        final var day = date.getDayOfMonth();
        final var daySuffix = switch (day % 10) {
            case 1 -> (day == 11) ? "th" : "st";
            case 2 -> (day == 12) ? "th" : "nd";
            case 3 -> (day == 13) ? "th" : "rd";
            default -> "th";
        };
        final var prettyDate =
            day + daySuffix + " of " + Constants.monthNames[date.getMonthValue() - 1] + ' ' + date.getYear();
        return Node.simple(Tag.P, Seq.of(
            new Node.Text("Published on the "),
            new Node.Element(
                Tag.TIME,
                Seq.of(Attribute.of("datetime", renderIsoDate(date))),
                Seq.of(new Node.Text(prettyDate))
            )
        ));
    }

    private static @NotNull String renderIsoDate(final @NotNull LocalDate date) {
        return date.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    private @Nullable Node.Element renderArticleTopics(final @NotNull Seq<String> topics) {
        if (topics.isEmpty()) {
            return null;
        }
        var contents = Seq.<Node>of(new Node.Text("Topics: "));
        boolean needsSeparator = false;
        for (final String topicName : topics) {
            if (needsSeparator) {
                contents = contents.appended(new Node.Text(", "));
            }
            contents = contents.appended(renderSimpleLink(headerRenderMode.getTopicArchiveUri(topicName), topicName));
            needsSeparator = true;
        }
        return Node.simple(Tag.P, contents);
    }

    private static @NotNull Node.Element renderTableOfContents(final @NotNull Seq<Section> childrenOfRoot) {
        return new Node.Element(
            Tag.NAV,
            Seq.of(Attribute.of("class", "toc"), Attribute.of("aria-labelledby", "toc-label")),
            Seq.of(Constants.tocLabel, renderTableOfContentsList(childrenOfRoot))
        );
    }

    private static @NotNull Node.Element renderTableOfContentsList(final @NotNull Seq<Section> children) {
        return Node.simple(Tag.OL, children.map(child -> {
            final var grandchildren = child.children();
            final var link = renderSimpleLink('#' + child.identifier().symbolName(), child.header());
            return Node.simple(
                Tag.LI,
                grandchildren.isEmpty() ? Seq.of(link) : Seq.of(link, renderTableOfContentsList(grandchildren))
            );
        }));
    }

    private static @NotNull Node.Element renderSubsection(final @NotNull Section section, final int nestingLevel) {
        return new Node.Element(
            Tag.SECTION,
            Seq.of(Attribute.of("id", section.identifier().symbolName())),
            section.body()
                .prepended(renderSectionHeader(section, nestingLevel))
                .concat(section.children().map(child -> renderSubsection(child, nestingLevel + 1)))
        );
    }

    private static @NotNull Node.Element renderSectionHeader(final @NotNull Section section, final int nestingLevel) {
        final var headingTag = Tag.byHtmlName("h" + Math.min(nestingLevel, 6));
        assert headingTag != null;
        return renderHeading(headingTag, '#' + section.identifier().symbolName(), section.header());
    }

    private static @NotNull Node.Element renderHeading(
        final @NotNull Tag tag,
        final @NotNull String target,
        final @NotNull String text
    ) {
        return Node.simple(tag, new Node.Element(
            Tag.A,
            Seq.of(Attribute.of("class", "section-link"), Attribute.of("href", target)),
            Seq.of(Node.simple(Tag.SPAN, new Node.Text(text)), Constants.sectionLinkMarker)
        ));
    }

    private static @NotNull Node.Element renderSimpleLink(final @NotNull String href, final @NotNull String text) {
        return new Node.Element(Tag.A, Seq.of(Attribute.of("href", href)), Seq.of(new Node.Text(text)));
    }

    private static @NotNull Node.Element renderBottomNav(
        final @Nullable DomainRelativeUri predecessorUri,
        final @Nullable DomainRelativeUri successorUri
    ) {
        final var predecessorLink = new Node.Element(
            Tag.LI,
            Seq.of(Attribute.of("class", "prev")),
            (predecessorUri == null)
                ? Seq.empty()
                : Seq.of(new Node.Element(Tag.A,
                Seq.of(Attribute.of("rel", "prev"), Attribute.of("href", predecessorUri.toString())),
                Seq.of(Constants.decorativeLeftArrow, new Node.Text("Older"))))
        );
        final var successorLink = new Node.Element(
            Tag.LI,
            Seq.of(Attribute.of("class", "next")),
            (successorUri == null)
                ? Seq.empty()
                : Seq.of(new Node.Element(Tag.A,
                Seq.of(Attribute.of("rel", "next"), Attribute.of("href", successorUri.toString())),
                Seq.of(new Node.Text("Newer"), Constants.decorativeRightArrow)))
        );
        return new Node.Element(
            Tag.NAV,
            Seq.of(Attribute.of("aria-label", "Chronological, secondary")),
            Seq.of(new Node.Element(
                Tag.UL,
                Seq.of(Attribute.of("id", "order-nav")),
                Seq.of(Constants.topLink, predecessorLink, Constants.archivesLink, successorLink)
            ))
        );
    }

    private final @NotNull HeaderRenderMode headerRenderMode;

    private static final class LineNumberRenderer {
        private LineNumberRenderer(final @NotNull Node.Element container) {
            originalContainer = container;
        }

        private @NotNull Node.Element render() {
            renderElement(new PendingElement(originalContainer, null));
            final var result = new Node.Element(
                originalContainer.tag(),
                Attributes.addedClass(originalContainer.attributes(), Constants.numberedClassName),
                newRootChildren
            );
            newRootChildren = Seq.empty();
            return result;
        }

        private void renderElement(final @NotNull PendingElement element) {
            for (final var child : element.original.children()) {
                switch (child) {
                    case Node.Text node -> renderText(node, element);
                    case Node.Element node -> renderElement(new PendingElement(node, element));
                }
            }
            closeElement(element);
        }

        private void renderText(final @NotNull Node.Text textNode, final @NotNull PendingElement parent) {
            final var string = textNode.text();
            final var length = string.length();
            final var firstLineFeedPosition = string.indexOf('\n');
            if (firstLineFeedPosition == -1) {
                parent.appendChild(textNode);
            } else if (firstLineFeedPosition == length - 1) {
                parent.appendChild(textNode);
                closeLine(parent);
            } else {
                int position = 0;
                int lineFeedPosition = firstLineFeedPosition;
                do {
                    final var nextLineStart = lineFeedPosition + 1;
                    parent.appendChild(new Node.Text(string.substring(position, nextLineStart)));
                    position = nextLineStart;
                    closeLine(parent);
                } while ((lineFeedPosition = string.indexOf('\n', position)) != -1);
                if (position < length) {
                    parent.appendChild(new Node.Text(string.substring(position, length)));
                }
            }
        }

        private void closeLine(final @NotNull PendingElement parent) {
            for (var it = parent; it != null; it = it.parent) {
                closeElement(it);
            }
        }

        private void closeElement(final @NotNull PendingElement element) {
            final var children = element.newChildren;
            element.newChildren = Seq.empty();
            if (element.parent == null) {
                if (!children.isEmpty()) {
                    appendLine(children);
                }
            } else {
                if (!children.isEmpty() || element.original.children().isEmpty()) {
                    final var original = element.original;
                    final var clone = new Node.Element(original.tag(), original.attributes(), children);
                    element.parent.appendChild(clone);
                }
            }
        }

        private void appendLine(final @NotNull Seq<@NotNull Node> nodes) {
            newRootChildren = newRootChildren.appended(Constants.lineNumberMarker).appended(wrapLineContent(nodes));
        }

        private static @NotNull Node.Element wrapLineContent(final @NotNull Seq<@NotNull Node> nodes) {
            return (nodes.exactSize() == 1 && nodes.first() instanceof Node.Element element)
                ? element
                : Node.simple(Tag.SPAN, nodes);
        }

        private final @NotNull Node.Element originalContainer;
        private @NotNull Seq<@NotNull Node> newRootChildren = Seq.empty();

        private static final class PendingElement {
            private PendingElement(final @NotNull Node.Element original, final @Nullable PendingElement parent) {
                this.original = original;
                this.parent = parent;
            }

            void appendChild(final @NotNull Node node) {
                newChildren = newChildren.appended(node);
            }

            private final @NotNull Node.Element original;
            private final @Nullable PendingElement parent;
            private @NotNull Seq<@NotNull Node> newChildren = Seq.empty();
        }
    }

    // Put constants in a separate class, so that they're created on first access rather than when the renderer class is
    // loaded.
    private static final class Constants {
        private static final Seq<Node> headPrefix = Seq.of(
            Node.empty(Tag.META_CHARSET_UTF8, Seq.empty()),
            Node.empty(Tag.META_NAMED, Seq.of(
                Attribute.of("name", "viewport"),
                Attribute.of("content", "width=device-width, initial-scale=1")
            )),
            Node.empty(Tag.META_NAMED, Seq.of(
                Attribute.of("name", "generator"),
                Attribute.of("content", "Some custom Common Lisp")
            ))
        );

        private static final Seq<Node> headSuffix = Seq.of(
            Node.empty(Tag.LINK, Seq.of(
                Attribute.of("rel", "alternate"),
                Attribute.of("href", '/' + RenderConstants.feedFileName),
                Attribute.of("title", RenderConstants.siteTitle),
                Attribute.of("type", "application/rss+xml")
            )),
            Node.empty(Tag.LINK, Seq.of(
                Attribute.of("rel", "stylesheet"),
                Attribute.of("href", "/static/theme.css")
            )),
            Node.empty(Tag.LINK, Seq.of(
                Attribute.of("rel", "license"),
                Attribute.of("href", "https://creativecommons.org/licenses/by-sa/4.0/")
            ))
        );

        private static final Seq<Node> header = Seq.of(
            new Node.Element(
                Tag.HEADER,
                Seq.of(Attribute.of("id", "top-header")),
                Seq.of(
                    new Node.Element(
                        Tag.A,
                        Seq.of(
                            Attribute.of("id", "skip-nav"),
                            Attribute.of("class", "at-only"),
                            Attribute.of("href", "#main")
                        ),
                        Seq.of(new Node.Text("Skip to main content"))
                    ),
                    Node.simple(Tag.SPAN, new Node.Text(RenderConstants.siteTitle))
                )
            ),
            new Node.Element(
                Tag.NAV,
                Seq.of(Attribute.of("aria-label", "Primary")),
                Seq.of(new Node.Element(
                    Tag.UL,
                    Seq.of(Attribute.of("id", "top-nav")),
                    Seq.of(
                        Node.simple(Tag.LI, renderSimpleLink("/", "Main page")),
                        Node.simple(Tag.LI, renderSimpleLink("/archives/", "Archives")),
                        Node.simple(Tag.LI, renderSimpleLink("https://github.com/Fanael/fanael.github.io/", "GitHub")),
                        Node.simple(Tag.LI, new Node.Element(
                            Tag.A,
                            Seq.of(Attribute.of("rel", "author"), Attribute.of("href", "/pages/about.html")),
                            Seq.of(new Node.Text("About"))
                        ))
                    )
                ))
            )
        );

        private static final Node.Element footer = Node.simple(Tag.FOOTER, new Node.Element(Tag.UL,
            Seq.of(Attribute.of("id", "footer")),
            Seq.of(
                Node.simple(Tag.LI, new Node.Text("Powered by HTML & CSS")),
                Node.simple(Tag.LI, new Node.Text(RenderConstants.copyrightLine)),
                Node.simple(Tag.LI, Seq.of(
                    new Node.Text("Licensed under a "),
                    new Node.Element(
                        Tag.A,
                        Seq.of(
                            Attribute.of("rel", "license"),
                            Attribute.of("href", "https://creativecommons.org/licenses/by-sa/4.0/")
                        ),
                        Seq.of(new Node.Text("Creative Commons Attribution-ShareAlike 4.0 International License"))
                    )
                ))
            )
        ));

        private static final Attribute.String ariaHidden = new Attribute.String("aria-hidden", "true");

        private static final Node.Element topLink = new Node.Element(
            Tag.LI,
            Seq.of(Attribute.of("class", "top")),
            Seq.of(new Node.Element(
                Tag.A,
                Seq.of(Attribute.of("href", "#skip-nav")),
                Seq.of(
                    new Node.Element(Tag.SPAN, Seq.of(ariaHidden), Seq.of(new Node.Text("↑ "))),
                    new Node.Text("Top"),
                    new Node.Element(Tag.SPAN, Seq.of(ariaHidden), Seq.of(new Node.Text(" ↑")))
                )
            ))
        );

        private static final Node.Element archivesLink =
            Node.simple(Tag.LI, renderSimpleLink("/archives/", "Blog archives"));

        private static final Node.Element decorativeLeftArrow =
            new Node.Element(Tag.SPAN, Seq.of(ariaHidden), Seq.of(new Node.Text("← ")));

        private static final Node.Element decorativeRightArrow =
            new Node.Element(Tag.SPAN, Seq.of(ariaHidden), Seq.of(new Node.Text(" →")));

        private static final Node.Element simpleBottomNav = renderBottomNav(null, null);

        private static final Node.Element sectionLinkMarker =
            new Node.Element(Tag.SPAN, Seq.of(ariaHidden), Seq.of(new Node.Text(" §")));

        private static final Node.Element tocLabel = new Node.Element(
            Tag.SPAN,
            Seq.of(Attribute.of("id", "toc-label")),
            Seq.of(new Node.Text("Table of contents"))
        );

        private static final Node.Element lineNumberMarker = Node.empty(Tag.SPAN, Attribute.of("class", "cx-ln"));

        private static final Node.Text newLine = new Node.Text("\n");

        private static final Attribute.String htmlLang = new Attribute.String("lang", "en");

        private static final Attribute.String idMain = new Attribute.String("id", "main");

        private static final String numberedClassName = "cx-numbered";

        private static final String[] monthNames = {
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
        };
    }
}
