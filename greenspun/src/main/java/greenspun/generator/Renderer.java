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
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The renderer: the primary means of turning article and archive page data structures into complete DOM trees.
 * <p>
 * Most of the interesting methods are package-private, because only {@link Generator} uses them.
 * <p>
 * This class is thread-safe: as it doesn't hold any mutable state whatsoever, multiple threads can access one instance
 * at the same time with no external synchronization.
 */
public final class Renderer {
    Renderer(final HeaderRenderMode headerRenderMode) {
        this.headerRenderMode = headerRenderMode;
    }

    /**
     * Wraps the given list of DOM nodes representing highlighted code into a single node.
     */
    public static Node wrapHighlightedCode(final Seq<Node> nodes, final String prettyLanguageName) {
        return wrapCodeBlock(Seq.of(Node.simple(Tag.CODE, nodes)), prettyLanguageName);
    }

    /**
     * Wraps the given list of DOM nodes representing a code block into a single node.
     */
    public static Node wrapCodeBlock(final Seq<Node> nodes, final String prettyLanguageName) {
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

    static Node.Element renderArchiveIndex(
        final Seq<ArchivedQuarter> quarters,
        final Seq<ArchivedTopic> topics,
        final Seq<ArchivedArticle> articles
    ) {
        return renderDocument(
            renderHead("Blog archive index", "Main page of blog archives"),
            Constants.simpleBottomNav,
            renderArchiveIndexBody(quarters, topics, articles)
        );
    }

    Node.Element renderFrontPage(final Seq<ArchivedArticle> articles) {
        final var header = Node.simple(Tag.HEADER, Node.simple(Tag.H1, new Node.Text("Latest articles")));
        return renderDocument(
            renderHead(null, "Latest posts on " + RenderConstants.siteTitle),
            Constants.simpleBottomNav,
            articles.isEmpty()
                ? Seq.of(header, Node.simple(Tag.P, new Node.Text("There are no articles yet.")))
                : renderExcerpts(articles).prepended(header)
        );
    }

    Node.Element renderTopicArchive(final String topicName, final Seq<ArchivedArticle> articles) {
        return renderArchive(
            "Blog archives for topic " + topicName,
            "Archives for topic " + topicName,
            articles
        );
    }

    Node.Element renderQuarterlyArchive(final Quarter quarter, final Seq<ArchivedArticle> articles) {
        return renderArchive(
            "Blog archives for the " + quarter,
            "Archives for the " + quarter,
            articles
        );
    }

    Node.Element renderArticle(final ArticleToRender article) {
        return renderDocument(
            renderHead(article.article().title(), article.article().description()),
            renderBottomNav(article.predecessorUri(), article.successorUri()),
            Seq.of(renderArticleBody(article.article()))
        );
    }

    private static Seq<Node> renderLineNumbers(final Seq<Node> nodes) {
        return (nodes.exactSize() == 1 && nodes.first() instanceof Node.Element parent)
            ? Seq.of(new LineNumberRenderer(parent).render())
            : nodes;
    }

    private static Seq<Node> renderArchiveIndexBody(
        final Seq<ArchivedQuarter> quarters,
        final Seq<ArchivedTopic> topics,
        final Seq<ArchivedArticle> articles
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

    private static <T> Node.Element renderArchiveIndexSection(
        final String name,
        final Seq<? extends T> elements,
        final Function<? super @NonNull T, ? extends Node> linkFormatter
    ) {
        return Node.simple(Tag.SECTION, Seq.of(
            Node.simple(Tag.H2, new Node.Text(name)),
            Node.simple(Tag.UL, elements.map(element -> Node.simple(Tag.LI, linkFormatter.apply(element))))
        ));
    }

    private Node.Element renderArchive(final String title, final String header, final Seq<ArchivedArticle> articles) {
        return renderDocument(
            renderHead(title, title),
            Constants.simpleBottomNav,
            renderExcerpts(articles)
                .prepended(renderArchiveTableOfContents(articles))
                .prepended(Node.simple(Tag.HEADER, Node.simple(Tag.H1, new Node.Text(header))))
        );
    }

    private static Node.Element renderDocument(
        final Node.Element head,
        final Node.Element bottomNav,
        final Seq<Node> mainContent
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

    private static Node.Element renderArchiveTableOfContents(final Seq<ArchivedArticle> articles) {
        return new Node.Element(
            Tag.NAV,
            Seq.of(Attribute.of("class", "toc"), Attribute.of("aria-labelledby", "toc-label")),
            Seq.of(
                Constants.tocLabel,
                Node.simple(Tag.OL, articles.map(article ->
                    Node.simple(Tag.LI, renderSimpleLink('#' + article.identifier(), article.article().title()))))));
    }

    private Seq<Node> renderExcerpts(final Seq<ArchivedArticle> articles) {
        final var result = new Seq.Builder<Node>();
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
            result.append(new Node.Element(
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
        return result.toSeq();
    }

    private static Node.Element renderHead(final @Nullable String title, final String description) {
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

    private Node.Element renderArticleBody(final Article article) {
        final var contents = new Seq.Builder<>(article.rootSection().body().prepended(renderArticleHeader(article)));
        final var children = article.rootSection().children();
        if (!children.isEmpty() && !article.inhibitTableOfContents()) {
            contents.append(renderTableOfContents(children));
        }
        for (final var child : children) {
            contents.append(renderSubsection(child, 2));
        }
        return Node.simple(Tag.ARTICLE, contents.toSeq());
    }

    private Node.Element renderArticleHeader(final Article article) {
        final var contents = new Seq.Builder<Node>(Seq.of(renderHeading(Tag.H1, "#main", article.title())));
        if (headerRenderMode.shouldRender()) {
            contents.append(renderPublicationDate(article.date()));
            final var topicsNode = renderArticleTopics(article.topics());
            if (topicsNode != null) {
                contents.append(topicsNode);
            }
        }
        return Node.simple(Tag.HEADER, contents.toSeq());
    }

    private static Node.Element renderPublicationDate(final LocalDate date) {
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

    private static String renderIsoDate(final LocalDate date) {
        return date.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    private Node.@Nullable Element renderArticleTopics(final Seq<String> topics) {
        if (topics.isEmpty()) {
            return null;
        }
        final var contents = new Seq.Builder<Node>(Seq.of(new Node.Text("Topics: ")));
        boolean needsSeparator = false;
        for (final String topicName : topics) {
            if (needsSeparator) {
                contents.append(new Node.Text(", "));
            }
            contents.append(renderSimpleLink(headerRenderMode.getTopicArchiveUri(topicName), topicName));
            needsSeparator = true;
        }
        return Node.simple(Tag.P, contents.toSeq());
    }

    private static Node.Element renderTableOfContents(final Seq<Section> childrenOfRoot) {
        return new Node.Element(
            Tag.NAV,
            Seq.of(Attribute.of("class", "toc"), Attribute.of("aria-labelledby", "toc-label")),
            Seq.of(Constants.tocLabel, renderTableOfContentsList(childrenOfRoot))
        );
    }

    private static Node.Element renderTableOfContentsList(final Seq<Section> children) {
        return Node.simple(Tag.OL, children.map(child -> {
            final var grandchildren = child.children();
            final var link = renderSimpleLink('#' + child.identifier().symbolName(), child.header());
            return Node.simple(
                Tag.LI,
                grandchildren.isEmpty() ? Seq.of(link) : Seq.of(link, renderTableOfContentsList(grandchildren))
            );
        }));
    }

    private static Node.Element renderSubsection(final Section section, final int nestingLevel) {
        return new Node.Element(
            Tag.SECTION,
            Seq.of(Attribute.of("id", section.identifier().symbolName())),
            section.body()
                .prepended(renderSectionHeader(section, nestingLevel))
                .concat(section.children().map(child -> renderSubsection(child, nestingLevel + 1)))
        );
    }

    private static Node.Element renderSectionHeader(final Section section, final int nestingLevel) {
        final var headingTag = Tag.byHtmlName("h" + Math.min(nestingLevel, 6));
        assert headingTag != null : "Heading tag not found? @AssumeAssertion(nullness)";
        return renderHeading(headingTag, '#' + section.identifier().symbolName(), section.header());
    }

    private static Node.Element renderHeading(final Tag tag, final String target, final String text) {
        return Node.simple(tag, new Node.Element(
            Tag.A,
            Seq.of(Attribute.of("class", "section-link"), Attribute.of("href", target)),
            Seq.of(Node.simple(Tag.SPAN, new Node.Text(text)), Constants.sectionLinkMarker)
        ));
    }

    private static Node.Element renderSimpleLink(final String href, final String text) {
        return new Node.Element(Tag.A, Seq.of(Attribute.of("href", href)), Seq.of(new Node.Text(text)));
    }

    private static Node.Element renderBottomNav(
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

    private final HeaderRenderMode headerRenderMode;

    private static final class LineNumberRenderer {
        private LineNumberRenderer(final Node.Element container) {
            originalContainer = container;
        }

        private Node.Element render() {
            renderElement(new PendingElement(originalContainer, null));
            return new Node.Element(
                originalContainer.tag(),
                Attributes.addedClass(originalContainer.attributes(), Constants.numberedClassName),
                newRootChildren.take()
            );
        }

        private void renderElement(final PendingElement element) {
            for (final var child : element.original.children()) {
                switch (child) {
                    case Node.Text node -> renderText(node, element);
                    case Node.Element node -> renderElement(new PendingElement(node, element));
                }
            }
            closeElement(element);
        }

        private void renderText(final Node.Text textNode, final PendingElement parent) {
            final var string = textNode.text();
            final var length = string.length();
            final var firstLineFeedPosition = string.indexOf('\n');
            if (firstLineFeedPosition == -1) {
                parent.newChildren.append(textNode);
            } else if (firstLineFeedPosition == length - 1) {
                parent.newChildren.append(textNode);
                closeLine(parent);
            } else {
                int position = 0;
                int lineFeedPosition = firstLineFeedPosition;
                do {
                    final var nextLineStart = lineFeedPosition + 1;
                    parent.newChildren.append(new Node.Text(string.substring(position, nextLineStart)));
                    position = nextLineStart;
                    closeLine(parent);
                } while ((lineFeedPosition = string.indexOf('\n', position)) != -1);
                if (position < length) {
                    parent.newChildren.append(new Node.Text(string.substring(position, length)));
                }
            }
        }

        private void closeLine(final PendingElement parent) {
            for (var it = parent; it != null; it = it.parent) {
                closeElement(it);
            }
        }

        private void closeElement(final PendingElement element) {
            final var children = element.newChildren.take();
            if (element.parent == null) {
                if (!children.isEmpty()) {
                    appendLine(children);
                }
            } else {
                if (!children.isEmpty() || element.original.children().isEmpty()) {
                    final var original = element.original;
                    final var clone = new Node.Element(original.tag(), original.attributes(), children);
                    element.parent.newChildren.append(clone);
                }
            }
        }

        private void appendLine(final Seq<Node> nodes) {
            newRootChildren.append(Constants.lineNumberMarker);
            newRootChildren.append(wrapLineContent(nodes));
        }

        private static Node.Element wrapLineContent(final Seq<Node> nodes) {
            return (nodes.exactSize() == 1 && nodes.first() instanceof Node.Element element)
                ? element
                : Node.simple(Tag.SPAN, nodes);
        }

        private final Node.Element originalContainer;
        private final Seq.Builder<Node> newRootChildren = new Seq.Builder<>();

        private static final class PendingElement {
            private PendingElement(final Node.Element original, final @Nullable PendingElement parent) {
                this.original = original;
                this.parent = parent;
            }

            private final Node.Element original;
            private final @Nullable PendingElement parent;
            private final Seq.Builder<Node> newChildren = new Seq.Builder<>();
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
            Node.empty(Tag.SCRIPT, Seq.of(
                Attribute.of("defer", true),
                Attribute.of("src", "/static/fixes.js")
            )),
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
                Node.simple(Tag.LI, new Node.Element(
                    Tag.A,
                    Seq.of(Attribute.of("href", "/pages/offline-mode.html")),
                    Seq.of(new Node.Text("Offline mode"))
                )),
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
