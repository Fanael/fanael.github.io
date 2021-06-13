// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
package greenspun.generator;

import java.net.URI;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import greenspun.article.Article;
import greenspun.article.Section;
import greenspun.dom.Attribute;
import greenspun.dom.Node;
import greenspun.dom.Tag;
import greenspun.util.collection.ImmutableList;
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
     * <p>
     * Exposed as public so that it's accessible to {@link greenspun.pygments.PygmentsCache}.
     */
    public static @NotNull Node wrapHighlightedCode(
        final @NotNull List<@NotNull Node> nodes,
        final @NotNull String prettyLanguageName
    ) {
        return Node.build(Tag.PRE, pre -> {
            pre.set("class", "codeblock");
            pre.set("data-code-language", prettyLanguageName);
            pre.append(Node.build(Tag.CODE, code -> code.append(nodes)));
        });
    }

    static @NotNull Node.Element renderArchiveIndex(
        final @NotNull List<@NotNull ArchivedQuarter> quarters,
        final @NotNull List<@NotNull ArchivedTopic> topics,
        final @NotNull List<@NotNull ArchivedArticle> articles
    ) {
        return renderDocument(
            renderHead("Blog archive index", "Main page of blog archives"),
            Constants.simpleBottomNav,
            main -> renderArchiveIndexBody(main, quarters, topics, articles)
        );
    }

    @NotNull Node.Element renderFrontPage(final @NotNull List<@NotNull ArchivedArticle> articles) {
        return renderDocument(
            renderHead(null, "Latest posts on " + RenderConstants.siteTitle),
            Constants.simpleBottomNav,
            main -> {
                main.append(Node.build(Tag.HEADER,
                    header -> header.append(Node.build(Tag.H1, h1 -> h1.appendText("Latest articles")))));
                if (articles.isEmpty()) {
                    main.append(Node.build(Tag.P, p -> p.appendText("There are no articles yet.")));
                } else {
                    renderExcerpts(main, articles);
                }
            }
        );
    }

    @NotNull Node.Element renderTopicArchive(
        final @NotNull String topicName,
        final @NotNull List<@NotNull ArchivedArticle> articles
    ) {
        return renderArchive(
            "Blog archives for topic " + topicName,
            "Archives for topic " + topicName,
            articles
        );
    }

    @NotNull Node.Element renderQuarterlyArchive(
        final @NotNull Quarter quarter,
        final @NotNull List<@NotNull ArchivedArticle> articles
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
            main -> main.append(renderArticleBody(article.article()))
        );
    }

    private static void renderArchiveIndexBody(
        final @NotNull Node.ElementBuilder parent,
        final @NotNull List<ArchivedQuarter> quarters,
        final @NotNull List<ArchivedTopic> topics,
        final @NotNull List<ArchivedArticle> articles
    ) {
        parent.append(Node.build(Tag.HEADER,
            header -> header.append(Node.build(Tag.H1, h1 -> h1.appendText("Blog archives")))));
        parent.append(renderArchiveIndexSection("By date", quarters,
            quarter -> renderSimpleLink(quarter.uri().toString(), "The " + quarter.quarter())));
        parent.append(renderArchiveIndexSection("By topic", topics,
            topic -> renderSimpleLink(topic.uri().toString(), topic.topic())));
        parent.append(renderArchiveIndexSection("By article title", articles,
            article -> renderSimpleLink(
                article.uri().toString(),
                renderIsoDate(article.article().date()) + " — " + article.article().title()
            )));
    }

    private static <T> Node.@NotNull Element renderArchiveIndexSection(
        final @NotNull String name,
        final @NotNull List<? extends T> elements,
        final @NotNull Function<? super T, ? extends Node> linkFormatter
    ) {
        return Node.build(Tag.SECTION, section -> {
            section.append(Node.build(Tag.H2, h2 -> h2.appendText(name)));
            section.append(Node.build(Tag.UL, ul -> {
                for (final var element : elements) {
                    ul.append(Node.build(Tag.LI, li -> li.append(linkFormatter.apply(element))));
                }
            }));
        });
    }

    private @NotNull Node.Element renderArchive(
        final @NotNull String title,
        final @NotNull String header,
        final @NotNull List<ArchivedArticle> articles
    ) {
        return renderDocument(
            renderHead(title, title),
            Constants.simpleBottomNav,
            main -> {
                main.append(Node.build(Tag.HEADER,
                    headerElement -> headerElement.append(Node.build(Tag.H1, h1 -> h1.appendText(header)))));
                main.append(renderArchiveTableOfContents(articles));
                renderExcerpts(main, articles);
            }
        );
    }

    private static @NotNull Node.Element renderDocument(
        final @NotNull Node.Element head,
        final @NotNull Node.Element bottomNav,
        final @NotNull Consumer<? super Node.ElementBuilder> mainBuildFunction
    ) {
        return Node.build(Tag.HTML, html -> {
            html.set(Constants.htmlLang);
            html.append(head);
            html.append(Node.build(Tag.BODY, body -> {
                body.append(Constants.header);
                body.append(Node.build(Tag.MAIN, main -> {
                    main.set(Constants.idMain);
                    mainBuildFunction.accept(main);
                }));
                body.append(bottomNav);
                body.append(Constants.footer);
            }));
        });
    }

    private static @NotNull Node.Element renderArchiveTableOfContents(final @NotNull List<ArchivedArticle> articles) {
        return Node.build(Tag.NAV, nav -> {
            nav.set("class", "toc");
            nav.set("aria-labelledby", "toc-label");
            nav.append(Constants.tocLabel);
            nav.append(Node.build(Tag.OL, ol -> {
                for (final var article : articles) {
                    ol.append(Node.build(Tag.LI,
                        li -> li.append(renderSimpleLink('#' + article.identifier(), article.article().title()))));
                }
            }));
        });
    }

    private void renderExcerpts(
        final @NotNull Node.ElementBuilder parent,
        final @NotNull List<ArchivedArticle> articles
    ) {
        for (final var article : articles) {
            final var innerArticle = article.article();
            final var title = innerArticle.title();
            parent.append(Node.build(Tag.ARTICLE, articleElement -> {
                articleElement.set("id", article.identifier());
                articleElement.append(Node.build(Tag.HEADER, header -> {
                    header.append(Node.build(Tag.H2,
                        h2 -> h2.append(renderSimpleLink(article.uri().toString(), title))));
                    header.append(renderPublicationDate(innerArticle.date()));
                    final var topics = renderArticleTopics(innerArticle.topics());
                    if (topics != null) {
                        header.append(topics);
                    }
                }));
                articleElement.append(innerArticle.rootSection().body());
                articleElement.append(Node.build(Tag.A, a -> {
                    a.set("class", "read-full");
                    a.set("href", article.uri().toString());
                    a.set("aria-label", "Read the full article: " + title);
                    a.appendText("Read the full article…");
                }));
            }));
        }
    }

    private static @NotNull Node.Element renderHead(
        final @Nullable String title,
        final @NotNull String description
    ) {
        return Node.build(Tag.HEAD, head -> {
            head.append(Constants.headPrefix);
            head.append(Node.build(Tag.META_NAMED, meta -> {
                meta.set("name", "description");
                meta.set("content", description);
            }));
            head.append(Constants.headSuffix);
            final var effectiveTitle =
                (title != null) ? (title + " - " + RenderConstants.siteTitle) : RenderConstants.siteTitle;
            head.append(Node.build(Tag.TITLE, titleElement -> titleElement.appendText(effectiveTitle)));
        });
    }

    private @NotNull Node.Element renderArticleBody(final @NotNull Article article) {
        return Node.build(Tag.ARTICLE, articleElement -> {
            articleElement.append(renderArticleHeader(article));
            articleElement.append(article.rootSection().body());
            final var children = article.rootSection().children();
            if (!children.isEmpty() && !article.inhibitTableOfContents()) {
                articleElement.append(renderTableOfContents(children));
            }
            for (final var child : children) {
                articleElement.append(renderSubsection(child, 2));
            }
        });
    }

    private @NotNull Node.Element renderArticleHeader(final @NotNull Article article) {
        return Node.build(Tag.HEADER, header -> {
            header.append(Node.build(Tag.H1, h1 -> {
                h1.appendText(article.title());
                h1.append(Constants.rootSectionHeaderLink);
            }));
            if (headerRenderMode.shouldRender()) {
                header.append(renderPublicationDate(article.date()));
                final var topicsNode = renderArticleTopics(article.topics());
                if (topicsNode != null) {
                    header.append(topicsNode);
                }
            }
        });
    }

    private static @NotNull Node.Element renderPublicationDate(final @NotNull LocalDate date) {
        return Node.build(Tag.P, p -> {
            p.appendText("Published on the ");
            p.append(Node.build(Tag.TIME, time -> {
                time.set("datetime", renderIsoDate(date));
                final var day = date.getDayOfMonth();
                final var daySuffix = switch (day % 10) {
                    case 1 -> (day == 11) ? "th" : "st";
                    case 2 -> (day == 12) ? "th" : "nd";
                    case 3 -> (day == 13) ? "th" : "rd";
                    default -> "th";
                };
                final var prettyDate =
                    day + daySuffix + " of " + Constants.monthNames[date.getMonthValue() - 1] + ' ' + date.getYear();
                time.appendText(prettyDate);
            }));
        });
    }

    private static @NotNull String renderIsoDate(final @NotNull LocalDate date) {
        return date.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    private @Nullable Node.Element renderArticleTopics(final @NotNull ImmutableList<String> topics) {
        return topics.isEmpty() ? null : Node.build(Tag.P, p -> {
            p.appendText("Topics: ");
            boolean needsSeparator = false;
            for (final String topicName : topics) {
                if (needsSeparator) {
                    p.appendText(", ");
                }
                p.append(renderSimpleLink(headerRenderMode.getTopicArchiveUri(topicName), topicName));
                needsSeparator = true;
            }
        });
    }

    private static @NotNull Node.Element renderTableOfContents(final @NotNull ImmutableList<Section> childrenOfRoot) {
        return Node.build(Tag.NAV, nav -> {
            nav.set("class", "toc");
            nav.set("aria-labelledby", "toc-label");
            nav.append(Constants.tocLabel);
            nav.append(renderTableOfContentsList(childrenOfRoot));
        });
    }

    private static @NotNull Node.Element renderTableOfContentsList(final @NotNull ImmutableList<Section> children) {
        return Node.build(Tag.OL, ol -> {
            for (final var child : children) {
                ol.append(Node.build(Tag.LI, li -> {
                    li.append(renderSimpleLink('#' + child.identifier().symbolName(), child.header()));
                    final var grandchildren = child.children();
                    if (!grandchildren.isEmpty()) {
                        li.append(renderTableOfContentsList(grandchildren));
                    }
                }));
            }
        });
    }

    private static @NotNull Node.Element renderSubsection(final @NotNull Section section, final int nestingLevel) {
        return Node.build(Tag.SECTION, sectionElement -> {
            sectionElement.set("id", section.identifier().symbolName());
            sectionElement.append(renderSectionHeader(section, nestingLevel));
            sectionElement.append(section.body());
            for (final var child : section.children()) {
                sectionElement.append(renderSubsection(child, nestingLevel + 1));
            }
        });
    }

    private static @NotNull Node.Element renderSectionHeader(final @NotNull Section section, final int nestingLevel) {
        final var headingTag = Tag.byHtmlName("h" + Math.min(nestingLevel, 6));
        assert headingTag != null;
        return Node.build(headingTag, heading -> {
            heading.appendText(section.header());
            heading.append(renderSectionHeaderLink(section.identifier().symbolName()));
        });
    }

    private static @NotNull Node.Element renderSectionHeaderLink(final @NotNull String targetId) {
        return Node.build(Tag.A, a -> {
            a.set("class", "section-header-link");
            a.set("href", '#' + targetId);
            a.set("aria-label", "anchor");
            a.appendText("§");
        });
    }

    private static @NotNull Node.Element renderSimpleLink(final @NotNull String href, final @NotNull String text) {
        return Node.build(Tag.A, a -> {
            a.set("href", href);
            a.appendText(text);
        });
    }

    private static @NotNull Node.Element renderBottomNav(
        final @Nullable URI predecessorUri,
        final @Nullable URI successorUri
    ) {
        return Node.build(Tag.NAV, nav -> {
            nav.set("aria-label", "Chronological, secondary");
            nav.append(Node.build(Tag.UL, ul -> {
                ul.set("id", "prevnext");
                ul.append(Constants.topLink);
                ul.append(Node.build(Tag.LI, li -> {
                    li.set("class", "prev");
                    if (predecessorUri != null) {
                        li.append(Node.build(Tag.A, a -> {
                            a.set("rel", "prev");
                            a.set("href", predecessorUri.toString());
                            a.appendText("← Older");
                        }));
                    }
                }));
                ul.append(Constants.archivesLink);
                ul.append(Node.build(Tag.LI, li -> {
                    li.set("class", "next");
                    if (successorUri != null) {
                        li.append(Node.build(Tag.A, a -> {
                            a.set("rel", "next");
                            a.set("href", successorUri.toString());
                            a.appendText("Newer →");
                        }));
                    }
                }));
            }));
        });
    }

    private final @NotNull HeaderRenderMode headerRenderMode;

    // Put constants in a separate class, so that they're created on first access rather than when the renderer class is
    // loaded.
    private static final class Constants {
        private static final ImmutableList<Node.Element> headPrefix = ImmutableList.of(
            Node.makeEmptyElement(Tag.META_CHARSET_UTF8),
            Node.build(Tag.META_NAMED, meta -> {
                meta.set("name", "viewport");
                meta.set("content", "width=device-width, initial-scale=1");
            }),
            Node.build(Tag.META_HTTP_EQUIV, meta -> {
                meta.set("name", "Content-Security-Policy");
                meta.set("content", "default-src 'self'; object-src 'none'");
            }),
            Node.build(Tag.META_NAMED, meta -> {
                meta.set("name", "generator");
                meta.set("content", "Some custom Common Lisp");
            })
        );

        private static final ImmutableList<Node.Element> headSuffix = ImmutableList.of(
            Node.build(Tag.LINK, link -> {
                link.set("rel", "alternate");
                link.set("href", '/' + RenderConstants.feedFileName);
                link.set("title", RenderConstants.siteTitle);
                link.set("type", "application/rss+xml");
            }),
            Node.build(Tag.LINK, link -> {
                link.set("rel", "stylesheet");
                link.set("href", "/static/theme.css");
            }),
            Node.build(Tag.LINK, link -> {
                link.set("rel", "license");
                link.set("href", "https://creativecommons.org/licenses/by-sa/4.0/");
            })
        );

        private static final ImmutableList<Node.Element> header = ImmutableList.of(
            Node.build(Tag.A, a -> {
                a.set("id", "skip-nav");
                a.set("href", "#main");
                a.appendText("Skip to main content");
            }),
            Node.build(Tag.HEADER, header -> {
                header.set("id", "mainheader");
                header.appendText(RenderConstants.siteTitle);
            }),
            Node.build(Tag.NAV, nav -> {
                nav.set("aria-label", "Primary");
                nav.append(Node.build(Tag.UL, ul -> {
                    ul.set("id", "navmenu");
                    ul.append(Node.build(Tag.LI, li -> li.append(renderSimpleLink("/", "Main page"))));
                    ul.append(Node.build(Tag.LI, li -> li.append(renderSimpleLink("/archives/", "Archives"))));
                    ul.append(Node.build(Tag.LI,
                        li -> li.append(renderSimpleLink("https://github.com/Fanael/fanael.github.io/", "GitHub"))));
                    ul.append(Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                        a.set("rel", "author");
                        a.set("href", "/pages/about.html");
                        a.appendText("About");
                    }))));
                }));
            })
        );

        private static final Node.Element footer =
            Node.build(Tag.FOOTER, footer -> footer.append(Node.build(Tag.UL, ul -> {
                ul.set("id", "footerstuff");
                ul.append(Node.build(Tag.LI, li -> li.appendText("Powered by HTML & CSS")));
                ul.append(Node.build(Tag.LI, li -> li.appendText(RenderConstants.copyrightLine)));
                ul.append(Node.build(Tag.LI, li -> {
                    li.appendText("Licensed under a ");
                    li.append(Node.build(Tag.A, a -> {
                        a.set("rel", "license");
                        a.set("href", "https://creativecommons.org/licenses/by-sa/4.0/");
                        a.appendText("Creative Commons Attribution-ShareAlike 4.0 International License");
                    }));
                }));
            })));

        private static final Node.Element topLink = Node.build(Tag.LI, li -> {
            li.set("class", "top");
            li.append(renderSimpleLink("#skip-nav", "↑ Top ↑"));
        });

        private static final Node.Element archivesLink = Node.build(Tag.LI,
            li -> li.append(renderSimpleLink("/archives/", "Blog archives")));

        private static final Node.Element simpleBottomNav = renderBottomNav(null, null);

        private static final Node.Element rootSectionHeaderLink = renderSectionHeaderLink("main");

        private static final Node.Element tocLabel = Node.build(Tag.SPAN, span -> {
            span.set("id", "toc-label");
            span.appendText("Table of contents");
        });

        private static final Attribute.String htmlLang = new Attribute.String("lang", "en");

        private static final Attribute.String idMain = new Attribute.String("id", "main");

        private static final String[] monthNames = {
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
        };
    }
}
