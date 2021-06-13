// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
package greenspun.generator;

import java.net.URI;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
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
        return Node.build(Tag.HTML, html -> {
            html.set(Constants.htmlLang);
            html.append(renderHead("Blog archive index", "Main page of blog archives"));
            html.append(Node.build(Tag.BODY, body -> {
                body.append(Constants.header);
                body.append(Node.build(Tag.MAIN, main -> {
                    main.set(Constants.idMain);
                    main.append(renderArchiveIndexBody(quarters, topics, articles));
                }));
                body.append(renderBottomNav(null, null));
                body.append(Constants.footer);
            }));
        });
    }

    @NotNull Node.Element renderFrontPage(final @NotNull List<@NotNull ArchivedArticle> articles) {
        return Node.build(Tag.HTML, html -> {
            html.set(Constants.htmlLang);
            html.append(renderHead(null, "Latest posts on " + RenderConstants.siteTitle));
            html.append(Node.build(Tag.BODY, body -> {
                body.append(Constants.header);
                body.append(Node.build(Tag.MAIN, main -> {
                    main.set(Constants.idMain);
                    main.append(renderFrontPageBody(articles));
                }));
                body.append(renderBottomNav(null, null));
                body.append(Constants.footer);
            }));
        });
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
        return Node.build(Tag.HTML, html -> {
            html.set(Constants.htmlLang);
            html.append(renderHead(article.article().title(), article.article().description()));
            html.append(Node.build(Tag.BODY, body -> {
                body.append(Constants.header);
                body.append(Node.build(Tag.MAIN, main -> {
                    main.set(Constants.idMain);
                    main.append(renderArticleBody(article));
                }));
                body.append(renderBottomNav(article.predecessorUri(), article.successorUri()));
                body.append(Constants.footer);
            }));
        });
    }

    private @NotNull ArrayList<Node.Element> renderFrontPageBody(final @NotNull List<ArchivedArticle> articles) {
        final var nodes = new ArrayList<Node.Element>();
        nodes.add(Node.build(Tag.HEADER,
            header -> header.append(Node.build(Tag.H1, h1 -> h1.appendText("Latest articles")))));
        if (articles.isEmpty()) {
            nodes.add(Node.build(Tag.P, p -> p.appendText("There are no articles yet.")));
        } else {
            nodes.addAll(renderExcerpts(articles));
        }
        return nodes;
    }

    private static @NotNull ImmutableList<Node.Element> renderArchiveIndexBody(
        final @NotNull List<ArchivedQuarter> quarters,
        final @NotNull List<ArchivedTopic> topics,
        final @NotNull List<ArchivedArticle> articles
    ) {
        final var quarterListElements = ImmutableList.map(quarters,
            quarter -> Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                a.set("href", quarter.uri().toString());
                a.appendText("The " + quarter.quarter());
            }))));
        final var topicListElements = ImmutableList.map(topics,
            topic -> Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                a.set("href", topic.uri().toString());
                a.appendText(topic.topic());
            }))));
        final var articleListElements = ImmutableList.map(articles,
            article -> Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                a.set("href", article.uri().toString());
                a.appendText(renderIsoDate(article.article().date()) + " — " + article.article().title());
            }))));
        return ImmutableList.of(
            Node.build(Tag.HEADER, header -> header.append(Node.build(Tag.H1, h1 -> h1.appendText("Blog archives")))),
            Node.build(Tag.SECTION, section -> {
                section.append(Node.build(Tag.H2, h2 -> h2.appendText("By date")));
                section.append(Node.build(Tag.UL, ul -> ul.append(quarterListElements)));
            }),
            Node.build(Tag.SECTION, section -> {
                section.append(Node.build(Tag.H2, h2 -> h2.appendText("By topic")));
                section.append(Node.build(Tag.UL, ul -> ul.append(topicListElements)));
            }),
            Node.build(Tag.SECTION, section -> {
                section.append(Node.build(Tag.H2, h2 -> h2.appendText("By article title")));
                section.append(Node.build(Tag.UL, ul -> ul.append(articleListElements)));
            })
        );
    }

    private @NotNull Node.Element renderArchive(
        final @NotNull String title,
        final @NotNull String header,
        final @NotNull List<ArchivedArticle> articles
    ) {
        return Node.build(Tag.HTML, html -> {
            html.set(Constants.htmlLang);
            html.append(renderHead(title, title));
            html.append(Node.build(Tag.BODY, body -> {
                body.append(Constants.header);
                body.append(Node.build(Tag.MAIN, main -> {
                    main.set(Constants.idMain);
                    main.append(Node.build(Tag.HEADER,
                        headerElement -> headerElement.append(Node.build(Tag.H1, h1 -> h1.appendText(header)))));
                    main.append(renderArchiveTableOfContents(articles));
                    main.append(renderExcerpts(articles));
                }));
                body.append(renderBottomNav(null, null));
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
                    ol.append(Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                        a.set("href", '#' + article.identifier());
                        a.append(new Node.Text(article.article().title()));
                    }))));
                }
            }));
        });
    }

    private @NotNull ImmutableList<Node.Element> renderExcerpts(final @NotNull List<ArchivedArticle> articles) {
        return ImmutableList.map(articles, article -> {
            final var title = article.article().title();
            return Node.build(Tag.ARTICLE, articleElement -> {
                articleElement.set("id", article.identifier());
                articleElement.append(Node.build(Tag.HEADER, header -> {
                    header.append(Node.build(Tag.H2, h2 -> h2.append(Node.build(Tag.A, a -> {
                        a.set("href", article.uri().toString());
                        a.append(new Node.Text(title));
                    }))));
                    header.append(renderPublicationDate(article.article().date()));
                    final var topics = renderArticleTopics(article.article().topics());
                    if (topics != null) {
                        header.append(topics);
                    }
                }));
                articleElement.append(article.article().rootSection().body());
                articleElement.append(Node.build(Tag.A, a -> {
                    a.set("class", "read-full");
                    a.set("href", article.uri().toString());
                    a.set("aria-label", "Read the full article: " + title);
                    a.appendText("Read the full article…");
                }));
            });
        });
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

    private @NotNull Node.Element renderArticleBody(final @NotNull ArticleToRender article) {
        final var innerArticle = article.article();
        return Node.build(Tag.ARTICLE, articleElement -> {
            articleElement.append(renderArticleHeader(article.article()));
            articleElement.append(innerArticle.rootSection().body());
            final var children = innerArticle.rootSection().children();
            if (!children.isEmpty() && !innerArticle.inhibitTableOfContents()) {
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
            final var nodes = new ArrayList<@NotNull Node>(2 * topics.size());
            for (final String topicName : topics) {
                nodes.add(Node.build(Tag.A, a -> {
                    a.set("href", headerRenderMode.getTopicArchiveUri(topicName));
                    a.appendText(topicName);
                }));
                nodes.add(new Node.Text(", "));
            }
            nodes.remove(nodes.size() - 1);
            p.append(nodes);
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
                    li.append(Node.build(Tag.A, a -> {
                        a.set("href", '#' + child.identifier().symbolName());
                        a.appendText(child.header());
                    }));
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
                    ul.append(Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                        a.set("href", "/");
                        a.appendText("Main page");
                    }))));
                    ul.append(Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                        a.set("href", "/archives/");
                        a.appendText("Archives");
                    }))));
                    ul.append(Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
                        a.set("href", "https://github.com/Fanael/fanael.github.io/");
                        a.appendText("GitHub");
                    }))));
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
            li.append(Node.build(Tag.A, a -> {
                a.set("href", "#skip-nav");
                a.appendText("↑ Top ↑");
            }));
        });

        private static final Node.Element archivesLink = Node.build(Tag.LI, li -> li.append(Node.build(Tag.A, a -> {
            a.set("href", "/archives/");
            a.appendText("Blog archives");
        })));

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
