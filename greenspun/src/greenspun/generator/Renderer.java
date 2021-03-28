// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
package greenspun.generator;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import greenspun.article.Section;
import greenspun.dom.Attribute;
import greenspun.dom.Node;
import greenspun.dom.Tag;
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
@SuppressWarnings("ClassCanBeRecord")
public final class Renderer {
    Renderer(
        final @NotNull HeaderRenderMode headerRenderMode,
        final @NotNull Function<@NotNull String, @Nullable String> convertTopicToArchiveUrl
    ) {
        this.headerRenderMode = headerRenderMode;
        this.convertTopicToArchiveUrl = convertTopicToArchiveUrl;
    }

    /**
     * Wraps the given list of DOM nodes representing highlighted code into a single node.
     * <p>
     * Exposed as public so that it's accessible to {@link greenspun.article.HtslConverter}.
     */
    public static @NotNull Node wrapHighlightedCode(
        final @NotNull List<@NotNull Node> nodes,
        final @NotNull String prettyLanguageName
    ) {
        return Node.buildElement(Tag.PRE)
            .setAttribute("class", "codeblock")
            .setAttribute("data-code-language", prettyLanguageName)
            .appendChild(Node.buildElement(Tag.CODE).appendChildren(nodes))
            .toElement();
    }

    static @NotNull Node renderTopicArchive(
        final @NotNull String topicName,
        final @NotNull List<@NotNull ArchivedArticle> articles,
        final @NotNull String canonicalUrl
    ) {
        return renderArchive(
            "Blog archives for topic " + topicName,
            "Archives for topic " + topicName,
            articles,
            canonicalUrl
        );
    }

    static @NotNull Node renderQuarterlyArchive(
        final @NotNull Quarter quarter,
        final @NotNull List<@NotNull ArchivedArticle> articles,
        final @NotNull String canonicalUrl
    ) {
        return renderArchive(
            "Blog archives for the " + quarter,
            "Archives for the " + quarter,
            articles,
            canonicalUrl
        );
    }

    static @NotNull Node renderArchiveIndex(
        final @NotNull List<@NotNull ArchivedQuarter> quarters,
        final @NotNull List<@NotNull ArchivedTopic> topics,
        final @NotNull List<@NotNull ArchivedArticle> articles,
        final @NotNull String canonicalUrl
    ) {
        final var quarterListElements = quarters.stream()
            .map(quarter -> Node.buildElement(Tag.LI)
                .appendChild(Node.buildElement(Tag.A)
                    .setAttribute("href", quarter.archiveUrl())
                    .appendChild(new Node.Text("The " + quarter.quarter())))
                .toElement())
            .toList();
        final var topicListElements = topics.stream()
            .map(topic -> Node.buildElement(Tag.LI)
                .appendChild(Node.buildElement(Tag.A)
                    .setAttribute("href", topic.archiveUrl())
                    .appendChild(new Node.Text(topic.topic())))
                .toElement())
            .toList();
        final var articleListElements = articles.stream()
            .map(article -> Node.buildElement(Tag.LI)
                .appendChild(Node.buildElement(Tag.A)
                    .setAttribute("href", article.url())
                    .appendChild(
                        new Node.Text(renderIsoDate(article.article().date()) + " — " + article.article().title())))
                .toElement())
            .toList();
        return Node.buildElement(Tag.HTML)
            .setAttribute(Constants.htmlLang)
            .appendChild(renderHead("Blog archive index", canonicalUrl, null))
            .appendChild(Node.buildElement(Tag.BODY)
                .appendChildren(Constants.header)
                .appendChild(Node.buildElement(Tag.MAIN)
                    .setAttribute(Constants.idMain)
                    .appendChild(Node.buildElement(Tag.HEADER)
                        .appendChild(Node.buildElement(Tag.H1).appendChild(new Node.Text("Blog archives"))))
                    .appendChild(Node.buildElement(Tag.SECTION)
                        .appendChild(Node.buildElement(Tag.H2).appendChild(new Node.Text("By date")))
                        .appendChild(Node.buildElement(Tag.UL).appendChildren(quarterListElements)))
                    .appendChild(Node.buildElement(Tag.SECTION)
                        .appendChild(Node.buildElement(Tag.H2).appendChild(new Node.Text("By topic")))
                        .appendChild(Node.buildElement(Tag.UL).appendChildren(topicListElements)))
                    .appendChild(Node.buildElement(Tag.SECTION)
                        .appendChild(Node.buildElement(Tag.H2).appendChild(new Node.Text("By article title")))
                        .appendChild(Node.buildElement(Tag.UL).appendChildren(articleListElements))))
                .appendChild(renderBottomNav(null, null))
                .appendChild(Constants.footer))
            .toElement();
    }

    @NotNull Node renderArticle(final @NotNull ArticleToRender article) {
        return Node.buildElement(Tag.HTML)
            .setAttribute(Constants.htmlLang)
            .appendChild(renderHead(article.article().title(), article.canonicalUrl(), article.article().description()))
            .appendChild(Node.buildElement(Tag.BODY)
                .appendChildren(Constants.header)
                .appendChild(Node.buildElement(Tag.MAIN)
                    .setAttribute(Constants.idMain)
                    .appendChild(renderArticleBody(article)))
                .appendChild(renderBottomNav(article.predecessorUrl(), article.successorUrl()))
                .appendChild(Constants.footer))
            .toElement();
    }

    private static @NotNull Node renderArchive(
        final @NotNull String title,
        final @NotNull String header,
        final @NotNull List<ArchivedArticle> articles,
        final @NotNull String canonicalUrl
    ) {
        return Node.buildElement(Tag.HTML)
            .setAttribute(Constants.htmlLang)
            .appendChild(renderHead(title, canonicalUrl, null))
            .appendChild(Node.buildElement(Tag.BODY)
                .appendChildren(Constants.header)
                .appendChild(Node.buildElement(Tag.MAIN)
                    .setAttribute(Constants.idMain)
                    .appendChild(Node.buildElement(Tag.HEADER)
                        .appendChild(Node.buildElement(Tag.H1).appendChild(new Node.Text(header))))
                    .appendChild(renderArchiveTableOfContents(articles))
                    .appendChildren(renderExcerpts(articles)))
                .appendChild(renderBottomNav(null, null))
                .appendChild(Constants.footer))
            .toElement();
    }

    private static @NotNull Node renderArchiveTableOfContents(final @NotNull List<ArchivedArticle> articles) {
        final var ol = Node.buildElement(Tag.OL);
        for (final var article : articles) {
            ol.appendChild(Node.buildElement(Tag.LI)
                .appendChild(Node.buildElement(Tag.A)
                    .setAttribute("href", '#' + article.identifier())
                    .appendChild(new Node.Text(article.article().title()))));
        }
        return Node.buildElement(Tag.NAV)
            .setAttribute("class", "toc")
            .setAttribute("aria-labelledby", "toc-label")
            .appendChild(Constants.tocLabel)
            .appendChild(ol)
            .toElement();
    }

    private static @NotNull List<Node> renderExcerpts(final @NotNull List<ArchivedArticle> articles) {
        final var nodes = new ArrayList<Node>();
        for (final var article : articles) {
            final var title = article.article().title();
            nodes.add(Node.buildElement(Tag.ARTICLE)
                .setAttribute("id", article.identifier())
                .appendChild(Node.buildElement(Tag.HEADER)
                    .appendChild(Node.buildElement(Tag.H2)
                        .appendChild(Node.buildElement(Tag.A)
                            .setAttribute("href", article.url())
                            .appendChild(new Node.Text(title))))
                    .appendChild(renderPublicationDate(article.article().date())))
                .appendChildren(article.article().rootSection().body())
                .appendChild(Node.buildElement(Tag.A)
                    .setAttribute("class", "read-full")
                    .setAttribute("href", article.url())
                    .setAttribute("aria-label", "Read the full article: " + title)
                    .appendChild(new Node.Text("Read the full article…")))
                .toElement());
        }
        return nodes;
    }

    private static @NotNull Node renderHead(
        final @NotNull String title,
        final @NotNull String canonicalUrl,
        final @Nullable String description
    ) {
        final var head = Node.buildElement(Tag.HEAD);
        head.appendChildren(Constants.headPrefix);
        if (description != null) {
            head.appendChild(Node.buildElement(Tag.META_NAMED)
                .setAttribute("name", "description")
                .setAttribute("content", description));
        }
        head.appendChild(Node.buildElement(Tag.LINK)
            .setAttribute("rel", "canonical")
            .setAttribute("href", canonicalUrl));
        head.appendChildren(Constants.headSuffix);
        head.appendChild(Node.buildElement(Tag.TITLE)
            .appendChild(new Node.Text(title + " - Fanael's random ruminations")));
        return head.toElement();
    }

    private @NotNull Node renderArticleBody(final @NotNull ArticleToRender article) {
        final var innerArticle = article.article();
        final var builder = Node.buildElement(Tag.ARTICLE);
        builder.appendChild(renderArticleHeader(article));
        builder.appendChildren(innerArticle.rootSection().body());
        final var children = innerArticle.rootSection().children();
        if (!children.isEmpty() && !innerArticle.inhibitTableOfContents()) {
            builder.appendChild(renderTableOfContents(children));
        }
        for (final var child : children) {
            builder.appendChild(renderSubsection(child, 2));
        }
        return builder.toElement();
    }

    private @NotNull Node renderArticleHeader(final @NotNull ArticleToRender article) {
        final var innerArticle = article.article();
        final var headerBuilder = Node.buildElement(Tag.HEADER);
        headerBuilder.appendChild(Node.buildElement(Tag.H1)
            .appendChild(Constants.rootSectionHeaderLink)
            .appendChild(new Node.Text(article.article().title())));
        if (headerRenderMode.shouldRender()) {
            headerBuilder.appendChild(Node.buildElement(Tag.P)
                .setAttribute("class", "permalink")
                .appendChild(Node.buildElement(Tag.A)
                    .setAttribute("href", article.canonicalUrl())
                    .appendChild(new Node.Text("Article permalink"))));
            headerBuilder.appendChild(renderPublicationDate(innerArticle.date()));
            if (!innerArticle.topics().isEmpty()) {
                headerBuilder.appendChild(Node.buildElement(Tag.P)
                    .appendChild(new Node.Text("Topics: "))
                    .appendChildren(renderArticleTopics(innerArticle.topics())));
            }
        }
        return headerBuilder.toElement();
    }

    private static @NotNull Node renderPublicationDate(final @NotNull LocalDate date) {
        final var day = date.getDayOfMonth();
        final var daySuffix = switch (day % 10) {
            case 1 -> (day == 11) ? "th" : "st";
            case 2 -> (day == 12) ? "th" : "nd";
            case 3 -> (day == 13) ? "th" : "rd";
            default -> "th";
        };
        final var prettyDate =
            day + daySuffix + " of " + Constants.monthNames[date.getMonthValue() - 1] + ' ' + date.getYear();
        return Node.buildElement(Tag.P)
            .appendChild(new Node.Text("Published on the "))
            .appendChild(Node.buildElement(Tag.TIME)
                .setAttribute("datetime", renderIsoDate(date))
                .appendChild(new Node.Text(prettyDate)))
            .toElement();
    }

    private static @NotNull String renderIsoDate(final @NotNull LocalDate date) {
        return date.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    private @NotNull List<Node> renderArticleTopics(final @NotNull List<String> topics) {
        final var nodes = new ArrayList<@NotNull Node>(2 * topics.size());
        for (final String topicName : topics) {
            final var link = convertTopicToArchiveUrl.apply(topicName);
            if (link == null) {
                nodes.add(new Node.Text(topicName));
            } else {
                nodes.add(Node.buildElement(Tag.A)
                    .setAttribute("href", link)
                    .appendChild(new Node.Text(topicName))
                    .toElement());
            }
            nodes.add(new Node.Text(", "));
        }
        if (!nodes.isEmpty()) {
            nodes.remove(nodes.size() - 1);
        }
        return nodes;
    }

    private static @NotNull Node renderTableOfContents(final @NotNull List<Section> childrenOfRoot) {
        return Node.buildElement(Tag.NAV)
            .setAttribute("class", "toc")
            .setAttribute("aria-labelledby", "toc-label")
            .appendChild(Constants.tocLabel)
            .appendChild(renderTableOfContentsList(childrenOfRoot))
            .toElement();
    }

    private static @NotNull Node renderTableOfContentsList(final @NotNull List<Section> children) {
        final var listBuilder = Node.buildElement(Tag.OL);
        for (final var child : children) {
            final var itemBuilder = Node.buildElement(Tag.LI);
            itemBuilder.appendChild(Node.buildElement(Tag.A)
                .setAttribute("href", '#' + child.identifier().symbolName())
                .appendChild(new Node.Text(child.header())));
            final var grandchildren = child.children();
            if (!grandchildren.isEmpty()) {
                itemBuilder.appendChild(renderTableOfContentsList(grandchildren));
            }
            listBuilder.appendChild(itemBuilder);
        }
        return listBuilder.toElement();
    }

    private static @NotNull Node renderSubsection(final @NotNull Section section, final int nestingLevel) {
        final var builder = Node.buildElement(Tag.SECTION);
        builder.setAttribute("id", section.identifier().symbolName());
        builder.appendChild(renderSectionHeader(section, nestingLevel));
        builder.appendChildren(section.body());
        for (final var child : section.children()) {
            builder.appendChild(renderSubsection(child, nestingLevel + 1));
        }
        return builder.toElement();
    }

    private static @NotNull Node renderSectionHeader(final @NotNull Section section, final int nestingLevel) {
        final var headingTag = Tag.byHtmlName("h" + Math.min(nestingLevel, 6));
        assert headingTag != null;
        return Node.buildElement(headingTag)
            .appendChild(renderSectionHeaderLink(section.identifier().symbolName()))
            .appendChild(new Node.Text(section.header()))
            .toElement();
    }

    private static @NotNull Node.Element renderSectionHeaderLink(final @NotNull String targetId) {
        return Node.buildElement(Tag.A)
            .setAttribute("class", "section-header-link")
            .setAttribute("href", '#' + targetId)
            .appendChild(new Node.Text("§"))
            .toElement();
    }

    private static @NotNull Node renderBottomNav(
        final @Nullable String predecessorUrl,
        final @Nullable String successorUrl
    ) {
        final var predecessorBuilder = Node.buildElement(Tag.LI);
        predecessorBuilder.setAttribute("class", "prev");
        if (predecessorUrl != null) {
            predecessorBuilder.appendChild(Node.buildElement(Tag.A)
                .setAttribute("rel", "prev")
                .setAttribute("href", predecessorUrl)
                .appendChild(new Node.Text("← Older")));
        }
        final var successorBuilder = Node.buildElement(Tag.LI);
        successorBuilder.setAttribute("class", "next");
        if (successorUrl != null) {
            successorBuilder.appendChild(Node.buildElement(Tag.A)
                .setAttribute("rel", "next")
                .setAttribute("href", successorUrl)
                .appendChild(new Node.Text("Newer →")));
        }
        return Node.buildElement(Tag.NAV)
            .setAttribute("aria-label", "Chronological, secondary")
            .appendChild(Node.buildElement(Tag.UL)
                .setAttribute("id", "prevnext")
                .appendChild(Constants.topLink)
                .appendChild(predecessorBuilder)
                .appendChild(Constants.archivesLink)
                .appendChild(successorBuilder))
            .toElement();
    }

    private final @NotNull HeaderRenderMode headerRenderMode;
    private final @NotNull Function<@NotNull String, @Nullable String> convertTopicToArchiveUrl;

    // Put constants in a separate class, so that they're created on first access rather than when the template is
    // loaded.
    private static final class Constants {
        private static final List<Node.Element> headPrefix = List.of(
            Node.makeEmptyElement(Tag.META_CHARSET_UTF8),
            Node.buildElement(Tag.META_NAMED)
                .setAttribute("name", "viewport")
                .setAttribute("content", "width=device-width, initial-scale=1")
                .toElement(),
            Node.buildElement(Tag.META_HTTP_EQUIV)
                .setAttribute("name", "Content-Security-Policy")
                .setAttribute("content", "default-src 'self'; object-src 'none'")
                .toElement(),
            Node.buildElement(Tag.META_NAMED)
                .setAttribute("name", "generator")
                .setAttribute("content", "Some custom Common Lisp")
                .toElement()
        );

        private static final List<Node.Element> headSuffix = List.of(
            Node.buildElement(Tag.LINK)
                .setAttribute("rel", "stylesheet")
                .setAttribute("href", "/static/theme.css").toElement(),
            Node.buildElement(Tag.LINK)
                .setAttribute("rel", "license")
                .setAttribute("href", "https://creativecommons.org/licenses/by-sa/4.0/").toElement()
        );

        private static final List<Node.Element> header = List.of(
            Node.buildElement(Tag.A)
                .setAttribute("id", "skip-nav")
                .setAttribute("href", "#main")
                .appendChild(new Node.Text("Skip to main content"))
                .toElement(),
            Node.buildElement(Tag.HEADER)
                .setAttribute("id", "mainheader")
                .appendChild(new Node.Text("Fanael's random ruminations"))
                .toElement(),
            Node.buildElement(Tag.NAV)
                .setAttribute("aria-label", "Primary")
                .appendChild(Node.buildElement(Tag.UL)
                    .setAttribute("id", "navmenu")
                    .appendChild(Node.buildElement(Tag.LI)
                        .appendChild(Node.buildElement(Tag.A)
                            .setAttribute("href", "/")
                            .appendChild(new Node.Text("Main page"))))
                    .appendChild(Node.buildElement(Tag.LI)
                        .appendChild(Node.buildElement(Tag.A)
                            .setAttribute("href", "/archives/")
                            .appendChild(new Node.Text("Archives"))))
                    .appendChild(Node.buildElement(Tag.LI)
                        .appendChild(Node.buildElement(Tag.A)
                            .setAttribute("href", "https://github.com/Fanael/fanael.github.io/")
                            .appendChild(new Node.Text("GitHub"))))
                    .appendChild(Node.buildElement(Tag.LI)
                        .appendChild(Node.buildElement(Tag.A)
                            .setAttribute("rel", "author")
                            .setAttribute("href", "/pages/about.html")
                            .appendChild(new Node.Text("About")))))
                .toElement()
        );

        private static final Node.Element footer = Node.buildElement(Tag.FOOTER)
            .appendChild(Node.buildElement(Tag.UL)
                .setAttribute("id", "footerstuff")
                .appendChild(Node.buildElement(Tag.LI)
                    .appendChild(new Node.Text("Powered by HTML & CSS")))
                .appendChild(Node.buildElement(Tag.LI)
                    .appendChild(new Node.Text("Copyright © 2019-2021 Fanael Linithien")))
                .appendChild(Node.buildElement(Tag.LI)
                    .appendChild(new Node.Text("Licensed under a "))
                    .appendChild(Node.buildElement(Tag.A)
                        .setAttribute("rel", "license")
                        .setAttribute("href", "https://creativecommons.org/licenses/by-sa/4.0/")
                        .appendChild(
                            new Node.Text("Creative Commons Attribution-ShareAlike 4.0 International License")))))
            .toElement();

        private static final Node.Element topLink = Node.buildElement(Tag.LI)
            .setAttribute("class", "top")
            .appendChild(Node.buildElement(Tag.A)
                .setAttribute("href", "#skip-nav")
                .appendChild(new Node.Text("↑ Top ↑")))
            .toElement();

        private static final Node.Element archivesLink = Node.buildElement(Tag.LI)
            .appendChild(Node.buildElement(Tag.A)
                .setAttribute("href", "/archives/")
                .appendChild(new Node.Text("Blog archives")))
            .toElement();

        private static final Node.Element rootSectionHeaderLink = renderSectionHeaderLink("main");

        private static final Node.Element tocLabel = Node.buildElement(Tag.SPAN)
            .setAttribute("id", "toc-label")
            .appendChild(new Node.Text("Table of contents"))
            .toElement();

        private static final Attribute.String htmlLang = new Attribute.String("lang", "en");

        private static final Attribute.String idMain = new Attribute.String("id", "main");

        private static final String[] monthNames = new String[]{
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
        };
    }
}
