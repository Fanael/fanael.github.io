// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
package greenspun.generator;

import java.io.Writer;
import java.net.URI;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.List;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import greenspun.util.collection.ImmutableList;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import greenspun.util.condition.exception.ParserConfigurationExceptionCondition;
import greenspun.util.condition.exception.TransformerExceptionCondition;
import org.jetbrains.annotations.NotNull;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

final class FeedRenderer {
    FeedRenderer() throws Unwind {
        try {
            document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        } catch (final ParserConfigurationException e) {
            throw ConditionContext.error(new ParserConfigurationExceptionCondition(e));
        }
    }

    void createDom(final @NotNull List<ArchivedArticle> articles, final @NotNull Instant buildTime) {
        final var utcTime = buildTime.atOffset(ZoneOffset.UTC);
        document.appendChild(buildElement("rss")
            .setAttribute("version", "2.0")
            .setAttribute("xmlns:atom", atomNamespace)
            .setAttribute("xmlns:sy", syNamespace)
            .appendChild(buildElement("channel")
                .appendChild(buildElement("atom:link")
                    .setAttribute("href", siteUri.resolve(RenderConstants.feedFileName).toString())
                    .setAttribute("rel", "self")
                    .setAttribute("type", "application/rss+xml"))
                .appendChild(createElementWithText("sy:updatePeriod", "hourly"))
                .appendChild(createElementWithText("sy:updateFrequency", "3"))
                .appendChild(createElementWithText("description", "Latest posts from " + RenderConstants.siteTitle))
                .appendChild(createElementWithText("link", siteUri.toString()))
                .appendChild(createElementWithText("title", RenderConstants.siteTitle))
                .appendChild(createElementWithText("language", "en"))
                .appendChild(
                    createElementWithText("lastBuildDate", utcTime.format(DateTimeFormatter.RFC_1123_DATE_TIME)))
                .appendChild(createElementWithText("copyright", RenderConstants.copyrightLine))
                .appendChildren(createItems(articles)))
            .toElement());
    }

    void serialize(final @NotNull Writer writer) throws Unwind {
        try {
            final var transformer = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            transformer.transform(new DOMSource(document), new StreamResult(writer));
        } catch (@SuppressWarnings("OverlyBroadCatchBlock") final TransformerException e) {
            throw ConditionContext.error(new TransformerExceptionCondition(e));
        }
    }

    private @NotNull ImmutableList<Element> createItems(final @NotNull List<ArchivedArticle> articles) {
        return ImmutableList.map(articles, article -> {
            final var fullUrl = siteUri.resolve(article.uri());
            final var innerArticle = article.article();
            final var publicationDate = innerArticle.date().atStartOfDay(ZoneOffset.UTC);
            return buildElement("item")
                .appendChild(createElementWithText("title", innerArticle.title()))
                .appendChild(createElementWithText("link", fullUrl.toString()))
                .appendChild(buildElement("guid")
                    .setAttribute("isPermaLink", "false")
                    .appendChild(createText(fullUrl.toString())))
                .appendChild(
                    createElementWithText("pubDate", publicationDate.format(DateTimeFormatter.RFC_1123_DATE_TIME)))
                .appendChild(createElementWithText("description", innerArticle.description()))
                .toElement();
        });
    }

    private @NotNull ElementBuilder buildElement(final @NotNull String tagName) {
        return new ElementBuilder(document.createElement(tagName));
    }

    private @NotNull Text createText(final @NotNull String text) {
        return document.createTextNode(text);
    }

    private @NotNull Element createElementWithText(final @NotNull String tagName, final @NotNull String content) {
        return buildElement(tagName).appendChild(createText(content)).toElement();
    }

    private static final URI siteUri = URI.create("https://fanael.github.io/");
    private static final String atomNamespace = "http://www.w3.org/2005/Atom";
    private static final String syNamespace = "http://purl.org/rss/1.0/modules/syndication/";

    private final @NotNull Document document;

    private static final class ElementBuilder {
        private ElementBuilder(final @NotNull Element element) {
            this.element = element;
        }

        private @NotNull Element toElement() {
            return element;
        }

        private @NotNull ElementBuilder setAttribute(final @NotNull String name, final @NotNull String value) {
            element.setAttribute(name, value);
            return this;
        }

        private @NotNull ElementBuilder appendChild(final @NotNull Node child) {
            element.appendChild(child);
            return this;
        }

        private @NotNull ElementBuilder appendChild(final @NotNull ElementBuilder builder) {
            element.appendChild(builder.toElement());
            return this;
        }

        private @NotNull ElementBuilder appendChildren(final @NotNull Collection<? extends Node> children) {
            for (final var child : children) {
                element.appendChild(child);
            }
            return this;
        }

        private final @NotNull Element element;
    }
}
