// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
package greenspun.generator;

import java.io.Writer;
import java.net.URI;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.exception.ParserConfigurationExceptionCondition;
import greenspun.util.condition.exception.TransformerExceptionCondition;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

final class FeedRenderer {
    FeedRenderer() {
        try {
            document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        } catch (final ParserConfigurationException e) {
            throw ConditionContext.error(new ParserConfigurationExceptionCondition(e));
        }
    }

    void createDom(final Seq<ArchivedArticle> articles, final Instant buildTime) {
        final var utcTime = buildTime.atOffset(ZoneOffset.UTC);
        document.appendChild(build("rss", rss -> {
            rss.set("version", "2.0");
            rss.set("xmlns:atom", atomNamespace);
            rss.set("xmlns:sy", syNamespace);
            rss.appendBuild("channel", channel -> {
                channel.appendBuild("atom:link", link -> {
                    link.set("href", siteUri.resolve(RenderConstants.feedFileName).toString());
                    link.set("rel", "self");
                    link.set("type", "application/rss+xml");
                });
                channel.append(createElementWithText("sy:updatePeriod", "hourly"));
                channel.append(createElementWithText("sy:updateFrequency", "3"));
                channel.append(createElementWithText("description", "Latest posts from " + RenderConstants.siteTitle));
                channel.append(createElementWithText("link", siteUri.toString()));
                channel.append(createElementWithText("title", RenderConstants.siteTitle));
                channel.append(createElementWithText("language", "en"));
                channel.append(
                    createElementWithText("lastBuildDate", utcTime.format(DateTimeFormatter.RFC_1123_DATE_TIME)));
                channel.append(createElementWithText("copyright", RenderConstants.copyrightLine));
                createItems(channel, articles);
            });
        }));
    }

    void serialize(final Writer writer) {
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

    private void createItems(final ElementBuilder parent, final Seq<ArchivedArticle> articles) {
        for (final var article : articles) {
            final var fullUrl = siteUri.resolve(article.uri().toString());
            final var innerArticle = article.article();
            final var publicationDate = innerArticle.date().atStartOfDay(ZoneOffset.UTC);
            parent.appendBuild("item", item -> {
                item.append(createElementWithText("title", innerArticle.title()));
                item.append(createElementWithText("link", fullUrl.toString()));
                item.appendBuild("guid", guid -> {
                    guid.set("isPermaLink", "false");
                    guid.appendText(fullUrl.toString());
                });
                item.append(
                    createElementWithText("pubDate", publicationDate.format(DateTimeFormatter.RFC_1123_DATE_TIME)));
                item.append(createElementWithText("description", innerArticle.description()));
            });
        }
    }

    private Element build(final String tagName, final BuildFunction buildFunction) {
        final var builder = new ElementBuilder(document.createElement(tagName));
        buildFunction.build(builder);
        return builder.element;
    }

    private Element createElementWithText(final String tagName, final String content) {
        return build(tagName, element -> element.appendText(content));
    }

    private static final URI siteUri = URI.create("https://fanael.github.io/");
    private static final String atomNamespace = "http://www.w3.org/2005/Atom";
    private static final String syNamespace = "http://purl.org/rss/1.0/modules/syndication/";

    private final Document document;

    private interface BuildFunction {
        void build(ElementBuilder builder);
    }

    private final class ElementBuilder {
        private ElementBuilder(final Element element) {
            this.element = element;
        }

        private void append(final Node child) {
            element.appendChild(child);
        }

        private void appendText(final String text) {
            append(document.createTextNode(text));
        }

        private void appendBuild(final String tagName, final BuildFunction buildFunction) {
            append(build(tagName, buildFunction));
        }

        private void set(final String name, final String value) {
            element.setAttribute(name, value);
        }

        private final Element element;
    }
}
