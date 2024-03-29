// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.util.EnumSet;
import java.util.HashSet;
import java.util.Map;
import greenspun.util.collection.seq.Seq;
import greenspun.util.condition.ConditionContext;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The DOM verifier.
 * <p>
 * This class implements a best-effort verification of the DOM tree, to ensure that the document can be serialized into
 * valid HTML.
 */
public final class Verifier {
    private Verifier() {
    }

    /**
     * Verifies the DOM tree rooted at {@code rootNode}.
     * <p>
     * If the DOM tree is valid, this method simply returns. Otherwise, if any verification errors have been found,
     * a fatal condition of type {@link VerificationErrorCondition} is signaled.
     */
    public static void verify(final Node rootNode) {
        final var verifier = new Verifier();
        verifier.verifyRoot(rootNode);
    }

    private void verifyRoot(final Node rootNode) {
        verify(rootNode, Context.ROOT);
        if (!verificationErrors.isEmpty()) {
            throw ConditionContext.error(new VerificationErrorCondition(verificationErrors));
        }
    }

    private void verify(final Node node, final Context context) {
        switch (node) {
            case final Node.Text ignored -> verifyTextNode(context);
            case final Node.Element element -> verifyElement(element, context);
        }
    }

    private void verifyTextNode(final Context context) {
        if (!rawTextContexts.contains(context)) {
            recordNestingError(null, context, rawTextContexts.toString());
        }
    }

    private void verifyElement(final Node.Element element, final Context context) {
        final var tag = element.tag();
        if (ancestors.contains(element)) {
            recordError("Element '" + tag.htmlName() + "' appears to be its own ancestor");
            return;
        }
        verifyTagContext(tag, context);
        verifyAttributes(element);
        verifyChildren(element, getEffectiveChildContext(tag, context));
    }

    private void verifyAttributes(final Node.Element element) {
        final var tag = element.tag();
        for (final var attribute : element.attributes()) {
            final var verifier = findAttributeVerifier(attribute, tag);
            if (verifier != null) {
                verifier.verify(new AttributeVerificationContext(this, tag, attribute));
            } else {
                recordAttributeError(tag, attribute.name(), "not a valid attribute for this element");
            }
        }

        for (final var attributeName : tag.requiredAttributes()) {
            if (Attributes.get(element.attributes(), attributeName) == null) {
                recordAttributeError(tag, attributeName, "required attribute not found");
            }
        }

        if (Attributes.get(element.attributes(), "id") instanceof final Attribute.String id) {
            final var value = id.value();
            if (!foundIds.add(value)) {
                recordAttributeError(tag, "id", "duplicate ID found: '" + value + '\'');
            }
        }
    }

    private static @Nullable AttributeVerifier findAttributeVerifier(final Attribute attribute, final Tag tag) {
        final var name = attribute.name();
        final var globalVerifier = globalAttributeTypes.get(name);
        if (globalVerifier != null) {
            return globalVerifier;
        }
        return tag.allowedAttributes().get(name);
    }

    private void verifyChildren(final Node.Element element, final @Nullable Context childContext) {
        if (childContext == null) {
            if (!element.children().isEmpty()) {
                final var tagName = element.tag().htmlName();
                recordError("Empty element '" + tagName + "' has children");
            }
        } else {
            final var previousAncestors = ancestors;
            ancestors = ancestors.appended(element);
            try {
                for (final var child : element.children()) {
                    verify(child, childContext);
                }
            } finally {
                ancestors = previousAncestors;
            }
        }
    }

    private void verifyTagContext(final Tag tag, final Context context) {
        if (!tag.allowedIn(context)) {
            recordNestingError(tag, context, tag.allowedContextsString());
        }
    }

    private void recordAttributeError(final Tag tag, final String attributeName, final String message) {
        recordError("Attribute '" + attributeName + "' of element '" + tag.htmlName() + "': " + message);
    }

    private void recordNestingError(
        final @Nullable Tag tag,
        final Context actualContext,
        final String allowedContexts
    ) {
        final var tagName = (tag == null)
            ? "A text node"
            : "A '" + tag.htmlName() + "' element";
        final var message =
            tagName + " found in context " + actualContext + ", but is allowed only in contexts " + allowedContexts;
        recordError(message);
    }

    private void recordError(final String message) {
        verificationErrors = verificationErrors.appended(new VerificationError(message, getAncestorTags()));
    }

    private Seq<Tag> getAncestorTags() {
        return ancestors.map(Node.Element::tag);
    }

    private static @Nullable Context getEffectiveChildContext(final Tag tag, final Context context) {
        return switch (tag.childContext()) {
            case final Context c -> c;
            case final ChildContext.None ignored -> null;
            case final ChildContext.Transparent ignored -> context;
        };
    }

    static final AttributeVerifier attributeIsBoolean =
        new AttributeTypeVerifier(Attribute.Boolean.class, "incorrect type, boolean expected");
    static final AttributeVerifier attributeIsInteger =
        new AttributeTypeVerifier(Attribute.Integer.class, "incorrect type, integer expected");
    static final AttributeVerifier attributeIsString =
        new AttributeTypeVerifier(Attribute.String.class, "incorrect type, string expected");

    private static final Map<String, AttributeVerifier> globalAttributeTypes = Map.of(
        "aria-hidden", new AriaHiddenVerifier(),
        "aria-label", attributeIsString,
        "aria-labelledby", attributeIsString,
        "class", attributeIsString,
        "id", attributeIsString,
        "lang", attributeIsString
    );
    private static final EnumSet<Context> rawTextContexts =
        EnumSet.of(Context.FLOW, Context.PHRASING, Context.TEXT_ONLY);

    private Seq<VerificationError> verificationErrors = Seq.empty();
    private Seq<Node.Element> ancestors = Seq.empty();
    private final HashSet<String> foundIds = new HashSet<>();

    interface AttributeVerifier {
        void verify(AttributeVerificationContext context);
    }

    record AttributeVerificationContext(Verifier verifier, Tag tag, Attribute attribute) {
        void recordError(final String message) {
            verifier.recordAttributeError(tag, attribute.name(), message);
        }
    }

    private record AttributeTypeVerifier(Class<? extends Attribute> type, String message) implements AttributeVerifier {
        @Override
        public void verify(final AttributeVerificationContext context) {
            if (!type.isInstance(context.attribute)) {
                context.recordError(message);
            }
        }
    }

    private static final class AriaHiddenVerifier implements AttributeVerifier {
        @Override
        public void verify(final AttributeVerificationContext context) {
            if (!(context.attribute instanceof final Attribute.String string) || !"true".equals(string.value())) {
                context.recordError("aria-hidden only accepts value of \"true\"");
            }
        }
    }
}
