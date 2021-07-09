// Copyright © 2021  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.dom;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Map;
import greenspun.util.UnreachableCodeReachedError;
import greenspun.util.collection.ImmutableList;
import greenspun.util.condition.ConditionContext;
import greenspun.util.condition.Unwind;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
    public static void verify(final @NotNull Node rootNode) throws Unwind {
        final var verifier = new Verifier();
        verifier.verifyRoot(rootNode);
    }

    private void verifyRoot(final @NotNull Node rootNode) throws Unwind {
        verify(rootNode, Context.ROOT);
        if (!verificationErrors.isEmpty()) {
            throw ConditionContext.error(new VerificationErrorCondition(ImmutableList.freeze(verificationErrors)));
        }
    }

    private void verify(final @NotNull Node node, final @NotNull Context context) {
        if (node instanceof Node.Text) {
            verifyTextNode(context);
        } else if (node instanceof Node.Element element) {
            verifyElement(element, context);
        } else {
            throw new UnreachableCodeReachedError();
        }
    }

    private void verifyTextNode(final @NotNull Context context) {
        if (!rawTextContexts.contains(context)) {
            recordNestingError(null, context, rawTextContexts.toString());
        }
    }

    private void verifyElement(final @NotNull Node.Element element, final @NotNull Context context) {
        final var tag = element.tag();
        if (ancestors.contains(element)) {
            recordError("Element '" + tag.htmlName() + "' appears to be its own ancestor");
            return;
        }
        verifyTagContext(tag, context);
        verifyAttributes(element);
        verifyChildren(element, getEffectiveChildContext(tag, context));
    }

    private void verifyAttributes(final @NotNull Node.Element element) {
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
            if (element.getAttribute(attributeName) == null) {
                recordAttributeError(tag, attributeName, "required attribute not found");
            }
        }

        if (element.getAttribute("id") instanceof Attribute.String id) {
            final var value = id.value();
            if (!foundIds.add(value)) {
                recordAttributeError(tag, "id", "duplicate ID found: '" + value + '\'');
            }
        }
    }

    private static @Nullable AttributeVerifier findAttributeVerifier(
        final @NotNull Attribute attribute,
        final @NotNull Tag tag
    ) {
        final var name = attribute.name();
        final var globalVerifier = globalAttributeTypes.get(name);
        if (globalVerifier != null) {
            return globalVerifier;
        }
        return tag.allowedAttributes().get(name);
    }

    private void verifyChildren(
        final @NotNull Node.Element element,
        final @Nullable Context childContext
    ) {
        if (childContext == null) {
            if (!element.children().isEmpty()) {
                final var tagName = element.tag().htmlName();
                recordError("Empty element '" + tagName + "' has children");
            }
        } else {
            ancestors.add(element);
            try {
                for (final var child : element.children()) {
                    verify(child, childContext);
                }
            } finally {
                ancestors.remove(ancestors.size() - 1);
            }
        }
    }

    private void verifyTagContext(final @NotNull Tag tag, final @NotNull Context context) {
        if (!tag.allowedIn(context)) {
            recordNestingError(tag, context, tag.allowedContextsString());
        }
    }

    private void recordAttributeError(
        final @NotNull Tag tag,
        final @NotNull String attributeName,
        final @NotNull String message
    ) {
        recordError("Attribute '" + attributeName + "' of element '" + tag.htmlName() + "': " + message);
    }

    private void recordNestingError(
        final @Nullable Tag tag,
        final @NotNull Context actualContext,
        final @NotNull String allowedContexts
    ) {
        final var tagName = (tag == null)
            ? "A text node"
            : "A '" + tag.htmlName() + "' element";
        final var message =
            tagName + " found in context " + actualContext + ", but is allowed only in contexts " + allowedContexts;
        recordError(message);
    }

    private void recordError(final @NotNull String message) {
        verificationErrors.add(new VerificationError(message, getAncestorTags()));
    }

    private @NotNull ImmutableList<Tag> getAncestorTags() {
        return ImmutableList.map(ancestors, Node.Element::tag);
    }

    private static @Nullable Context getEffectiveChildContext(final @NotNull Tag tag, final @NotNull Context context) {
        final var childContext = tag.childContext();
        if (childContext instanceof Context c) {
            return c;
        } else if (childContext instanceof ChildContext.None) {
            return null;
        } else if (childContext instanceof ChildContext.Transparent) {
            return context;
        } else {
            throw new UnreachableCodeReachedError();
        }
    }

    static final AttributeVerifier attributeIsBoolean =
        new AttributeTypeVerifier(Attribute.Boolean.class, "incorrect type, boolean expected");
    static final AttributeVerifier attributeIsInteger =
        new AttributeTypeVerifier(Attribute.Integer.class, "incorrect type, integer expected");
    static final AttributeVerifier attributeIsString =
        new AttributeTypeVerifier(Attribute.String.class, "incorrect type, string expected");

    private static final Map<String, AttributeVerifier> globalAttributeTypes = Map.of(
        "aria-label", attributeIsString,
        "aria-labelledby", attributeIsString,
        "class", attributeIsString,
        "id", attributeIsString,
        "lang", attributeIsString
    );
    private static final EnumSet<Context> rawTextContexts =
        EnumSet.of(Context.FLOW, Context.PHRASING, Context.TEXT_ONLY);

    private final ArrayList<@NotNull VerificationError> verificationErrors = new ArrayList<>();
    private final ArrayList<Node.Element> ancestors = new ArrayList<>();
    private final HashSet<String> foundIds = new HashSet<>();

    interface AttributeVerifier {
        void verify(@NotNull AttributeVerificationContext context);
    }

    record AttributeVerificationContext(
        @NotNull Verifier verifier,
        @NotNull Tag tag,
        @NotNull Attribute attribute
    ) {
        void recordError(final @NotNull String message) {
            verifier.recordAttributeError(tag, attribute.name(), message);
        }
    }

    private record AttributeTypeVerifier(
        @NotNull Class<? extends Attribute> type,
        @NotNull String message
    ) implements AttributeVerifier {
        @Override
        public void verify(final @NotNull AttributeVerificationContext context) {
            if (!type.isInstance(context.attribute)) {
                context.recordError(message);
            }
        }
    }
}
