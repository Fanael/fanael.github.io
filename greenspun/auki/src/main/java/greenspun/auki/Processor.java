// Copyright © 2021-2022  Fanael Linithien
// SPDX-License-Identifier: AGPL-3.0-or-later
package greenspun.auki;

import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;
import java.util.function.Consumer;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import greenspun.auki.annotations.Open;

@SupportedAnnotationTypes("*")
public final class Processor extends AbstractProcessor {
    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.latestSupported();
    }

    @Override
    public boolean process(final Set<? extends TypeElement> annotations, final RoundEnvironment roundEnv) {
        verifyAllClasses(roundEnv.getRootElements());
        return false;
    }

    private void verifyAllClasses(final Iterable<? extends Element> elements) {
        for (final var element : elements) {
            if (element instanceof TypeElement type) {
                verifyClass(type);
            }
        }
    }

    private void verifyClass(final TypeElement type) {
        if (isOpen(type)) {
            verifyOpenClass(type);
        } else {
            if (!type.getModifiers().contains(Modifier.FINAL)) {
                error("Non-final, non-abstract, non-@Open class found", type);
            }
            verifyFinalClass(type);
        }

        verifyAllClasses(type.getEnclosedElements());
    }

    private void verifyOpenClass(final TypeElement type) {
        final var typeModifiers = type.getModifiers();
        if (typeModifiers.contains(Modifier.FINAL)) {
            error("An abstract or @Open class cannot be final", type);
        }

        if (!Collections.disjoint(typeModifiers, implicitlyOpenModifiers) && isAnnotatedOpen(type)) {
            warn("The @Open annotation is redundant for abstract, sealed and non-sealed classes and interfaces", type);
        }

        forAllMethods(type, this::verifyMethodFinalOrOpen);
    }

    private void verifyMethodFinalOrOpen(final ExecutableElement method) {
        if (verifyMethodCommon(method)) {
            return;
        }

        final var methodModifiers = method.getModifiers();
        final var isFinal = methodModifiers.contains(Modifier.FINAL);
        final var isOpen = isMethodOpen(method);
        if (isOpen && isFinal) {
            error("An abstract or @Open method cannot be final", method);
        } else if (!isOpen && !isFinal) {
            error("Non-final, non-abstract, non-@Open method found", method);
        }

        if (methodModifiers.contains(Modifier.ABSTRACT) && isAnnotatedOpen(method)) {
            warn("The @Open annotation is redundant for abstract methods", method);
        } else if (methodModifiers.contains(Modifier.DEFAULT) && isAnnotatedOpen(method)) {
            warn("The @Open annotation is redundant for default methods", method);
        }
    }

    private void verifyFinalClass(final TypeElement type) {
        forAllMethods(type, this::verifyMethodNotOpen);
    }

    private void verifyMethodNotOpen(final ExecutableElement method) {
        if (verifyMethodCommon(method)) {
            return;
        }

        if (isAnnotatedOpen(method)) {
            error("A method in a final class cannot be @Open", method);
        }
    }

    private boolean verifyMethodCommon(final ExecutableElement method) {
        final var modifiers = method.getModifiers();
        if (modifiers.contains(Modifier.STATIC)) {
            if (isAnnotatedOpen(method)) {
                error("A static method cannot be @Open", method);
            }
            return true;
        } else if (modifiers.contains(Modifier.PRIVATE)) {
            if (isAnnotatedOpen(method)) {
                error("A private method cannot be @Open", method);
            }
            return true;
        } else {
            return false;
        }
    }

    private void error(final String message, final Element element) {
        printMessage(Diagnostic.Kind.ERROR, message, element);
    }

    private void warn(final String message, final Element element) {
        printMessage(Diagnostic.Kind.WARNING, message, element);
    }

    private void printMessage(final Diagnostic.Kind kind, final String message, final Element element) {
        processingEnv.getMessager().printMessage(kind, message, element);
    }

    private static void forAllMethods(final TypeElement type, final Consumer<ExecutableElement> consumer) {
        for (final var child : type.getEnclosedElements()) {
            if (child instanceof ExecutableElement codeElement && codeElement.getKind() == ElementKind.METHOD) {
                consumer.accept(codeElement);
            }
        }
    }

    private static boolean isMethodOpen(final ExecutableElement method) {
        return isOpen(method) || method.getModifiers().contains(Modifier.DEFAULT);
    }

    private static boolean isOpen(final Element element) {
        return !Collections.disjoint(element.getModifiers(), implicitlyOpenModifiers) || isAnnotatedOpen(element);
    }

    private static boolean isAnnotatedOpen(final Element element) {
        return element.getAnnotation(Open.class) != null;
    }

    private static final EnumSet<Modifier> implicitlyOpenModifiers =
        EnumSet.of(Modifier.ABSTRACT, Modifier.SEALED, Modifier.NON_SEALED);
}
