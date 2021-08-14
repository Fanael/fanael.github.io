;; Copyright © 2019  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "How to: accidentally break empty base optimization"
  :description "Empty base optimization in C++ is very brittle, how (not) to break it?"
  :date (2019 12 20)
  :topics ("i-hate-c++")
  :children (the-problem the-temporary-workaround the-future-solution)
  (p "A reasonably common idiom in C++ code is the use of the following or similar class to inhibit move and copy operations, to avoid having to repeat the four "
     (code "delete")
     "d functions in every class: ")
  ((highlighted-code :language :c++)
   "class noncopyable {
public:
    noncopyable(const noncopyable&) = delete;
    noncopyable(noncopyable&&) = delete;
    noncopyable& operator=(const noncopyable&) = delete;
    noncopyable& operator=(noncopyable&&) = delete;
protected:
    noncopyable() = default;
    ~noncopyable() = default;
};")
  (sidenote (p "I'm personally not a huge fan of the " (code "noncopyable") " name, because it describes " (em "how") " instead of " (em "why") ", but that's neither here nor there."))
  (p "The " (code "noncopyable") " class can be mixed in using inheritance, like so:")
  ((highlighted-code :language :c++)
   "class foo : private noncopyable {
    // …
};")
  (p "So far, so good, but there's more to it than meets the eye."))

(defsection the-problem
  :header "The problem"
  (p "This definition of " (code "noncopyable") " relies heavily on " (dfn "empty base optimization") ": normally, every object in C++ has size of at least one byte and (equivalently) a unique address, but in certain conditions this requirement can be relaxed for " (em "empty base subobjects") ":")
  ((highlighted-code :language :c++)
   "class foo : private noncopyable {
    int x;
};

// Always true.
static_assert(sizeof(noncopyable) >= 1);
// True on every implementation that doesn't add gratuitous padding after
// the last member.
static_assert(sizeof(foo) == sizeof(int));")
  (p "There is a problem, however. " (strong "Empty base optimization can be applied only if the base class type is not the type, or a possibly-indirect base type of that type, of the first member") ", as the two subobjects are explicitly required to have distinct addresses. From this requirement it follows that the empty base optimization is " (strong "prohibited") " in the following code:")
  ((highlighted-code :language :c++)
   "class foo : private noncopyable {
    int x;
};

class bar : private noncopyable {
    foo x;
};

// Always true (!), as the noncopyable base subobject of foo and
// the noncopyable base subobject of bar are required to have distinct
// addresses.
static_assert(sizeof(bar) > sizeof(foo));")
  (p "Now, if non-copyability of " (code "foo") " is a part of its public API, one could argue that the solution is to just not declare " (code "bar") " as non-copyable; but if it's merely an implementation detail, not explicitly making " (code "bar") " non-copyable couples it too tightly to the implementation of " (code "foo") " and as such should be, in my opinion at least, avoided."))

(defsection the-temporary-workaround
  :header "The (temporary) workaround"
  (p "There are ways to work around this problem. One such way is to use a preprocessor macro instead of mixing in a base class, but the C++ preprocessor is absolutely awful, so I'd rather not go this route.")
  (p "The solution I personally prefer exploits the fact that one of the requirements for the empty base optimization to apply is that the base class is not of " (em "the same type") " as the first non-static data member or one of its bases. What if there was a way to create similar types that are completely distinct? There is, that's precisely what C++ templates are for:")
  ((highlighted-code :language :c++)
   "template <typename> // This is the only change!
class noncopyable {
public:
    noncopyable(const noncopyable&) = delete;
    noncopyable(noncopyable&&) = delete;
    noncopyable& operator=(const noncopyable&) = delete;
    noncopyable& operator=(noncopyable&&) = delete;
protected:
    noncopyable() = default;
    ~noncopyable() = default;
};")
  (p "As long as the template parameters " (var "T") " and " (var "U") " are distinct types, " (code "noncopyable<T>") " and " (code "noncopyable<U>") " are distinct types, too.")
  (p "Making " (code "noncopyable") " a template requires every type to somehow use a unique type as a parameter. Fortunately, there is an obvious solution: make every type use " (em "itself") " as the parameter:")
  ((highlighted-code :language :c++)
   "class foo : private noncopyable<foo> {
    int x;
};

class bar : private noncopyable<bar> {
    foo x;
};

// noncopyable<foo> and noncopyable<bar> are distinct types, so empty base
// optimization applies.
// True on every implementation that doesn't add gratuitous padding after
// the last member.
static_assert(sizeof(bar) == sizeof(foo));")
  (p "Granted, this doesn't protect against types that deliberately use another class as the dummy parameter. This is a non-issue in practice, however, as any such code can be immediately caught and rejected at review stage. The goal is to protect against Murphy, not Machiavelli."))

(defsection the-future-solution
  :header "The (future) solution"
  (p "C++20 solves this problem by allowing an arbitrary non-static data member to disable the unique address requirement through the " (code "[[no_unique_address]]") " attribute, allowing " (code "noncopyable") " to be used as a regular member instead of a base class:")
  ((highlighted-code :language :c++)
   "class foo {
    [[no_unique_address]] noncopyable disable_copy_and_move;
    int x;
};

// True on every implementation that doesn't add gratuitous padding after
// the last member.
static_assert(sizeof(foo) == sizeof(int));")
  (p "An alternative, arguably semantically the cleanest, solution is to wait until "
     ((a :href "http://open-std.org/JTC1/SC22/WG21/docs/papers/2019/p0707r4.pdf") "the metaclass proposal (PDF, 1.5 MB)")
     " lands and implement " (code "noncopyable") " as a metaclass. There's no guarantee however when (and " (em "if") ") it will land, while C++20 is coming soon, and GCC 9 and Clang 9 already implement " (code "[[no_unique_address]]") "."))
