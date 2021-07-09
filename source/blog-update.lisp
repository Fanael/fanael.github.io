;; Copyright © 2021  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Blog update"
  :description "A quick look at the recent improvements to the blog itself"
  :date (2021 04 01)
  :children (omg-rss-feed new-generator)
  :topics ("meta")
  (p "In the past few days I've made some changes — some immediately noticeable, some less so — to the blog. Why not have a look at them?")
  (aside (p "I've been writing another, more substantial and much larger article for quite some time. I initially wanted to publish it in December, then January, then March, but getting it into a shape I'm comfortable with takes much more time and effort than I anticipated. Sorry about that."))
  (p "The big, immediately noticeable change is that the "
     ((a :href "/") "main page")
     " is no longer a copy of the latest article. I've changed it to the tried and true format of using the introductory section of the last several articles, where \"several\" is defined here as five.")
  (p "With that change, the need for displaying an article permalink in the header vanished: since pages aren't getting copied anymore, there's no need for an explicit, unambiguous, canonical link to an article. The address the browser displays when you read an article "
     (em "is")
     " now the canonical link, putting one in the article header as well would just be superfluous.")
  (p "While the other changes are minor and not as readily — if at all — noticeable to most readers, they still warrant coverage, in their own subsections."))

(defsection omg-rss-feed
  :header "Oh my god, it has an RSS feed!"
  (aside
   (p "Originally the internal ID of this section was just "
      (code "rss-feed")
      ", which triggered a "
      ((a :href "https://github.com/gorhill/uBlock") "uBlock Origin")
      " rule, causing it to be hidden. Nice."))
  (p ((a :href "https://en.wikipedia.org/wiki/RSS") (dfn "RSS") ", " (dfn "Really Simple Syndication"))
     ", is an ancient by web standards format that allows users to keep track of updates to multiple websites in one news aggregator program. While this sounds phenomenally convenient for "
     (em "actual people")
     ", a certain Mountain View–based corporation, best known for its tendency to abandon projects, couldn't figure out how to embed spyware in it, so the popularity of this format has significantly diminished compared to its heyday 15 years ago.")
  (p "If you're one of the people who still keeps using RSS, you might've noticed that "
     ((a :href "/feed.xml") "this blog now has an RSS feed")
     " that you can subscribe to. It is, naturally, updated automatically by the static site generator on every rebuild, using the metadata contained in article files."))

(defsection new-generator
  :header "New & improved generator"
  (p "Speaking of the static site generator, if you ever were curious enough to take a look at what this site is made in, you've noticed it's using a "
     ((a :href "https://github.com/Fanael/fanael.github.io/tree/cl-generator/blog-generator/src")
      "bespoke generator written in Common Lisp")
     ", with articles being written in a domain–specific language that can best be described as a weird way of writing HTML as S-expressions.")
  (p "Or rather, it "
     (em "was")
     ". As much as I like the "
     (em "ideals")
     " of Lisp, I like static typing even more, so I decided to rewrite it in a different language. There were several contenders, but I eventually settled on a language that offers some additional niceties on top of being statically typed: great portability, high performance, memory safety and easy concurrency and parallelism. It's… Java.")
  (p "Yes, "
     (em "Java")
     ". The Java known for over–the–top verbosity and all sorts of enterprise-y boilerplate. Surprisingly, modern Java — I'm using version 16 at the moment — is "
     (em "pretty decent")
     ". Records solve the issue of immutable classes that just hold data rather well. Lambdas allow some limited forms of functional programming. Copious use of "
     (code "@NotNull")
     " and "
     (code "@Nullable")
     " lets static analysis catch any potential null pointer exceptions before they're allowed to happen. Sure, it's not quite integrated into the type system. Sure, there's still boilerplate. But all in all, writing Java turned out to be a non–horrible experience.")
  (p "The "
     (code "generator")
     " metadata element of this site still claims that \"some custom Common Lisp\" was used, and it will remain so: the generator contains "
     ((a :href "https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule")
      "an ad hoc, informal, bug-ridden, slow implementation of half of Common Lisp")
     ". The inputs are still in the same S-expression–based format, and I just couldn't give up conditions and restarts. If the generator signals an error — and it does that a lot, as it's pretty strict in what HTML it deems acceptable — the ability to fix the problem in the input file, reload it and continue as if nothing happened, without aborting the build, is "
     (em "just that good")
     "."))
