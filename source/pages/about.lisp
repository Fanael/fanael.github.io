;; Copyright © 2019  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "About…"
  :description "I'm Fanael Linithien, a programmer with strong low-level interests. On this blog I will cover any programming/CE/SE topics that strike my fancy."
  :date (1970 01 01) ; ignored anyway
  :inhibit-table-of-contents t
  :children (about-author about-blog))

(defsection about-author
  :header "About the author"
  (p "I'm Fanael Linithien, a programmer with strong low-level interests. I probably could tell you what micro-operations a given x86 instruction decodes to on any recent or older CPU if woken up in the middle of the night.")
  (p "I don't really " (em "like") " any of the programming languages I've had the (mis)fortune of working with, of which there are many, as every extant language is inevitably flawed in multiple ways, in part due to working on similarly — if not moreso — flawed hardware; nevertheless, I do appreciate some of the " (em "ideas") " behind some languages, such as Lisp or Rust, if not the actual languages themselves.")
  (p "I used to be pretty active in the Emacs community, so "
     ((a :href "https://github.com/Fanael/") "my GitHub profile")
     " is dominated by my Emacs packages. I'm no longer as active due to health issues, but I still use Emacs a lot.")
  (p "I'm also a non-binary person, preferred third person pronouns she/her."))

(defsection about-blog
  :header "About the blog"
  (p "On this blog I will cover any programming, computer science, and software engineering topics that strike my fancy, including, but not limited do, low-level optimization, design and implementation of tracing garbage collectors, and other topics I deem interesting.")
  (p "Since I'm a strong believer in "
     ((a :href "https://en.wikipedia.org/wiki/Free_software") "free software")
     ", and "
     ((a :href "https://en.wikipedia.org/wiki/Free-culture_movement") "free culture")
     " in general, all contents on this website are licensed under "
     ((a :href "https://creativecommons.org/licenses/by-sa/4.0/" :rel "license") "CC BY-SA 4.0")
     ". The source files from which the website is generated are dual-licensed under either CC BY-SA 4.0 or "
     ((a :href "https://www.gnu.org/licenses/gpl-3.0.en.html") "GPL version 3.0")
     " or later, due to their ambiguous nature as both code and data. The custom static web page generator used is also licensed under GPL version 3.0 or later. "
     ((a :href "https://github.com/Fanael/fanael.github.io") "The sources are available on GitHub")
     ".")
  (p "This blog is "
     (em "entirely")
     " static, the pages are generated ahead of time, the browser receives complete HTML. This means there's "
     (em "absolutely no JavaScript")
     " present; if there is, it's been injected by a third party. I'd appreciate "
     ((a :href "https://github.com/Fanael/fanael.github.io/issues") "receiving an issue report")
     " if that's the case so I can move to a different hosting.")
  (p "Due to technical and personal reasons, there is no comment facility. "
     ((a :href "https://github.com/Fanael/fanael.github.io/issues") "GitHub issues")
     " can serve as replacement.")
  (p "All opinions presented are mine and not of my employers, whether past, current or future."))
