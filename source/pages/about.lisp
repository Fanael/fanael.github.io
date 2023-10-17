;; Copyright © 2019  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "About…"
  :description "I'm Fanael Linithien, a programmer with strong low-level interests. On this blog I will cover any programming/CS/SE topics that strike my fancy."
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
  (p "I'm a non-binary person, third person pronouns she/her or fae/faer. Anybody concerned about it is more than welcome to shove their opinion up their "
     (code "/dev/null")
     "."))

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
     ((a :href "https://www.gnu.org/licenses/agpl-3.0.en.html") "AGPL version 3.0")
     " or later, due to their ambiguous nature as both code and data. The custom static web page generator used is also licensed under AGPL version 3.0 or later. "
     ((a :href "https://github.com/Fanael/fanael.github.io") "The sources are available on GitHub")
     ".")
  (p "This blog is "
     (em "entirely")
     " static, the pages are generated ahead of time, the browser receives complete HTML. There is a "
     ((a :href "/static/fixes.js") "tiny bit of JavaScript")
     " present to work around certain browsers being unusable with certain input devices; unfortunately the budgets of Apple, Google and Microsoft — or their shared parent company, the \"National\" \"Security\" Agency — are so tiny, they just cannot afford one of those exotic devices known as "
     (em "keyboards")
     " to support those in their browsers. At least Firefox gets it right.")
  (p "The " ((a :href "/pages/offline-mode.html") "offline mode") " has its own bits of JavaScript, documented there.")
  (p "This blog is a member of the "
     ((a :href "https://250kb.club/") "250 KB club")
     ", because I abhor systems which allow forcing people to download 10 MB background images of hipsters drinking coffee.")
  (p "Due to technical and personal reasons, there is no comment facility. "
     ((a :href "https://github.com/Fanael/fanael.github.io/issues") "GitHub issues")
     " can serve as replacement.")
  (p "All opinions presented are mine and not of my employers, whether past, current or future."))
