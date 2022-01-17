;; Copyright © 2021  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later
(defarticle
  :title "Another article"
  :description "This article does not have a description."
  :date (2021 03 29)
  :topics ("qux" "bar")
  ;; A comment, to see if they're ignored correctly.
  (p "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Convallis tellus id interdum velit laoreet. Maecenas sed enim ut sem viverra aliquet. Mauris sit amet massa vitae tortor. Mauris augue neque gravida in fermentum et. Fringilla est ullamcorper eget nulla facilisi etiam. Id aliquet lectus proin nibh nisl condimentum id. Vestibulum mattis ullamcorper velit sed ullamcorper morbi tincidunt. Volutpat ac tincidunt vitae semper quis lectus nulla at volutpat. Diam quis enim lobortis scelerisque fermentum dui faucibus. Massa vitae tortor condimentum lacinia quis vel eros donec. Facilisis gravida neque convallis a.")
  (p "Egestas quis ipsum suspendisse ultrices gravida dictum. Eu mi bibendum neque egestas congue quisque. Et tortor consequat id porta nibh venenatis cras sed felis. Eu nisl nunc mi ipsum faucibus vitae aliquet nec ullamcorper. Egestas sed tempus urna et pharetra pharetra massa. Purus non enim praesent elementum facilisis leo vel fringilla. Aliquet porttitor lacus luctus accumsan. Dignissim suspendisse in est ante in nibh. Phasellus egestas tellus rutrum tellus. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor. Congue quisque egestas diam in arcu cursus euismod. Risus viverra adipiscing at in. Nec nam aliquam sem et tortor consequat id porta nibh. Massa enim nec dui nunc mattis. Nullam non nisi est sit.")
  ((code-block :language "Text")
   ((code :class "foo")
    ((span :class "c1") "blah blah blah
"
     ((span :class "c2") "lorem ipsum
or something
")
     "This really just tests if line number reformatter supports arbitrary nested structures, "
     ((span :class "c3") "as it should.")))))
