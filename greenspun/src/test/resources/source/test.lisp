;; Copyright © 2021  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later
(defarticle
  :title "A test"
  :description "blah blah blah"
  :children (c1 c2)
  :date (2021 03 28)
  :topics ("foo" "bar" "qux")
  (p (em "Lorem ipsum dolor sit amet") ", consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Porta non pulvinar neque laoreet suspendisse interdum consectetur. Fermentum leo vel orci porta non pulvinar neque. Turpis massa tincidunt dui ut ornare lectus sit amet. Sed blandit libero volutpat sed cras ornare arcu dui. Viverra accumsan in nisl nisi scelerisque eu ultrices vitae auctor. In nisl nisi scelerisque eu ultrices vitae. Eleifend mi in nulla posuere sollicitudin aliquam. Arcu cursus vitae congue mauris rhoncus aenean. Integer enim neque volutpat ac tincidunt vitae semper."))

(defsection c1
  :header "Section 1"
  :children (c11 c12)
  (p "Urna et pharetra pharetra massa massa ultricies. Semper feugiat nibh sed pulvinar proin gravida hendrerit. Venenatis lectus magna fringilla urna porttitor rhoncus dolor purus non. Sit amet luctus venenatis lectus magna. Vel pretium lectus quam id. Lacinia at quis risus sed vulputate odio ut enim. Donec et odio pellentesque diam volutpat. Consequat interdum varius sit amet. Volutpat consequat mauris nunc congue nisi vitae suscipit tellus. Facilisi nullam vehicula ipsum a arcu cursus vitae. Lacus laoreet non curabitur gravida arcu ac. Faucibus purus in massa tempor nec feugiat nisl. Vehicula ipsum a arcu cursus vitae congue mauris rhoncus aenean. Id porta nibh venenatis cras sed felis eget velit aliquet. Turpis egestas pretium aenean pharetra magna. Suscipit tellus mauris a diam maecenas sed enim. Sem integer vitae justo eget magna fermentum iaculis eu."))

(defsection c2
  :header "Section 2"
  (p "Eget duis at tellus at urna condimentum. Faucibus vitae aliquet nec ullamcorper sit. Potenti nullam ac tortor vitae. In fermentum posuere urna nec tincidunt praesent semper. Maecenas pharetra convallis posuere morbi leo urna. Morbi tincidunt augue interdum velit euismod in pellentesque. Euismod nisi porta lorem mollis aliquam ut porttitor leo a. Aliquet bibendum enim facilisis gravida neque convallis a cras semper. Nibh nisl condimentum id venenatis a condimentum vitae. Lacus sed turpis tincidunt id aliquet risus feugiat. Ullamcorper velit sed ullamcorper morbi tincidunt ornare massa eget egestas. Consequat semper viverra nam libero. Egestas pretium aenean pharetra magna ac placerat. Tellus elementum sagittis vitae et leo duis ut diam. Volutpat lacus laoreet non curabitur gravida arcu ac tortor dignissim. Nisl suscipit adipiscing bibendum est ultricies integer. Vitae suscipit tellus mauris a. Lectus sit amet est placerat in. Eget magna fermentum iaculis eu non. Lorem donec massa sapien faucibus et."))

(defsection c12
  :header "Section 1.2"
  ((image-figure
    :src "/static/x.png"
    :width 1024
    :height 768
    :alt "A Windows 98 console window displaying a truncated list of Universal Chess Interface options supported by Stockfish and the information about the used compiler.")
   "Stockfish running in a Windows 98 console window")
  (p "Viverra justo nec ultrices dui sapien eget mi. A lacus vestibulum sed arcu non odio euismod. Volutpat consequat mauris nunc congue nisi vitae suscipit tellus. Pretium nibh ipsum consequat nisl vel pretium lectus quam. Ipsum suspendisse ultrices gravida dictum fusce. Sed velit dignissim sodales ut eu sem integer. Donec massa sapien faucibus et molestie ac feugiat sed lectus. Rhoncus aenean vel elit scelerisque mauris pellentesque pulvinar. Iaculis urna id volutpat lacus laoreet. Ultrices neque ornare aenean euismod elementum nisi quis eleifend. Lectus magna fringilla urna porttitor rhoncus dolor purus non enim. Et ultrices neque ornare aenean. Sagittis eu volutpat odio facilisis mauris sit. Aliquet enim tortor at auctor urna nunc id."))

(defsection c11
  :header "Section 1.1"
  ((highlighted-code :language :java)
   "final class Main {
    private Main() {
    }

    public static void main(final String[] args) {
        System.out.println(\"Hello world!\");
    }
}")
  ((code-block :language "Yadda yadda yadda")
   (code "blah blah blah"))
  (p "Sem et tortor consequat id porta nibh venenatis cras sed. Elit duis tristique sollicitudin nibh sit amet commodo. Diam sit amet nisl suscipit adipiscing bibendum est. Egestas quis ipsum suspendisse ultrices gravida dictum fusce ut placerat. Elit duis tristique sollicitudin nibh sit amet commodo. Platea dictumst quisque sagittis purus sit amet volutpat consequat. Placerat orci nulla pellentesque dignissim enim sit amet. Id nibh tortor id aliquet lectus. Faucibus purus in massa tempor nec feugiat nisl. Posuere lorem ipsum dolor sit. Libero volutpat sed cras ornare arcu dui. Egestas integer eget aliquet nibh praesent tristique. Habitasse platea dictumst vestibulum rhoncus est. Facilisis magna etiam tempor orci eu lobortis elementum. Id faucibus nisl tincidunt eget nullam. Donec adipiscing tristique risus nec feugiat."))
