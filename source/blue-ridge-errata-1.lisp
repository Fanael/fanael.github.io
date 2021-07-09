;; Copyright © 2020  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Blue Ridge errata #1: root pointers during evacuation"
  :description "Explanation and solution of a serious Blue Ridge design problem involving root pointers during evacuation"
  :date (2020 02 08)
  :children (what-goes-wrong corruption-soup-ingredients just-reorder-phases tradeoff phases-updated)
  :topics ("garbage-collection")
  (p "While trying to implement concurrent evacuation I've found out that there is a serious problem with "
     ((a :href "/incremental-low-pause-gc.html") "my simple incremental low-pause garbage collector")
     " design. Implementing the "
     ((a :href "/incremental-low-pause-gc.html#phases") "phase ordering")
     " exactly as specified "
     (strong "would corrupt the root set")
     ", resulting in not very fun to debug crashes, even with extensive heap verification and lots of assertions everywhere. Why? Let's go through the pertinent assumptions, and the way they interact, carefully, and find a solution."))

(defsection what-goes-wrong
  :header "What goes wrong?"
  (p "This innocuous, perfectly reasonable code attempts to access from-space memory of an evacuated object:")
  ((highlighted-code :language :c++)
   "root_pointer<widget> blue_widget = get_widget(color::blue);
// Suppose our blue widget gets evacuated by incremental GC during this call,
// and evacuation hasn't completed yet, so the update roots phase hasn't run.
prepare_for_frobbing();
// blue_widget now points to from-space memory, frobbing it won't affect
// the to-space copy!
// Or if the evacuation process poisons the from-space memory, frobbing it
// will crash.
frob(blue_widget);"))

(defsection corruption-soup-ingredients
  :header "The ingredients of our corruption soup"
  (p "There are several assumptions the original design makes that together cause the problem:")
  (ul (li (b "Root pointers are always valid")
          ", as far as the mutator is concerned. This makes accessing data through root pointers " (em "fast") ", because there is no barrier code to execute, it's just a regular memory access through a pointer.")
      (li (b "Evacuation is concurrent") ". Obviously, if it's not concurrent, the issue magically disappears, but Blue Ridge is a mostly concurrent collector, so evacuation better stay concurrent.")
      (li (b "Objects directly pointed to by roots can move") ". Treating all objects pointed to from the root set as pinned is not something I'd consider desirable, even if pinning in a regionalized collector can be easy and cheap compared to traditional collectors with unified heap."))
  (p "Clearly, something needs to change, but none of the apparent solutions sound satisfactory. Introducing a barrier to resolve the to-space address of a root pointer is too expensive — just think of a just-in-time compiled implementation that keeps some of the root pointers in CPU registers having to run a non-trivial chunk of code containing several conditionals every time it wants to access something, the overhead would be prohibitive. Stop-the-world evacuation is out of the question in a mostly-concurrent collector, and treating root set pointees as pinned is a hack. Situation appears bleak…"))

(defsection just-reorder-phases
  :header "Easy, just reorder phases!"
  (p "…except there is a way out: what if the update roots phase ran " (em "before") " concurrent evacuation, thus ensuring the mutator can never have from-space pointers in the root set (remember that the load reference barrier prevents the mutator from loading a from-space pointer from the heap " (em "during") " evacuation already)? It cannot " (em "just") " update the root pointers, because it doesn't know the to-space addresses of those objects yet, because evacuation hasn't started yet, so this phase has to evacuate those objects itself; we can now call this phase " (b "evacuate roots") "."))

(defsection tradeoff
  :header "It's a tradeoff"
  (p ((a :href "https://en.wikipedia.org/wiki/Food_Not_Bombs") "There is such thing as a free lunch")
     ", but this is not it, so naturally, there is a catch. Updating pointers is almost trivial: look at the pointee to see if it has a forwarding pointer, and change the pointer being updated to that value if it does. Evacuation is more involved, as it requires copying the object, so naturally, it is slower. Since this is a stop-the-world phase, the worst-case latency is going to rise. It's still affected only by the root set size, though, only the constant factor is larger, which is acceptable in my case, as this approach is much simpler than the alternatives listed above, relative simplicity being another important factor of the design after all."))

(defsection phases-updated
  :header "Phase ordering, updated"
  (p "For reference, the updated "
     ((a :href "/incremental-low-pause-gc.html#phases") "list of phases")
     " is now as follows.")
  (ol (li "Mark roots.")
      (li "Incremental mark.")
      (li "Remark roots.")
      (li (b "Evacuate roots") ", the subject of this post.")
      (li "Incremental evacuation.")
      (li "Incremental update references.")
      (li "Cleanup.")))
