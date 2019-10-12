;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "A simple incremental low-pause GC design"
  :description "Notes on a mostly-concurrent single-threaded incremental regionalized mark-copy garbage collector design."
  :date (2019 10 12)
  :children (intended-characteristics heap-layout phases invariants-and-barriers conclusion)
  (p "There are too many garbage-collected runtimes using very simplistic designs, such as reference counting or naïve stop the world mark and sweep. These are valid choices in that they're simple and correct, but unfortunately they induce undue costs, such as unpredictable and unbounded pause times.")
  (p "I decided to have a go at designing and implementing something better as a part of my custom lisp-dialect implementation, and this blog post is an attempt to describe and organize this design. "
     (strong "Since my toy lisp is single-threaded, so is its garbage collector")
     ". It is possible to extend this design into a parallel one, it just is not one of my goals.")
  (p "It should be noted that this design is in no way revolutionary, I'm merely following existing techniques. In particular, "
     ((a :href "https://wiki.openjdk.java.net/display/shenandoah/Main") "Red Hat's excellent Shenandoah")
     " has been a major inspiration."))

(defsection intended-characteristics
  :header "Intended characteristics"
  (p "There are a few characteristics the desired collector should meet:")
  (ul
   (li "It should be "
       (b "incremental")
       ". Since my toy lisp is single-threaded, interleaving the execution of mutator and the collector is the only option. The simplest way to achieve that is to run a small number of steps every time the mutator allocates, so that's what I'll go with.")
   (li "It should have a "
       (b "dynamically resizeable heap")
       ". I dislike having to guess at startup how much memory will a program require, so I want to avoid that. It should also promptly release unused memory to the operating system, to avoid memory-hogging.")
   (li "It should, as mentioned previously, "
       (b "minimize the pause time")
       ".")
   (li "It should be "
       (b "compacting")
       " to combat heap fragmentation. This also means it should be "
       (b "precise")
       ", as conservative moving collectors are possible, but difficult to get right.")
   (li "It should "
       (b (em "not")
          " be generational")
       ". There are real world workloads, such as LRU caches, that don't perform well with generational collectors."))
  (p "Do note that "
     (em "throughput is not a goal")
     ". While good throughput obviously would be nice, minimizing latency and footprint is more important.")
  (p "With these in mind, I decided on a mostly-concurrent incremental regionalized mark-copy collector described in more detail below."))

(defsection heap-layout
  :header "Heap layout"
  :children (region-metadata keeping-track-of-regions)
  (p "Since the heap is intended to be dynamically resizeable in both directions, using a contiguous heap is not a possibility, as growing a contiguous heap would essentially require a huge stop-the-world pause to copy everything over and update the references.")
  (p "Instead, the heap is allocated and freed one "
     (b "region")
     " at a time, where each region is a large-ish chunk of memory, arbitrarily sized at 16 megabytes to allow the usage of hardware's huge pages for backing each region on pretty much every architecture out there. This value may need experimental tuning to achieve the best performance.")
  (p "Objects are allocated by taking a fresh region, called "
     (b "nursery")
     ", and bumping a pointer. Once the free space in the current nursery runs out, another one is allocated, and a collection cycle may be started.")
  (p "Objects too big to fit in a standard region, such as large arrays, are allocated separately in a region of their own, termed a "
     (b "humongous")
     " region, which holds only that single object. In fact, a limit smaller than the entire size of a standard region is probably better, to minimize the wasted space in standard regions in presence of frequent large allocations, such as one quarter or even one eighth of a standard region."))

(defsection region-metadata
  :header "Region metadata"
  (p "Internally, a region is represented as a simple structure along the lines of:")
  ((highlighted-code :language :c++)
   "struct region {
    void* region_start;
    size_t region_size;
    dynamic_bitset mark_bits;
    bool is_humongous;
};")
  (p "The marking bits of each object are stored "
     (em "externally")
     " in the "
     (code "mark_bits")
     " bitmap of their parent region, to facilitate fast iteration over "
     (em "live")
     " objects, which is an important operation during evacuation and reference updating. Scanning for the next set bit is significantly faster of an operation than finding the next live object by parsing the heap."))

(defsection keeping-track-of-regions
  :header "Keeping track of regions"
  (p "Regions are kept in a "
     (b "slot map")
     " data structure: each region has a "
     (em "stable")
     " numeric ID that's used to directly index into the underlying array of the slot map: a region receives an ID upon creation and as long as the region stays live, that ID won't suddenly change to refer to a different region.")
  (p "Additionally, during some phases a bitmap of region IDs is kept to indicate which regions already existed at the start of the phase, which will be discussed more in "
     ((a :href "#invariants-and-barriers") "the description of invariants and barriers"
      "."))
  (p "If object pinning is needed to interface with GC-unaware code, it can be implemented with ease by storing an additional bitmap of region IDs that holds the regions that contain pinned objects, and not attempting to move objects from those regions."))

(defsection phases
  :header "Phases"
  (p "Like any tracing garbage collector, this collector works in phases. Most of the work is done in the concurrent phases, due to low-latency focus.")
  (ol
   (li (b "Mark roots")
       ": a stop-the-world phase where the objects pointed to from the root set are marked reachable and added to the scanning queue. Marking roots concurrently is hard, and since the size of the root set tends to stay constant no matter the heap size, pausing the mutator is not too big of a deal.")
   (li (b "Incremental mark")
       ": an incremental phase where the objects in the scanning queue are scanned for references, marking the pointed-to objects and putting "
       (em "them")
       " in the scanning queue. This is where most of the classical marking work is done.")
   (li (b "Remark roots")
       ": a stop-the-world phase where the root set is scanned "
       (em "again")
       " to pick up any possible changes in the live set. This very rarely results in any significant amount of work, because almost everything has already been marked. Additionally, regions that don't contain any live objects are immediately freed in this stage, and a collection set of regions that contain both live objects and enough dead objects to warrant collection is constructed.")
   (li (b "Incremental evacuation")
       ": an incremental phase where the live objects are copied from regions in the collection set to freshly-allocated regions. This is where most of the classical evacuation work is done. An important thing to note is that once an object has been copied "
       (strong "no reads or written happen to the copy in the collection set")
       ", all using the fresh copy instead.")
   (li (b "Update roots")
       ": a stop-the-world phase where the references that point to the collection set in the root set are updated to point to the fresh object copies instead. Similar caveats as with the mark roots phase apply.")
   (li (b "Update references")
       ": an incremental phase where a heap walk is performed to replace every references to a live object in the collection set with one to the fresh copy.")
   (li (b "Cleanup")
       ": the collection set regions are simply freed.")))

(defsection invariants-and-barriers
  :header "Invariants and barriers"
  :children (write-barrier load-reference-barrier)
  (p "Since this collector is incremental, it requires cooperation with the mutator to ensure that certain properties are never violated. These invariants are enforced by injecting "
     (b "barriers")
     " (nothing to do with processor memory barriers!) into the mutator for certain operations. There are two notable invariants that require barriers in this design:")
  (ul
   (li (b "The tri-color invariant")
       ": during marking, objects are split into three sets: black, gray and white."
       (ul
        (li (b "Black")
            " objects are those that are known to be reachable and have already been traversed. In this collector, these are the objects whose mark bit is set and are not in the scanning queue.")
        (li (b "Gray")
            " objects are those that are known to be reachable, but have not yet been traversed. This corresponds to objects whose mark bit is set and "
            (em "are")
            " in the scanning queue.")
        (li (b "White")
            " objects are those that haven't yet been determined to be reachable, this corresponds to object whose mark bit is clear. All objects that remain white after remarking roots are dead."))
       "The invariant is therefore "
       (strong "no black objects points to a white object")
       ".")
   (li (b "The strong to-space invariant")
       ": during evacuation and reference updating, all reads and writes happen to/from the fresh copy of an object."))
  (p "The mutator can freely read and modify the heap with no restrictions during the incremental phases, so every time a potentially problematic operation occurs — a write in case of the tri-color invariant, a reference read in case of the strong to-space invariant — the collector needs to execute a barrier to have a chance to maintain the invariants.")
  (p "Somewhat arbitrarily I chose to have the regions created during incremental marking be assumed black, so they're not scanned. If they contain garbage, this garbage won't be reclaimed until the next cycle, but is otherwise harmless.")
  (p "As a consequence of the strong to-space invariant, the objects created after the stop-the-world update roots phase are guaranteed to not contain pointers to the collection set, because there is no way to obtain such pointers anymore. This lets the incremental reference update skip those object, saving some work."))

(defsection write-barrier
  :header "Write barrier"
  (p "If a reference inside a black object X is changed during the incremental marking phase to point to a white object Y, the tri-color invariant is violated. If nothing was done to remedy this situation and it is the only reference to Y, Y will be incorrectly treated as dead and collected, corrupting the heap.")
  (p "Corrupting the heap is obviously a very undesirable situation, but there are several ways to avoid it. The solution I chose to employ is the so-called "
     (b "snapshot-at-the-beginning")
     " (SATB) technique.")
  (p "A collector employing SATB doesn't do anything "
     (em "directly")
     " about the tri-color invariant violation, choosing to leave the black-to-white link alone, observing that as long as the white object is reachable from some gray object, it will be retained, thus ensuring there's no tri-color violation "
     (strong "at the end of the marking phase")
     ".")
  (p "A SATB write barrier ensures that whenever a reference to a white object from a gray object is "
     (em "removed")
     ", the white object is colored gray. This effectively makes the collector pretend that the object graph didn't change since the beginning of the marking phase, hence the name \"snapshot-at-the-beginning\".")
  (p "In terms of code:")
  ((highlighted-code :language :c++)
   "void garbage_collector::write_barrier(object& referrer, object& old_referent)
{
    if(!is_marking()) {
        // No need to do anything if not in marking phase.
        return;
    }
    if(is_marked(referrer) && !is_marked(old_referent)) {
        mark(old_referent);
        scanning_queue.push(old_referent);
    }
}"))

(defsection load-reference-barrier
  :header "Load-reference barrier"
  (p "To ensure that an live object in a region in the collection set has only one evacuated copy, and that once it does have one all reads and writes are from/to that copy, a load reference barrier is employed, used, as the name implies, when "
     (em "loading")
     " a "
     (em "reference")
     " from the heap. Loading non-reference objects, such as integers, requires no barrier.")
  (p "The barrier itself doesn't require too much complicated reasoning: if the object is "
     (strong "not")
     " in the collection set, no need to do anything. Otherwise, look at the forwarding Brooks pointer in the object header. If present, use that address, otherwise evacuate the object.")
  (p "As a minor optimization, the reference being read can be updated to point to the new address, to save a tiny bit of work during reference update phase by doing it now.")
  (p "In terms of code:")
  ((highlighted-code :language :c++)
   "reference garbage_collector::load_reference_barrier(reference& ref)
{
    if(!in_evacuation() && !in_update_references()) {
        // Nothing to do here, maybe check if the object has no forwarding
        // pointer for debugging.
        return ref;
    }
    if(!collection_regions.contains(region_of_object(ref))) {
        // Ditto.
        return ref;
    }
    if(!is_forwarded(ref)) {
        assert(in_evacuation());
        ref = evacuate_object(ref);
    } else {
        ref = get_forwarding_pointer(ref);
    }
    return ref;
}"))

(defsection conclusion
  :header "Conclusion"
  (p "That's about it for the design. While it certainly is more complicated than just a stop-the-world mark-and-sweep, it's also significantly better in many important ways such as latency, and is not "
     (em "too")
     " hard to understand, it just has many moving parts.")
  (p "I am currently in the process of implementing it, to see how well it performs in practice, and how well it's amenable to tuning. I'll come back with measurements once that's ready."))
