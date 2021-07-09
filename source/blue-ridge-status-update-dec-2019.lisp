;; Copyright © 2019  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Blue Ridge status update: December 2019"
  :description "Status update on the implementation of the Blue Ridge garbage collector as of December 2019"
  :date (2019 12 08)
  :topics ("garbage-collection")
  (p "Just a \"quick\" update on the development status of "
     ((a :href "/incremental-low-pause-gc.html") "my low-pause incremental garbage collector")
     " (code name "
     (dfn "Blue Ridge")
     "): it's progressing rather nicely, considering how little motivation my clinically depressed self is able to muster up most days.")
  (aside (p "No points for guessing where the code name comes from."))
  (p "The garbage collector as currently implemented, in just above 2 thousand lines of code, is already "
     (em "working")
     ", that is, it correctly allocates memory and collects garbage, "
     (em "even under address and undefined behavior sanitizers")
     ", though it's limited to stop-the-world operation at the moment. This is merely a pragmatic decision on what order to do things in: I'd rather have heap verifiers implemented and tested before diving into incrementality, as catching any potential heap corruptions or invariant violations right away is much preferable to trying to divine the cause of a crash from a core dump.")
  (p "That being said, most of the hard work for supporting incremental collections has already been done. Most of the collector's code is written in a way that makes incremental work easy enough, it just happens to not be used in that way yet.")
  (p "Performance appears to be fine so far. Pure allocation rate of object that die quickly is limited by how fast the CPU is able to write object data to memory, like in any good moving GC, as the hot path of allocation consists of just advancing a pointer. Collection time is dominated by marking, as one could expect. Throughput, measured by taking the wall-clock time of a program that allocates a linked list of 10 million conses then throws it away, repeating 500 times, is not bad either, being within the same order of magnitude as "
     ((a :href "https://wiki.openjdk.java.net/display/shenandoah/Main") "HotSpot's Shenandoah collector")
     " in its passive (stop-the-world only) mode, and "
     (em "beating handily")
     " its Parallel and G1 collectors with all four collectors limited to a 1 GB heap, which again, is not unexpected: this is not a very good workload+heap size combination for generational collectors, as they're forced to prematurely tenure conses, which then results in a significant number of painful major collections, while Blue Ridge and Shenandoah aren't affected due to their non-generational nature.")
  (p "Blue Ridge in its current state also features "
     (dfn "policies")
     ", switchable at runtime, that are informed whenever interesting events happen, such as a new region being allocated or the current phase being changed, and that in turn decide whether or not start a cycle, and if so what kind — incremental or stop-the-world — and what regions should belong to the collection set. It is through this mechanism that the aforementioned 1 GB heap limit has been implemented, as Blue Ridge's heap is normally discontinuous, dynamically resizable in either direction and theoretically unlimited.")
  (p "All things considered, the development of my garbage collector is progressing well, if slowly due to circumstances beyond my control. I hope I'll be able to have a functionally complete version soon, because that's where the real fun begins: playing around with all the knobs the get the lowest latency, lowest footprint and highest throughput possible."))
