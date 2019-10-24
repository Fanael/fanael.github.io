;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Hardware prefetching in Pentium III"
  :description "Determining the existence and discovering the basic characteristics of the hardware prefetcher in Pentium III"
  :date (2019 10 25)
  :children (test results conclusion)
  (p "Nowadays pretty much every processor featuring cache hierarchy has some form of automatic memory prefetch mechanism. In small low-power processors it may be very simplistic and only prefetch if the addresses are increasing, in high-performance processors it may try to predict all sorts of regular and semi-regular patterns, but the basic mechanism "
     (em "will be")
     " present in some form.")
  (p "We weren't, however, always so lucky. The first mainstream x86 processor with hardware prefetching logic was the Pentium 4 Willamette, first released in November 2000, which used the highly contentious NetBurst microarchitecture. Due to Willamette's mediocrity, Intel didn't kill off their older P6 microarchitecture, and so the hardware prefetching was later supposedly added to the P6-based Pentium III Tualatin (released in June 2001), where its efficacy was disputed due to much lower CPU-chipset bandwidth compared to NetBurst.")
  (aside
   (p "The P6 microarchitecture introduced in Pentium Pro, in 1995, and used with minor changes in Pentium II and III, is a direct ancestor of Intel's current high-performance x86 microarchitectures — at the time of writing, Ice Like for mobile, Skylake for everything else. There were many improvements in the last 24 years, and most major bottlenecks were fixed, but the family resemblance is still there.")
   (p "P6's direct successor was Pentium M, created for mobile processors due to NetBurst's high power draw rendering it unsuitable for those applications. The Core microarchitecture powering the famous Core 2 series, that let Intel take the performance crown back from AMD after years of humiliation due to their marketing-driven, physics-ignoring bet on NetBurst, was a direct successor of Pentium M."))
  (p "But how effective was the hardware prefetching in Tualatin, really? And is it really true that Coppermine, the Pentium III revision preceding Tualatin, didn't feature automatic prefetching? Fortunately, by choosing an appropriate test case and using CPU performance counters, it is possible to perform a gray-box test that answers these questions."))

(defsection test
  :header "The test"
  (p "The thing I want to test is whether or not Pentium III's prefetcher, if existent and effective, can handle accessing memory linearly with large variable strides. The first algorithm with such an access pattern that came to my mind was "
     ((a :href "https://en.wikipedia.org/wiki/Shellsort") "Shell sort")
     ". When used with the "
     ((a :href "https://en.wikipedia.org/wiki/Smooth_number") "3-smooth numbers")
     " (i.e. numbers of the form "
     (i "2" (sup "p") "3" (sup "q"))
     ", with natural " (var "p") " and " (var "q")
     ") as the gap sequence, its time complexity is "
     (i "O(nlog" (sup "2") "n)")
     ", which is practical even for large values of "
     (var "n")
     ".")
  (p "As a comparison baseline the obvious choice is then "
     ((a :href "https://en.wikipedia.org/wiki/Heapsort") "heap sort")
     ", due to its infamy for being "
     (em "extremely")
     " cache-hostile and already being available in the C++ standard library. In fact, if comparisons and swaps are cheap, heap sort's cache hostility can and does make up for its theoretical complexity advantage, making it not much faster than 3-smooth Shell sort. On my Core i5-4590 (Haswell) the difference between the two algorithms is indeed less than a factor of two:")
  ((pre :class "codeblock" :data-code-language "Shell (interactive)")
   "$ g++ -O3 p3-prefetch-shell-sort.cc && time ./a.out # heap sort
./a.out  2,77s user 0,00s system 99% cpu 2,771 total
$ g++ -O3 -DUSE_SHELL_SORT p3-prefetch-shell-sort.cc && time ./a.out # Shell sort
./a.out  4,52s user 0,00s system 99% cpu 4,517 total")
  (p "The implementation of Shell sort has been lifted from Wikipedia and C++-ified with no other significant changes:")
  ((highlighted-code :language :c++)
   "void shell_sort(std::uint32_t* array, std::size_t length)
{
    if(length <= 1) {
        return;
    }

    auto current_gap = std::find_if(std::begin(gaps), std::end(gaps),
        [&](std::size_t gap){return gap >= length;});
    do {
        --current_gap;
        const auto gap = *current_gap;
        for(std::size_t i = gap; i < length; ++i) {
            auto temp = std::move(array[i]);
            std::size_t j = i;
            for(; j >= gap && array[j - gap] > temp; j -= gap) {
                array[j] = std::move(array[j - gap]);
            }
            array[j] = std::move(temp);
        }
    } while(current_gap > std::begin(gaps));
}")
  (p ((a :href "/static/p3-prefetch-shell-sort.cc") "The complete source of the benchmark program")
     " is available under "
     ((a :href "https://creativecommons.org/publicdomain/zero/1.0/") "the CC0 license")
     ".")
  (aside
   (p "In practice, neither algorithm is a very good choice for general-purpose sorting; instead, use something "
      (em "really")
      " cache-friendly "
      (em "and")
      " theoretically efficient, like introsort or merge sort, one of these is almost certainly used to implement the sorting routine in the standard library of your language of choice already, so just use that. Shell sort and heap sort were only chosen here for their memory access patterns."))
  (p "With that code, a Coppermine Pentium III at 1 GHz (thanks to "
     (b "daemon32")
     " for running the test!), a Tualatin Pentium III at 1.13 GHz, a Dothan Pentium M at 1.6 GHz for comparison purposes, GCC 8, Intel's performance event register reference, and the excellent Linux "
     ((cite :class "program-name") "perf")
     " tool, let's go measuring."))

(defsection results
  :header "The results"
  :children (results-coppermine results-tualatin results-dothan)
  (p "There is a problem. The Intel performance monitoring event reference doesn't mention anything about prefetching in its P6 chapter! Prefetching events are mentioned in the Pentium M reference, however, and considering P6 family history, it's not unreasonable to believe the same events would be used in Pentium III. These events are, in syntax suitable for "
     (code "perf stat -e")
     ", "
     (kbd "cpu/event=0xf0,name=upward-prefetches/")
     " and "
     (kbd "cpu/event=0xf8,name=downward-prefetches/")
     ". They count the number of "
     (em "prefetch requests")
     ", and therefore the number of prefetched "
     (em "cache lines")
     ". This will be important later on.")
  (p "So first things first: "
     (strong "these events don't report anything on Coppermine, always showing zeros")
     ". This adds credence to the idea that Coppermine and earlier P6 models didn't have hardware prefetching unit. On Tualatin, however, things are more interesting, so without further ado, let's dive into the performance counter stats!"))

(defsection results-coppermine
  :header "Coppermine"
  (p "Heap sort:")
  ((pre :class "codeblock" :data-code-language "perf-stat") "Performance counter stats for './x-noshellsort':

      33276.769267      task-clock (msec)         #    0.999 CPUs utilized
               204      context-switches          #    0.006 K/sec
                 0      cpu-migrations            #    0.000 K/sec
             14715      page-faults               #    0.442 K/sec
       32854157356      cycles                    #    0.987 GHz                     [20.00%]
       25399721497      stalled-cycles-frontend   #   77.31% frontend cycles idle    [20.01%]
   <not supported>      stalled-cycles-backend:HG
        7109652282      instructions:HG           #    0.22  insns per cycle
                                                  #    3.57  stalled cycles per insn [20.01%]
        1188575659      branches:HG               #   35.718 M/sec                   [20.01%]
         196353276      branch-misses:HG          #   16.52% of all branches         [20.00%]

      33.307405391 seconds time elapsed")
  (p "Shell sort:")
  ((pre :class "codeblock" :data-code-language "perf-stat") "Performance counter stats for './x-shellsort':

     133899.702618      task-clock (msec)         #    0.999 CPUs utilized
               647      context-switches          #    0.005 K/sec
                 0      cpu-migrations            #    0.000 K/sec
             14717      page-faults               #    0.110 K/sec
      132163280671      cycles                    #    0.987 GHz                     [20.00%]
       96185812551      stalled-cycles-frontend   #   72.78% frontend cycles idle    [20.00%]
   <not supported>      stalled-cycles-backend:HG
       53356713225      instructions:HG           #    0.40  insns per cycle
                                                  #    1.80  stalled cycles per insn [20.00%]
        8501189300      branches:HG               #   63.489 M/sec                   [20.00%]
         269043077      branch-misses:HG          #    3.16% of all branches         [20.00%]

     133.982751047 seconds time elapsed")
  (p "The first observation: it's "
     (em "slow")
     ". But more importantly, it tells us "
     (em "why")
     " it is slow: the out-of-order engine is full, but instructions cannot retire because they're still waiting on memory accesses to complete (as there are no other significant sources of stalls in this program), which stalls the front-end too. Heap sort stalls a lot because it's heap sort; Shell sort stalls a lot, albeit a bit less, because with large gap sizes, at least until they get small enough to resemble insertion sort cache-wise, the program performs lots of memory accesses to far away addresses that either haven't had a chance to get into cache yet, or have already been evicted to make room for newer ones, and there's no prefetching to do it ahead-of-time."))

(defsection results-tualatin
  :header "Tualatin"
  (p "Heap sort:")
  ((pre :class "codeblock" :data-code-language "perf-stat") "Performance counter stats for './a.out':

         31,190.69 msec task-clock:u              #    0.993 CPUs utilized
                 0      context-switches:u        #    0.000 K/sec
                 0      cpu-migrations:u          #    0.000 K/sec
             1,429      page-faults:u             #    0.046 K/sec
    33,344,788,971      cycles:u                  #    1.069 GHz                      (28.53%)
     7,084,795,042      instructions:u            #    0.21  insn per cycle
                                                  #    3.60  stalled cycles per insn  (28.60%)
    25,490,259,991      stalled-cycles-frontend:u #   76.44% frontend cycles idle     (28.60%)
     1,221,344,046      branches:u                #   39.157 M/sec                    (28.59%)
       195,036,348      branch-misses:u           #   15.97% of all branches          (28.59%)
        71,322,504      upward-prefetches:u       #    2.287 M/sec                    (28.57%)
                 0      downward-prefetches:u     #    0.000 K/sec                    (28.51%)

      31.404312810 seconds time elapsed

      30.886442000 seconds user
       0.287093000 seconds sys")
  (p "Shell sort:")
  ((pre :class "codeblock" :data-code-language "perf-stat") "Performance counter stats for './a.out':

         72,946.84 msec task-clock:u              #    0.991 CPUs utilized
                 0      context-switches:u        #    0.000 K/sec
                 0      cpu-migrations:u          #    0.000 K/sec
             1,434      page-faults:u             #    0.020 K/sec
    78,228,149,161      cycles:u                  #    1.072 GHz                      (28.58%)
    53,121,702,516      instructions:u            #    0.68  insn per cycle
                                                  #    0.82  stalled cycles per insn  (28.57%)
    43,812,136,162      stalled-cycles-frontend:u #   56.01% frontend cycles idle     (28.58%)
     8,465,544,250      branches:u                #  116.051 M/sec                    (28.58%)
       264,197,085      branch-misses:u           #    3.12% of all branches          (28.56%)
       704,750,223      upward-prefetches:u       #    9.661 M/sec                    (28.55%)
                 0      downward-prefetches:u     #    0.000 K/sec                    (28.58%)

      73.608832025 seconds time elapsed

      72.634633000 seconds user
       0.267156000 seconds sys")
  (p "Heap sort is still being heap sort, but "
     (strong "Shell sort is almost two times as fast")
     ", and the only relevant hardware difference was the introduction of hardware prefetching in Tualatin. It's also obvious from these numbers that Tualatin's hardware prefetching "
     (strong "only considers ascending address sequences")
     ", as the downward prefetch counter is stuck at 0.")
  (p "The prefetch rate, that is cache line size — the original P6 is a weird member of the P6 family, as "
     (strong "its cache line size is 32 bytes")
      ", not 64 as in all of its descendants! — times the number of prefetch requests divided by time, is about 295 MB/s. Since the array is 60 million bytes, or slightly over 57 MB, and only gap sizes of 8 and below touch adjacent cache lines, my educated guess is that it's likely the prefetcher "
      (em "does")
      " handle variable strides. Sure, it's unlikely to be perfect. Sure, the memory bandwidth is limited, with the 133 MHz front-side bus the theoretical maximum is about a gigabyte per second, less in practice. But the prefetcher is there, handles variable strides, and does it well enough that at least in this case it greatly improves performance."))

(defsection results-dothan
  :header "Dothan"
  (p "This is not a Pentium III. It's a Pentium M, it's not even the original P6 anymore, it contains many improvements compared to its predecessor, most of which survive to this day in modern P6 descendants. It also has significantly bigger cache and faster processor-chipset, and thus processor-memory, interface. But let's still see how it handles this test, because this is the CPU for which the performance events used are documented, and out of sheer curiosity.")
  (p "Heap sort:")
  ((pre :class "codeblock" :data-code-language "perf-stat") "Performance counter stats for './a.out':

         16,842.52 msec task-clock:u              #    0.996 CPUs utilized
                 0      context-switches:u        #    0.000 K/sec
                 0      cpu-migrations:u          #    0.000 K/sec
             1,433      page-faults:u             #    0.085 K/sec
    26,310,833,013      cycles:u                  #    1.562 GHz                      (28.57%)
     7,093,179,520      instructions:u            #    0.27  insn per cycle
                                                  #    2.62  stalled cycles per insn  (28.56%)
    18,575,088,740      stalled-cycles-frontend:u #   70.60% frontend cycles idle     (28.58%)
     1,217,221,905      branches:u                #   72.271 M/sec                    (28.56%)
       207,078,239      branch-misses:u           #   17.01% of all branches          (28.55%)
        21,031,331      upward-prefetches:u       #    1.249 M/sec                    (28.57%)
        92,661,140      downward-prefetches:u     #    5.502 M/sec                    (28.60%)

      16.903282957 seconds time elapsed

      16.698724000 seconds user
       0.139888000 seconds sys")
  (p "Shell sort:")
  ((pre :class "codeblock" :data-code-language "perf-stat") "Performance counter stats for './a.out':

         33,594.70 msec task-clock:u              #    0.996 CPUs utilized
                 0      context-switches:u        #    0.000 K/sec
                 0      cpu-migrations:u          #    0.000 K/sec
             1,435      page-faults:u             #    0.043 K/sec
    52,612,387,270      cycles:u                  #    1.566 GHz                      (28.59%)
    53,098,993,093      instructions:u            #    1.01  insn per cycle
                                                  #    0.43  stalled cycles per insn  (28.55%)
    23,082,925,885      stalled-cycles-frontend:u #   43.87% frontend cycles idle     (28.52%)
     8,469,337,848      branches:u                #  252.103 M/sec                    (28.57%)
       290,400,768      branch-misses:u           #    3.43% of all branches          (28.61%)
       355,720,357      upward-prefetches:u       #   10.589 M/sec                    (28.59%)
            12,126      downward-prefetches:u     #    0.361 K/sec                    (28.57%)

      33.716368417 seconds time elapsed

      33.424539000 seconds user
       0.155648000 seconds sys")
  (p "Both algorithms benefit from the increased cache, but Shell sort more so. This is also the first time we've seen more than 1 instruction per cycle in this post. Not that it's somehow an great result: my Haswell box mentioned earlier executes 2.4 instructions per cycle in that same test, for whoever is wondering, but that's a processor released 10 years later than this Pentium M.")
  (p "The total number of upward prefetches, when expressed in "
     (em "bytes")
     " — Pentium M has now-standard 64-byte cache lines — is almost exactly the same as on Tualatin, confirming that the upward prefetch counter indeed works there."))

(defsection conclusion
  :header "The conclusion"
  (p "This test shows that the legends are true. Coppermine doesn't have automatic hardware prefetching, and thus presumably Katmai and older P6 processors don't either. It was introduced in a limited, but functional, form in Tualatin: hardware prefetching only works for ascending addresses, and is limited by slow memory interface, but can handle variable strides, and even the undocumented performance events appear to function correctly."))
