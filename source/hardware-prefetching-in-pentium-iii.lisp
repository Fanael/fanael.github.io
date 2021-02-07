;; Copyright © 2019-2021  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Hardware prefetching in Pentium III"
  :description "Determining the existence and discovering the basic characteristics of the hardware prefetcher in Pentium III"
  :date (2019 10 25)
  :topics ("microarchitecture-archeology")
  :children (test results conclusion)
  (p "Nowadays pretty much every processor featuring cache hierarchy has some form of automatic memory prefetch mechanism. In small low-power processors it may be very simplistic and only prefetch if the addresses are increasing, in high-performance processors it may try to predict all sorts of regular and semi-regular patterns, but the basic mechanism "
     (em "will be")
     " present in some form.")
  (p "We weren't, however, always so lucky. The first mainstream x86 processor with hardware prefetching logic was the Pentium 4 Willamette, first released in November 2000, which used the highly contentious NetBurst microarchitecture. Due to Willamette's mediocrity, Intel didn't kill off their older P6 microarchitecture, and so the hardware prefetching was later supposedly added to the P6-based Pentium III Tualatin (released in June 2001), where its efficacy was disputed due to much lower CPU-chipset bandwidth compared to NetBurst.")
  (aside
   (p "The P6 microarchitecture introduced in Pentium Pro, in 1995, and used with minor changes in Pentium II and III, is a direct ancestor of Intel's current high-performance x86 microarchitectures — at the time of writing, Ice Lake for mobile, Skylake for everything else. There were many improvements in the last 24 years, and most major bottlenecks were fixed, but the family resemblance is still there.")
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
   (samp "$ g++ -O3 p3-prefetch-shell-sort.cc && time ./a.out # heap sort
./a.out  2,77s user 0,00s system 99% cpu 2,771 total
$ g++ -O3 -DUSE_SHELL_SORT p3-prefetch-shell-sort.cc && time ./a.out # Shell sort
./a.out  4,52s user 0,00s system 99% cpu 4,517 total"))
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
  (figure
   (figcaption "Performance event counts for " (em "heap") " sort on Coppermine")
   ((div :class "holder")
    ((table :class "perf-stat")
     (thead
      (tr (th "Event") (th "Value") (th "Unit") (th "Comment") (th "Time active")))
     (tbody
      (tr (th "Task clock") (td "33,276.77") (td "msec") (td "0.999 CPUs utilized") (td))
      (tr (th "Context switches") (td "204") (td) (td "0.006 K/sec") (td))
      (tr (th "Page faults") (td "14,715") (td) (td "0.442 K/sec") (td))
      (tr (th "Cycles") (td "32,854,157,356") (td) (td "0.987 GHz") (td "20.00%"))
      (tr (th "Stalled cycles, frontend") (td "25,399,721,497") (td) (td "77.31% frontend cycles idle") (td "20.01%"))
      (tr (th "Instructions") (td "7,109,652,282") (td) (td "0.22 instructions per cycle" (br) "3.57 stalled cycles per instruction") (td "20.01%"))
      (tr (th "Branches") (td "1,188,575,659") (td) (td "35.718 M/sec") (td "20.01%"))
      (tr (th "Mispredicted branches") (td "196,353,276") (td) (td "16.52% of all branches") (td "20.00%"))
      (tr (th "Elapsed time") (td "33.307") (td "seconds") (td) (td))))))
  (figure
   (figcaption "Performance event counts for " (em "shell") " sort on Coppermine")
   ((div :class "holder")
    ((table :class "perf-stat")
     (thead
      (tr (th "Event") (th "Value") (th "Unit") (th "Comment") (th "Time active")))
     (tbody
      (tr (th "Task clock") (td "133,899.70") (td "msec") (td "0.999 CPUs utilized") (td))
      (tr (th "Context switches") (td "647") (td) (td "0.005 K/sec") (td))
      (tr (th "Page faults") (td "14,717") (td) (td "0.110 K/sec") (td))
      (tr (th "Cycles") (td "132,163,280,671") (td) (td "0.987 GHz") (td "20.00%"))
      (tr (th "Stalled cycles, frontend") (td "96,185,812,551") (td) (td "72.78% frontend cycles idle") (td "20.00%"))
      (tr (th "Instructions") (td "53,356,713,225") (td) (td "0.40 instructions per cycle" (br) "1.80 stalled cycles per instruction") (td "20.00%"))
      (tr (th "Branches") (td "8,501,189,300") (td) (td "63.489 M/sec") (td "20.00%"))
      (tr (th "Mispredicted branches") (td "269,043,077") (td) (td "3.16% of all branches") (td "20.00%"))
      (tr (th "Elapsed time") (td "133.983") (td "seconds") (td) (td))))))
  (p "The first observation: it's "
     (em "slow")
     ". But more importantly, it tells us "
     (em "why")
     " it is slow: the out-of-order engine is full, but instructions cannot retire because they're still waiting on memory accesses to complete (as there are no other significant sources of stalls in this program), which stalls the front-end too. Heap sort stalls a lot because it's heap sort; Shell sort stalls a lot, albeit a bit less, because with large gap sizes, at least until they get small enough to resemble insertion sort cache-wise, the program performs lots of memory accesses to far away addresses that either haven't had a chance to get into cache yet, or have already been evicted to make room for newer ones, and there's no prefetching to do it ahead-of-time."))

(defsection results-tualatin
  :header "Tualatin"
  (figure
   (figcaption "Performance event counts for " (em "heap") " sort on Tualatin")
   ((div :class "holder")
    ((table :class "perf-stat")
     (thead
      (tr (th "Event") (th "Value") (th "Unit") (th "Comment") (th "Time active")))
     (tbody
      (tr (th "Task clock") (td "31,190.69") (td "msec") (td "0.993 CPUs utilized") (td))
      (tr (th "Page faults") (td "1,429") (td) (td "0.046 K/sec") (td))
      (tr (th "Cycles") (td "33,344,788,971") (td) (td "1.069 GHz") (td "28.53%"))
      (tr (th "Stalled cycles, frontend") (td "25,490,259,991") (td) (td "76.44% frontend cycles idle") (td "28.60%"))
      (tr (th "Instructions") (td "7,084,795,042") (td) (td "0.21 instructions per cycle" (br) "3.60 stalled cycles per instruction") (td "28.60%"))
      (tr (th "Branches") (td "1,221,344,046") (td) (td "39.157 M/sec") (td "28.59%"))
      (tr (th "Mispredicted branches") (td "195,036,348") (td) (td "15.97% of all branches") (td "28.59%"))
      (tr (th "Upward prefetches") (td "71,322,504") (td) (td "2.287 M/sec") (td "28.57%"))
      (tr (th "Downward prefetches") (td "0") (td) (td "0.000 K/sec") (td "28.51%"))
      (tr (th "Elapsed time") (td "31.404") (td "seconds") (td) (td))))))
  (figure
   (figcaption "Performance event counts for " (em "shell") " sort on Tualatin")
   ((div :class "holder")
    ((table :class "perf-stat")
     (thead
      (tr (th "Event") (th "Value") (th "Unit") (th "Comment") (th "Time active")))
     (tbody
      (tr (th "Task clock") (td "72,946.84") (td "msec") (td "0.991 CPUs utilized") (td))
      (tr (th "Page faults") (td "1,434") (td) (td "0.020 K/sec") (td))
      (tr (th "Cycles") (td "78,228,149,161") (td) (td "1.072 GHz") (td "28.58%"))
      (tr (th "Stalled cycles, frontend") (td "43,812,136,162") (td) (td "56.01% frontend cycles idle") (td "28.58%"))
      (tr (th "Instructions") (td "53,121,702,516") (td) (td "0.68 instructions per cycle" (br) "0.82 stalled cycles per instruction") (td "28.57%"))
      (tr (th "Branches") (td "8,465,544,250") (td) (td "116.051 M/sec") (td "28.58%"))
      (tr (th "Mispredicted branches") (td "264,197,085") (td) (td "3.12% of all branches") (td "28.56%"))
      (tr (th "Upward prefetches") (td "704,750,223") (td) (td "9.661 M/sec") (td "28.55%"))
      (tr (th "Downward prefetches") (td "0") (td) (td "0.000 K/sec") (td "28.58%"))
      (tr (th "Elapsed time") (td "73.609") (td "seconds") (td) (td))))))
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
  (figure
   (figcaption "Performance event counts for " (em "heap") " sort on Dothan Pentium M")
   ((div :class "holder")
    ((table :class "perf-stat")
     (thead
      (tr (th "Event") (th "Value") (th "Unit") (th "Comment") (th "Time active")))
     (tbody
      (tr (th "Task clock") (td "16,842.52") (td "msec") (td "0.996 CPUs utilized") (td))
      (tr (th "Page faults") (td "1,433") (td) (td "0.085 K/sec") (td))
      (tr (th "Cycles") (td "26,310,833,013") (td) (td "1.562 GHz") (td "28.57%"))
      (tr (th "Stalled cycles, frontend") (td "18,575,088,740") (td) (td "70.60% frontend cycles idle") (td "28.58%"))
      (tr (th "Instructions") (td "7,093,179,520") (td) (td "0.27 instructions per cycle" (br) "2.62 stalled cycles per instruction") (td "28.56%"))
      (tr (th "Branches") (td "1,217,221,905") (td) (td "72.271 M/sec") (td "28.56%"))
      (tr (th "Mispredicted branches") (td "195,036,348") (td) (td "17.01% of all branches") (td "28.55%"))
      (tr (th "Upward prefetches") (td "21,031,331") (td) (td "1.249 M/sec") (td "28.57%"))
      (tr (th "Downward prefetches") (td "92,661,140") (td) (td "5.502 M/sec") (td "28.60%"))
      (tr (th "Elapsed time") (td "16.903") (td "seconds") (td) (td))))))
  (figure
   (figcaption "Performance event counts for " (em "shell") " sort on Dothan Pentium M")
   ((div :class "holder")
    ((table :class "perf-stat")
     (thead
      (tr (th "Event") (th "Value") (th "Unit") (th "Comment") (th "Time active")))
     (tbody
      (tr (th "Task clock") (td "33,594.70") (td "msec") (td "0.996 CPUs utilized") (td))
      (tr (th "Page faults") (td "1,435") (td) (td "0.043 K/sec") (td))
      (tr (th "Cycles") (td "52,612,387,270") (td) (td "1.566 GHz") (td "28.59%"))
      (tr (th "Stalled cycles, frontend") (td "23,082,925,885") (td) (td "43.87% frontend cycles idle") (td "28.55%"))
      (tr (th "Instructions") (td "53,098,993,093") (td) (td "1.01 instructions per cycle" (br) "0.43 stalled cycles per instruction") (td "28.55%"))
      (tr (th "Branches") (td "8,469,337,848") (td) (td "252.103 M/sec") (td "28.57%"))
      (tr (th "Mispredicted branches") (td "290,400,768") (td) (td "3.43% of all branches") (td "28.61%"))
      (tr (th "Upward prefetches") (td "355,720,357") (td) (td "10.589 M/sec") (td "28.59%"))
      (tr (th "Downward prefetches") (td "12,126") (td) (td "0.361 K/sec") (td "28.57%"))
      (tr (th "Elapsed time") (td "33.716") (td "seconds") (td) (td))))))
  (p "Both algorithms benefit from the increased cache, but Shell sort more so. This is also the first time we've seen more than 1 instruction per cycle in this post. Not that it's somehow an great result: my Haswell box mentioned earlier executes 2.4 instructions per cycle in that same test, for whoever is wondering, but that's a processor released 10 years later than this Pentium M.")
  (p "The total number of upward prefetches, when expressed in "
     (em "bytes")
     " — Pentium M has now-standard 64-byte cache lines — is almost exactly the same as on Tualatin, confirming that the upward prefetch counter indeed works there."))

(defsection conclusion
  :header "The conclusion"
  (p "This test shows that the legends are true. Coppermine doesn't have automatic hardware prefetching, and thus presumably Katmai and older P6 processors don't either. It was introduced in a limited, but functional, form in Tualatin: hardware prefetching only works for ascending addresses, and is limited by slow memory interface, but can handle variable strides, and even the undocumented performance events appear to function correctly."))
