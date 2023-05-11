;; Copyright © 2023  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Measuring pg_stat_statements overhead"
  :description "Measuring the overhead of enabling pg_stat_statements on relatively weak modern-ish hardware"
  :date (2023 05 12)
  :topics ("postgresql")
  :children (the-target-machine the-benchmark)
  (p ((a :href "https://www.postgresql.org/docs/current/pgstatstatements.html") (code "pg_stat_statements"))
     " is a fantastically useful PostgreSQL extension that collects execution statistics of all statements executed by the server, to allow the database administrator to monitor and analyze any possible performance issue. For a good reason, it is one of the, if not "
     (em "the")
     " most installed extension, and even a cursory internet search will reveal numerous sources extolling its virtues.")
  (p "Of course, collecting those statistics has some small performance overhead, which is "
     ((a :href "https://dba.stackexchange.com/a/303555") "widely reported to be negligible")
     ". There's nothing wrong with checking the veracity of those claims ourselves, however: hardware and software differences can matter, after all, and the hardware I'm running this database on is not exactly usual."))

(defsection the-target-machine
  :header "The target machine"
  (p "The particular machine I'm most interested in measuring the overhead on is a repurposed older thin client, so by server standards, it's "
     (em "laughably puny")
     ". It features such cutting edge hardware as:")
  (ul (li "A dual-core " ((a :href "https://en.wikipedia.org/wiki/Jaguar_(microarchitecture)") "AMD Jaguar") " system-on-chip, clocked at 1.65 GHz."
          (sidenote (p "Fun fact: AMD Jaguar is the CPU microarchitecture powering two of the eighth generation video game consoles, its use outside of embedded spaces is otherwise rare.")))
      (li "4 GB of DDR3 RAM, at 1600 MT/s, "
          (em "single-channel")
          ". The SoC doesn't support more memory channels; at least it supports ECC.")
      (li "128 GB SATA SSD."))
  (p "Unlike typical server hardware, it is also completely silent, being 100% passively cooled, and draws very little power, even at full load.")
  (sidenote (p "Now that I think about it, when compared to some of the smaller cloud instances, this is honestly quite beefy, and there are no noisy neighbors to worry about, and the storage performance is much higher than what basic cloud storage offers, unless you explicitly provision enough throughput and I/O operations per second, which of course costs you extra… huh, I'm starting to think that the entire cloud thing may be a bit of a raw deal."))
  (p "Software-wise, there's nothing special: just a regular Debian bookworm with PostgreSQL 15.2 installed from Debian's repositories. The PostgreSQL settings are quite vanilla as well, I didn't do anything interesting in "
     (code "postgresql.conf")
     " apart from the usual stuff like changing "
     (code "shared_buffers")
     ", "
     (code "random_page_cost")
     " and some logging settings."))

(defsection the-benchmark
  :header "The benchmark"
  (p "What I'm most interested in is what is the worst case overhead I can "
     (em "realistically")
     " expect: assume all statements are simple and do very little, if any, I/O. For this reason, I used "
     ((a :href "https://www.postgresql.org/docs/current/pgbench.html") (code "pgbench"))
     " with scale factor of 40 and in select only mode. With scale factor this small, the database fits entirely not just in RAM, but also in PostgreSQL's shared buffers, while "
     (code "pgbench")
     "'s select only mode uses a simple, but still realistic "
     (code "SELECT")
     " statement. While one could argue that something like "
     (code "SELECT 1")
     " would be even simpler, it does not quite satisfy the criterion of being a realistic query a program would execute against the database.")
  (p "With that in mind, let's run "
     (code "pgbench")
     " with two client connections — one per thread — for 5 minutes, using prepared statements, then restart the server with "
     (code "pg_stat_statements")
     " enabled and do it again.")
  (figure
   (figcaption "Benchmark results, " (code "SELECT") " only")
   (figcontent
    (table
     (thead
      (tr (th (code "pg_stat_statements") " state") (th "Transactions per second") (th "Mean latency") (th "Standard deviation") (th "Total transactions")))
     (tbody
      (tr (th "off")                                (td "12235")                   (td "0.162 ms")     (td "0.051 ms")           (td "3,670,487"))
      (tr (th "on")                                 (td "11922")                   (td "0.167 ms")     (td "0.057 ms")           (td "3,576,490"))))))
  (p "We can see that enabling "
     (code "pg_stat_statements")
     " resulted in a 2.5% hit to transaction throughput and 3% hit to mean transaction latency. Well worth it, considering the utility of this extension, and in line with the expectations of being negligible.")
  (p "We could also try using "
     (code "pgbench")
     "'s default transaction type, to see what happens if we add inserts and updates, albeit simple ones still, into the mix. Those operations perform more work than pure selects, for example checking foreign key constraints, updating indexes and appending every data modification to the write-ahead log to ensure crash resilience, so the time spent on collecting statistics should be proportionally smaller.")
  (figure
   (figcaption "Benchmark results, mixed transaction")
   (figcontent
    (table
     (thead
      (tr (th (code "pg_stat_statements") " state") (th "Transactions per second") (th "Mean latency") (th "Standard deviation") (th "Total transactions")))
     (tbody
      (tr (th "off")                                (td "1402.8")                  (td "1.423 ms")     (td "0.415 ms")           (td "420,849"))
      (tr (th "on")                                 (td "1384.2")                  (td "1.443 ms")     (td "0.424 ms")           (td "415,244"))))))
  (p "Indeed, the overhead is even smaller now, with only 1.4% drop in throughput and mean latency."))
