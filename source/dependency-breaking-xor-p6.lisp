;; Copyright © 2021  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Dependency-breaking zeroing XOR in P6"
  :description "An attempt to discover if the zeroing XOR idiom is dependency-breaking in any P6 and Pentium M models"
  :date (2021 04 08)
  :topics ("microarchitecture-archeology" "mythbusting")
  :children (test results conclusion)
  (p "In x86 assembly language, a common idiom for setting the value of a register to 0 is to use the exclusive-or instruction with both operands being the same register, such as "
     (code "xor eax, eax")
     ". It was originally intended as a size optimization: the obvious "
     (code "mov eax, 0")
     " is encoded as "
     (em "five")
     " bytes, of which four are used to store the constant 0, while the exclusive-or solution needs merely "
     (em "two")
     ", and is equally as fast, so it quickly became widespread.")
  (p "By the time the P6 microarchitecture was being designed, the "
     (code "xor")
     " zeroing idiom was already nigh-universal in compiler output and hand-written assembly alike, so it was specifically recognized as a zeroing idiom for the purpose of avoiding partial register stalls in code such as this:")
  ((highlighted-code :language :nasm)
   "    xor eax, eax
    mov al, [ecx]
    ; use eax
")
  (p "In code tuned for the original Pentium or earlier processors, this was the usual way of zero-extending an 8-bit (or 16-bit with "
     (code "ax")
     " instead of "
     (code "al")
     ") value into the full 32-bit register, as the "
     (code "movzx")
     " instruction was slower. P6, starting from the very first Pentium Pro, recognized that after a "
     (code "xor")
     " of a register with itself, the register held 0, which avoided the partial register stall that would otherwise occur when modifying a low part of a register followed by operations on the full 32 bits.")
  (p "Unfortunately, the Pentium Pro as originally designed was too ambitious to be realized using then-available lithography technology without making the chip too big — and thus too prone to manufacturing defects — so some features had to go. Segment register renaming and beefier secondary decoders were some of the notable victims of that process.")
  (p "I assume that the ability to recognize that the exclusive-or zeroing idiom doesn't "
     (em "really")
     " depend on the previous value of a register, so that it can be dispatched immediately without waiting for the old value — thus breaking the dependency chain — met the same fate; the Pentium Pro shipped without it.")
  (p "Some of the cut features were introduced in later models: segment register renaming, for example, was added back in the Pentium II. Maybe dependency-breaking zeroing XOR was added in later P6 models too? After all, it seems such a simple yet important thing, and indeed, I remember seeing people claim that's the case in some old forum posts and mailing list messages. On the other hand, some sources, such as "
     ((a :href "https://www.agner.org/optimize/") "Agner Fog's optimization manuals")
     " say that not only it was never present in any of the P6 processors, it was also missing in Pentium M.")
  (p "Whatever the case may be, there's only one way to make sure: test it!"))

(defsection test
  :header "The test"
  (p "To test for dependency-breaking behavior, we simply need to create a long dependency chain, interspersed by the tested instructions. I somewhat arbitrarily chose multiplication as the instruction to form the chain with, because its significant latency, yet single-cycle reciprocal throughput makes it trivial to see if we're bound by throughput (in which case the dependency chain is broken) or by latency (in which case it's not). Therefore, the tested loop will look like this, using the "
     ((a :href "https://www.nasm.us/") "Netwide Assembler")
     " syntax:")
  ((highlighted-code :language :nasm)
   ".loop:
    %rep 10
    xor eax, eax
    imul eax, eax
    %endrep
    dec ecx
    jnz .loop")
  (p "MMX and SSE instruction sets were tested the same way, using "
     (code "pxor")
     " and "
     (code "xorps")
     " respectively instead of "
     (code "xor")
     ", and "
     (code "pmullw")
     " and "
     (code "mulss")
     " respectively as the multiplication.")
  (p "The initial value of "
     (code "ecx")
     " doesn't matter much, it only needs to be big enough to make process initialization costs and noise insignificant. I chose one million."))

(defsection results
  :header "The results"
  :children (tualatin dothan yonah core))

(defsection tualatin
  :header "P6: Tualatin"
  (p "The first machine I tested was my trusty old ThinkPad, with a Tualatin Pentium III. It was the third and final release of Pentium III and P6 proper, so surely if P6 recognized zeroing XOR as dependency-breaking at any point, it would do it here. After all, it was released "
     (em "after")
     " the first NetBurst processors, where same-register exclusive-or "
     (em "was")
     " breaking dependency chains. When we run the test code under "
     ((cite :class "program-name") "perf")
     ", we get the following cycle counts:")
  (ul
   (li (b (code "mov eax, ecx")) " (control): 14.5 million cycles")
   (li (b (code "xor eax, eax")) ": 50.3 million cycles")
   (li (b (code "pxor mm0, mm0")) ": 40.5 million cycles")
   (li (b (code "xorps xmm0, xmm0")) ": 51.2 million cycles"))
  (p "Zeroing exclusive-or clearly does "
     (em "not")
     " break dependency chains at all. If it did, the cycle counters would be more similar to the control. We're limited by the latency of multiplication: 3 cycles for 16-bit SIMD integer, 4 cycles for 32-bit scalar integer and floating point, plus one cycle for the exclusive-or itself."))

(defsection dothan
  :header "Pentium M: Dothan"
  (p "Since it appears that dependency-breaking zeroing XOR is not present in the last revision of P6 proper, time to go for a newer target: Pentium M. I tested Dothan, the second revision of Pentium M, built using 90 nm process. The results appear as follows:")
  (ul
   (li (b (code "mov eax, ecx")) " (control): 12.0 million cycles")
   (li (b (code "xor eax, eax")) ": 50.2 million cycles")
   (li (b (code "pxor mm0, mm0")) ": 40.3 million cycles")
   (li (b (code "xorps xmm0, xmm0")) ": 51.2 million cycles"))
  (p "Exact same story here: none of the zeroing exclusive-or operations break dependency chains."))

(defsection yonah
  :header "Enhanced Pentium M: Yonah"
  (p "If not Pentium M, then maybe the short-lived Enhanced Pentium M? It did feature some notable improvements to the microarchitecture, after all, so let's try it too:")
  (ul
   (li (b (code "mov eax, ecx")) " (control): 12.0 million cycles")
   (li (b (code "xor eax, eax")) ": 12.7 million cycles")
   (li (b (code "pxor mm0, mm0")) ": 40.3 million cycles")
   (li (b (code "xorps xmm0, xmm0")) ": 18.6 million cycles"))
  (p "Oh. Huh. It appears that zeroing XOR "
     (em "is")
     " recognized as dependency-breaking, at least with regards to general purpose and SSE registers. The zeroing operation still appears to consume an execution unit, which explains why the SSE version is slower: since the vector execution units are 64-bit, the "
     (code "xorps")
     " instruction is split into two micro-operations, each going to a different execution unit, of which there are just "
     (em "two")
     ".")
  (p "Zeroing exclusive-or on MMX registers is presumably not recognized due to the fact that MMX registers alias the x87 register stack, complicating the implementation a bit."))

(defsection core
  :header "Core, just for comparison"
  (p "In Core, a microarchitecture where almost every aspect was greatly improved compared to Pentium M and P6, zeroing XOR idioms are recognized directly by the register renaming mechanism and do not consume execution units. As such, its results appear quite… uniform:")
  (ul
   (li (b (code "mov eax, ecx")) " (control): 10.3 million cycles")
   (li (b (code "xor eax, eax")) ": 10.3 million cycles")
   (li (b (code "pxor mm0, mm0")) ": 10.3 million cycles")
   (li (b (code "xorps xmm0, xmm0")) ": 10.3 million cycles")))

(defsection conclusion
  :header "The conclusion"
  (p "Agner Fog is correct here, as I expected: neither P6 nor the first two revisions of Pentium M recognize same-register exclusive-or as dependency breaking, even though they do recognize it as preventing certain kinds of partial register stalls. The first P6 descendant to feature this behavior is the "
     (em "third")
     " revision of Pentium M, the so-called Enhanced Pentium M, where same-register exclusive-or breaks the dependency chain on the given register for general purpose and SSE registers, but not MMX registers."))
