;; Copyright © 2020  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "The legend of \"x86 CPUs decode instructions into RISC form interally\""
  :description "A dive into what x86 instruction are decoded into various processors, to determine the truth of the idea that they're just RISC-like internally."
  :children
  (setting-the-stage
   p6-long-long-ago
   pentium-m-micro-fusion
   core-improved-fusion
   sandy-bridge-branch-fusion
   what-about-others
   final-verdict)
  :date (2020 06 30)
  :topics ("microarchitectural-musings" "mythbusting")
  (p "There is a widespread idea that modern high-performance x86 processors work by decoding the \"complex\" x86 instructions into \"simple\" RISC-like instructions that the rest of the pipeline then operates on. But how close is this idea to how the processors "
     (em "actually")
     " work internally?")
  (p "To answer this question, let's analyze how different x86 processors, ranging from the first \"modern\" Intel microarchitecture, P6, to their current designs, handle the following simple loop (the code is 32-bit just to allow us to discuss very old x86 processors):")
  ((pre :class "codeblock" :data-code-language "x86 assembly")
   ".loop:
    add [edx], eax
    add edx, 4
    sub eax, 1
    jnz .loop
"))

(defsection setting-the-stage
  :header "RISC-V: setting the stage"
  (p "First things first, though: what would that code look like on a RISC architecture? We need this to establish a target to compare to. There are many different RISC architectures in the wild, so let's just arbitrarily choose "
     ((a :href "https://riscv.org/") "RISC-V")
     ", because it's free and open:")
  ((pre :class "codeblock" :data-code-language "RISC-V assembly")
   (code ".loop:
    lw a2, 0(a0)
    add a2, a2, a1
    sw a2, 0(a0)
    addi a1, a1, -1
    addi a0, a0, 4
    bnez a1, .loop
"))
  (p "This is pretty much what we'd expect on any architecture that closely follows RISC principles: the \"complex\" operation of adding a register to a memory location is split into three instructions, namely a memory load to a temporary register, then integer addition that operates entirely on registers, and finally a memory store. The other three x86 instructions: addition of a constant to a register, subtraction of a constant, and a conditional branch are already simple enough and are therefore almost identical in RISC-V.")
  (aside (p "There's a difference in the branch between RISC-V and x86 here: x86 provides conditional branches based on flag bits set by arithmetic operations, like "
            (code "sub")
            " in our example, whereas RISC-V's branches work by directly comparing the values of two registers, here "
            (code "a1")
            " and the always-zero "
            (code "x0")
            ". This difference doesn't matter much in this case, because we're comparing the result of an arithmetic operation against zero, so the difference comes out to needing to tell RISC-V what register to compare to zero, while on x86 what's tested against zero is implied to be the result of the preceding arithmetic operation.")))

(defsection p6-long-long-ago
  :header "P6: long, long ago"
  (p "Introduced in 1995 with the Pentium Pro, P6 was the first out-of-order x86 microarchitecture. It was later used with minor changes in the Pentium II and III, and is a direct ancestor of Intel's modern high-performance microarchitectures. How does it handle our loop?")
  (p "The first instruction, "
     (code "add [edx], eax")
     ", is decoded into the following "
     (strong "four")
     " micro-operations:")
  (ol (li "Load a 32-bit value from the address contained in "
          (code "edx")
          " into a temporary unnamed register.")
      (li "Add the value in "
          (code "eax")
          " to the value loaded by the first micro-operation, affecting flags.")
      (li "Send the "
          (em "result of the addition")
          " to the memory store unit. Note that the "
          (em "address")
          " of the store is not used here!")
      (li "Send the "
          (em "address")
          " contained in "
          (code "edx")
          " to the memory store unit."))
  (p "The following three instructions are all simple, decoding directly into one micro-operation each.")
  (p "We end up with "
     (em "seven")
     " micro-operations total. That's actually one more than our RISC-V code has instructions, because the memory store is split into two! The reason behind splitting the memory store into micro-operations is a design quirk: each micro-operation in P6 can have up to two inputs. The x86 architecture supports addressing modes of the form "
     (code "register + register * scale + constant")
     ", which have two register inputs. The data to store to memory is another input, bringing us to three… which is one more than we can encode. Therefore, stores are split into a \"store data\" micro-operation with one input, and \"store address\" with two.")
  (p "Verdict: the legend is absolutely true for P6, the micro-operations are very RISC-like, arguably more than an actual RISC architecture due to an implementation quirk."))

(defsection pentium-m-micro-fusion
  :header "Pentium M: introduction of micro-fusion"
  (p "The successor of P6 was Pentium M, where the \"M\" likely stood for \"mobile\". It was used primarily in laptops, where the concurrent NetBurst wasn't feasible due to its excessive power consumption and thermal requirements. There were a few Pentium M-based server processors, and there were socket adapters that let one use these processors in standard desktop socket 478 motherboards, but weren't very common.")
  (p "Pentium M introduced "
     (dfn "micro-operation fusion")
     ", or micro-fusion for short, where some pairs of micro-operations decoded from the same instructions could be joined together. These fused pairs were kept together in as much of the pipeline as possible: they were generated as as one by the instruction decoders, they were treated as one micro-operation by the register renaming mechanism, they were using a single entry in the reorder buffer, and they were treated as one in retirement stations. Just about the only place where they weren't treated as one micro-operation was in the execution units themselves, as for example the memory load unit wouldn't know what to do with integer addition, so it never received that portion of the micro-fused pair. Therefore, arguably, micro-fused pairs "
     (em "were single micro-operations")
     " for all intents and purposes, splitting was just an implementation detail of the execution units.")
  (p "The only instruction that was decoded into multiple micro-operations in P6 was the addition to a memory location. Can it benefit from micro-fusion in Pentium M? Indeed, it can, it's now decoded into three micro-operations that pretty much "
     (em "exactly")
     " match how our RISC-V code does the same operation:")
  (ol (li "Load a 32-bit value from the address contained in "
          (code "edx")
          " into a temporary unnamed register.")
      (li "Add the value in "
          (code "eax")
          " to the value loaded by the first micro-operation, affecting flags.")
      (li "Send the "
          (em "result of the addition "
              (strong "and")
              " the address")
          " to the memory store unit."))
  (p "The addition, the subtraction and the conditional branch all remain single micro-operations in Pentium M.")
  (p "Verdict: we have a "
     (em "perfect match")
     " between our RISC-V code and Pentium M micro-operations in this particular case."))

(defsection core-improved-fusion
  :header "Core: improved micro-fusion"
  (p "The immensely successful Core architecture was first released in 2006, when Intel finally realized that NetBurst was a dead-end, and took their mobile-focused Pentium M, which was derived from P6, and improved it further, giving us the legendary Core 2 Solo/Duo/Quad processors.")
  (p "The improvements over Pentium M were many, such as AMD64 support, an additional instruction decoder and proper 128-bit vector execution units, but the one we're interested is improved micro-fusion.")
  (p "In Pentium M, there were two cases where micro-fusion applied: the two parts of a store could be fused together, and memory loads could be fused together with common arithmetic operations in instructions like "
     (code "add eax, [edx]")
     ". Unfortunately, when the memory location was the "
     (em "destination")
     " operand, Pentium M could only fuse the two parts of a store. Core, however, lifted this restriction, allowing it do perform both kinds of micro-fusion at once. Thus on Core the first instruction of our loop is decoded into just "
     (strong "two")
     " micro-operations:")
  (ol (li "Load a 32-bit value from the address contained in "
          (code "edx")
          " into a temporary register, "
          (em (strong "and"))
          " add the value in "
          (code "eax")
          " to it, affecting flags.")
      (li "Send the "
          (em "result of the addition "
              (strong "and")
              " the address in "
              (code "edx"))
          " to the memory store unit."))
  (p "As before, the remaining three instructions were just decoded into one micro-operation each.")
  (p "Verdict: things are getting murky here: \"add the value from a memory location to a register\" is not really a very RISC-like operation."))

(defsection sandy-bridge-branch-fusion
  :header "Sandy Bridge: (improved) branch fusion"
  (p "2011's Sandy Bridge was the first P6-derived design based on a physical register file, finally solving the problem of permanent register read stalls that plagued P6 and its descendants up to this point. In many other ways, it was a significant yet merely evolutionary improvement over the preceding Core and Nehalem microarchitectures.")
  (p "What's important to us here though is the improvements to branch fusion that Sandy Bridge introduced over its predecessors.")
  (p (dfn "Branch fusion")
     ", often called using the general term "
     (dfn "macro-fusion")
     " — but in all currently existing x86 processors it's restricted to branches, so I'll stick with the more precise term — is the act of fusing together a branch and a preceding arithmetic instruction, typically a comparison. In x86 it was actually first introduced in Core, but it was restricted to fusing "
     (code "cmp")
     " and "
     (code "test")
     " instructions with the immediately following conditional branch in certain cases. What we have is a regular subtraction, so Core wasn't able to fuse it.")
  (p "Sandy Bridge, however, recognizes more patterns as eligible for branch fusion. Our pattern, a subtraction of an immediate from a register followed by a jump if the result was not zero, "
     (em "is among these new patterns")
     ". On Sandy Bridge and newer processors our entire loop thus decodes to "
     (strong "four")
     " micro-operations:")
  (ol (li "Load a 32-bit value from the address contained in "
          (code "edx")
          " into a temporary register, and add the value in "
          (code "eax")
          " to it, affecting flags.")
      (li "Send the result of the addition and the address in "
          (code "edx")
          " to the memory store unit.")
      (li "Add 4 to "
          (code "edx")
          ", affecting flags.")
      (li "Subtract 1 from "
          (code "eax")
          ", affecting flags, jumping back to the start of the loop if the result is not zero."))
  (aside (p "In fact, on Sandy Bridge and newer "
            (code "sub")
            " with register destination can be fused with a following branch even if the second operand is a memory location, meaning the sequence "
            (code "sub eax, [rdx]")
            " followed by "
            (code "jz .foo")
            " can be fused into a single micro-operation!"))
  (p "Verdict: on Sandy Bridge, and newer high-performance Intel processors, the correspondence between micro-operations and RISC instructions is completely lost. In fact, we have as many micro-operations as the \"complex\" x86 instructions we started with."))

(defsection what-about-others
  :header "What about AMD processors and Intel Atoms? Or NetBurst (shudders)?"
  (p "So far, this article focused entirely on P6-derived microarchitectures, but there are several other out-of-order x86 microarchitecture families enjoying, or having enjoyed, significant use: NetBurst (which was atrocious), the Intel Atom family, AMD K7/K8/K10, AMD Bobcat/Jaguar/Puma, AMD's Bulldozer family (which was as bad as NetBurst, but in different ways), and last but definitely not least AMD Zen (which I consider amazing).")
  (p "Let's focus on everything on that list but NetBurst first. The reason for this grouping is simple: "
     "none of these microarchitectures ever split instructions like "
     (code "add [edx], eax")
     " into multiple micro-operations. And none of them are capable of branch-fusing a branch with a preceding subtraction — the Bulldozer and Zen families can perform branch fusion, but only "
     (code "cmp")
     " and "
     (code "test")
     " instructions are eligible. We feed all of these processor our loop of four instructions, they're going to decode it into four micro-operations, each corresponding directly to the original instruction.")
  (p "As for NetBurst, it behaved very much like P6 in this regard, with the exception that stores which used addressing modes with only one input register were kept as one micro-operation, only complex addressing modes required splitting into store-data and store-address; our example loop was thus decoded to six micro-operations corresponding directly to the RISC-V code."))

(defsection final-verdict
  :header "Final verdict"
  (p "There is some truth to the story that x86 processors decode instructions into RISC-like form internally. This was, in fact, pretty much how P6 worked, later improvements however made the correspondence tortuous at best. Some microarchitecture families, on the other hand, never did anything of the sort, meaning it was never anywhere near a true statement for them."))
