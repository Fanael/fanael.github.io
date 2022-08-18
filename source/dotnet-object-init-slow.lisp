;; Copyright © 2022  Fanael Linithien
;; SPDX-License-Identifier: AGPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title ".NET object initialization is surprisingly slow"
  :description "A dive into .NET's surprisingly slow initialization of objects with reference fields compared to OpenJDK"
  :date (2022 08 18)
  :topics ("java" "dot-net" "garbage-collection")
  :children (setup openjdk-generated-code dot-net-generated-code)
  (p "…sometimes, at least. When compared to Java under OpenJDK.")
  (p "I was performing some experiments with various ways of implementing "
     ((a :href "https://en.wikipedia.org/wiki/Persistent_data_structure") "persistent")
     " random-access sequences. At one point, after implementing some of them in Java, I had the brilliant idea to try and implement some of them in C# as well and see how performance compares.")
  (p "Unfortunately, this comparison was not very favorable towards C#: it consistently performed around 4-6× slower, "
     (em "four to six times")
     ", than the "
     (em "same")
     " data structure in Java. That was certainly… "
     (em "not")
     " what I expected. I was expecting the performance to be very close, if not C#-favored. After all, the Common Language Runtime supports value types, whereas the Java Virtual Machine at the moment does not, and some of the data structures I tested can benefit from reduced indirection.")
  (p "Profiling revealed that the hottest functions were those that created new objects. While this does make sense, as persistent data structures using path copying "
     (em "do")
     " create many objects, spending north of 80% wall clock time on "
     (em "just")
     " allocating new objects, instead of on actual logic, is certainly a lot.")
  (p "The next step I took to understand why those functions take so much time was, naturally, looking at the generated code. The difference were quite stark, and explained the performance gap almost immediately, so why don't we take a look?"))

(defsection setup
  :header "The setup"
  (p "But first, let's come up with a minimal yet representative micro­benchmark that successfully shows the speed difference between the two platforms. I'll show the Java version, the C# translation is trivial and therefore left as an exercise for the reader.")
  ((highlighted-code :language :java)
   "public final class Main {
    private Main() {
    }

    public static void main(final String[] args) {
        measure(25_000); // warm-up
        measure(25_000_000);
    }

    private static void measure(final int repeats) {
        final var startTime = System.nanoTime();
        for (int i = 0; i < repeats; i += 1) {
            build(8, i);
        }
        final var endTime = System.nanoTime();
        System.out.format(\"Took %.3f s\\n\", (endTime - startTime) / 1.0e9);
    }

    private static Foo build(final int level, final int value) {
        if (level < 0) {
            return null;
        }
        final var child = build(level - 1, value);
        return new Foo(value, child, child, child, child);
    }

    private record Foo(int value, Foo one, Foo two, Foo three, Foo four) {
    }
}")
  (p "This code creates a nondescript quaternary tree of height 8 with significant node sharing, immediately throws it away, then repeats that process a bunch of times. Almost no logic at all, just memory allocations.")
  (info-box
   (p "This is "
      (em "not")
      " a good way to do micro­benchmarking on platforms with optimizing compilers: for example, one problem is that the compiler could realize that the call to "
      (code "build")
      " does nothing apart from allocating a bunch of useless objects, and therefore could be completely eliminated.")
   (p "I was careful enough to disable inlining of "
      (code "build")
      " using "
      (code "-XX:CompileCommand")
      ", which is enough to inhibit allocation elision "
      (em "in this case, on the current version of OpenJDK")
      ", then "
      (em "verified")
      " the generated code to see if the compiler didn't do anything destructive anyway. If you're not a trained professional who likes living dangerously, you should prefer libraries like the "
      ((a :href "https://github.com/openjdk/jmh") "Java micro­benchmark harness")
      " instead, which take care of those nasty optimizing compilers. And handle just-in-time compiler warm-up better. And more."))
  (p "Running it on my Zen 2 desktop, the Java code running under OpenJDK 18.0.2 (under Linux) executes in "
     (b "897 ms")
     ", 95% confidence interval [888.5 ms, 905.9 ms], while the equivalent C# code under .NET 6.0.8 (under Windows) takes "
     (b "2942 ms")
     ", 95% CI [2575 ms, 3308.4 ms], or "
     (b "over three times as much on average")
     ", with large standard deviation, which close to my prior observations. Same version of .NET under Linux is even slower, likely because "
     ((a :href "https://github.com/dotnet/runtime/issues/67776") " it does not have assembly intrinsics for some operations on non-Windows platforms")
     " yet.")
  (p "Enough waffling about, let's take a look at the disassembly, shall we?"))

(defsection openjdk-generated-code
  :header "OpenJDK generated code"
  ((highlighted-code :language :nasm)
   "build:
    mov [rsp - 0x14000], eax ; probe for stack overflow
    push rbp
    sub rsp, 32
    mov ebp, edx ; save the integer value for later
    test esi, esi ; reached base case?
    jl .return_null
    dec esi
    db 0x66, 0x66 ; align the address in the call…
    nop ; …for possible patching
    call build
    mov [rsp], rax ; save the child in case we need to call into runtime
    mov rax, [r15 + 0xF0] ; load thread allocation pointer
    mov r10, rax
    add r10, 32 ; bump the pointer by object size
    cmp r10, [r15 + 0x100] ; compare it against thread allocation limit
    jae .allocate_through_runtime
    mov [r15 + 0xF0], r10 ; store updated allocation pointer
    prefetchnta [r10 + 0x100] ; prefetch for future allocations
    mov qword [rax], 1 ; store mark word of the newly allocated object
    mov dword [rax + 8], 0xC031F0 ; store the Main$Foo class word
.fill_object:
    mov [rax + 12], ebp ; store the integer value field
    mov r10, [rsp] ; reload the saved child reference
    mov r11, r10 ; lol C2, redundant move
    mov [rax + 16], r11d ; store field one…
    mov [rax + 20], r11d ; …two…
    mov [rax + 24], r11d ; …three…
    mov [rax + 28], r11d ; …and finally four
.epilogue:
    add rsp, 32
    pop rbp
.stack_watermark_check:
    cmp rsp, [r15 + 0x338] ; poll stack watermark barrier
    ja .stack_watermark_safepoint
    ret

.return_null:
    xor eax, eax
    jmp .epilogue

.allocate_through_runtime:
    mov rsi, 0x800C031F0 ; Main$Foo class metadata
    call _new_instance_Java
    jmp .fill_object

.stack_watermark_safepoint:
    ; stack pointer above high water mark, need to enter safepoint
    mov r10, .stack_watermark_check
    mov [r15 + 0x350], r10
    jmp SafepointBlob")
  (p "What we see here is pretty sensible. Sure, there's a couple of minor imperfections here and there, but overall C2, the HotSpot virtual machine optimizing compiler, did a good job. The code is laid out in such a way that "
     ((a :href "https://shipilev.net/jvm/anatomy-quarks/28-frequency-based-code-layout/") "the common case has no taken branches")
     ". Allocation is dealt with by "
     ((a :href "https://shipilev.net/jvm/anatomy-quarks/4-tlab-allocation/") "bumping a pointer")
     ". The address of the thread-local context is kept in "
     (code "r15")
     ", so there's no need to load it from anywhere.")
  (p "Note that object references occupy 4 bytes of space in our object, not 8 like native pointers: on 64-bit platforms, HotSpot can, under certain conditions, "
     ((a :href "https://shipilev.net/jvm/anatomy-quarks/23-compressed-references/") "compress the references")
     ". The speed penalty caused by decoding compressed references ranges from none to minuscule on common platforms, so this saves memory and improves cache utilization at very little cost.")
  (p "Most importantly, though, object initialization is as simple as it gets: after filling the object header, the fields are initialized with simple memory stores. There are no garbage collector write barriers of any kind here: the compiler realized it's storing fields of a freshly-allocated object, which must be in the young generation by definition, so barriers can be safely elided.")
  (p "To show that barrier elision is an optimization, we can tell the runtime to not inline the constructor of our record and look at its disassembly, here with the parallel garbage collector, because its "
     ((a :href "https://shipilev.net/jvm/anatomy-quarks/13-intergenerational-barriers/") "inter­generational write barrier")
     " is relatively simple:")
  ((highlighted-code :language :nasm)
   "constructor_disabled_inlining:
    sub rsp, 24
    mov [rsp + 16], rbp ; probably should've been a push
    mov [rsi + 12], edx ; store the integer field
    mov r10, rsi ; copy the address for card table marking
    mov r11, rcx
    mov [rsi + 16], r11d ; store field one…
    shr r10, 9 ; compute card table entry index
    mov [rsi + 20], r8d ; …two…
    mov r8, r9
    mov [rsi + 24], r8d ; …three…
    mov r8, rdi
    mov [rsi + 28], r8d ; …and finally four
    mov r11, 0x7F1ACAE2F000 ; card table base address
    mov [r11 + r10], r12b ; mark card table entry (r12 = 0)
    add rsp, 16
    pop rbp ; see, told you saving rbp should've been a push
    cmp rsp, [r15 + 0x338] ; poll stack watermark barrier
    ja .stack_watermark_safepoint
    ret")
  (p "We can clearly see a card-marking write barrier at the end. A more naïve compiler would emit a barrier after every write of a reference field, and the baseline compiler C1 does exactly that, but C2 was able to coalesce those barriers into one."))

(defsection dot-net-generated-code
  :header ".NET generated code"
  ((highlighted-code :language :nasm)
   "Build:
    push rdi
    push rsi
    push rbx
    sub rsp, 32
    mov esi, edx ; save the integer value for later
    test ecx, ecx ; reached base case?
    jge .recursive_call
    xor eax, eax
    add rsp, 32
    pop rbx
    pop rsi
    pop rdi
    ret
.recursive_call:
    dec ecx
    mov edx, esi
    call stub_for_Build ; which is just \"jmp Build\"
    mov rdi, rax ; save the child in callee-saved register
    mov rcx, 0x7FFF74422F40 ; Main.Foo class metadata
    call CORINFO_HELP_NEWSFAST
    mov rbx, rax ; save the new object in callee-saved register
    mov [rbx + 40], esi ; store the integer value field
    lea rcx, [rbx + 8]
    mov rdx, rdi
    call CORINFO_HELP_ASSIGN_REF ; store field one…
    lea rcx, [rbx + 16]
    mov rdx, rdi
    call CORINFO_HELP_ASSIGN_REF ; …two…
    lea rcx, [rbx + 24]
    mov rdx, rdi
    call CORINFO_HELP_ASSIGN_REF ; …three…
    lea rcx, [rbx + 32]
    mov rdx, rdi
    call CORINFO_HELP_ASSIGN_REF ; …and finally four
    mov rax, rbx
    add rsp, 32
    pop rbx
    pop rsi
    pop rdi
    ret")
  (p "Uh-oh. Are those function calls for allocation and reference field assignment? Presumably the reference field assignment function also performs write barrier shenanigans, seeing as the integer field is stored directly? Let's see what those functions look like, then, starting with allocation.")
  ((highlighted-code :language :nasm)
   "CORINFO_HELP_NEWSFAST:
    mov edx, [rcx + 4] ; load object size
    mov r11, [rel _tls_index] ; load the thread-local storage index
    mov rax, [gs:0x58] ; load TLS base from thread environment block
    mov rax, [rax + r11 * 8] ; load our TLS entry
    mov r11d, CurrentThreadInfo - tls_start
    mov r11, [rax + r11] ; load the thread-local context pointer
    mov r10, [r11 + 0x60] ; load thread allocation limit
    mov rax, [r11 + 0x58] ; load thread allocation pointer
    add rdx, rax
    cmp rdx, r10
    ja .allocate_through_runtime
    mov [r11 + 0x58], rdx ; store updated allocation pointer
    mov [rax], rcx ; store class metadata
    ret
.allocate_through_runtime:
    jmp JIT_NEW")
  (p "Allocation turns out to be just pointer bumping after all. Unfortunately, to get to the thread-local context, we need to perform four memory loads first, two of which depend on previous loads; whereas OpenJDK just keeps it in a register. That, and the fact it's a function call, certainly introduces some additional cost, but not enough to explain the observed difference.")
  (p "What about that reference field assignment then?")
  ((highlighted-code :language :nasm)
   "CORINFO_HELP_ASSIGN_REF:
    mov [rcx], rdx ; perform the assignment
    nop dword [rax] ; align the address in mov for patching
    mov rax, 0x1B9BA871018 ; start of young generation?
    cmp rdx, rax ; in bounds?
    jb .exit
    nop ; align the address in mov for patching
    mov rax, 0x1B9A34FAF60 ; level 0 card table address
    shr rcx, 11
    cmp byte [rcx + rax], 0xFF ; already marked?
    jne .update_card_table_0
    rep ret
.update_card_table_0:
    mov byte [rcx + rax], 0xFF ; mark the card table entry
    shr rcx, 10
    xchg ax, ax ; align the address in mov for patching
    mov rax, 0x1B9DA81326C ; level 1 card table address
    cmp byte [rcx + rax], 0xFF ; already marked?
    jne .update_card_table_1
    rep ret
.update_card_table_1:
    mov byte [rcx + rax], 0xFF ; mark the card table entry
    ret
    nop dword [rax]
.exit:
    rep ret")
  (p "There it is! This, honestly, explains "
     (em "everything")
     ". Note the early out condition: it skips card marking work entirely only if the "
     (em "value we're assigning")
     " points into the "
     (em "old")
     " generation. Even if the referrer and the referent are both young, we still end up touching the card tables.")
  (p "Note that card table updates are conditional: each entry is written to only if it's not already marked. This is done to avoid some "
     ((a :href "https://en.wikipedia.org/wiki/False_sharing") "false sharing")
     " with multi-threaded mutators on multi­processor systems. It does introduce some branching, but in our case at least, those branches tend to be very predictable.")
  (p "In the end, all of this work ends up hurting performance, making .NET slower than OpenJDK in this case. Yes, processors try to make any code fast anyway, but out-of-order execution is not magic. Processors can try to reorder instructions as much as they can to fill their execution units, but dependency chains restrict that, they still have to run every instruction, and inefficiencies can quickly add up.")
  (p "Improvements to the just-in-time compiler, such as emitting certain crucial intrinsic operations like allocations and write barriers inline, and introducing the ability to coalesce and/or elide barriers, would certainly be welcome."))
