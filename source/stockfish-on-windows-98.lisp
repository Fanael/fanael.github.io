;; Copyright © 2020  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defarticle
  :title "Current Stockfish on Windows 98?!"
  :description "Porting current versions of Stockfish to Windows 98: is it possible, and how much tinkering does it require?"
  :children
  (naive-attempt
   rebuilding-winpthreads
   deadlock-on-startup
   implementing-try-enter-critical-section
   dynamic-dispatch
   damn-it-stockfish
   it-finally-works)
  :date (2020 09 24)
  :topics ("retro" "windows-9x")
  (p "A few weeks ago I had this weird idea: would it be possible to make an up-to-date version of "
     ((a :href "https://stockfishchess.org/") "Stockfish")
     " run on Windows 98? There's an immediate problem with that: Stockfish is written in C++17, which is the still-current, at the time of writing, revision of the C++ language; surely there are no C++17 capable compilers that can still target something as old as Windows 98?")

  ((div :class "note")
   (p "Disclaimer: in this article, by \"Windows 98\" I mean Windows 98 second edition with the main files of the "
      ((a :href "https://www.techtalk.cc/viewtopic.php?t=65") "unofficial service pack")
      " installed, which is a convenient bundle of official and unofficial patches. I don't care about Windows 98 first edition, nor the second edition "
      (em "without")
      " those updates installed. Note that "
      ((a :href "https://sourceforge.net/projects/kernelex/") "KernelEx")
      " is "
      (em "not needed")
      ".")
   (p "I'm fully aware this has no practical value, but it was a fun thing to do."))

  (p "As it turns out, the GNU toolchain, comprised of "
     ((a :href "https://www.gnu.org/software/binutils/") "GNU binutils")
     " and "
     ((a :href "https://gcc.gnu.org/") "GCC, the GNU Compiler Collection")
     ", doesn't particularly care about all the different Windows versions and will happily output a binary that works on any 32-bit Windows.")

  (p "The compiler itself, assembler and linker are only a part of a functional compiler package, though. There's one part we're missing: the runtime library, which provides the system-specific program initialization code that runs before the "
     (code "main")
     " function. Since we're using GCC, the obvious choice is the "
     ((a :href "https://sourceforge.net/projects/mingw-w64/") "mingw-w64")
     " runtime. It also provides Windows API headers, bindings to (pretty much all the extant versions of) the Microsoft Visual C++ standard C library, and a POSIX threading API compatibility layer, "
     ((cite :class "program-name") "winpthreads")
     ", which is required for C++ standard threading library support in GCC.")

  (p "Fortunately for us, a minimal Windows program compiled with GCC with mingw-w64 appears to work fine when executed under Windows 98:")
  ((highlighted-code :language :c++)
   "#include <windows.h>

int WINAPI WinMain(HINSTANCE, HINSTANCE, char*, int)
{
    MessageBoxA(nullptr, \"Hello world!\", \"A test\", MB_OK);
    return 0;
}")
  (p "So maybe all we have to do is to compile Stockfish and it will just work on Windows 98…?"))

(defsection naive-attempt
  :header "Naïve attempt: don't do anything, just try it!"
  (p "Alas, as can be expected, it's not that easy. If we don't do anything apart from just compiling Stockfish as-is with a stock GCC + mingw-w64 toolchain, trying to run the resulting binary on Windows 98 will just give us errors about unresolved imports.")

  (p "There are three symbols the binary requires that are not present under Windows 98, all imported from "
     (code "kernel32.dll")
     ": "
     (code "AddVectoredExceptionHandler")
     ", "
     (code "RemoveVectoredExceptionHandler")
     " and "
     (code "GetTickCount64")
     ". Looking for these identifiers in Stockfish source code reveals nothing, so where are they coming from?")

  (p "Looking at the disassembly, all three of these functions are used inside the aforementioned "
     ((cite :class "program-name") "winpthreads")
     " library. Can we do anything about that?"))

(defsection rebuilding-winpthreads
  :header "Rebuilding winpthreads"
  (p "None of those three functions are used for anything critical. The vectored exception handling APIs, introduced in Windows XP, are used only to allow threads to be named under a debugger. "
     (code "GetTickCount64")
     ", added in Vista, is used for internal time tracking, but there exists a fallback path for older systems in the source code. The obvious solution would be to just patch them out and rebuild "
     ((cite :class "program-name") "winpthreads")
     ":")
  ((highlighted-code :language :diff)
   "diff --git a/mingw-w64-libraries/winpthreads/src/misc.c b/mingw-w64-libraries/winpthreads/src/misc.c
--- a/mingw-w64-libraries/winpthreads/src/misc.c
+++ b/mingw-w64-libraries/winpthreads/src/misc.c
@@ -55,7 +55,7 @@ unsigned long long _pthread_rel_time_in_ms(const struct timespec *ts)
 static unsigned long long
 _pthread_get_tick_count (long long *frequency)
 {
-#if defined (_WIN32_WINNT) && (_WIN32_WINNT >= _WIN32_WINNT_VISTA)
+#if 0 && defined (_WIN32_WINNT) && (_WIN32_WINNT >= _WIN32_WINNT_VISTA)
   (void) frequency; /* unused */
   return GetTickCount64 ();
 #else
diff --git a/mingw-w64-libraries/winpthreads/src/thread.c b/mingw-w64-libraries/winpthreads/src/thread.c
--- a/mingw-w64-libraries/winpthreads/src/thread.c
+++ b/mingw-w64-libraries/winpthreads/src/thread.c
@@ -57,7 +57,7 @@ static pthread_t idListNextId = 0;
 #if !defined(_MSC_VER)
 #define USE_VEH_FOR_MSC_SETTHREADNAME
 #endif
-#if !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
+#if 1 || !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
 /* forbidden RemoveVectoredExceptionHandler/AddVectoredExceptionHandler APIs */
 #undef USE_VEH_FOR_MSC_SETTHREADNAME
 #endif
@@ -109,7 +109,7 @@ SetThreadName (DWORD dwThreadID, LPCSTR szThreadName)
    /* Without a debugger we *must* have an exception handler,
     * otherwise raising an exception will crash the process.
     */
-#if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
+#if 0 && WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
    if ((!IsDebuggerPresent ()) && (SetThreadName_VEH_handle == NULL))
 #else
    if (!IsDebuggerPresent ())
")

  (p "After rebuilding "
     ((cite :class "program-name") "winpthreads")
     " and re-linking Stockfish, the resulting binary now launches under Windows 98!"))

(defsection deadlock-on-startup
  :header "Deadlock on startup"
  (p "Yes, the binary "
     (em "launches")
     ", but it doesn't "
     (em "work")
     ". It prints the version banner, but then hangs forever. Damn.")

  (p "There are actually two issues that cause the hang. One is relatively easy to fix: "
     ((cite :class "program-name") "winpthreads")
     " uses "
     (code "GetHandleInformation")
     " for testing if the Win32 object handles are valid. This function exists on Windows 98 for limited NT compatibility, but it doesn't actually do anything, always returning 0 and setting the last error code to "
     (code "ERROR_CALL_NOT_IMPLEMENTED")
     ". Since this is mostly a debugging feature, it can safely be removed with another trivial patch:")
  ((highlighted-code :language :diff)
   "diff --git a/mingw-w64-libraries/winpthreads/src/misc.h b/mingw-w64-libraries/winpthreads/src/misc.h
--- a/mingw-w64-libraries/winpthreads/src/misc.h
+++ b/mingw-w64-libraries/winpthreads/src/misc.h
@@ -62,7 +62,7 @@ typedef long long LONGBAG;
 typedef long LONGBAG;
 #endif

-#if !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
+#if 1 || !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
 #undef GetHandleInformation
 #define GetHandleInformation(h,f)  (1)
 #endif
")

  (p "The other issue is "
     (em "much")
     " more severe: there is another function like "
     (code "GetHandleInformation")
     " that \"exists\" on Windows 98, but is not actually implemented and always returns with an error. It's "
     (code "TryEnterCriticalSection")
     ", used several times in the implementation of condition variables. The code is written under the assumption that "
     (code "TryEnterCriticalSection")
     " actually works as described: if the critical section is not currently locked, it expects the call to take ownership of the critical section and succeed, but on Windows 98, this will never happen.")

  (aside (p "Critical sections are the Win32 name for regular user-mode "
            ((a :href "https://en.wikipedia.org/wiki/Lock_(computer_science)") "mutexes")
            ". Win32 also offers kernel-mode mutexes, which are heavy and slow, as they require system calls for every operation, so they're usually only used for inter-process communication."))

  (p "It's not possible to work around that problem by patching away "
     (code "TryEnterCriticalSection")
     " calls without a significant re-architecture of "
     ((cite :class "program-name") "winpthreads")
     "'s condition variables. Clearly, a different approach is needed this time."))

(defsection implementing-try-enter-critical-section
  :header "Implementing that one missing function ourselves"
  (p (code "TryEnterCriticalSection")
     " being present, but not actually implemented on Windows 98 is the only obstacle left, albeit a significant one. The algorithm itself is simple: find out where the atomic indicating whether the section is locked is, and use an operation like atomic compare-and-swap to change it, returning true if the section wasn't locked before. Unfortunately, the structure of critical sections is completely undocumented, and there aren't many good sources that are still alive on what critical sections looked like in that old operating system, so finding out where the atomic variable "
     (em "is")
     " requires some work.")

  (p "From what I can gather from old debug symbols and looking at disassembly, the "
     (code "CRITICAL_SECTION")
     " structure looks similar to this in Windows 98:")
  ((highlighted-code :language :c++)
   "struct critical_section {
    std::uint8_t type; // Always 4.
    struct critical_section_impl* impl;
    // Padding for NT compatibility, where the structure is completely
    // different.
    std::uint32_t reserved[4];
};
static_assert(sizeof(critical_section) == 24);")

  (p "That's not very useful, it's essentially just a pointer to something else. So what does "
     (code "critical_section_impl")
     " look like? That's where our atomic actually is, together with a recursion counter, current owner and some uninteresting to us stuff:")
  ((highlighted-code :language :c++)
   "struct critical_section_impl {
    std::uint8_t type;
    int recursion_count;
    void* owner_thread;
    std::uint32_t reserved;
    std::atomic<int> lock_count; // Starts at 1 when not owned, decreasing.
    void* internal_pointers[3];
};
static_assert(sizeof(critical_section_impl) == 32);")

  (p "There is another complication waiting for us here: the owner thread is represented as a pointer to its TDBX, which is the "
     (em "kernel-mode")
     " data structure representing a thread, internal to "
     (code "vwin32.vxd")
     ", the process management part of the Windows 9x kernel.")

  (p "Fortunately, the (undocumented) "
     ((a :href "https://en.wikipedia.org/wiki/Win32_Thread_Information_Block") "thread information block")
     " does contain a pointer to the thread's TDBX. Unfortunately, its location varies between system versions: it's at offset "
     (code "0x50")
     " from the start of the TIB in Windows 95 (according to Windows 95 System Programming Secrets) and 98, but at offset "
     (code "0x80")
     " in Windows Me (according to KernelEx sources). While I don't care about Windows 95 at all, Windows Me would be nice to support if only because it's the successor of Windows 98.")

  (p "With all that in mind, we can implement our version of "
     (code "TryEnterCriticalSection")
     " for Windows 98 and Me, using a C++ template to parameterize the code over the TDBX pointer offset:")
  ((highlighted-code :language :c++)
   "constexpr std::uint8_t critical_section_type = 4;

template <unsigned TdbxOffset>
void* get_current_tdbx()
{
    const char* tib;
    asm(\"mov {%%fs:0x18, %0|%0, fs:[0x18]}\" : \"=r\"(tib));
    return *reinterpret_cast<void* const*>(tib + TdbxOffset);
}

template <unsigned TdbxOffset>
bool try_enter_9x_impl(critical_section_impl* cs)
{
    const auto current_tdbx = get_current_tdbx<TdbxOffset>();
    int actual_lock_count = 1;
    if(cs->lock_count.compare_exchange_strong(actual_lock_count, 0)) {
        cs->owner_thread = current_tdbx;
        ++cs->recursion_count;
        return true;
    } else if(cs->owner_thread == current_tdbx) {
        cs->lock_count.fetch_sub(1, std::memory_order_relaxed);
        ++cs->recursion_count;
        return true;
    } else {
        return false;
    }
}

template <unsigned TdbxOffset>
[[gnu::stdcall]] int try_enter_9x(CRITICAL_SECTION* cs)
{
    const auto actual = reinterpret_cast<critical_section*>(cs);
    if(actual->type != critical_section_type) {
        [[unlikely]] RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, 0);
        return 0;
    }

    return try_enter_9x_impl<TdbxOffset>(actual->impl);
}"))

(defsection dynamic-dispatch
  :header "Dynamic dispatch"
  (p "We could implement "
     (code "TryEnterCriticalSection")
     " by just always calling "
     (code "try_enter_9x")
     ", but that would break compatibility with Windows NT. Having one binary that can run on anything from Windows 98 to Windows 10 is just cleaner in my opinion, so let's choose the implementation at runtime, based on the operating system version.")

  (p "To do that, we can use the "
     (code "GetVersion")
     " Windows API function, which has the nice guarantee of returning a value with the highest bit clear if and only if the operating system is some version of Windows NT, together with the usual "
     (code "LoadLibraryA")
     " and "
     (code "GetProcAddress")
     " calls to retrieve the address of the actual "
     (code "TryEnterCriticalSection")
     " implementation on Windows NT:")
  ((highlighted-code :language :c++)
   "constexpr unsigned tdbx_offset_98 = 0x50;
constexpr unsigned tdbx_offset_me = 0x80;

constexpr std::uint32_t windows_9x_mask = 0x80000000;
constexpr std::uint32_t windows_me_version = 0xC0005A04;

[[gnu::stdcall]] int try_enter_dispatch(CRITICAL_SECTION* cs);

using try_enter_type = decltype(&TryEnterCriticalSection);
std::atomic<try_enter_type> implementation = &try_enter_dispatch;

[[gnu::stdcall]] int try_enter_dispatch(CRITICAL_SECTION* cs)
{
    const auto system_version = GetVersion();
    if((system_version & windows_9x_mask) == 0) {
        const auto kernel32 = LoadLibraryA(\"kernel32.dll\");
        const auto impl = reinterpret_cast<try_enter_type>(
            GetProcAddress(kernel32, \"TryEnterCriticalSection\"));
        implementation.store(impl, std::memory_order_relaxed);
    } else {
        const auto impl = system_version == windows_me_version
            ? &try_enter_9x<tdbx_offset_me>
            : &try_enter_9x<tdbx_offset_98>;
        implementation.store(impl, std::memory_order_relaxed);
    }
    return implementation.load(std::memory_order_relaxed)(cs);
}

extern \"C\"
[[gnu::stdcall]] int TryEnterCriticalSection_compat(CRITICAL_SECTION* cs)
{
    return implementation.load(std::memory_order_relaxed)(cs);
}")

  (p ((a :href "/static/try-enter-critsec-9x.cc") "The complete, compilable source code")
     " of the fallback implementation of "
     (code "TryEnterCriticalSection")
     " is available under "
     ((a :href "https://creativecommons.org/publicdomain/zero/1.0/") "the CC0 license")
     ".")

  (p "All that's left now is to force "
     ((cite :class "program-name") "winpthreads")
     " to use our function instead of the system one by adding a few lines to "
     (code "misc.h")
     " and rebuilding the "
     ((cite :class "program-name") "winpthreads")
     " static library: ")
  ((highlighted-code :language :c++)
   "#ifndef TryEnterCriticalSection
int WINAPI TryEnterCriticalSection_compat(CRITICAL_SECTION* cs);
#define TryEnterCriticalSection TryEnterCriticalSection_compat
#endif")

  (p "Now we can just build Stockfish using our modified "
     ((cite :class "program-name") "winpthreads")
     ", remembering to link our fallback implementation, and…"))

(defsection damn-it-stockfish
  :header "Damn it, Stockfish"
  (p "When I first attempted this ordeal, at the beginning of September, that was indeed everything that's needed to get Stockfish running on Windows 98. A recent change to Stockfish broke it however: now the binary will fail to run, because the import "
     (code "GetLargePageMinimum")
     " is unresolved. Not a big deal, it's used in exactly one place in Stockfish code: large page allocation code. The call can be replaced with a constant 0 to allow Windows 98 to run it, at the cost of the binary no longer supporting large pages. A slight speed loss on modern versions of Windows, but the point here is to run it on Windows 98.")
  (p "After this one last trivial change…"))

(defsection it-finally-works
  :header "It finally works!"
  (p "It really does! Of course, since it's just a chess "
     (em "engine")
     ", it's only a console program:")
  ((image-figure
    :src "/static/stockfish-cmd.png"
    :width 652
    :height 359
    :alt "A Windows 98 console window displaying a truncated list of Universal Chess Interface options supported by Stockfish and the information about the used compiler.")
   "Stockfish running in a Windows 98 console window")
  (p "Since it's just an engine, we should be able to use any Universal Chess Interface-capable graphical program with it, and indeed, the Arena chess interface version 1.1, from 2004, has no problems with using this build of Stockfish as an engine:")
  ((image-figure
    :src "/static/sos-vs-stockfish.png"
    :width 1024
    :height 744
    :alt "Arena 1.1 running a match between SOS 5 playing white and current Stockfish playing black, with white to move. Stockfish has a material advantage of one pawn. Both engines agree that black is better.")
   "SOS 5 for Arena (white) playing against current Stockfish (black) under Arena 1.1, running on Windows 98")
  (p "Now I can watch Stockfish demolish the old engines shipped with a 16-year-old chess program, running under a 22-year-old operating system, on 18-year-old hardware, in peace."))
