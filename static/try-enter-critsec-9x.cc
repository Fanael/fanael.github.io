// SPDX-License-Identifier: CC0-1.0
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <windows.h>

struct critical_section {
    std::uint8_t type; // Always 4.
    struct critical_section_impl* impl;
    // Padding for NT compatibility, where the structure is completely
    // different.
    std::uint32_t reserved[4];
};
static_assert(sizeof(critical_section) == 24);

struct critical_section_impl {
    std::uint8_t type;
    int recursion_count; // Starts at 1, decreasing.
    void* owner_thread;
    std::uint32_t reserved;
    std::atomic<int> lock_count;
    void* internal_pointers[3];
};
static_assert(sizeof(critical_section_impl) == 32);

namespace {

constexpr std::uint8_t critical_section_type = 4;

template <unsigned TdbxOffset>
void* get_current_tdbx()
{
    const char* tib;
    asm("mov {%%fs:0x18, %0|%0, fs:[0x18]}" : "=r"(tib));
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
}

constexpr unsigned tdbx_offset_98 = 0x50;
constexpr unsigned tdbx_offset_me = 0x80;

constexpr std::uint32_t windows_9x_mask = 0x80000000;
constexpr std::uint32_t windows_me_version = 0xC0005A04;

[[gnu::stdcall]] int try_enter_dispatch(CRITICAL_SECTION* cs);

using try_enter_type = decltype(&TryEnterCriticalSection);
std::atomic<try_enter_type> implementation(&try_enter_dispatch);

[[gnu::stdcall]] int try_enter_dispatch(CRITICAL_SECTION* cs)
{
    const auto system_version = GetVersion();
    if((system_version & windows_9x_mask) == 0) {
        const auto kernel32 = LoadLibraryA("kernel32.dll");
        const auto impl = reinterpret_cast<try_enter_type>(
            GetProcAddress(kernel32, "TryEnterCriticalSection"));
        implementation.store(impl, std::memory_order_relaxed);
    } else {
        const auto impl = system_version == windows_me_version
            ? &try_enter_9x<tdbx_offset_me>
            : &try_enter_9x<tdbx_offset_98>;
        implementation.store(impl, std::memory_order_relaxed);
    }
    return implementation.load(std::memory_order_relaxed)(cs);
}

}

extern "C"
[[gnu::stdcall]] int TryEnterCriticalSection_compat(CRITICAL_SECTION* cs)
{
    return implementation.load(std::memory_order_relaxed)(cs);
}
