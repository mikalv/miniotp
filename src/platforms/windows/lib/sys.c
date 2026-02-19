/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 mdma contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/**
 * @file sys.c
 * @brief Windows platform implementation for AtomVM
 */

// windows_sys.h must be first - defines listener_event_t and EventListener
#include "windows_sys.h"

#include "sys.h"
#include "avmpack.h"
#include "defaultatoms.h"
#include "iff.h"
#include "scheduler.h"
#include "utils.h"

// Forward declarations for listener functions
static void event_listener_add_to_polling_set(EventListener *listener, GlobalContext *glb);
static void listener_event_remove_from_polling_set(listener_event_t event, GlobalContext *glb);
static bool event_listener_is_event(EventListener *listener, listener_event_t event);

// Platform uses listeners
#include "listeners.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct WindowsPlatformData
{
    struct ListHead listeners;
#ifndef AVM_NO_SMP
    CRITICAL_SECTION listeners_mutex;
#endif
    HANDLE signal_event;  // Manual-reset event for sys_signal
    int ATOMIC listeners_poll_count;
    int ATOMIC select_events_poll_count;

    // High-resolution timer frequency
    LARGE_INTEGER perf_freq;
};

// Forward declarations
static void file_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global);

struct FileAVMPack
{
    struct AVMPackData base;
    void *data;
    size_t size;
};

static const struct AVMPackInfo file_avm_pack_info = {
    .destructor = file_avm_pack_destructor
};

static void file_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    UNUSED(global);
    struct FileAVMPack *file_pack = CONTAINER_OF(obj, struct FileAVMPack, base);
    free(file_pack->data);
    free(file_pack);
}

void sys_init_platform(GlobalContext *global)
{
    struct WindowsPlatformData *platform = malloc(sizeof(struct WindowsPlatformData));
    if (!platform) {
        fprintf(stderr, "Failed to allocate platform data\n");
        abort();
    }

    list_init(&platform->listeners);

#ifndef AVM_NO_SMP
    InitializeCriticalSection(&platform->listeners_mutex);
#endif

    // Create manual-reset event for signaling
    platform->signal_event = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (!platform->signal_event) {
        fprintf(stderr, "Failed to create signal event\n");
        abort();
    }

    platform->listeners_poll_count = 0;
    platform->select_events_poll_count = 0;

    // Get performance counter frequency for high-resolution timing
    QueryPerformanceFrequency(&platform->perf_freq);

    global->platform_data = platform;
}

void sys_free_platform(GlobalContext *global)
{
    struct WindowsPlatformData *platform = global->platform_data;

    // Clean up listeners
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &platform->listeners) {
        sys_listener_destroy(item);
    }

#ifndef AVM_NO_SMP
    DeleteCriticalSection(&platform->listeners_mutex);
#endif

    if (platform->signal_event) {
        CloseHandle(platform->signal_event);
    }

    free(platform);
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    struct WindowsPlatformData *platform = glb->platform_data;

    // Wait for signal event or timeout
    DWORD wait_time = (timeout_ms == SYS_POLL_EVENTS_WAIT_FOREVER) ? INFINITE : (DWORD)timeout_ms;

    DWORD result = WaitForSingleObject(platform->signal_event, wait_time);

    if (result == WAIT_OBJECT_0) {
        // Signal received, reset the event
        ResetEvent(platform->signal_event);
    }

    // Process listeners
#ifndef AVM_NO_SMP
    EnterCriticalSection(&platform->listeners_mutex);
#endif

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &platform->listeners) {
        EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
        EventListener *new_listener = listener->handler(glb, listener);
        if (new_listener != listener) {
            list_remove(item);
            if (new_listener != NULL) {
                list_append(&platform->listeners, &new_listener->listeners_list_head);
            }
        }
    }

#ifndef AVM_NO_SMP
    LeaveCriticalSection(&platform->listeners_mutex);
#endif
}

void sys_register_select_event(GlobalContext *glb, ErlNifEvent event, bool is_write)
{
    UNUSED(glb);
    UNUSED(event);
    UNUSED(is_write);
    // TODO: Implement select events using IOCP or WSAEventSelect
    fprintf(stderr, "sys_register_select_event: not yet implemented on Windows\n");
}

void sys_unregister_select_event(GlobalContext *glb, ErlNifEvent event, bool is_write)
{
    UNUSED(glb);
    UNUSED(event);
    UNUSED(is_write);
    // TODO: Implement select events
}

void sys_register_listener(GlobalContext *global, EventListener *listener)
{
    struct WindowsPlatformData *platform = global->platform_data;

#ifndef AVM_NO_SMP
    EnterCriticalSection(&platform->listeners_mutex);
#endif

    list_append(&platform->listeners, &listener->listeners_list_head);
    platform->listeners_poll_count = -1;  // Invalidate

#ifndef AVM_NO_SMP
    LeaveCriticalSection(&platform->listeners_mutex);
#endif

    sys_signal(global);
}

void sys_unregister_listener(GlobalContext *global, EventListener *listener)
{
    struct WindowsPlatformData *platform = global->platform_data;

#ifndef AVM_NO_SMP
    EnterCriticalSection(&platform->listeners_mutex);
#endif

    list_remove(&listener->listeners_list_head);
    platform->listeners_poll_count = -1;  // Invalidate

#ifndef AVM_NO_SMP
    LeaveCriticalSection(&platform->listeners_mutex);
#endif
}

// Implementation of listener helper functions required by listeners.h
static void event_listener_add_to_polling_set(EventListener *listener, GlobalContext *glb)
{
    UNUSED(listener);
    UNUSED(glb);
    // Windows doesn't need to add to polling set
    // WaitForMultipleObjects handles this differently
}

static void listener_event_remove_from_polling_set(listener_event_t event, GlobalContext *glb)
{
    UNUSED(event);
    UNUSED(glb);
    // Windows doesn't need to remove from polling set
    // WaitForMultipleObjects handles this differently
}

static bool event_listener_is_event(EventListener *listener, listener_event_t event)
{
    return listener->event == event;
}

#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
void sys_signal(GlobalContext *glb)
{
    struct WindowsPlatformData *platform = glb->platform_data;
    SetEvent(platform->signal_event);
}
#endif

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **data)
{
    UNUSED(global);

    HANDLE file = CreateFileA(
        path,
        GENERIC_READ,
        FILE_SHARE_READ,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );

    if (file == INVALID_HANDLE_VALUE) {
        return AVM_OPEN_CANNOT_OPEN;
    }

    LARGE_INTEGER file_size;
    if (!GetFileSizeEx(file, &file_size)) {
        CloseHandle(file);
        return AVM_OPEN_CANNOT_READ;
    }

    void *file_data = malloc((size_t)file_size.QuadPart);
    if (!file_data) {
        CloseHandle(file);
        return AVM_OPEN_FAILED_ALLOC;
    }

    DWORD bytes_read;
    if (!ReadFile(file, file_data, (DWORD)file_size.QuadPart, &bytes_read, NULL) ||
        bytes_read != (DWORD)file_size.QuadPart) {
        free(file_data);
        CloseHandle(file);
        return AVM_OPEN_CANNOT_READ;
    }

    CloseHandle(file);

    if (!avmpack_is_valid(file_data, (unsigned int)file_size.QuadPart)) {
        free(file_data);
        return AVM_OPEN_INVALID;
    }

    struct FileAVMPack *file_pack = malloc(sizeof(struct FileAVMPack));
    if (!file_pack) {
        free(file_data);
        return AVM_OPEN_FAILED_ALLOC;
    }

    avmpack_data_init(&file_pack->base, &file_avm_pack_info);
    file_pack->base.data = file_data;
    file_pack->data = file_data;
    file_pack->size = (size_t)file_size.QuadPart;

    *data = &file_pack->base;
    return AVM_OPEN_OK;
}

void sys_time(struct timespec *t)
{
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);

    // Convert FILETIME (100-nanosecond intervals since Jan 1, 1601) to Unix time
    ULARGE_INTEGER ull;
    ull.LowPart = ft.dwLowDateTime;
    ull.HighPart = ft.dwHighDateTime;

    // Subtract the difference between 1601 and 1970 (in 100-ns intervals)
    ull.QuadPart -= 116444736000000000ULL;

    t->tv_sec = (time_t)(ull.QuadPart / 10000000ULL);
    t->tv_nsec = (long)((ull.QuadPart % 10000000ULL) * 100);
}

void sys_monotonic_time(struct timespec *t)
{
    LARGE_INTEGER counter;
    LARGE_INTEGER freq;

    QueryPerformanceCounter(&counter);
    QueryPerformanceFrequency(&freq);

    t->tv_sec = (time_t)(counter.QuadPart / freq.QuadPart);
    t->tv_nsec = (long)(((counter.QuadPart % freq.QuadPart) * 1000000000LL) / freq.QuadPart);
}

uint64_t sys_monotonic_time_u64(void)
{
    LARGE_INTEGER counter;
    QueryPerformanceCounter(&counter);
    return (uint64_t)counter.QuadPart;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    return (ms * (uint64_t)freq.QuadPart) / 1000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    return (t * 1000) / (uint64_t)freq.QuadPart;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    HANDLE file = CreateFileA(
        path,
        GENERIC_READ,
        FILE_SHARE_READ,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );

    if (file == INVALID_HANDLE_VALUE) {
        return NULL;
    }

    LARGE_INTEGER file_size;
    if (!GetFileSizeEx(file, &file_size)) {
        CloseHandle(file);
        return NULL;
    }

    void *file_data = malloc((size_t)file_size.QuadPart);
    if (!file_data) {
        CloseHandle(file);
        return NULL;
    }

    DWORD bytes_read;
    if (!ReadFile(file, file_data, (DWORD)file_size.QuadPart, &bytes_read, NULL) ||
        bytes_read != (DWORD)file_size.QuadPart) {
        free(file_data);
        CloseHandle(file);
        return NULL;
    }

    CloseHandle(file);

    if (!iff_is_valid_beam(file_data)) {
        free(file_data);
        return NULL;
    }

    Module *mod = module_new_from_iff_binary(global, file_data, (unsigned long)file_size.QuadPart);
    if (!mod) {
        free(file_data);
        return NULL;
    }

    return mod;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    UNUSED(glb);
    UNUSED(driver_name);
    UNUSED(opts);

    // TODO: Implement port drivers for Windows
    // For now, return NULL to indicate no driver found
    return NULL;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

// JIT is not supported on Windows yet
ModuleNativeEntryPoint sys_map_native_code(const uint8_t *native_code, size_t size, size_t offset)
{
    UNUSED(native_code);
    UNUSED(size);
    UNUSED(offset);
    return NULL;
}

bool sys_get_cache_native_code(GlobalContext *global, Module *mod, uint16_t *version,
    ModuleNativeEntryPoint *entry_point, uint32_t *labels)
{
    UNUSED(global);
    UNUSED(mod);
    UNUSED(version);
    UNUSED(entry_point);
    UNUSED(labels);
    return false;
}

void sys_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version,
    ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    UNUSED(global);
    UNUSED(mod);
    UNUSED(version);
    UNUSED(entry_point);
    UNUSED(labels);
}
