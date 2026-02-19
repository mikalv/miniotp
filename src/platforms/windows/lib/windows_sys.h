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
 * @file windows_sys.h
 * @brief Windows-specific system definitions for AtomVM
 */

#ifndef _WINDOWS_SYS_H_
#define _WINDOWS_SYS_H_

// MSVC compatibility: must be before any other includes
#ifdef _MSC_VER

// MSVC doesn't support GCC's __attribute__ syntax
#define __attribute__(x)

// MSVC uses __declspec(deprecated) instead of __attribute__((deprecated))
// But since we're removing __attribute__, deprecated functions won't warn

// _Static_assert is available in MSVC as static_assert
#ifndef _Static_assert
#define _Static_assert static_assert
#endif

// Prevent min/max macro conflicts
#ifndef NOMINMAX
#define NOMINMAX
#endif

#endif // _MSC_VER

// Guard against redefinition
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>

// ErlNifEvent is a HANDLE on Windows (for IOCP/WaitForMultipleObjects)
// Define guard macro before typedef to prevent conflict with erl_nif.h
#ifndef TYPEDEF_ERL_NIF_EVENT
#define TYPEDEF_ERL_NIF_EVENT
typedef HANDLE ErlNifEvent;
#endif
#define ERL_NIF_EVENT_INVALID INVALID_HANDLE_VALUE

// Event listener types for Windows
// listener_event_t is a HANDLE on Windows (for WaitForMultipleObjects)
typedef HANDLE listener_event_t;

// Forward declarations
struct GlobalContext;
struct EventListener;

// Event handler function type
typedef struct EventListener *(*event_handler_t)(struct GlobalContext *glb, struct EventListener *listener);

// List head structure (from list.h)
#include "list.h"

// Windows event listener structure
struct EventListener
{
    struct ListHead listeners_list_head;
    event_handler_t handler;
    listener_event_t event;  // HANDLE for Windows
};

#endif // _WINDOWS_SYS_H_
