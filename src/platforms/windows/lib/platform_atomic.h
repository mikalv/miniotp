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
 * @file platform_atomic.h
 * @brief Windows-specific atomic operations for AtomVM
 */

#ifndef _PLATFORM_ATOMIC_H_
#define _PLATFORM_ATOMIC_H_

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>

// MSVC doesn't support GCC's _Atomic keyword, use volatile instead
#define ATOMIC volatile

/**
 * @brief Atomic compare and exchange for int.
 * @details Atomically compares *obj with *expected. If equal, replaces *obj
 * with desired and returns true. Otherwise, stores *obj into *expected
 * and returns false.
 */
static inline int ATOMIC_COMPARE_EXCHANGE_WEAK_INT(volatile int *obj, int *expected, int desired)
{
    LONG old = InterlockedCompareExchange((volatile LONG *)obj, (LONG)desired, (LONG)*expected);
    if (old == *expected) {
        return 1;  // success
    } else {
        *expected = (int)old;
        return 0;  // failure
    }
}

/**
 * @brief Atomic load for pointer-sized integers.
 */
static inline intptr_t ATOMIC_LOAD_PTR(volatile intptr_t *obj)
{
#ifdef _WIN64
    return (intptr_t)InterlockedCompareExchange64((volatile LONG64 *)obj, 0, 0);
#else
    return (intptr_t)InterlockedCompareExchange((volatile LONG *)obj, 0, 0);
#endif
}

/**
 * @brief Atomic store for pointer-sized integers.
 */
static inline void ATOMIC_STORE_PTR(volatile intptr_t *obj, intptr_t value)
{
#ifdef _WIN64
    InterlockedExchange64((volatile LONG64 *)obj, (LONG64)value);
#else
    InterlockedExchange((volatile LONG *)obj, (LONG)value);
#endif
}

/**
 * @brief Atomic increment.
 */
static inline long ATOMIC_INCREMENT(volatile long *obj)
{
    return InterlockedIncrement((volatile LONG *)obj);
}

/**
 * @brief Atomic decrement.
 */
static inline long ATOMIC_DECREMENT(volatile long *obj)
{
    return InterlockedDecrement((volatile LONG *)obj);
}

#endif // _PLATFORM_ATOMIC_H_
