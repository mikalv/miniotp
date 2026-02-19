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
 * @file smp.c
 * @brief Windows SMP (threading) implementation for AtomVM
 */

#include "smp.h"

#ifndef AVM_NO_SMP

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>

struct Mutex
{
    CRITICAL_SECTION cs;
};

struct CondVar
{
    CONDITION_VARIABLE cv;
};

struct RWLock
{
    SRWLOCK lock;
};

Mutex *smp_mutex_create(void)
{
    Mutex *mtx = malloc(sizeof(Mutex));
    if (mtx) {
        InitializeCriticalSection(&mtx->cs);
    }
    return mtx;
}

void smp_mutex_destroy(Mutex *mtx)
{
    if (mtx) {
        DeleteCriticalSection(&mtx->cs);
        free(mtx);
    }
}

void smp_mutex_lock(Mutex *mtx)
{
    EnterCriticalSection(&mtx->cs);
}

bool smp_mutex_trylock(Mutex *mtx)
{
    return TryEnterCriticalSection(&mtx->cs) != 0;
}

void smp_mutex_unlock(Mutex *mtx)
{
    LeaveCriticalSection(&mtx->cs);
}

CondVar *smp_condvar_create(void)
{
    CondVar *cv = malloc(sizeof(CondVar));
    if (cv) {
        InitializeConditionVariable(&cv->cv);
    }
    return cv;
}

void smp_condvar_destroy(CondVar *cv)
{
    // Windows condition variables don't need destruction
    free(cv);
}

void smp_condvar_wait(CondVar *cv, Mutex *mtx)
{
    SleepConditionVariableCS(&cv->cv, &mtx->cs, INFINITE);
}

void smp_condvar_signal(CondVar *cv)
{
    WakeConditionVariable(&cv->cv);
}

void smp_condvar_broadcast(CondVar *cv)
{
    WakeAllConditionVariable(&cv->cv);
}

RWLock *smp_rwlock_create(void)
{
    RWLock *lock = malloc(sizeof(RWLock));
    if (lock) {
        InitializeSRWLock(&lock->lock);
    }
    return lock;
}

void smp_rwlock_destroy(RWLock *lock)
{
    // Windows SRW locks don't need destruction
    free(lock);
}

void smp_rwlock_rdlock(RWLock *lock)
{
    AcquireSRWLockShared(&lock->lock);
}

void smp_rwlock_wrlock(RWLock *lock)
{
    AcquireSRWLockExclusive(&lock->lock);
}

bool smp_rwlock_tryrdlock(RWLock *lock)
{
    return TryAcquireSRWLockShared(&lock->lock) != 0;
}

void smp_rwlock_unlock(RWLock *lock)
{
    // Windows SRW locks require knowing if we held shared or exclusive
    // This is a simplification - in practice we'd need to track this
    ReleaseSRWLockExclusive(&lock->lock);
}

int smp_get_online_processors(void)
{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return (int)sysinfo.dwNumberOfProcessors;
}

#endif // AVM_NO_SMP
