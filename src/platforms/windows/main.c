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
 * @file main.c
 * @brief Windows entry point for AtomVM
 */

#include "avm_version.h"
#include "avmpack.h"
#include "context.h"
#include "globalcontext.h"
#include "iff.h"
#include "module.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void print_help(const char *program_name)
{
    printf("AtomVM %s for Windows\n\n", ATOMVM_VERSION);
    printf("Usage: %s <avm_file> [args...]\n\n", program_name);
    printf("Options:\n");
    printf("  -h, --help     Show this help message\n");
    printf("  -v, --version  Show version information\n");
}

static void print_version(void)
{
    printf("AtomVM %s\n", ATOMVM_VERSION);
    printf("Platform: Windows (%s)\n", SYSTEM_ARCHITECTURE);
}

int main(int argc, char *argv[])
{
    // Set console output to UTF-8
    SetConsoleOutputCP(CP_UTF8);

    if (argc < 2) {
        print_help(argv[0]);
        return 1;
    }

    // Handle flags
    if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
        print_help(argv[0]);
        return 0;
    }

    if (strcmp(argv[1], "-v") == 0 || strcmp(argv[1], "--version") == 0) {
        print_version();
        return 0;
    }

    const char *avm_path = argv[1];

    // Create global context
    GlobalContext *glb = globalcontext_new();
    if (!glb) {
        fprintf(stderr, "Failed to create global context\n");
        return 1;
    }

    // Open AVM pack
    struct AVMPackData *pack_data;
    enum OpenAVMResult open_result = sys_open_avm_from_file(glb, avm_path, &pack_data);

    switch (open_result) {
        case AVM_OPEN_OK:
            break;
        case AVM_OPEN_CANNOT_OPEN:
            fprintf(stderr, "Cannot open file: %s\n", avm_path);
            globalcontext_destroy(glb);
            return 1;
        case AVM_OPEN_CANNOT_READ:
            fprintf(stderr, "Cannot read file: %s\n", avm_path);
            globalcontext_destroy(glb);
            return 1;
        case AVM_OPEN_INVALID:
            fprintf(stderr, "Invalid AVM file: %s\n", avm_path);
            globalcontext_destroy(glb);
            return 1;
        case AVM_OPEN_FAILED_ALLOC:
            fprintf(stderr, "Memory allocation failed\n");
            globalcontext_destroy(glb);
            return 1;
        case AVM_OPEN_NOT_SUPPORTED:
            fprintf(stderr, "Operation not supported\n");
            globalcontext_destroy(glb);
            return 1;
    }

    // Add the pack to the global context
    synclist_append(&glb->avmpack_data, &pack_data->avmpack_head);

    // Find and load the entry module (marked with * in pack)
    const void *startup_beam = NULL;
    uint32_t startup_beam_size = 0;
    const char *startup_module_name = NULL;

    avmpack_fold(pack_data, avmpack_find_startup, &startup_beam, &startup_beam_size, &startup_module_name);

    if (!startup_beam) {
        fprintf(stderr, "No startup module found in AVM pack\n");
        globalcontext_destroy(glb);
        return 1;
    }

    // Load the startup module
    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (!mod) {
        fprintf(stderr, "Failed to load startup module\n");
        globalcontext_destroy(glb);
        return 1;
    }

    // Register the module
    globalcontext_insert_module(glb, mod);

    // Find the start/0 function
    int start_label = module_search_exported_function(mod, globalcontext_existing_term_from_atom_string(glb, "start"), 0, glb);
    if (start_label == 0) {
        fprintf(stderr, "No start/0 function found in module\n");
        globalcontext_destroy(glb);
        return 1;
    }

    // Create the initial context
    Context *ctx = context_new(glb);
    if (!ctx) {
        fprintf(stderr, "Failed to create context\n");
        globalcontext_destroy(glb);
        return 1;
    }

    ctx->leader = 1;

    // Start execution
    context_execute_loop(ctx, mod, "start", 0);

    // Cleanup
    int exit_value = 0;
    if (ctx->x[0] == ERROR_ATOM) {
        exit_value = 1;
    }

    globalcontext_destroy(glb);

    return exit_value;
}
