#
# This file is part of AtomVM.
#
# Copyright 2024 mdma contributors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

cmake_minimum_required(VERSION 3.16)

project(AtomVM-Windows)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# AtomVM core library path
set(ATOMVM_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../libAtomVM")

# Windows platform sources
set(WINDOWS_PLATFORM_SOURCES
    main.c
    lib/sys.c
    lib/smp.c
    lib/platform_defaultatoms.c
)

# Include directories
include_directories(
    ${ATOMVM_LIB_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/lib
    ${CMAKE_CURRENT_BINARY_DIR}
)

# Build libAtomVM
file(GLOB ATOMVM_SOURCES
    "${ATOMVM_LIB_DIR}/*.c"
)

# Remove JIT sources (not supported on Windows yet)
list(FILTER ATOMVM_SOURCES EXCLUDE REGEX "jit.*\\.c$")

# Create static library
add_library(libAtomVM STATIC ${ATOMVM_SOURCES})
target_compile_definitions(libAtomVM PRIVATE
    AVM_DISABLE_JIT=1
    AVM_BUILD_RUNTIME_ONLY=1
    WIN32_LEAN_AND_MEAN
)

# Create AtomVM executable
add_executable(AtomVM ${WINDOWS_PLATFORM_SOURCES})

target_compile_definitions(AtomVM PRIVATE
    AVM_DISABLE_JIT=1
    AVM_BUILD_RUNTIME_ONLY=1
    WIN32_LEAN_AND_MEAN
)

target_link_libraries(AtomVM
    libAtomVM
    ws2_32      # Winsock for networking
    winmm       # Windows multimedia (timing)
)

# Install
install(TARGETS AtomVM DESTINATION bin)

# Generate version header
configure_file(
    "${ATOMVM_LIB_DIR}/version.h.in"
    "${CMAKE_CURRENT_BINARY_DIR}/avm_version.h"
    @ONLY
)
