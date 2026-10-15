cmake_minimum_required(VERSION 3.16)
project(minischeme C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

if(MSVC)
    add_compile_options(/W3)
else()
    add_compile_options(-Wall -Wextra -pedantic -Wno-unused-parameter)
endif()

include_directories(include)

set(MINISCHEME_SOURCES
    src/minischeme.c
    src/minischeme_vm.c
    src/minischeme_gc.c
    src/minischeme_prim.c
    src/minischeme_bigint.c
    src/minischeme_math.c
    src/minischeme_fasl.c
    src/minischeme_cfasl.c
    src/minischeme_lib.c
    src/minischeme_help.c
)

add_library(minischeme_static STATIC ${MINISCHEME_SOURCES})
set_target_properties(minischeme_static PROPERTIES OUTPUT_NAME minischeme)

add_executable(minischeme src/minischeme.c ${MINISCHEME_SOURCES})
target_compile_definitions(minischeme PRIVATE MINISCHEME_MAIN)

enable_testing()

add_executable(test_compiler tests/test_compiler.c ${MINISCHEME_SOURCES})
add_test(NAME test_compiler COMMAND test_compiler)

add_executable(test_msbc tests/test_msbc.c ${MINISCHEME_SOURCES})
add_test(NAME test_msbc COMMAND test_msbc)

add_executable(test_stress tests/test_stress.c ${MINISCHEME_SOURCES})
add_test(NAME test_stress COMMAND test_stress)
