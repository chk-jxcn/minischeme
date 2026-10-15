cmake_minimum_required(VERSION 3.16)
project(minischeme C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

if(MSVC)
    add_compile_options(/W3)
else()
    add_compile_options(-Wall -Wextra -pedantic -Wno-unused-parameter)
    if(WIN32)
        add_compile_definitions(__USE_MINGW_ANSI_STDIO=1)
    endif()
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
    src/minischeme_stubs.c
)

add_library(minischeme_static STATIC ${MINISCHEME_SOURCES})
set_target_properties(minischeme_static PROPERTIES OUTPUT_NAME minischeme)

add_executable(minischeme ${MINISCHEME_SOURCES})
target_compile_definitions(minischeme PRIVATE MINISCHEME_MAIN MINISCHEME_EMBED_INIT)

enable_testing()
add_test(NAME test_arithmetic COMMAND minischeme -e "(+ 1 2)")
add_test(NAME test_factorial COMMAND minischeme -e "(letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 10))")
