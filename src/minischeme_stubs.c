#include <stdbool.h>
#include <stddef.h>
#include "minischeme.h"

void mini_register_gui_lib(MiniVM *vm) {
    (void)vm;
}

void mini_register_api_lib(MiniVM *vm) {
    (void)vm;
}

bool mini_app_storage_ok(size_t bytes, size_t old, char *why, size_t cap) {
    (void)bytes;
    (void)old;
    (void)why;
    (void)cap;
    return true;
}
