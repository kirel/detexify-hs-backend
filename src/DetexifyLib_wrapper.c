#include "DetexifyLib.h"
#include <HsFFI.h>

void init_hs(const char* path) {
  hs_init(0, 0);
  init_detexify(path);
}

void exit_hs() {
  hs_exit();
}
