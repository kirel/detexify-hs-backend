#include "DetexifyLib.h"
#include <stdlib.h>
#include <HsFFI.h>

void init_hs(const char* path) {
  int argc = 2;
  char *argv[] = { "+RTS", "-N", NULL };
  char **pargv = argv;
  hs_init(&argc, &pargv);
  init_detexify(path);
}

void exit_hs() {
  hs_exit();
}
