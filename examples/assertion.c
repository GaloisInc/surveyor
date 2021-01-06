#include <crucible.h>

void callee(int x, int y) {
  int z = x + y;
  crucible_assert(z > 1, __FILE__, __LINE__);

}

int main() {
  int x = crucible_int8_t("nondet_x");
  int y = crucible_int8_t("nondet_y");
  callee(x, y);

  return 0;
}
