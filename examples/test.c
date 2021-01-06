#include <crucible.h>

void callee(int x, int y) {
  int z = x + y;
 if(x > 1)
    crucible_breakpoint("bp", x, x * 4, y + 5 / (5 & z));

}

int main() {
  int x = crucible_int8_t("nondet_x");
  int y = crucible_int8_t("nondet_y");
  callee(x, y);

  return 0;
}
