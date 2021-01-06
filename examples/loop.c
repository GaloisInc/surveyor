#include <crucible.h>

int gcd(int a, int b) {
   if (b == 0)
   return a;
   return gcd(b, a % b);
}

void callee(int x, int y) {
  int z = x + gcd(x, y);
  crucible_debug_assert(z > 1, __FILE__, __LINE__);

}

int main() {
  int x = crucible_int8_t("nondet_x");
  int y = crucible_int8_t("nondet_y");
  callee(x, y);

  return 0;
}
