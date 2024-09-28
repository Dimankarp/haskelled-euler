
#include <cmath>
#include <iostream>

int calc_divisors(int num) {
  int count = 0;
  int sqrtx = std::sqrt(num);
  for (int i = 1; i < sqrtx; ++i) {
    count += (num % i == 0);
  }
  count *= 2;
  count += (num % sqrtx == 0);
  return count;
}

int main() {
  int cur = 1;
  int i = 1;

  while (i < 100'000) {
    if (calc_divisors(cur) > 500) {
      std::cout << cur << std::endl;
      return 0;
    }
    ++i;
    cur += i;
  }
  return -1;
}