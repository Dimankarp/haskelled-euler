
#include <iostream>
#include <vector>
bool is_leap_year(int year) {
  return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

int length_of_month(int month, bool is_leap_year) {
  switch (month) {
  case 4:
  case 6:
  case 9:
  case 11:
    return 30;
  case 2:
    return is_leap_year ? 29 : 28;
  default:
    return 31;
  }
}

using std::vector;

struct YearInfo {
  int sundays;
  int next_year_weekday;
};

int main() {
  vector<YearInfo> weekday_to_sundays(6);
  vector<YearInfo> weekday_to_sundays_leap(6);

  for (int i = 0; i < 7; ++i) {
    int cur = i;
    int count = 0;
    int cur_leap = i;
    int count_leap = 0;

    for (int m = 1; m < 13; ++m) {
      count += (cur == 6);
      cur = (length_of_month(m, false) + cur) % 7;

      count_leap += (cur_leap == 6);
      cur_leap = (length_of_month(m, true) + cur_leap) % 7;
    }
    weekday_to_sundays[i] = {.sundays = count, .next_year_weekday = cur};
    weekday_to_sundays_leap[i] = {.sundays = count_leap,
                                  .next_year_weekday = cur_leap};
  }

  int sundays = 0;
  int weekday = ((1901 - 1900) * 365) % 7;

  for (int year = 1901; year < 2001; ++year) {
    YearInfo info = is_leap_year(year) ? weekday_to_sundays_leap[weekday]
                                       : weekday_to_sundays[weekday];
    sundays += info.sundays;
    weekday = info.next_year_weekday;
  }
  std::cout << sundays << std::endl;
  return 0;
}