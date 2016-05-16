#include <stdio.h>

static __thread char buffer[128];

char* d10p(double x)
{
  snprintf(buffer, 128, "%.10f", x);
  return buffer;
}

char* d10p2(double x, double y)
{
  snprintf(buffer, 128, "%.10f\t%.10f\n", x, y);
  return buffer;
}
