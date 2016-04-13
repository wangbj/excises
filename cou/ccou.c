#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

static const int minBound = 0x80808080;
static int C[20][200002];

static inline int max(int a, int b)
{
  return a > b? a : b;
}

static inline int min(int a, int b)
{
  return a < b? a : b;
}

static inline int ilogn(int n)
{
  int r = 0;
  while (n) {
    n >>= 1;
    ++r;
  }
  return r;
}

static inline void initdp(int n)
{
  int i;
  int k = ilogn(n);
  
  for (i = 0; i <= k; i++) {
    memset(C[i], 0x80, (1+n) * sizeof(int));
  }
}

static inline int value(char c)
{
  if (c == 'P') return 1;
  return -1;
}

int ccou(const char* s, int n)
{
  int i, k, t, L = ilogn(n);
  int r = minBound;
  int x, y;

  if (n == 1) {
    return value(s[0]);
  } else if (n == 2) {
    return max(-1, value(s[0]) + value(s[1]));
  }

  initdp(n);

  C[0][1] = value(s[0]);
  for (i = 2; i <= n; i++) {
    C[0][i] = C[0][i-1] + value(s[i-1]);
  }

  for (k = 0; k < L; k++) {
    for (i = 1; i < n; i++) {
      if (C[k][i] != minBound) {
	C[k][i+1] = max(C[k][i] + value(s[i]), C[k][i+1]);
	t = 1 + i + (1 << k);
	x = min(t, n);
	y = C[k][i] - value(s[i-1]) - 1 + (t > n? 0: value(s[x-1]));
	C[1+k][x] = max(y, C[1+k][x]);
      }
    }
  }
  for (i = 0; i <= L; i++) {
    if (C[i][n] > r) {
      r = C[i][n];
    }
  }

  return r;
}

#ifdef _TEST_
int main(int argc, char* argv[])
{
  int nt;
  char* line = NULL;
  ssize_t n;
  size_t size;

  n = scanf("%d\n", &nt);
  assert(n > 0);
  while(nt--) {
    n = getline(&line, &size, stdin);
    line[--n] = '\0';
    printf("%d\n", ccou(line, n));
  }
  free(line);

  return 0;
}
#endif
