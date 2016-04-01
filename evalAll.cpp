#include <cstdio>
#include <cstdlib>

int main() {
  double taus[] = {0.01, 1, 10, 100};
  int antisymMethods[] = {0, 1, 2, 3};
  char buf[64];

  for(int i = 0; i < sizeof(taus) / sizeof(taus[0]); i++) {
    double curTau = taus[i];
    for(int j = 0; j < sizeof(antisymMethods) / sizeof(antisymMethods[0]); j++) {
      int curMethod = antisymMethods[j];
      sprintf(buf, "nohup ./projector %d < input_he_%g.txt > output_he_%g_M%d.txt &", curMethod, curTau, curTau, curMethod);
      printf("\x1b[32mSubmit: %s \x1b[0m \n", buf);
      system(buf);
    }
  }

  return 0;
}
