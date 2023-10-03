#include <stdio.h>
#include "network.h"

int main() {
    int rslt = network();
    printf("Prediction (C code): %d\n", rslt);
    printf("Prediction (Keras): %d\n", y_pred[test_sample]);
    printf("Actual value: %d\n", y_test[test_sample]);
    return 0;
}