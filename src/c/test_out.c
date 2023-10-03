#include <stdio.h>
#include "network.h"

int main() {
    int rslt = network();
    printf("Prediction (C code): %d", rslt);
    printf("Prediction (Keras): %d", y_pred[0]);
    return 0;
}