#ifndef __NETWORK_H
#define __NETWORK_H

#include <stdbool.h>

// Values exported from Python ANN implementation
extern double weights_layer0[16][30];
extern double biases_layer0[16];
extern double weights_layer1[8][16];
extern double biases_layer1[8];
extern double weights_layer2[1][8];
extern double biases_layer2[1];
extern double x[100][30];

extern bool y_pred[100];
extern short y_test[100];
extern int test_sample;


// Network function from ann.c
int network(void);

#endif