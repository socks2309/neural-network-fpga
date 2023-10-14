#include <math.h>
#include "../../include/network.h"


// Mention the sample that will be fetched from features.c
int test_sample = 0;

int network (void) {

    int n_feature = 16;
    int n_layer0_neuron = 16;
    int n_layer1_neuron = 8;
    int n_layer2_neuron = 1;

    double y_layer0[n_layer0_neuron];
    double y_layer1[n_layer1_neuron];
    double y_layer2 = 0.0;
    unsigned int result;
    unsigned int status = 0;

    int i, j;
    double temp = 0.0;
    // Code for layer 0
    for (i = 0; i < n_layer0_neuron; i++) {
        for (j = 0; j < n_feature; j++) {
            temp = temp + (weights_layer0[i][j] * x[test_sample][j]);
        }
        temp = temp + biases_layer0[i];

        // ReLU activation
        if(temp > 0) {
            y_layer0[i] = temp;
        }
        else {
            y_layer0[i] = 0;
        }
        temp = 0.0;
    }

    // Code for layer 1
    n_feature = n_layer0_neuron;
    for (i = 0; i < n_layer1_neuron; i++) {
        for (j = 0; j < n_feature; j++) {
            temp = temp + (weights_layer1[i][j] * y_layer0[j]);
        }
        temp = temp + biases_layer1[i];

        // ReLU activation
        if(temp > 0) {
            y_layer0[i] = temp;
        }
        else {
            y_layer0[i] = 0;
        }
        temp = 0.0;
    }

    // Code for output layer
    n_feature = n_layer1_neuron;
    for (i = 0; i < n_feature; i++) {
        temp = temp + (weights_layer2[0][i] * y_layer1[i]);
    }
    double output = temp + biases_layer2[0];

    // Sigmoid activation
    double sig_act = (double) 1 / (1 + exp(-output));
    if(sig_act > 0.5) result = 1;
    else result = 0; 
    status = 1;
    
    return result;
}