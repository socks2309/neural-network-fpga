#include<stdio.h>
#include<math.h>
#include "model_params_layer0.c"
#include "model_params_layer1.c"
#include "model_params_layer2.c"
#include "feature.c"
long main()
{
int n_feature = 30;
int n_layer0_neuron = 16;
int n_layer1_neuron = 8;
int layer2_neuron =1;
double y_layer0[n_layer0_neuron];
double y_layer1[n_layer1_neuron];
double y_layer2 = 0;
double tmp = 0;
double yy = 0;
short i,j,result;
short status=0;
if(status==0) {
//code for layer_0
for(j=0;j<n_layer0_neuron;j++) {
for(i=0;i<n_feature;i++)
{
tmp = tmp + (w0[j][i] * x[i]);
}
tmp = tmp + b0[j];
//pass output to Relu activation
if(tmp > 0)
y_layer0[j] = tmp;
else
y_layer0[j] = 0;
//clear tmp variable to store next data
tmp = 0; 

 }
 
 //code for layer_1
 n_feature = n_layer0_neuron;
for(j=0;j<n_layer1_neuron;j++) {
for(i=0;i<n_feature;i++)
{
tmp = tmp + (w1[j][i] * y_layer0[i]);
}
tmp = tmp + b1[j];
//pass output to Relu activation
if(tmp > 0)
y_layer1[j] = tmp;
else
y_layer1[j] = 0;
//clear tmp variable to store next data
tmp = 0; 

 }
 
 //code for final layer (output layer) with Sigmoid activation
 n_feature = n_layer1_neuron;
 for(i=0;i<n_feature;i++)
{
tmp = tmp + (w2[i] * y_layer1[i]);
}
yy = tmp + b2;
//pass output to Sigmoid activation
double tmp1=0;
tmp1 = 1/(1+exp(-yy));
if(tmp1 > 0.5)
result=1;
else
result=0;
//printf("\n ypred in fixed point: %f\n",yy);
printf("\nPredicted value for this feature using C: %d\n", result);
status=1;
return result;
}
}


