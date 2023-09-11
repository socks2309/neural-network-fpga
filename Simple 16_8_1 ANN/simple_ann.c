#include<stdio.h>
#include "model_params_layer0.c"
#include "model_params_layer1.c"
#include "model_params_layer2.c"
#include "feature.c"
int main()
{
/*int x[]={-2258,-1422,-2388,-2333,-2838,
-2913,-2585,-2756,2357,-349,
-2832,-1801,-2904,-2298,-2034,
-2227,-1567,-2579,-1176,-2065,
-2340,-490,-2602,-2346,-1903,
-1990,-1574,-2108,1328,-719
};//feature vector
*/
int n_feature = 30;
int n_layer0_neuron = 16;
int n_layer1_neuron = 8;
int layer2_neuron =1;
long y_layer0[n_layer0_neuron];
long y_layer1[n_layer1_neuron];
long y_layer2 = 0;
long tmp = 0;
long yy = 0;
int i,j,result;
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
int tmp1=0;
//activation function: Sigmoid (approximated) with 12 bit fixed point representation
if(yy<-12288)
tmp1=41;
else if(yy>-12288 && yy<-8192)
tmp1=205;
else if(yy >= -8192 && yy<-6963)
tmp1=614;
else if(yy>= -6963 && yy < -5325)
tmp1=819;
else if(yy>= -5325 && yy < -3277)
tmp1=1229;
else if(yy>= -3277 && yy < -1638)
tmp1=1638;
else if(yy>= -1638 && yy < 410)
tmp1=2048;
else if(yy>= 410 && yy < 2048)
tmp1=2458;
else if(yy>=2048 && yy < 3686)
tmp1=2867;
else if(yy>= 3686 && yy < 5734)
tmp1=3277;
else if(yy>= 5734 && yy < 7373)
tmp1=3482;
else
tmp1=3686;
//predict output with boundary at 0.5
if(tmp1 > 2048)
result=1;
else
result=0;
printf("\n ypred in fixed point: %d\n",tmp1);
printf("\nPredicted value for this feature using fixed point is: %d\n", result);
return result;

}


