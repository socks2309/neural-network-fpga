#include<stdio.h>
int main()
{
///******Model parameters extracted from Python *******//
//******Copyright: Dr. Subir Kr. Maity. School of Electronics, KIIT, Bhubaneswar ******//
//****12-bit quantization used*****//

int QF = 4096;
int flag = 0;
int weight[]={-1131,-1242,-1191,-1444,-364,
2846,-3406,-3702,165,1496,
-4905,1442,-3036,-3994,-1126,
2536,1053,-1458,722,2136,
-3594,-5748,-3136,-3715,-3237,
-43,-3822,-3898,-3710,-714
};
int bias=434;
int X[]={-2258,-1422,-2388,-2333,-2838,
-2913,-2585,-2756,2357,-349,
-2832,-1801,-2904,-2298,-2034,
-2227,-1567,-2579,-1176,-2065,
-2340,-490,-2602,-2346,-1903,
-1990,-1574,-2108,1328,-719
};
//calculate w*x+b
//total 30 weights corresponds to 30 feature X[]
long yy=0;
int result;
int i;

for(i=0;i<30;i++){
yy = yy + ( weight[i] * X[i]);
//printf("\n%d:sum of w*x is: %ld",i,yy);
}

yy += bias;
//printf("\nyy val:%ld",yy);
//fed y into sigmoid function
int tmp1=0;
//activation function: Sigmoid (approximated)
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

if(tmp1 > 2048)
result=1;
else
result=0;
printf("\n ypred in fixed point: %d\n",tmp1);
//printf("\nPredicted value for this feature using fixed point is: %d\n", result);
return tmp1;

}
