`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: School of Electronics Engineering, KIIT University
// Copyright: Dr. Subir Maity 
// 
// Create Date: 08.09.2023 17:01:41
// Design Name: 
// Module Name: logistic
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


 // Implement Logistic regression test
// feature and weight saved into RAM
// 

module logistic(rst,ypred,done);
input rst;
output reg ypred;
output reg done;

reg [15:0] ram_weight [0:29]; //RAM for storing weight
reg [15:0] ram_feature [0:29]; //RAM for storing test features
reg signed[15:0] bias;
reg signed[63:0] tmp;
reg signed[15:0] tmp1;
reg [15:0] w,x;
reg [63:0] res;
integer i;

//load model Parameters

initial begin
$readmemb("ram_weight.mem",ram_weight);
$readmemb("ram_feature.mem",ram_feature);
tmp = 0;
tmp1 = 0;
bias = 16'b0000000110110010;
done = 0;
end

always @(*)
begin

if(rst)
ypred=1'bz;

if(!done) begin
for(i=0;i<30;i=i+1)
begin
w = ram_weight[i]; //copy data from RAM to temporary reg
x = ram_feature[i];
//tmp = tmp + (ram_weight[i] * ram_feature[i]);
res = w[14:0] * x[14:0]; //perform operation of data bits except sign bit
if(w[15] ^ x[15]==0) //decide sign of multiplication by XORing two MSBs
tmp = tmp + res;
else
tmp = tmp - res;

$display("W:%d, X: %d",w,x);
$display("output of w*x is : %d",tmp);
end
tmp = tmp + bias;
//pass the data into sigmoid activation
//This section implements Sigmoid activation function whose output varies between 0 to 4096 (ideally)
//which is mapped to 0 and 1
if(tmp <-12288)
tmp1 = 41;
else if(tmp >-12288 && tmp <-8192)
tmp1 = 205;
else if(tmp >= -8192 && tmp <-6963)
tmp1 = 614;
else if(tmp >= -6963 && tmp < -5325)
tmp1 = 819;
else if(tmp>= -5325 && tmp < -3277)
tmp1 = 1229;
else if(tmp>= -3277 && tmp < -1638)
tmp1 = 1638;
else if(tmp >= -1638 && tmp < 410)
tmp1 = 2048;
else if(tmp >= 410 && tmp < 2048)
tmp1 = 2458;
else if(tmp >=2048 && tmp < 3686)
tmp1 = 2867;
else if(tmp >= 3686 && tmp < 5734)
tmp1 = 3277;
else if(tmp >= 5734 && tmp < 7373)
tmp1 = 3482;
else
tmp1 = 3686;
//predict the label with a probability of 0.5
if(tmp1 > 2048)
ypred = 1;
else
ypred = 0;
$display("output of sigmoid function(ypred): %d",tmp1);
end
done = 1;
end 

endmodule


