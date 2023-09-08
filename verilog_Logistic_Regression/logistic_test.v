`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: School of Electronics Engineering, KIIT University
// Copyright: Dr. Subir Maity 
// 
// Create Date: 08.09.2023 17:14:05
// Design Name: 
// Module Name: logreg_test1
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
// Test bench for testing logistic regression : Predicted level is : 1
//////////////////////////////////////////////////////////////////////////////////


module logreg_test1();
reg rst;
wire ypred,done;
logistic UUT(rst,ypred,done);
initial begin
rst=1;
#100;
rst=0;
#500;
$display("Predicted label: %b, completion status: %b", ypred,done);
$finish;
end
endmodule

