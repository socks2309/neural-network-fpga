**ANN Implementation using Floating Point Arithmatic** </br>
This directory contain ANN implementation and its synthesis into Verilog HDL using floating point arithmetic.
Run the ann.py file to train the model and to export model parameters as C header and run C simulation, conversion to Verilog using HLS tool and behavioral simulation using ICARUS Verilog simulator.
</br>
change the number in for loop **for jj in range(100):** in ann.py file to any value below 100 to simulate no of test features. Result will be printed on Terminal.
</br>
All 100 test data has been tested after FPGA implementation(behavoiral simulation in Verilog) and gives excellent agreement with Actual data, Keras Predicted result, C simulation result.

# ANN using floating point arithmetic
In this directory, the ann.py implements the same network as in **Simple 16_8_1 ANN** but instead of converting to fixed-point, the calculations are done in floating point to test if Bambu is able to allocate resources for it or not. 