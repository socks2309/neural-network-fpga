# Implementing a neural network on an FPGA
This ongoing project aims to implement a neural network on an FPGA. The languages used to achieve this are C and Verilog. Phase 1 of the project aims to implement floating point/fixed point arithmetic using the IEEE 754 standard. The first goal of this project is to implement a simple ANN and then work our way up to CNNs, etc. This project is part of the B.Tech in Electronics and Telecommunication Engineering degree at KIIT University.

# File Description
The **_decimal_to_binary.c_** file implements the single precision IEEE 754 standard for any floating point number from -inf to +inf.<br />
logistic_regression.ipynb is Jupyter notebook file to train a model for breast cancer prediction and then model parameters are converted into fixed point for further implementation into C.<br />
**_logreg_test2.c_** is the code written in C to predict the output using the trained model parameters from previous Jupyter notebook file. Fixed point arithmetic used with 12 bit quantization factor. One approximated Sigmoid function is implemented in fixed point version (12 bit).<br />

# Directory details
**_"verilog_Logistic_Regression"_** contain handwritten verilog design and testbench code to implement a logistic regression model for prediction of cancer. The model parameters are exported from Python annd converted to fixed point.<br />
**_"Logistic_reg_HLS_Bambu"_**-> same problem is implemented using high level synthesis tool BAMBU and tested. 
