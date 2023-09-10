# Description
This directory contain handwritten Verilog code to implement a Logistic regression function to detect Breast cancer. The model is trained in Python (Jupyter notebook .ipynb is provided inside the directory). <br />
A 12 bit quantization scheme is used to convert floating point values of model weight and features into fixed point representation.
The Verilog design code and testbench is implemented to test only one feature. With slight modification, all the feature can be tested. One approximated Sigmoid function has been realized to minimize the 
hardware requirement (due to exp() operation in Sigmoid function).
