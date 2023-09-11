# This directory contains source code to implement a Simple fully connected Neural network in Python and then the design has been exported into Verilog using High level synthesis tool
## Workflow
1. Implementation and training of fully connected neural network in Python (_Jupyter notebook .ipynb available_)
2. Exporting and dumping trained model parameters into C file after converting them into _Fixed point_ scheme with 12 bit quantization
3. Implementation of fully conncted neural network in C from scratch with pre-trained and exported model parameters.
4. Using High level synthesis tool converting C code into _Verilog_
5. Synthesize Verilog code in VIVADO and check report
6. Simulate the design after writing testbench (_main_tb.v_). Note: Simulation has been tested on ICARUS VERILOG.
   Note: During behavioral simulation, one feature ata atime has been tested. We have tested approximately 25 feature and it shows same result with the
   result in Python environment. The testbench code may be modified to incorporate all the test features.
