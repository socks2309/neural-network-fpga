#!/usr/bin/env bash

##############################################################################
#                                                                            #
#                  Run this script to automate everything                    #
#                                                                            #
##############################################################################

# Read Bambu path
read -p "Enter Bambu AppImage as path (Eg. /home/{user}/bambu-0.9.7.AppImage): " bambu_path 

# Run ANN model script on Python
python3 ~/neural-network-fpga/main-branch/src/python/ann.py

# Removed previously ran Bambu HLS files
cd ~/neural-network-fpga/main-branch/src/hdl/
if test -f network.v; then
    mv network_tb.v ../
    cd ..
    rm -r hdl
    mkdir hdl && mv network_tb.v hdl/
fi

# Calling Bambu tool
cd ~/neural-network-fpga/main-branch/src/hdl/ && $bambu_path ~/neural-network-fpga/main-branch/src/c/ann.c --top-fname=network --fp-subnormal -lm

# Icarus Verilog compilation 
iverilog -o result network.v network_tb.v
vvp result

# Viewing C result and Keras results
cd ~/neural-network-fpga/main-branch/src/c/ && gcc test_out.c -lm
./a.out