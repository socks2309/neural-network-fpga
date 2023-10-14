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

# Building C files
cd ~/neural-network-fpga/main-branch/
if [ -d "build" ]; then
	rm -rf build
	mkdir build && cd ~/neural-network-fpga/main-branch/build/
	cmake ../ -G "Unix Makefiles" -DCMAKE_BUILD_TYPE:STRING=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE && make
fi
mkdir build && cd ~/neural-network-fpga/main-branch/build/
cmake ../ -G "Unix Makefiles" -DCMAKE_BUILD_TYPE:STRING=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE && make

# Icarus Verilog compilation
cd ~/neural-network-fpga/main-branch/src/hdl/
iverilog -o result network.v network_tb.v
vvp result

# Viewing C result
cd ~/neural-network-fpga/main-branch/build/ && ./c_output