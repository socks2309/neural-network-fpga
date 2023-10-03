# Neural Network on FPGA
This project aims to implement a neural network on an FPGA with an open-source HLS tool called "Bambu HLS". The first phase of this project aims to implement a simple ANN and then work our way up to implementing CNNs in the subsequent phases. 

## File descriptions
### 1.  ieee_conversions Directory
This directory contains the mechanism by which a floating point number is converted to its respective IEEE 754 notation. The `floating_point_conversion.c` file contains code that converts a floating point number to its corresponding binary string and then saves it to a text file. The `mem_retrieval.c` file contains the code that retrieves the hexadecimal representation of the input floating point number stored in the memory.

### 2. src Directory
This directory is the actual source of this project. The `script.sh` in the main directory automates the entire flow of the code present in this directory. 
- The **python** folder has the `ann.py` script that implements the ANN model and extracts the parameters located in the **c** folder. 
- The **c** folder has the `C` files that will be the input to the HLS tool and will subsequently be converted to its corresponding Verilog file.
- The **hdl** folder is where the HLS tool will store the design code, testbench, and the `.mem` files. It will also provide a `shell` script and a `tcl` script to automate the process of creating the project in Vivado (This can be changed in Bambu options). Do not delete the `network_tb.v` as it is not automatically generated.

## 3. script.sh
This `shell` script automates everything for you so you don't have to individually compile and run everything.

## Tools used
This project utilizes a tool called [**Bambu HLS**](https://panda.deib.polimi.it/?page_id=31) developed by the System Architectures Group at Politecnico di Milano, Italy. Please make sure to download the tool (download the AppImage) and place it preferably in your `home` directory (`echo ~` to find out yours) or follow the instructions to build it from the source given on their website which would automatically add Bambu to your path. 

## How to run this project?
1. Install [`git`](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
2. Install Icarus Verilog and Gtkwave (`sudo apt-get install iverilog gtkwave`) 
3. Install the latest version of Python
4. Install GCC
5. Run the following in your terminal :
- `cd ~`
- `git clone --branch mk2 https://github.com/socks2309/neural-network-fpga.git ~/neural-network-fpga/main-branch/` 
- `cd /neural-network-fpga/main-branch/` 
- `chmod +x ./script.sh`
- `./script.sh`
