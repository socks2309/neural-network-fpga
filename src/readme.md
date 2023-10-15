# ANN using floating point arithmetic

In this directory, the `ann.py` script in the **python** folder implements a simple ANN and the calculations are done in floating point. After implementing and compiling the ANN model, the script extracts the parameters and features to the corresponding `C` files in the **c** directory

## Changing test input

In the `ann.c` file in the **c** folder, change the global variable `test_sample` to any value under 100 to test your desired test sample. The test samples are loaded into a `C` file called `features.c`.
