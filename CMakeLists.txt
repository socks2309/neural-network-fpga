cmake_minimum_required(VERSION 3.10)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

project(nn_fpga VERSION 1.0)

add_executable(
    c_output debug/test_out.c 
    src/c/ann.c
    src/c/features.c
    src/c/keras_predicted_features.c
    src/c/params_layer0.c
    src/c/params_layer1.c
    src/c/params_layer2.c
    src/c/test_features.c)
add_compile_options(-lm)
target_include_directories(c_output PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
