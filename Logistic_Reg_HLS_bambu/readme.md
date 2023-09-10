# Description
This directory contains Synthesized code generated using a open source high level synthesis tool called "Bambu". The Synthesis tool takes the input as C code and converted it to synthesizable Verilog code.
The generated code is tested in Vivado 2016 and also testbench is simulated using ICARUS VERILOG. There are some bug while running using Vivado XSIM simulator and simulatiion failed but ICARUS VERILOG simulator gives
expected result. Synthesis of translated Verilog code is successfully performed using VIVADO. <br />
# Minimal Command used to Synthesize the C code
>> bambu logistic_regression.c --top-fname main <br/>
>> iverilog -o test main.v main_tb.v <br />
>> vvp test </br>
>> gtkwave logreg.vcd
</br>
observe output on GTKWave. When done_port = 1 , the simulator gives expected output. For detailed description regarding Bambu HLS tool please visit https://panda.deib.polimi.it/?page_id=31
or  https://github.com/ferrandi/PandA-bambu.
<br />
Make sure that ICARUS VERILOG, GTKWave is installed in your system. Also follow the tutorial of Bambu and instruction to install into your machine.

