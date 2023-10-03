create_clock -period 10 -name clock [get_ports clock]
set_max_delay 10 -from [all_inputs] -to [all_outputs]
set_max_delay 10 -from [all_inputs] -to [all_registers]
set_max_delay 10 -from [all_registers] -to [all_outputs]
