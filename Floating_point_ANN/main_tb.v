module main_tb();
 reg clock, reset, start_port;
 wire  done_port;
 wire [31:0] return_port;
  // IN
  main DUT(clock,reset,start_port,done_port,return_port);
  
  initial begin
  $dumpfile("ann.vcd");
  $dumpvars;
  clock=0;reset=0;start_port=0;
  #50;
  reset=1;
  #50;
  start_port=1;
  #20;
  //$monitor("Completion Status:%d, Predicted label(HDL): %d",done_port,return_port);
  #70000;
  //if(done_port==1)
  $display("Predicted label (Verilog): %d",return_port);
  $finish;
  end
  always #5 clock=~clock;
  endmodule
