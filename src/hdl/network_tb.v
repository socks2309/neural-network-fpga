module network_tb();
    reg clock, reset, start_port;
    wire done_port;
    wire [31:0] return_port;
    // IN
    network inst(.clock(clock), .reset(reset), .start_port(start_port), .done_port(done_port), .return_port(return_port));
    initial begin
        $dumpfile("ann.vcd");
        $dumpvars;
        clock = 0; reset = 0; start_port = 0;
        #50 reset = 1;
        #50 start_port = 1;
        #50000;
        // if (done_port == 1)
        $display("Predicted label is (Verilog) : %d", return_port);
        $finish;
    end
    always #10 clock = ~clock;
endmodule