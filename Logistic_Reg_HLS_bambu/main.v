// 
// Politecnico di Milano
// Code created using PandA - Version: PandA 0.9.7 - Revision 8b59b7ac7b9ab30cd20960921ab47ea5009163f1-main - Date 2023-09-10T19:11:24
// /tmp/.mount_bambu-pmJpJY/usr/bin/bambu executed with: /tmp/.mount_bambu-pmJpJY/usr/bin/bambu --top-fname=main logistic_regression.c 
// 
// Send any bug to: panda-info@polimi.it
// ************************************************************************
// The following text holds for all the components tagged with PANDA_LGPLv3.
// They are all part of the BAMBU/PANDA IP LIBRARY.
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with the PandA framework; see the files COPYING.LIB
// If not, see <http://www.gnu.org/licenses/>.
// ************************************************************************

`ifdef __ICARUS__
  `define _SIM_HAVE_CLOG2
`endif
`ifdef VERILATOR
  `define _SIM_HAVE_CLOG2
`endif
`ifdef MODEL_TECH
  `define _SIM_HAVE_CLOG2
`endif
`ifdef VCS
  `define _SIM_HAVE_CLOG2
`endif
`ifdef NCVERILOG
  `define _SIM_HAVE_CLOG2
`endif
`ifdef XILINX_SIMULATOR
  `define _SIM_HAVE_CLOG2
`endif
`ifdef XILINX_ISIM
  `define _SIM_HAVE_CLOG2
`endif

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>, Christian Pilato <christian.pilato@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module constant_value(out1);
  parameter BITSIZE_out1=1,
    value=1'b0;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = value;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module register_SE(clock,
  reset,
  in1,
  wenable,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input clock;
  input reset;
  input [BITSIZE_in1-1:0] in1;
  input wenable;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  reg [BITSIZE_out1-1:0] reg_out1 =0;
  assign out1 = reg_out1;
  always @(posedge clock)
    if (wenable)
      reg_out1 <= in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module register_STD(clock,
  reset,
  in1,
  wenable,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input clock;
  input reset;
  input [BITSIZE_in1-1:0] in1;
  input wenable;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  reg [BITSIZE_out1-1:0] reg_out1 =0;
  assign out1 = reg_out1;
  always @(posedge clock)
    reg_out1 <= in1;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ADDRESS_DECODING_LOGIC_NN(clock,
  reset,
  in1,
  in2,
  in3,
  out1,
  sel_LOAD,
  sel_STORE,
  S_oe_ram,
  S_we_ram,
  S_addr_ram,
  S_Wdata_ram,
  Sin_Rdata_ram,
  Sout_Rdata_ram,
  S_data_ram_size,
  Sin_DataRdy,
  Sout_DataRdy,
  proxy_in1,
  proxy_in2,
  proxy_in3,
  proxy_sel_LOAD,
  proxy_sel_STORE,
  proxy_out1,
  dout_a,
  dout_b,
  memory_addr_a,
  memory_addr_b,
  din_value_aggregated_swapped,
  be_swapped,
  bram_write);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_in2=1, PORTSIZE_in2=2,
    BITSIZE_in3=1, PORTSIZE_in3=2,
    BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2,
    BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2,
    BITSIZE_out1=1, PORTSIZE_out1=2,
    BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2,
    BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2,
    BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2,
    BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2,
    BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2,
    BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2,
    BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2,
    BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2,
    BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2,
    address_space_begin=0,
    address_space_rangesize=4,
    BUS_PIPELINED=1,
    BRAM_BITSIZE=32,
    PRIVATE_MEMORY=0,
    USE_SPARSE_MEMORY=1,
    HIGH_LATENCY=0,
    BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2,
    BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2,
    BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2,
    BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2,
    BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2,
    BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2,
    BITSIZE_dout_a=1, PORTSIZE_dout_a=2,
    BITSIZE_dout_b=1, PORTSIZE_dout_b=2,
    BITSIZE_memory_addr_a=1, PORTSIZE_memory_addr_a=2,
    BITSIZE_memory_addr_b=1, PORTSIZE_memory_addr_b=2,
    BITSIZE_din_value_aggregated_swapped=1, PORTSIZE_din_value_aggregated_swapped=2,
    BITSIZE_be_swapped=1, PORTSIZE_be_swapped=2,
    BITSIZE_bram_write=1, PORTSIZE_bram_write=2,
    nbit_read_addr=32,
    n_byte_on_databus=4,
    n_mem_elements=4,
    max_n_reads=2,
    max_n_writes=2,
    max_n_rw=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  input [(PORTSIZE_dout_a*BITSIZE_dout_a)+(-1):0] dout_a;
  input [(PORTSIZE_dout_b*BITSIZE_dout_b)+(-1):0] dout_b;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  output [(PORTSIZE_memory_addr_a*BITSIZE_memory_addr_a)+(-1):0] memory_addr_a;
  output [(PORTSIZE_memory_addr_b*BITSIZE_memory_addr_b)+(-1):0] memory_addr_b;
  output [(PORTSIZE_din_value_aggregated_swapped*BITSIZE_din_value_aggregated_swapped)+(-1):0] din_value_aggregated_swapped;
  output [(PORTSIZE_be_swapped*BITSIZE_be_swapped)+(-1):0] be_swapped;
  output [PORTSIZE_bram_write-1:0] bram_write;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_addr = BITSIZE_S_addr_ram/*n_bytes ==  1 ? 1 : $clog2(n_bytes)*/;
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 1 : $clog2(n_byte_on_databus);
    parameter nbits_address_space_rangesize = $clog2(address_space_rangesize);
  `else
    parameter nbit_addr = BITSIZE_S_addr_ram/*n_bytes ==  1 ? 1 : log2(n_bytes)*/;
    parameter nbits_address_space_rangesize = log2(address_space_rangesize);
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 1 : log2(n_byte_on_databus);
  `endif
   parameter memory_bitsize = 2*BRAM_BITSIZE;
  
  function [n_byte_on_databus*max_n_writes-1:0] CONV;
    input [n_byte_on_databus*max_n_writes-1:0] po2;
  begin
    case (po2)
      1:CONV=(1<<1)-1;
      2:CONV=(1<<2)-1;
      4:CONV=(1<<4)-1;
      8:CONV=(1<<8)-1;
      16:CONV=(1<<16)-1;
      32:CONV=(1<<32)-1;
      default:CONV=-1;
    endcase
  end
  endfunction
  
  wire [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] tmp_addr;
  wire [n_byte_on_databus*max_n_writes-1:0] conv_in;
  wire [n_byte_on_databus*max_n_writes-1:0] conv_out;
  wire [PORTSIZE_S_addr_ram-1:0] cs;
  wire [PORTSIZE_S_oe_ram-1:0] oe_ram_cs;
  wire [PORTSIZE_S_we_ram-1:0] we_ram_cs;
  wire [nbit_addr*max_n_rw-1:0] relative_addr;
  wire [memory_bitsize*max_n_writes-1:0] din_value_aggregated;
  wire [memory_bitsize*PORTSIZE_S_Wdata_ram-1:0] S_Wdata_ram_int;
  wire [memory_bitsize*max_n_reads-1:0] out1_shifted;
  wire [memory_bitsize*max_n_reads-1:0] dout;
  wire [nbits_byte_offset*max_n_rw-1:0] byte_offset;
  wire [n_byte_on_databus*max_n_writes-1:0] be;
  
  reg [PORTSIZE_S_we_ram-1:0] we_ram_cs_delayed =0;
  reg [PORTSIZE_S_oe_ram-1:0] oe_ram_cs_delayed =0;
  reg [PORTSIZE_S_oe_ram-1:0] oe_ram_cs_delayed_registered =0;
  reg [PORTSIZE_S_oe_ram-1:0] oe_ram_cs_delayed_registered1 =0;
  reg [max_n_reads-1:0] delayed_swapped_bit =0;
  reg [max_n_reads-1:0] delayed_swapped_bit_registered =0;
  reg [max_n_reads-1:0] delayed_swapped_bit_registered1 =0;
  reg [nbits_byte_offset*max_n_reads-1:0] delayed_byte_offset =0;
  reg [nbits_byte_offset*max_n_reads-1:0] delayed_byte_offset_registered =0;
  reg [nbits_byte_offset*max_n_reads-1:0] delayed_byte_offset_registered1 =0;
  
  generate
  genvar ind2;
  for (ind2=0; ind2<PORTSIZE_in2; ind2=ind2+1)
    begin : Lind2
      assign tmp_addr[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2] = (proxy_sel_LOAD[ind2]||proxy_sel_STORE[ind2]) ? proxy_in2[(ind2+1)*BITSIZE_proxy_in2-1:ind2*BITSIZE_proxy_in2] : in2[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2];
    end
  endgenerate
  
  generate
  genvar i2;
    for (i2=0;i2<max_n_reads;i2=i2+1)
    begin : L_copy
        assign dout[(memory_bitsize/2)+memory_bitsize*i2-1:memory_bitsize*i2] = delayed_swapped_bit[i2] ? dout_a[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2] : dout_b[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2];
        assign dout[memory_bitsize*(i2+1)-1:memory_bitsize*i2+(memory_bitsize/2)] = delayed_swapped_bit[i2] ? dout_b[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2] : dout_a[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2];
        always @(posedge clock)
        begin
          if(HIGH_LATENCY == 0)
            delayed_swapped_bit[i2] <= !relative_addr[nbits_byte_offset+i2*nbit_addr-1];
          else if(HIGH_LATENCY == 1)
          begin
            delayed_swapped_bit_registered[i2] <= !relative_addr[nbits_byte_offset+i2*nbit_addr-1];
            delayed_swapped_bit[i2] <= delayed_swapped_bit_registered[i2];
          end
          else
          begin
            delayed_swapped_bit_registered1[i2] <= !relative_addr[nbits_byte_offset+i2*nbit_addr-1];
            delayed_swapped_bit_registered[i2] <= delayed_swapped_bit_registered1[i2];
            delayed_swapped_bit[i2] <= delayed_swapped_bit_registered[i2];
          end
        end
    end
  endgenerate
  
  generate
  genvar i3;
    for (i3=0; i3<PORTSIZE_S_addr_ram; i3=i3+1)
    begin : L3
      if(PRIVATE_MEMORY==0 && USE_SPARSE_MEMORY==0)
        assign cs[i3] = (S_addr_ram[(i3+1)*BITSIZE_S_addr_ram-1:i3*BITSIZE_S_addr_ram] >= (address_space_begin)) && (S_addr_ram[(i3+1)*BITSIZE_S_addr_ram-1:i3*BITSIZE_S_addr_ram] < (address_space_begin+address_space_rangesize));
      else if(PRIVATE_MEMORY==0)
        assign cs[i3] = S_addr_ram[(i3+1)*BITSIZE_S_addr_ram-1:i3*BITSIZE_S_addr_ram+nbits_address_space_rangesize] == address_space_begin[nbit_addr-1:nbits_address_space_rangesize];
      else
        assign cs[i3] = 1'b0;
    end
  endgenerate
  
  generate
  genvar i4;
    for (i4=0; i4<PORTSIZE_S_oe_ram; i4=i4+1)
    begin : L4
      assign oe_ram_cs[i4] = S_oe_ram[i4] & cs[i4];
    end
  endgenerate
  
  generate
  genvar i5;
    for (i5=0; i5<PORTSIZE_S_we_ram; i5=i5+1)
    begin : L5
      assign we_ram_cs[i5] = S_we_ram[i5] & cs[i5];
    end
  endgenerate
  
  generate
  genvar i6;
    for (i6=0; i6<max_n_rw; i6=i6+1)
    begin : L6
      if(PRIVATE_MEMORY==0 && USE_SPARSE_MEMORY==0 && i6< PORTSIZE_S_addr_ram)
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = ((i6 < max_n_writes && (sel_STORE[i6]==1'b1 || proxy_sel_STORE[i6]==1'b1)) || (i6 < max_n_reads && (sel_LOAD[i6]==1'b1 || proxy_sel_LOAD[i6]==1'b1))) ? tmp_addr[(i6+1)*BITSIZE_in2-1:i6*BITSIZE_in2]-address_space_begin: S_addr_ram[(i6+1)*BITSIZE_S_addr_ram-1:i6*BITSIZE_S_addr_ram]-address_space_begin;
      else if(PRIVATE_MEMORY==0 && i6< PORTSIZE_S_addr_ram)
        assign relative_addr[(i6)*nbit_addr+nbits_address_space_rangesize-1:i6*nbit_addr] = ((i6 < max_n_writes && (sel_STORE[i6]==1'b1 || proxy_sel_STORE[i6]==1'b1)) || (i6 < max_n_reads && (sel_LOAD[i6]==1'b1 || proxy_sel_LOAD[i6]==1'b1))) ? tmp_addr[(i6)*BITSIZE_in2+nbits_address_space_rangesize-1:i6*BITSIZE_in2] : S_addr_ram[(i6)*BITSIZE_S_addr_ram+nbits_address_space_rangesize-1:i6*BITSIZE_S_addr_ram];
      else if(USE_SPARSE_MEMORY==1)
        assign relative_addr[(i6)*nbit_addr+nbits_address_space_rangesize-1:i6*nbit_addr] = tmp_addr[(i6)*BITSIZE_in2+nbits_address_space_rangesize-1:i6*BITSIZE_in2];
      else
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6+1)*BITSIZE_in2-1:i6*BITSIZE_in2]-address_space_begin;
    end
  endgenerate
  
  generate
  genvar i7;
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_A
      if (n_mem_elements==1)
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = !relative_addr[nbits_byte_offset+i7*nbit_addr-1] ? relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr] : (relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr-1]+ 1'b1) >> 1;
    end
  endgenerate
  
  generate
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_B
      if (n_mem_elements==1)
        assign memory_addr_b[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_b[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = !relative_addr[nbits_byte_offset+i7*nbit_addr-1] ? (relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr-1] + 1'b1) >> 1 : relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr];
    end
  endgenerate
  
  generate
  genvar i8;
    for (i8=0; i8<max_n_rw; i8=i8+1)
    begin : L8
      if (n_byte_on_databus==2)
        assign byte_offset[(i8+1)*nbits_byte_offset-1:i8*nbits_byte_offset] = {nbits_byte_offset{1'b0}};
      else
        assign byte_offset[(i8+1)*nbits_byte_offset-1:i8*nbits_byte_offset] = {1'b0, relative_addr[nbits_byte_offset+i8*nbit_addr-2:i8*nbit_addr]};
    end
  endgenerate
  
  generate
  genvar i9, i10;
    for (i9=0; i9<max_n_writes; i9=i9+1)
    begin : byte_enable
      if(PRIVATE_MEMORY==0 && i9 < PORTSIZE_S_data_ram_size)
      begin
        assign conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = proxy_sel_STORE[i9] ? proxy_in3[BITSIZE_proxy_in3+BITSIZE_proxy_in3*i9-1:3+BITSIZE_proxy_in3*i9] : (sel_STORE[i9] ? in3[BITSIZE_in3+BITSIZE_in3*i9-1:3+BITSIZE_in3*i9] : S_data_ram_size[BITSIZE_S_data_ram_size+BITSIZE_S_data_ram_size*i9-1:3+BITSIZE_S_data_ram_size*i9]);
        assign conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = CONV(conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus]);
        assign be[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] << byte_offset[(i9+1)*nbits_byte_offset-1:i9*nbits_byte_offset];
      end
      else
      begin
        assign conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = proxy_sel_STORE[i9] ? proxy_in3[BITSIZE_proxy_in3+BITSIZE_proxy_in3*i9-1:3+BITSIZE_proxy_in3*i9] : in3[BITSIZE_in3+BITSIZE_in3*i9-1:3+BITSIZE_in3*i9];
        assign conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = CONV(conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus]);
        assign be[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] << byte_offset[(i9+1)*nbits_byte_offset-1:i9*nbits_byte_offset];
      end
    end
  endgenerate
  
  generate
    for (i9=0; i9<max_n_writes; i9=i9+1)
    begin : L9_swapped
      for (i10=0; i10<n_byte_on_databus/2; i10=i10+1)
      begin  : byte_enable_swapped
        assign be_swapped[i10+i9*n_byte_on_databus] = !relative_addr[nbits_byte_offset+i9*nbit_addr-1] ? be[i10+i9*n_byte_on_databus] : be[i10+i9*n_byte_on_databus+n_byte_on_databus/2];
        assign be_swapped[i10+i9*n_byte_on_databus+n_byte_on_databus/2] =  !relative_addr[nbits_byte_offset+i9*nbit_addr-1] ? be[i10+i9*n_byte_on_databus+n_byte_on_databus/2] : be[i10+i9*n_byte_on_databus];
      end
    end
  endgenerate
    
  generate
  genvar i13;
    for (i13=0; i13<PORTSIZE_S_Wdata_ram; i13=i13+1)
    begin : L13
      if (BITSIZE_S_Wdata_ram < memory_bitsize)
        assign S_Wdata_ram_int[memory_bitsize*(i13+1)-1:memory_bitsize*i13] = {{memory_bitsize-BITSIZE_S_Wdata_ram{1'b0}}, S_Wdata_ram[(i13+1)*BITSIZE_S_Wdata_ram-1:BITSIZE_S_Wdata_ram*i13]};
      else
        assign S_Wdata_ram_int[memory_bitsize*(i13+1)-1:memory_bitsize*i13] = S_Wdata_ram[memory_bitsize+BITSIZE_S_Wdata_ram*i13-1:BITSIZE_S_Wdata_ram*i13];
    end
  endgenerate
  
  generate
  genvar i14;
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14
      if(PRIVATE_MEMORY==0 && i14 < PORTSIZE_S_Wdata_ram)
        assign din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8 : (sel_STORE[i14] ? in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8 : S_Wdata_ram_int[memory_bitsize*(i14+1)-1:memory_bitsize*i14] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8);
      else
        assign din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8 : in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8;
    end
  endgenerate
  
  generate
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14_swapped
      assign din_value_aggregated_swapped[(i14)*memory_bitsize+memory_bitsize/2-1:i14*memory_bitsize] = !relative_addr[nbits_byte_offset+i14*nbit_addr-1] ? din_value_aggregated[(i14)*memory_bitsize+memory_bitsize/2-1:i14*memory_bitsize] : din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize+memory_bitsize/2];
      assign din_value_aggregated_swapped[(i14+1)*memory_bitsize-1:i14*memory_bitsize+memory_bitsize/2] = !relative_addr[nbits_byte_offset+i14*nbit_addr-1] ?  din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize+memory_bitsize/2] : din_value_aggregated[(i14)*memory_bitsize+memory_bitsize/2-1:i14*memory_bitsize];
    end
  endgenerate
  
  generate
  genvar i15;
    for (i15=0; i15<max_n_reads; i15=i15+1)
    begin : L15
      assign out1_shifted[(i15+1)*memory_bitsize-1:i15*memory_bitsize] = dout[(i15+1)*memory_bitsize-1:i15*memory_bitsize] >> delayed_byte_offset[(i15+1)*nbits_byte_offset-1:i15*nbits_byte_offset]*8;
    end
  endgenerate
  
  generate
  genvar i20;
    for (i20=0; i20<max_n_reads; i20=i20+1)
    begin : L20
      assign out1[(i20+1)*BITSIZE_out1-1:i20*BITSIZE_out1] = out1_shifted[i20*memory_bitsize+BITSIZE_out1-1:i20*memory_bitsize];
      assign proxy_out1[(i20+1)*BITSIZE_proxy_out1-1:i20*BITSIZE_proxy_out1] = out1_shifted[i20*memory_bitsize+BITSIZE_proxy_out1-1:i20*memory_bitsize];
    end
  endgenerate
  
  generate
  genvar i16;
    for (i16=0; i16<PORTSIZE_S_oe_ram; i16=i16+1)
    begin : L16
      always @(posedge clock )
      begin
        if(reset == 1'b0)
          begin
            oe_ram_cs_delayed[i16] <= 1'b0;
            if(HIGH_LATENCY != 0) oe_ram_cs_delayed_registered[i16] <= 1'b0;
            if(HIGH_LATENCY == 2) oe_ram_cs_delayed_registered1[i16] <= 1'b0;
          end
        else
          if(HIGH_LATENCY == 0)
          begin
            oe_ram_cs_delayed[i16] <= oe_ram_cs[i16] & (!oe_ram_cs_delayed[i16] | BUS_PIPELINED);
          end
          else if(HIGH_LATENCY == 1)
          begin
            oe_ram_cs_delayed_registered[i16] <= oe_ram_cs[i16] & ((!oe_ram_cs_delayed_registered[i16] & !oe_ram_cs_delayed[i16]) | BUS_PIPELINED);
            oe_ram_cs_delayed[i16] <= oe_ram_cs_delayed_registered[i16];
          end
          else
          begin
            oe_ram_cs_delayed_registered1[i16] <= oe_ram_cs[i16] & ((!oe_ram_cs_delayed_registered1[i16] & !oe_ram_cs_delayed_registered[i16] & !oe_ram_cs_delayed[i16]) | BUS_PIPELINED);
            oe_ram_cs_delayed_registered[i16] <= oe_ram_cs_delayed_registered1[i16];
            oe_ram_cs_delayed[i16] <= oe_ram_cs_delayed_registered[i16];
          end
        end
      end
  endgenerate
  
  always @(posedge clock)
  begin
    if(HIGH_LATENCY == 0)
      delayed_byte_offset <= byte_offset[nbits_byte_offset*max_n_reads-1:0];
    else if(HIGH_LATENCY == 1)
    begin
      delayed_byte_offset_registered <= byte_offset[nbits_byte_offset*max_n_reads-1:0];
      delayed_byte_offset <= delayed_byte_offset_registered;
    end
    else
    begin
      delayed_byte_offset_registered1 <= byte_offset[nbits_byte_offset*max_n_reads-1:0];
      delayed_byte_offset_registered <= delayed_byte_offset_registered1;
      delayed_byte_offset <= delayed_byte_offset_registered;
    end
  end
  
  
  generate
  genvar i17;
    for (i17=0; i17<PORTSIZE_S_we_ram; i17=i17+1)
    begin : L17
      always @(posedge clock )
      begin
        if(reset == 1'b0)
          we_ram_cs_delayed[i17] <= 1'b0;
        else
          we_ram_cs_delayed[i17] <= we_ram_cs[i17] & !we_ram_cs_delayed[i17];
      end
    end
  endgenerate
  
  generate
  genvar i18;
    for (i18=0; i18<PORTSIZE_Sout_Rdata_ram; i18=i18+1)
    begin : L18
      if(PRIVATE_MEMORY==1)
        assign Sout_Rdata_ram[(i18+1)*BITSIZE_Sout_Rdata_ram-1:i18*BITSIZE_Sout_Rdata_ram] = Sin_Rdata_ram[(i18+1)*BITSIZE_Sin_Rdata_ram-1:i18*BITSIZE_Sin_Rdata_ram];
      else if (BITSIZE_Sout_Rdata_ram <= memory_bitsize)
        assign Sout_Rdata_ram[(i18+1)*BITSIZE_Sout_Rdata_ram-1:i18*BITSIZE_Sout_Rdata_ram] = oe_ram_cs_delayed[i18] ? out1_shifted[BITSIZE_Sout_Rdata_ram+i18*memory_bitsize-1:i18*memory_bitsize] : Sin_Rdata_ram[(i18+1)*BITSIZE_Sin_Rdata_ram-1:i18*BITSIZE_Sin_Rdata_ram];
      else
        assign Sout_Rdata_ram[(i18+1)*BITSIZE_Sout_Rdata_ram-1:i18*BITSIZE_Sout_Rdata_ram] = oe_ram_cs_delayed[i18] ? {{BITSIZE_S_Wdata_ram-memory_bitsize{1'b0}}, out1_shifted[(i18+1)*memory_bitsize-1:i18*memory_bitsize]} : Sin_Rdata_ram[(i18+1)*BITSIZE_Sin_Rdata_ram-1:i18*BITSIZE_Sin_Rdata_ram];
    end
  endgenerate
  
  generate
  genvar i19;
    for (i19=0; i19<PORTSIZE_Sout_DataRdy; i19=i19+1)
    begin : L19
      if(PRIVATE_MEMORY==0)
        assign Sout_DataRdy[i19] = (i19 < PORTSIZE_S_oe_ram && oe_ram_cs_delayed[i19]) | Sin_DataRdy[i19] | (i19 < PORTSIZE_S_we_ram && we_ram_cs_delayed[i19]);
      else
        assign Sout_DataRdy[i19] = Sin_DataRdy[i19];
    end
  endgenerate
  
  generate
  genvar i21;
    for (i21=0; i21<PORTSIZE_bram_write; i21=i21+1)
    begin : L21
      if(i21 < PORTSIZE_S_we_ram)
        assign bram_write[i21] = (sel_STORE[i21] || proxy_sel_STORE[i21] || we_ram_cs[i21]);
      else
        assign bram_write[i21] = (sel_STORE[i21] || proxy_sel_STORE[i21]);
    end
    endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module BRAM_MEMORY_CORE_SMALL(clock,
  bram_write0,
  bram_write1,
  memory_addr_a,
  memory_addr_b,
  din_value_aggregated,
  be,
  dout_a,
  dout_b);
  parameter BITSIZE_dout_a=1,
    BITSIZE_dout_b=1,
    BITSIZE_memory_addr_a=1,
    BITSIZE_memory_addr_b=1,
    BITSIZE_din_value_aggregated=1,
    BITSIZE_be=1,
    MEMORY_INIT_file="array.mem",
    n_byte_on_databus=4,
    n_mem_elements=4,
    n_bytes=4,
    HIGH_LATENCY=0;
  // IN
  input clock;
  input bram_write0;
  input bram_write1;
  input [BITSIZE_memory_addr_a-1:0] memory_addr_a;
  input [BITSIZE_memory_addr_b-1:0] memory_addr_b;
  input [BITSIZE_din_value_aggregated-1:0] din_value_aggregated;
  input [BITSIZE_be-1:0] be;
  // OUT
  output [BITSIZE_dout_a-1:0] dout_a;
  output [BITSIZE_dout_b-1:0] dout_b;
  
  reg bram_write01 =0;
  reg bram_write11 =0;
  reg [BITSIZE_memory_addr_a-1:0] memory_addr_a1 =0;
  reg [BITSIZE_memory_addr_b-1:0] memory_addr_b1 =0;
  reg [BITSIZE_be-1:0] be1 =0;
  reg [BITSIZE_din_value_aggregated-1:0] din_value_aggregated1 =0;
  reg [(n_byte_on_databus)*8-1:0] dout_a_tmp =0;
  reg [(n_byte_on_databus)*8-1:0] dout_b_tmp =0;
  reg [(n_byte_on_databus)*8-1:0] dout_a_registered =0;
  reg [(n_byte_on_databus)*8-1:0] dout_b_registered =0;
  reg [(n_byte_on_databus)*8-1:0] memory [0:n_mem_elements-1]/* synthesis syn_ramstyle = "no_rw_check" */ ;
  
  generate
    if(HIGH_LATENCY==2)
    begin
      always @ (posedge clock)
      begin
         memory_addr_a1 <= memory_addr_a;
         memory_addr_b1 <= memory_addr_b;
         bram_write01 <= bram_write0;
         bram_write11 <= bram_write1;
         be1 <= be;
         din_value_aggregated1 <= din_value_aggregated;
      end
    end
  endgenerate
  
  assign dout_a = dout_a_tmp;
  assign dout_b = dout_b_tmp;
  initial
  begin
    $readmemb(MEMORY_INIT_file, memory, 0, n_mem_elements-1);
  end
  
  always @(posedge clock)
  begin
    if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
    begin
      if (bram_write0)
      begin : L11_write
        integer i11;
        for (i11=0; i11<n_byte_on_databus; i11=i11+1)
        begin
          if(be[i11])
            memory[memory_addr_a][i11*8+:8] <= din_value_aggregated[i11*8+:8];
        end
      end
    end
    else
    begin
      if (bram_write01)
      begin : L11_write1
        integer i11;
        for (i11=0; i11<n_byte_on_databus; i11=i11+1)
        begin
          if(be1[i11])
            memory[memory_addr_a1][i11*8+:8] <= din_value_aggregated1[i11*8+:8];
        end
      end
    end
    if(HIGH_LATENCY == 0)
      dout_a_tmp <= memory[memory_addr_a];
    else if(HIGH_LATENCY == 1)
    begin
      dout_a_registered <= memory[memory_addr_a];
      dout_a_tmp <= dout_a_registered;
    end
    else
    begin
      dout_a_registered <= memory[memory_addr_a1];
      dout_a_tmp <= dout_a_registered;
    end
    if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
    begin
      if (bram_write1)
      begin : L22_write
        integer i22;
        for (i22=0; i22<n_byte_on_databus; i22=i22+1)
        begin
          if(be[i22+n_byte_on_databus])
            memory[memory_addr_b][i22*8+:8] <= din_value_aggregated[(i22+n_byte_on_databus)*8+:8];
        end
      end
    end
    else
    begin
      if (bram_write11)
      begin : L22_write1
        integer i22;
        for (i22=0; i22<n_byte_on_databus; i22=i22+1)
        begin
          if(be1[i22+n_byte_on_databus])
            memory[memory_addr_b1][i22*8+:8] <= din_value_aggregated1[(i22+n_byte_on_databus)*8+:8];
        end
      end
    end
    if(HIGH_LATENCY == 0)
      dout_b_tmp <= memory[memory_addr_b];
    else if(HIGH_LATENCY == 1)
    begin
      dout_b_registered <= memory[memory_addr_b];
      dout_b_tmp <= dout_b_registered;
    end
    else
    begin
      dout_b_registered <= memory[memory_addr_b1];
      dout_b_tmp <= dout_b_registered;
    end
  end

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module TRUE_DUAL_PORT_BYTE_ENABLING_RAM(clock,
  bram_write0,
  bram_write1,
  memory_addr_a,
  memory_addr_b,
  din_value_aggregated,
  be,
  dout_a,
  dout_b);
  parameter BITSIZE_dout_a=1,
    BITSIZE_dout_b=1,
    BITSIZE_memory_addr_a=1,
    BITSIZE_memory_addr_b=1,
    BITSIZE_din_value_aggregated=1,
    BITSIZE_be=1,
    MEMORY_INIT_file="array.mem",
    BRAM_BITSIZE=32,
    n_byte_on_databus=4,
    n_mem_elements=4,
    HIGH_LATENCY=0;
  // IN
  input clock;
  input bram_write0;
  input bram_write1;
  input [BITSIZE_memory_addr_a-1:0] memory_addr_a;
  input [BITSIZE_memory_addr_b-1:0] memory_addr_b;
  input [BITSIZE_din_value_aggregated-1:0] din_value_aggregated;
  input [BITSIZE_be-1:0] be;
  // OUT
  output [BITSIZE_dout_a-1:0] dout_a;
  output [BITSIZE_dout_b-1:0] dout_b;
  
  wire [n_byte_on_databus-1:0] we_a;
  wire [n_byte_on_databus-1:0] we_b;
  reg [n_byte_on_databus-1:0] we_a1 =0;
  reg [n_byte_on_databus-1:0] we_b1 =0;
  reg [BITSIZE_din_value_aggregated-1:0] din_value_aggregated1 =0;
  
  reg [BITSIZE_dout_a-1:0] dout_a =0;
  reg [BITSIZE_dout_a-1:0] dout_a_registered =0;
  reg [BITSIZE_dout_b-1:0] dout_b =0;
  reg [BITSIZE_dout_b-1:0] dout_b_registered =0;
  reg [BITSIZE_memory_addr_a-1:0] memory_addr_a1 =0;
  reg [BITSIZE_memory_addr_b-1:0] memory_addr_b1 =0;
  reg [BRAM_BITSIZE-1:0] memory [0:n_mem_elements-1] /* synthesis syn_ramstyle = "no_rw_check" */;
  
  initial
  begin
    $readmemb(MEMORY_INIT_file, memory, 0, n_mem_elements-1);
  end
  
  always @(posedge clock)
  begin
    if(HIGH_LATENCY==0)
    begin
      dout_a <= memory[memory_addr_a];
    end
    else if(HIGH_LATENCY==1)
    begin
      dout_a_registered <= memory[memory_addr_a];
      dout_a <= dout_a_registered;
    end
    else
    begin
      memory_addr_a1 <= memory_addr_a;
      we_a1 <= we_a;
      din_value_aggregated1 <= din_value_aggregated;
      dout_a_registered <= memory[memory_addr_a1];
      dout_a <= dout_a_registered;
    end
  end
  
  generate
  genvar i11;
    for (i11=0; i11<n_byte_on_databus; i11=i11+1)
    begin : L11_write_a
      always @(posedge clock)
      begin
        if(HIGH_LATENCY==0||HIGH_LATENCY==1)
        begin
          if(we_a[i11])
            memory[memory_addr_a][(i11+1)*8-1:i11*8] <= din_value_aggregated[(i11+1)*8-1:i11*8];
        end
        else
        begin
          if(we_a1[i11])
            memory[memory_addr_a1][(i11+1)*8-1:i11*8] <= din_value_aggregated1[(i11+1)*8-1:i11*8];
        end
      end
    end
  endgenerate
  
    always @(posedge clock)
    begin
      if(HIGH_LATENCY==0)
      begin
        dout_b <= memory[memory_addr_b];
      end
      else if(HIGH_LATENCY==1)
      begin
        dout_b_registered <= memory[memory_addr_b];
        dout_b <= dout_b_registered;
      end
      else
      begin
        memory_addr_b1 <= memory_addr_b;
        we_b1 <= we_b;
        dout_b_registered <= memory[memory_addr_b1];
        dout_b <= dout_b_registered;
      end
    end
    for (i11=0; i11<n_byte_on_databus; i11=i11+1)
    begin : L11_write_b
      always @(posedge clock)
      begin
        if(HIGH_LATENCY==0||HIGH_LATENCY==1)
        begin
          if(we_b[i11])
            memory[memory_addr_b][(i11+1)*8-1:i11*8] <= din_value_aggregated[(i11+1+n_byte_on_databus)*8-1:(i11+n_byte_on_databus)*8];
        end
        else
        begin
          if(we_b1[i11])
            memory[memory_addr_b1][(i11+1)*8-1:i11*8] <= din_value_aggregated1[(i11+1+n_byte_on_databus)*8-1:(i11+n_byte_on_databus)*8];
        end
      end
    end
  
  generate
  genvar i2_a;
    for (i2_a=0; i2_a<n_byte_on_databus; i2_a=i2_a+1)
    begin  : write_enable_a
      assign we_a[i2_a] = (bram_write0) && be[i2_a];
    end
  endgenerate
  
  generate
  genvar i2_b;
    for (i2_b=0; i2_b<n_byte_on_databus; i2_b=i2_b+1)
    begin  : write_enable_b
      assign we_b[i2_b] = (bram_write1) && be[i2_b+n_byte_on_databus];
    end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module BRAM_MEMORY_NN_CORE(clock,
  bram_write,
  memory_addr_a,
  din_value_aggregated_swapped,
  be_swapped,
  dout_a);
  parameter BITSIZE_bram_write=1, PORTSIZE_bram_write=2,
    BITSIZE_dout_a=1, PORTSIZE_dout_a=2,
    BITSIZE_memory_addr_a=1, PORTSIZE_memory_addr_a=2,
    BITSIZE_din_value_aggregated_swapped=1, PORTSIZE_din_value_aggregated_swapped=2,
    BITSIZE_be_swapped=1, PORTSIZE_be_swapped=2,
    MEMORY_INIT_file="array.mem",
    BRAM_BITSIZE=32,
    n_bytes=32,
    n_byte_on_databus=4,
    n_mem_elements=4,
    max_n_reads=2,
    max_n_writes=2,
    memory_offset=16,
    n_byte_on_databus_offset=2,
    HIGH_LATENCY=0;
  // IN
  input clock;
  input [PORTSIZE_bram_write-1:0] bram_write;
  input [(PORTSIZE_memory_addr_a*BITSIZE_memory_addr_a)+(-1):0] memory_addr_a;
  input [(PORTSIZE_din_value_aggregated_swapped*BITSIZE_din_value_aggregated_swapped)+(-1):0] din_value_aggregated_swapped;
  input [(PORTSIZE_be_swapped*BITSIZE_be_swapped)+(-1):0] be_swapped;
  // OUT
  output [(PORTSIZE_dout_a*BITSIZE_dout_a)+(-1):0] dout_a;
  
  generate
  if(n_mem_elements == 1)
  begin
    BRAM_MEMORY_CORE_SMALL #(.BITSIZE_memory_addr_a(BITSIZE_memory_addr_a), .BITSIZE_memory_addr_b(BITSIZE_memory_addr_a), .BITSIZE_din_value_aggregated((n_byte_on_databus)*8), .BITSIZE_be(n_byte_on_databus), .BITSIZE_dout_a((n_byte_on_databus/2)*8), .BITSIZE_dout_b((n_byte_on_databus/2)*8), .MEMORY_INIT_file(MEMORY_INIT_file), .n_byte_on_databus(n_byte_on_databus/2), .n_mem_elements(n_mem_elements), .n_bytes(n_bytes), .HIGH_LATENCY(HIGH_LATENCY)) BRAM_MEMORY_instance_small (.clock(clock), .bram_write0(bram_write[0]), .bram_write1(bram_write[1]), .memory_addr_a(memory_addr_a[BITSIZE_memory_addr_a-1:0]), .memory_addr_b(memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]), .din_value_aggregated({din_value_aggregated_swapped[2*BRAM_BITSIZE+(n_byte_on_databus/2)*8+memory_offset-1:2*BRAM_BITSIZE+memory_offset],din_value_aggregated_swapped[(n_byte_on_databus/2)*8+memory_offset-1:memory_offset]}), .be({be_swapped[n_byte_on_databus+n_byte_on_databus/2+n_byte_on_databus_offset-1:n_byte_on_databus+n_byte_on_databus_offset],be_swapped[n_byte_on_databus/2+n_byte_on_databus_offset-1:n_byte_on_databus_offset]}), .dout_a(dout_a[BRAM_BITSIZE-1:0]), .dout_b(dout_a[2*BRAM_BITSIZE-1:BRAM_BITSIZE]));
  end
  else
  begin
    TRUE_DUAL_PORT_BYTE_ENABLING_RAM #(.BITSIZE_memory_addr_a(BITSIZE_memory_addr_a), .BITSIZE_memory_addr_b(BITSIZE_memory_addr_a), .BITSIZE_din_value_aggregated((n_byte_on_databus)*8), .BITSIZE_be(n_byte_on_databus), .BITSIZE_dout_a((n_byte_on_databus/2)*8), .BITSIZE_dout_b((n_byte_on_databus/2)*8), .MEMORY_INIT_file(MEMORY_INIT_file), .BRAM_BITSIZE(BRAM_BITSIZE), .n_byte_on_databus(n_byte_on_databus/2), .n_mem_elements(n_mem_elements), .HIGH_LATENCY(HIGH_LATENCY)) TRUE_DUAL_PORT_BYTE_ENABLING_RAM_instance (.clock(clock), .bram_write0(bram_write[0]), .bram_write1(bram_write[1]), .memory_addr_a(memory_addr_a[BITSIZE_memory_addr_a-1:0]), .memory_addr_b(memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]), .din_value_aggregated({din_value_aggregated_swapped[2*BRAM_BITSIZE+(n_byte_on_databus/2)*8+memory_offset-1:2*BRAM_BITSIZE+memory_offset],din_value_aggregated_swapped[(n_byte_on_databus/2)*8+memory_offset-1:memory_offset]}), .be({be_swapped[n_byte_on_databus+n_byte_on_databus/2+n_byte_on_databus_offset-1:n_byte_on_databus+n_byte_on_databus_offset],be_swapped[n_byte_on_databus/2+n_byte_on_databus_offset-1:n_byte_on_databus_offset]}), .dout_a(dout_a[BRAM_BITSIZE-1:0]), .dout_b(dout_a[2*BRAM_BITSIZE-1:BRAM_BITSIZE]));
  end
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_BRAM_NN_SP(clock,
  reset,
  in1,
  in2,
  in3,
  out1,
  sel_LOAD,
  sel_STORE,
  S_oe_ram,
  S_we_ram,
  S_addr_ram,
  S_Wdata_ram,
  Sin_Rdata_ram,
  Sout_Rdata_ram,
  S_data_ram_size,
  Sin_DataRdy,
  Sout_DataRdy,
  proxy_in1,
  proxy_in2,
  proxy_in3,
  proxy_sel_LOAD,
  proxy_sel_STORE,
  proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_in2=1, PORTSIZE_in2=2,
    BITSIZE_in3=1, PORTSIZE_in3=2,
    BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2,
    BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2,
    BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2,
    BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2,
    BITSIZE_out1=1, PORTSIZE_out1=2,
    BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2,
    BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2,
    BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2,
    BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2,
    BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2,
    BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2,
    BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2,
    MEMORY_INIT_file_a="array_a.mem",
    MEMORY_INIT_file_b="array_b.mem",
    n_elements=1,
    data_size=32,
    address_space_begin=0,
    address_space_rangesize=4,
    BUS_PIPELINED=1,
    BRAM_BITSIZE=32,
    PRIVATE_MEMORY=0,
    USE_SPARSE_MEMORY=1,
    HIGH_LATENCY=0,
    BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2,
    BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2,
    BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2,
    BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2,
    BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2,
    BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  parameter n_bytes = (n_elements*data_size)/8;
  parameter memory_bitsize = 2*BRAM_BITSIZE;
  parameter n_byte_on_databus = memory_bitsize/8;
  parameter n_mem_elements = n_bytes/(n_byte_on_databus) + (n_bytes%(n_byte_on_databus) == 0 ? 0 : 1);
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : $clog2(n_mem_elements);
  `else
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : log2(n_mem_elements);
  `endif
  parameter max_n_writes = PORTSIZE_sel_STORE > PORTSIZE_S_we_ram ? PORTSIZE_sel_STORE : PORTSIZE_S_we_ram;
  parameter max_n_reads = PORTSIZE_sel_LOAD > PORTSIZE_S_oe_ram ? PORTSIZE_sel_LOAD : PORTSIZE_S_oe_ram;
  parameter max_n_rw = max_n_writes > max_n_reads ? max_n_writes : max_n_reads;
  
  wire [max_n_writes-1:0] bram_write;
  
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_a;
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_b;
  wire [n_byte_on_databus*max_n_writes-1:0] be_swapped;
  
  wire [memory_bitsize*max_n_writes-1:0] din_value_aggregated_swapped;
  wire [(memory_bitsize/2)*max_n_reads-1:0] dout_a;
  wire [(memory_bitsize/2)*max_n_reads-1:0] dout_b;
  
  
  BRAM_MEMORY_NN_CORE #(.PORTSIZE_bram_write(max_n_writes), .BITSIZE_bram_write(1), .BITSIZE_dout_a(memory_bitsize/2), .PORTSIZE_dout_a(max_n_reads), .BITSIZE_memory_addr_a(nbit_read_addr), .PORTSIZE_memory_addr_a(max_n_rw), .BITSIZE_din_value_aggregated_swapped(memory_bitsize), .PORTSIZE_din_value_aggregated_swapped(max_n_writes), .BITSIZE_be_swapped(n_byte_on_databus), .PORTSIZE_be_swapped(max_n_writes), .MEMORY_INIT_file(MEMORY_INIT_file_a), .BRAM_BITSIZE(BRAM_BITSIZE), .n_bytes(n_bytes), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .memory_offset(0), .n_byte_on_databus_offset(0), .HIGH_LATENCY(HIGH_LATENCY)) BRAM_MEMORY_NN_instance_a(.clock(clock), .bram_write(bram_write), .memory_addr_a(memory_addr_a), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .dout_a(dout_a));
  
  generate
    if (n_bytes > BRAM_BITSIZE/8)
    begin : SECOND_MEMORY
      BRAM_MEMORY_NN_CORE #(.PORTSIZE_bram_write(max_n_writes), .BITSIZE_bram_write(1), .BITSIZE_dout_a((memory_bitsize/2)), .PORTSIZE_dout_a(max_n_reads), .BITSIZE_memory_addr_a(nbit_read_addr), .PORTSIZE_memory_addr_a(max_n_rw), .BITSIZE_din_value_aggregated_swapped(memory_bitsize), .PORTSIZE_din_value_aggregated_swapped(max_n_writes), .BITSIZE_be_swapped(n_byte_on_databus), .PORTSIZE_be_swapped(max_n_writes), .MEMORY_INIT_file(MEMORY_INIT_file_b), .BRAM_BITSIZE(BRAM_BITSIZE), .n_bytes(n_bytes), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .memory_offset(memory_bitsize/2), .n_byte_on_databus_offset(n_byte_on_databus/2), .HIGH_LATENCY(HIGH_LATENCY)) BRAM_MEMORY_NN_instance_b(.clock(clock), .bram_write(bram_write), .memory_addr_a(memory_addr_b), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .dout_a(dout_b));
    end
  else
    assign dout_b = {(memory_bitsize/2)*max_n_reads{1'b0}};
  endgenerate
  
  ADDRESS_DECODING_LOGIC_NN #(.BITSIZE_in1(BITSIZE_in1), .PORTSIZE_in1(PORTSIZE_in1), .BITSIZE_in2(BITSIZE_in2), .PORTSIZE_in2(PORTSIZE_in2), .BITSIZE_in3(BITSIZE_in3), .PORTSIZE_in3(PORTSIZE_in3), .BITSIZE_sel_LOAD(BITSIZE_sel_LOAD), .PORTSIZE_sel_LOAD(PORTSIZE_sel_LOAD), .BITSIZE_sel_STORE(BITSIZE_sel_STORE), .PORTSIZE_sel_STORE(PORTSIZE_sel_STORE), .BITSIZE_out1(BITSIZE_out1), .PORTSIZE_out1(PORTSIZE_out1), .BITSIZE_S_oe_ram(BITSIZE_S_oe_ram), .PORTSIZE_S_oe_ram(PORTSIZE_S_oe_ram), .BITSIZE_S_we_ram(BITSIZE_S_we_ram), .PORTSIZE_S_we_ram(PORTSIZE_S_we_ram), .BITSIZE_Sin_DataRdy(BITSIZE_Sin_DataRdy), .PORTSIZE_Sin_DataRdy(PORTSIZE_Sin_DataRdy), .BITSIZE_Sout_DataRdy(BITSIZE_Sout_DataRdy), .PORTSIZE_Sout_DataRdy(PORTSIZE_Sout_DataRdy), .BITSIZE_S_addr_ram(BITSIZE_S_addr_ram), .PORTSIZE_S_addr_ram(PORTSIZE_S_addr_ram), .BITSIZE_S_Wdata_ram(BITSIZE_S_Wdata_ram), .PORTSIZE_S_Wdata_ram(PORTSIZE_S_Wdata_ram), .BITSIZE_Sin_Rdata_ram(BITSIZE_Sin_Rdata_ram), .PORTSIZE_Sin_Rdata_ram(PORTSIZE_Sin_Rdata_ram), .BITSIZE_Sout_Rdata_ram(BITSIZE_Sout_Rdata_ram), .PORTSIZE_Sout_Rdata_ram(PORTSIZE_Sout_Rdata_ram), .BITSIZE_S_data_ram_size(BITSIZE_S_data_ram_size), .PORTSIZE_S_data_ram_size(PORTSIZE_S_data_ram_size), .address_space_begin(address_space_begin), .address_space_rangesize(address_space_rangesize), .BUS_PIPELINED(BUS_PIPELINED), .BRAM_BITSIZE(BRAM_BITSIZE), .PRIVATE_MEMORY(PRIVATE_MEMORY), .USE_SPARSE_MEMORY(USE_SPARSE_MEMORY), .HIGH_LATENCY(HIGH_LATENCY), .BITSIZE_proxy_in1(BITSIZE_proxy_in1), .PORTSIZE_proxy_in1(PORTSIZE_proxy_in1), .BITSIZE_proxy_in2(BITSIZE_proxy_in2), .PORTSIZE_proxy_in2(PORTSIZE_proxy_in2), .BITSIZE_proxy_in3(BITSIZE_proxy_in3), .PORTSIZE_proxy_in3(PORTSIZE_proxy_in3), .BITSIZE_proxy_sel_LOAD(BITSIZE_proxy_sel_LOAD), .PORTSIZE_proxy_sel_LOAD(PORTSIZE_proxy_sel_LOAD), .BITSIZE_proxy_sel_STORE(BITSIZE_proxy_sel_STORE), .PORTSIZE_proxy_sel_STORE(PORTSIZE_proxy_sel_STORE), .BITSIZE_proxy_out1(BITSIZE_proxy_out1), .PORTSIZE_proxy_out1(PORTSIZE_proxy_out1), .BITSIZE_dout_a(memory_bitsize/2), .PORTSIZE_dout_a(max_n_reads), .BITSIZE_dout_b(memory_bitsize/2), .PORTSIZE_dout_b(max_n_reads), .BITSIZE_memory_addr_a(nbit_read_addr), .PORTSIZE_memory_addr_a(max_n_rw), .BITSIZE_memory_addr_b(nbit_read_addr), .PORTSIZE_memory_addr_b(max_n_rw), .BITSIZE_din_value_aggregated_swapped(memory_bitsize), .PORTSIZE_din_value_aggregated_swapped(max_n_writes), .BITSIZE_be_swapped(n_byte_on_databus), .PORTSIZE_be_swapped(max_n_writes), .BITSIZE_bram_write(1), .PORTSIZE_bram_write(max_n_writes), .nbit_read_addr(nbit_read_addr), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .max_n_rw(max_n_rw)) ADDRESS_DECODING_LOGIC_NN_instance (.clock(clock), .reset(reset), .in1(in1), .in2(in2), .in3(in3), .out1(out1), .sel_LOAD(sel_LOAD), .sel_STORE(sel_STORE), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .Sin_Rdata_ram(Sin_Rdata_ram), .Sout_Rdata_ram(Sout_Rdata_ram), .S_data_ram_size(S_data_ram_size), .Sin_DataRdy(Sin_DataRdy), .Sout_DataRdy(Sout_DataRdy), .proxy_in1(proxy_in1), .proxy_in2(proxy_in2), .proxy_in3(proxy_in3), .proxy_sel_LOAD(proxy_sel_LOAD), .proxy_sel_STORE(proxy_sel_STORE), .proxy_out1(proxy_out1), .dout_a(dout_a), .dout_b(dout_b), .memory_addr_a(memory_addr_a), .memory_addr_b(memory_addr_b), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .bram_write(bram_write));
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_BRAM_NN(clock,
  reset,
  in1,
  in2,
  in3,
  in4,
  out1,
  sel_LOAD,
  sel_STORE,
  S_oe_ram,
  S_we_ram,
  S_addr_ram,
  S_Wdata_ram,
  Sin_Rdata_ram,
  Sout_Rdata_ram,
  S_data_ram_size,
  Sin_DataRdy,
  Sout_DataRdy,
  proxy_in1,
  proxy_in2,
  proxy_in3,
  proxy_sel_LOAD,
  proxy_sel_STORE,
  proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_in2=1, PORTSIZE_in2=2,
    BITSIZE_in3=1, PORTSIZE_in3=2,
    BITSIZE_in4=1, PORTSIZE_in4=2,
    BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2,
    BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2,
    BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2,
    BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2,
    BITSIZE_out1=1, PORTSIZE_out1=2,
    BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2,
    BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2,
    BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2,
    BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2,
    BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2,
    BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2,
    BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2,
    MEMORY_INIT_file_a="array_a.mem",
    MEMORY_INIT_file_b="array_b.mem",
    n_elements=1,
    data_size=32,
    address_space_begin=0,
    address_space_rangesize=4,
    BUS_PIPELINED=1,
    BRAM_BITSIZE=32,
    PRIVATE_MEMORY=0,
    USE_SPARSE_MEMORY=1,
    BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2,
    BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2,
    BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2,
    BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2,
    BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2,
    BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_in4-1:0] in4;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  ARRAY_1D_STD_BRAM_NN_SP #(.BITSIZE_in1(BITSIZE_in1), .PORTSIZE_in1(PORTSIZE_in1), .BITSIZE_in2(BITSIZE_in2), .PORTSIZE_in2(PORTSIZE_in2), .BITSIZE_in3(BITSIZE_in3), .PORTSIZE_in3(PORTSIZE_in3), .BITSIZE_sel_LOAD(BITSIZE_sel_LOAD), .PORTSIZE_sel_LOAD(PORTSIZE_sel_LOAD), .BITSIZE_sel_STORE(BITSIZE_sel_STORE), .PORTSIZE_sel_STORE(PORTSIZE_sel_STORE), .BITSIZE_S_oe_ram(BITSIZE_S_oe_ram), .PORTSIZE_S_oe_ram(PORTSIZE_S_oe_ram), .BITSIZE_S_we_ram(BITSIZE_S_we_ram), .PORTSIZE_S_we_ram(PORTSIZE_S_we_ram), .BITSIZE_out1(BITSIZE_out1), .PORTSIZE_out1(PORTSIZE_out1), .BITSIZE_S_addr_ram(BITSIZE_S_addr_ram), .PORTSIZE_S_addr_ram(PORTSIZE_S_addr_ram), .BITSIZE_S_Wdata_ram(BITSIZE_S_Wdata_ram), .PORTSIZE_S_Wdata_ram(PORTSIZE_S_Wdata_ram), .BITSIZE_Sin_Rdata_ram(BITSIZE_Sin_Rdata_ram), .PORTSIZE_Sin_Rdata_ram(PORTSIZE_Sin_Rdata_ram), .BITSIZE_Sout_Rdata_ram(BITSIZE_Sout_Rdata_ram), .PORTSIZE_Sout_Rdata_ram(PORTSIZE_Sout_Rdata_ram), .BITSIZE_S_data_ram_size(BITSIZE_S_data_ram_size), .PORTSIZE_S_data_ram_size(PORTSIZE_S_data_ram_size), .BITSIZE_Sin_DataRdy(BITSIZE_Sin_DataRdy), .PORTSIZE_Sin_DataRdy(PORTSIZE_Sin_DataRdy), .BITSIZE_Sout_DataRdy(BITSIZE_Sout_DataRdy), .PORTSIZE_Sout_DataRdy(PORTSIZE_Sout_DataRdy), .MEMORY_INIT_file_a(MEMORY_INIT_file_a), .MEMORY_INIT_file_b(MEMORY_INIT_file_b), .n_elements(n_elements), .data_size(data_size), .address_space_begin(address_space_begin), .address_space_rangesize(address_space_rangesize), .BUS_PIPELINED(BUS_PIPELINED), .BRAM_BITSIZE(BRAM_BITSIZE), .PRIVATE_MEMORY(PRIVATE_MEMORY), .USE_SPARSE_MEMORY(USE_SPARSE_MEMORY), .BITSIZE_proxy_in1(BITSIZE_proxy_in1), .PORTSIZE_proxy_in1(PORTSIZE_proxy_in1), .BITSIZE_proxy_in2(BITSIZE_proxy_in2), .PORTSIZE_proxy_in2(PORTSIZE_proxy_in2), .BITSIZE_proxy_in3(BITSIZE_proxy_in3), .PORTSIZE_proxy_in3(PORTSIZE_proxy_in3), .BITSIZE_proxy_sel_LOAD(BITSIZE_proxy_sel_LOAD), .PORTSIZE_proxy_sel_LOAD(PORTSIZE_proxy_sel_LOAD), .BITSIZE_proxy_sel_STORE(BITSIZE_proxy_sel_STORE), .PORTSIZE_proxy_sel_STORE(PORTSIZE_proxy_sel_STORE), .BITSIZE_proxy_out1(BITSIZE_proxy_out1), .PORTSIZE_proxy_out1(PORTSIZE_proxy_out1), .HIGH_LATENCY(0)) ARRAY_1D_STD_BRAM_NN_instance (.out1(out1), .Sout_Rdata_ram(Sout_Rdata_ram), .Sout_DataRdy(Sout_DataRdy), .proxy_out1(proxy_out1), .clock(clock), .reset(reset), .in1(in1), .in2(in2), .in3(in3), .sel_LOAD(sel_LOAD & in4), .sel_STORE(sel_STORE & in4), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .Sin_Rdata_ram(Sin_Rdata_ram), .S_data_ram_size(S_data_ram_size), .Sin_DataRdy(Sin_DataRdy), .proxy_in1(proxy_in1), .proxy_in2(proxy_in2), .proxy_in3(proxy_in3), .proxy_sel_LOAD(proxy_sel_LOAD), .proxy_sel_STORE(proxy_sel_STORE));
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module addr_expr_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module IUdata_converter_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  generate
  if (BITSIZE_out1 <= BITSIZE_in1)
  begin
    assign out1 = in1[BITSIZE_out1-1:0];
  end
  else
  begin
    assign out1 = {{(BITSIZE_out1-BITSIZE_in1){in1[BITSIZE_in1-1]}},in1};
  end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module read_cond_FU(in1,
  out1);
  parameter BITSIZE_in1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output out1;
  assign out1 = in1 != {BITSIZE_in1{1'b0}};
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module lut_expr_FU(in1,
  in2,
  in3,
  in4,
  in5,
  in6,
  in7,
  in8,
  in9,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input in2;
  input in3;
  input in4;
  input in5;
  input in6;
  input in7;
  input in8;
  input in9;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  reg[7:0] cleaned_in0;
  wire [7:0] in0;
  wire[BITSIZE_in1-1:0] shifted_s;
  assign in0 = {in9, in8, in7, in6, in5, in4, in3, in2};
  generate
    genvar i0;
    for (i0=0; i0<8; i0=i0+1)
    begin : L0
          always @(*)
          begin
             if (in0[i0] == 1'b1)
                cleaned_in0[i0] = 1'b1;
             else
                cleaned_in0[i0] = 1'b0;
          end
    end
  endgenerate
  assign shifted_s = in1 >> cleaned_in0;
  assign out1[0] = shifted_s[0];
  generate
     if(BITSIZE_out1 > 1)
       assign out1[BITSIZE_out1-1:1] = 0;
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module bit_and_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  assign out1 = in1 & in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module bit_ior_concat_expr_FU(in1,
  in2,
  in3,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_in3=1,
    BITSIZE_out1=1,
    OFFSET_PARAMETER=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  input signed [BITSIZE_in3-1:0] in3;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  
  parameter nbit_out = BITSIZE_out1 > OFFSET_PARAMETER ? BITSIZE_out1 : 1+OFFSET_PARAMETER;
  wire signed [nbit_out-1:0] tmp_in1;
  wire signed [OFFSET_PARAMETER-1:0] tmp_in2;
  generate
    if(BITSIZE_in1 >= nbit_out)
      assign tmp_in1=in1[nbit_out-1:0];
    else
      assign tmp_in1={{(nbit_out-BITSIZE_in1){in1[BITSIZE_in1-1]}},in1};
  endgenerate
  generate
    if(BITSIZE_in2 >= OFFSET_PARAMETER)
      assign tmp_in2=in2[OFFSET_PARAMETER-1:0];
    else
      assign tmp_in2={{(OFFSET_PARAMETER-BITSIZE_in2){in2[BITSIZE_in2-1]}},in2};
  endgenerate
  assign out1 = {tmp_in1[nbit_out-1:OFFSET_PARAMETER] , tmp_in2};
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module cond_expr_FU(in1,
  in2,
  in3,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_in3=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  input signed [BITSIZE_in3-1:0] in3;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  assign out1 = in1 != 0 ? in2 : in3;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module lshift_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1,
    PRECISION=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 <<< in2[arg2_bitsize-1:0];
    else
      assign out1 = in1 <<< in2;
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module lt_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 < in2;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module mult_expr_FU(clock,
  in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1,
    PIPE_PARAMETER=0;
  // IN
  input clock;
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  generate
    if(PIPE_PARAMETER==1)
    begin
      reg signed [BITSIZE_out1-1:0] out1_reg;
      assign out1 = out1_reg;
      always @(posedge clock)
      begin
        out1_reg <= in1 * in2;
      end
    end
    else if(PIPE_PARAMETER>1)
    begin
      reg signed [BITSIZE_in1-1:0] in1_in;
      reg signed [BITSIZE_in2-1:0] in2_in;
      wire signed [BITSIZE_out1-1:0] mult_res;
      reg signed [BITSIZE_out1-1:0] mul [PIPE_PARAMETER-2:0];
      integer i;
      assign mult_res = in1_in * in2_in;
      always @(posedge clock)
      begin
        in1_in <= in1;
        in2_in <= in2;
        mul[PIPE_PARAMETER-2] <= mult_res;
        for (i=0; i<PIPE_PARAMETER-2; i=i+1)
          mul[i] <= mul[i+1];
      end
      assign out1 = mul[0];
    end
    else
    begin
      assign out1 = in1 * in2;
    end
    endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ne_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 != in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module plus_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  assign out1 = in1 + in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module rshift_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1,
    PRECISION=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 >>> (in2[arg2_bitsize-1:0]);
    else
      assign out1 = in1 >>> in2;
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_le_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 <= in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_lshift_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1,
    PRECISION=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 << in2[arg2_bitsize-1:0];
    else
      assign out1 = in1 << in2;
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_pointer_plus_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1,
    LSB_PARAMETER=-1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  wire [BITSIZE_out1-1:0] in1_tmp;
  wire [BITSIZE_out1-1:0] in2_tmp;
  assign in1_tmp = in1;
  assign in2_tmp = in2;generate if (BITSIZE_out1 > LSB_PARAMETER) assign out1[BITSIZE_out1-1:LSB_PARAMETER] = (in1_tmp[BITSIZE_out1-1:LSB_PARAMETER] + in2_tmp[BITSIZE_out1-1:LSB_PARAMETER]); else assign out1 = 0; endgenerate
  generate if (LSB_PARAMETER != 0 && BITSIZE_out1 > LSB_PARAMETER) assign out1[LSB_PARAMETER-1:0] = 0; endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module multi_read_cond_FU(in1,
  out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_out1=1;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module BMEMORY_CTRLN(clock,
  in1,
  in2,
  in3,
  in4,
  sel_LOAD,
  sel_STORE,
  out1,
  Min_oe_ram,
  Mout_oe_ram,
  Min_we_ram,
  Mout_we_ram,
  Min_addr_ram,
  Mout_addr_ram,
  M_Rdata_ram,
  Min_Wdata_ram,
  Mout_Wdata_ram,
  Min_data_ram_size,
  Mout_data_ram_size,
  M_DataRdy);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_in2=1, PORTSIZE_in2=2,
    BITSIZE_in3=1, PORTSIZE_in3=2,
    BITSIZE_in4=1, PORTSIZE_in4=2,
    BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2,
    BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2,
    BITSIZE_out1=1, PORTSIZE_out1=2,
    BITSIZE_Min_oe_ram=1, PORTSIZE_Min_oe_ram=2,
    BITSIZE_Min_we_ram=1, PORTSIZE_Min_we_ram=2,
    BITSIZE_Mout_oe_ram=1, PORTSIZE_Mout_oe_ram=2,
    BITSIZE_Mout_we_ram=1, PORTSIZE_Mout_we_ram=2,
    BITSIZE_M_DataRdy=1, PORTSIZE_M_DataRdy=2,
    BITSIZE_Min_addr_ram=1, PORTSIZE_Min_addr_ram=2,
    BITSIZE_Mout_addr_ram=1, PORTSIZE_Mout_addr_ram=2,
    BITSIZE_M_Rdata_ram=8, PORTSIZE_M_Rdata_ram=2,
    BITSIZE_Min_Wdata_ram=8, PORTSIZE_Min_Wdata_ram=2,
    BITSIZE_Mout_Wdata_ram=8, PORTSIZE_Mout_Wdata_ram=2,
    BITSIZE_Min_data_ram_size=1, PORTSIZE_Min_data_ram_size=2,
    BITSIZE_Mout_data_ram_size=1, PORTSIZE_Mout_data_ram_size=2;
  // IN
  input clock;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_in4-1:0] in4;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_Min_oe_ram-1:0] Min_oe_ram;
  input [PORTSIZE_Min_we_ram-1:0] Min_we_ram;
  input [(PORTSIZE_Min_addr_ram*BITSIZE_Min_addr_ram)+(-1):0] Min_addr_ram;
  input [(PORTSIZE_M_Rdata_ram*BITSIZE_M_Rdata_ram)+(-1):0] M_Rdata_ram;
  input [(PORTSIZE_Min_Wdata_ram*BITSIZE_Min_Wdata_ram)+(-1):0] Min_Wdata_ram;
  input [(PORTSIZE_Min_data_ram_size*BITSIZE_Min_data_ram_size)+(-1):0] Min_data_ram_size;
  input [PORTSIZE_M_DataRdy-1:0] M_DataRdy;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [PORTSIZE_Mout_oe_ram-1:0] Mout_oe_ram;
  output [PORTSIZE_Mout_we_ram-1:0] Mout_we_ram;
  output [(PORTSIZE_Mout_addr_ram*BITSIZE_Mout_addr_ram)+(-1):0] Mout_addr_ram;
  output [(PORTSIZE_Mout_Wdata_ram*BITSIZE_Mout_Wdata_ram)+(-1):0] Mout_Wdata_ram;
  output [(PORTSIZE_Mout_data_ram_size*BITSIZE_Mout_data_ram_size)+(-1):0] Mout_data_ram_size;
  
  parameter max_n_writes = PORTSIZE_sel_STORE > PORTSIZE_Mout_we_ram ? PORTSIZE_sel_STORE : PORTSIZE_Mout_we_ram;
  parameter max_n_reads = PORTSIZE_sel_LOAD > PORTSIZE_Mout_oe_ram ? PORTSIZE_sel_STORE : PORTSIZE_Mout_oe_ram;
  parameter max_n_rw = max_n_writes > max_n_reads ? max_n_writes : max_n_reads;
  wire  [(PORTSIZE_in2*BITSIZE_in2)-1:0] tmp_addr;
  wire [PORTSIZE_sel_LOAD-1:0] int_sel_LOAD;
  wire [PORTSIZE_sel_STORE-1:0] int_sel_STORE;
  assign int_sel_LOAD = sel_LOAD & in4;
  assign int_sel_STORE = sel_STORE & in4;
  assign tmp_addr = in2;
  generate
  genvar i;
    for (i=0; i<max_n_rw; i=i+1)
    begin : L0
      assign Mout_addr_ram[(i+1)*BITSIZE_Mout_addr_ram-1:i*BITSIZE_Mout_addr_ram] = ((i < PORTSIZE_sel_LOAD && int_sel_LOAD[i]) || (i < PORTSIZE_sel_STORE && int_sel_STORE[i])) ? (tmp_addr[(i+1)*BITSIZE_in2-1:i*BITSIZE_in2]) : Min_addr_ram[(i+1)*BITSIZE_Min_addr_ram-1:i*BITSIZE_Min_addr_ram];
    end
    endgenerate
  assign Mout_oe_ram = int_sel_LOAD | Min_oe_ram;
  assign Mout_we_ram = int_sel_STORE | Min_we_ram;
  generate
    for (i=0; i<max_n_reads; i=i+1)
    begin : L1
      assign out1[(i+1)*BITSIZE_out1-1:i*BITSIZE_out1] = M_Rdata_ram[i*BITSIZE_M_Rdata_ram+BITSIZE_out1-1:i*BITSIZE_M_Rdata_ram];
  end
  endgenerate
  generate
    for (i=0; i<max_n_rw; i=i+1)
    begin : L2
      assign Mout_Wdata_ram[(i+1)*BITSIZE_Mout_Wdata_ram-1:i*BITSIZE_Mout_Wdata_ram] = int_sel_STORE[i] ? in1[(i+1)*BITSIZE_in1-1:i*BITSIZE_in1] : Min_Wdata_ram[(i+1)*BITSIZE_Min_Wdata_ram-1:i*BITSIZE_Min_Wdata_ram];
  end
  endgenerate
  generate
    for (i=0; i<max_n_rw; i=i+1)
    begin : L3
      assign Mout_data_ram_size[(i+1)*BITSIZE_Mout_data_ram_size-1:i*BITSIZE_Mout_data_ram_size] = ((i < PORTSIZE_sel_LOAD && int_sel_LOAD[i]) || (i < PORTSIZE_sel_STORE && int_sel_STORE[i])) ? (in3[(i+1)*BITSIZE_in3-1:i*BITSIZE_in3]) : Min_data_ram_size[(i+1)*BITSIZE_Min_data_ram_size-1:i*BITSIZE_Min_data_ram_size];
    end
    endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_ge_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 >= in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_gt_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 > in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_ne_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 != in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_plus_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 + in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_rshift_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1,
    PRECISION=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 >> (in2[arg2_bitsize-1:0]);
    else
      assign out1 = in1 >> in2;
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2013-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module bus_merger(in1,
  out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_out1=1;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  function [BITSIZE_out1-1:0] merge;
    input [BITSIZE_in1*PORTSIZE_in1-1:0] m;
    reg [BITSIZE_out1-1:0] res;
    integer i1;
  begin
    res={BITSIZE_in1{1'b0}};
    for(i1 = 0; i1 < PORTSIZE_in1; i1 = i1 + 1)
    begin
      res = res | m[i1*BITSIZE_in1 +:BITSIZE_in1];
    end
    merge = res;
  end
  endfunction
  
  assign out1 = merge(in1);
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module join_signal(in1,
  out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2,
    BITSIZE_out1=1;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  generate
  genvar i1;
  for (i1=0; i1<PORTSIZE_in1; i1=i1+1)
    begin : L1
      assign out1[(i1+1)*(BITSIZE_out1/PORTSIZE_in1)-1:i1*(BITSIZE_out1/PORTSIZE_in1)] = in1[(i1+1)*BITSIZE_in1-1:i1*BITSIZE_in1];
    end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module split_signal(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1, PORTSIZE_out1=2;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ASSIGN_UNSIGNED_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ASSIGN_VECTOR_BOOL_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module UUdata_converter_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  generate
  if (BITSIZE_out1 <= BITSIZE_in1)
  begin
    assign out1 = in1[BITSIZE_out1-1:0];
  end
  else
  begin
    assign out1 = {{(BITSIZE_out1-BITSIZE_in1){1'b0}},in1};
  end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>, Christian Pilato <christian.pilato@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module MUX_GATE(sel,
  in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_out1=1;
  // IN
  input sel;
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = sel ? in1 : in2;
endmodule

// Datapath RTL description for __internal_bambu_memcpy
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module datapath___internal_bambu_memcpy(clock,
  reset,
  in_port_dest,
  in_port_src,
  return_port,
  M_Rdata_ram,
  M_DataRdy,
  Min_oe_ram,
  Min_we_ram,
  Min_addr_ram,
  Min_Wdata_ram,
  Min_data_ram_size,
  Mout_oe_ram,
  Mout_we_ram,
  Mout_addr_ram,
  Mout_Wdata_ram,
  Mout_data_ram_size,
  fuselector_BMEMORY_CTRLN_11_i0_LOAD,
  fuselector_BMEMORY_CTRLN_11_i0_STORE,
  selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0,
  selector_MUX_15_reg_0_0_0_0,
  selector_MUX_19_reg_4_0_0_0,
  selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0,
  selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1,
  selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0,
  selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0,
  wrenable_reg_0,
  wrenable_reg_1,
  wrenable_reg_2,
  wrenable_reg_3,
  wrenable_reg_4,
  wrenable_reg_5,
  wrenable_reg_6,
  wrenable_reg_7,
  OUT_CONDITION___internal_bambu_memcpy_329_729,
  OUT_MULTIIF___internal_bambu_memcpy_329_34299,
  OUT_MULTIIF___internal_bambu_memcpy_329_34306);
  // IN
  input clock;
  input reset;
  input [31:0] in_port_dest;
  input [31:0] in_port_src;
  input [63:0] M_Rdata_ram;
  input [1:0] M_DataRdy;
  input [1:0] Min_oe_ram;
  input [1:0] Min_we_ram;
  input [19:0] Min_addr_ram;
  input [63:0] Min_Wdata_ram;
  input [11:0] Min_data_ram_size;
  input fuselector_BMEMORY_CTRLN_11_i0_LOAD;
  input fuselector_BMEMORY_CTRLN_11_i0_STORE;
  input selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0;
  input selector_MUX_15_reg_0_0_0_0;
  input selector_MUX_19_reg_4_0_0_0;
  input selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0;
  input selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1;
  input selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0;
  input selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0;
  input wrenable_reg_0;
  input wrenable_reg_1;
  input wrenable_reg_2;
  input wrenable_reg_3;
  input wrenable_reg_4;
  input wrenable_reg_5;
  input wrenable_reg_6;
  input wrenable_reg_7;
  // OUT
  output [31:0] return_port;
  output [1:0] Mout_oe_ram;
  output [1:0] Mout_we_ram;
  output [19:0] Mout_addr_ram;
  output [63:0] Mout_Wdata_ram;
  output [11:0] Mout_data_ram_size;
  output OUT_CONDITION___internal_bambu_memcpy_329_729;
  output OUT_MULTIIF___internal_bambu_memcpy_329_34299;
  output OUT_MULTIIF___internal_bambu_memcpy_329_34306;
  // Component and signal declarations
  wire [31:0] io_signal_in_port_dest_return_port;
  wire [31:0] null_out_signal_BMEMORY_CTRLN_11_i0_out1_1;
  wire [31:0] out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0;
  wire [31:0] out_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0;
  wire [31:0] out_MUX_15_reg_0_0_0_0;
  wire [31:0] out_MUX_19_reg_4_0_0_0;
  wire [9:0] out_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0;
  wire [9:0] out_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1;
  wire [9:0] out_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0;
  wire [6:0] out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0;
  wire out_const_0;
  wire [4:0] out_const_1;
  wire [6:0] out_const_2;
  wire out_const_3;
  wire [1:0] out_const_4;
  wire [2:0] out_const_5;
  wire [4:0] out_const_6;
  wire [6:0] out_const_7;
  wire [9:0] out_conv_in_port_dest_32_10;
  wire [9:0] out_conv_in_port_src_32_10;
  wire [7:0] out_conv_out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0_32_8;
  wire [5:0] out_conv_out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0_7_6;
  wire [6:0] out_conv_out_const_1_5_7;
  wire [31:0] out_conv_out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0_8_32;
  wire [9:0] out_conv_out_reg_1_reg_1_32_10;
  wire [31:0] out_conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1_1_32;
  wire [31:0] out_conv_out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2_1_32;
  wire [9:0] out_conv_out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076_32_10;
  wire [7:0] out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0;
  wire out_lut_expr_FU_2_i0_fu___internal_bambu_memcpy_329_34302;
  wire out_multi_read_cond_FU_3_i0_fu___internal_bambu_memcpy_329_34299;
  wire out_multi_read_cond_FU_7_i0_fu___internal_bambu_memcpy_329_34306;
  wire out_read_cond_FU_10_i0_fu___internal_bambu_memcpy_329_729;
  wire [31:0] out_reg_0_reg_0;
  wire [31:0] out_reg_1_reg_1;
  wire out_reg_2_reg_2;
  wire [31:0] out_reg_3_reg_3;
  wire [31:0] out_reg_4_reg_4;
  wire [9:0] out_reg_5_reg_5;
  wire out_reg_6_reg_6;
  wire [7:0] out_reg_7_reg_7;
  wire [0:0] out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1;
  wire [0:0] out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2;
  wire out_ui_ge_expr_FU_8_8_8_12_i0_fu___internal_bambu_memcpy_329_673;
  wire out_ui_ge_expr_FU_8_8_8_12_i1_fu___internal_bambu_memcpy_329_675;
  wire out_ui_gt_expr_FU_0_32_32_13_i0_fu___internal_bambu_memcpy_329_34143;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_14_i0_fu___internal_bambu_memcpy_329_34073;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_14_i1_fu___internal_bambu_memcpy_329_34080;
  wire out_ui_ne_expr_FU_32_0_32_15_i0_fu___internal_bambu_memcpy_329_34151;
  wire [31:0] out_ui_plus_expr_FU_32_0_32_16_i0_fu___internal_bambu_memcpy_329_634;
  wire [31:0] out_ui_plus_expr_FU_32_0_32_17_i0_fu___internal_bambu_memcpy_329_662;
  wire [9:0] out_ui_pointer_plus_expr_FU_16_0_16_18_i0_fu___internal_bambu_memcpy_329_672;
  wire [9:0] out_ui_pointer_plus_expr_FU_16_0_16_18_i1_fu___internal_bambu_memcpy_329_674;
  wire [9:0] out_ui_pointer_plus_expr_FU_16_16_16_19_i0_fu___internal_bambu_memcpy_329_34130;
  wire [9:0] out_ui_pointer_plus_expr_FU_16_16_16_19_i1_fu___internal_bambu_memcpy_329_34134;
  wire [31:0] out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076;
  wire [31:0] out_ui_pointer_plus_expr_FU_32_32_32_20_i1_fu___internal_bambu_memcpy_329_34083;
  wire [7:0] out_ui_rshift_expr_FU_16_0_16_21_i0_fu___internal_bambu_memcpy_329_34167;
  wire [7:0] out_ui_rshift_expr_FU_16_0_16_21_i1_fu___internal_bambu_memcpy_329_34180;
  wire [7:0] out_ui_rshift_expr_FU_16_0_16_22_i0_fu___internal_bambu_memcpy_329_34173;
  wire [7:0] out_ui_rshift_expr_FU_16_0_16_22_i1_fu___internal_bambu_memcpy_329_34177;
  wire [31:0] out_vb_assign_conn_obj_2_ASSIGN_VECTOR_BOOL_FU_vb_assign_3;
  wire [63:0] sig_in_bus_mergerMout_Wdata_ram0_0;
  wire [19:0] sig_in_bus_mergerMout_addr_ram1_0;
  wire [11:0] sig_in_bus_mergerMout_data_ram_size2_0;
  wire [1:0] sig_in_bus_mergerMout_oe_ram3_0;
  wire [1:0] sig_in_bus_mergerMout_we_ram4_0;
  wire [63:0] sig_in_vector_bus_mergerMout_Wdata_ram0_0;
  wire [19:0] sig_in_vector_bus_mergerMout_addr_ram1_0;
  wire [11:0] sig_in_vector_bus_mergerMout_data_ram_size2_0;
  wire [1:0] sig_in_vector_bus_mergerMout_oe_ram3_0;
  wire [1:0] sig_in_vector_bus_mergerMout_we_ram4_0;
  wire [63:0] sig_out_bus_mergerMout_Wdata_ram0_;
  wire [19:0] sig_out_bus_mergerMout_addr_ram1_;
  wire [11:0] sig_out_bus_mergerMout_data_ram_size2_;
  wire [1:0] sig_out_bus_mergerMout_oe_ram3_;
  wire [1:0] sig_out_bus_mergerMout_we_ram4_;
  
  ASSIGN_UNSIGNED_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) ASSIGN_UNSIGNED_FU_u_assign_1 (.out1(out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1),
    .in1(out_const_0));
  ASSIGN_UNSIGNED_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) ASSIGN_UNSIGNED_FU_u_assign_2 (.out1(out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2),
    .in1(out_const_0));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_3 (.out1(out_vb_assign_conn_obj_2_ASSIGN_VECTOR_BOOL_FU_vb_assign_3),
    .in1(out_reg_3_reg_3));
  BMEMORY_CTRLN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(10),
    .PORTSIZE_in2(2),
    .BITSIZE_in3(6),
    .PORTSIZE_in3(2),
    .BITSIZE_in4(1),
    .PORTSIZE_in4(2),
    .BITSIZE_sel_LOAD(1),
    .PORTSIZE_sel_LOAD(2),
    .BITSIZE_sel_STORE(1),
    .PORTSIZE_sel_STORE(2),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2),
    .BITSIZE_Min_oe_ram(1),
    .PORTSIZE_Min_oe_ram(2),
    .BITSIZE_Min_we_ram(1),
    .PORTSIZE_Min_we_ram(2),
    .BITSIZE_Mout_oe_ram(1),
    .PORTSIZE_Mout_oe_ram(2),
    .BITSIZE_Mout_we_ram(1),
    .PORTSIZE_Mout_we_ram(2),
    .BITSIZE_M_DataRdy(1),
    .PORTSIZE_M_DataRdy(2),
    .BITSIZE_Min_addr_ram(10),
    .PORTSIZE_Min_addr_ram(2),
    .BITSIZE_Mout_addr_ram(10),
    .PORTSIZE_Mout_addr_ram(2),
    .BITSIZE_M_Rdata_ram(32),
    .PORTSIZE_M_Rdata_ram(2),
    .BITSIZE_Min_Wdata_ram(32),
    .PORTSIZE_Min_Wdata_ram(2),
    .BITSIZE_Mout_Wdata_ram(32),
    .PORTSIZE_Mout_Wdata_ram(2),
    .BITSIZE_Min_data_ram_size(6),
    .PORTSIZE_Min_data_ram_size(2),
    .BITSIZE_Mout_data_ram_size(6),
    .PORTSIZE_Mout_data_ram_size(2)) BMEMORY_CTRLN_11_i0 (.out1({null_out_signal_BMEMORY_CTRLN_11_i0_out1_1,
      out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0}),
    .Mout_oe_ram(sig_in_vector_bus_mergerMout_oe_ram3_0),
    .Mout_we_ram(sig_in_vector_bus_mergerMout_we_ram4_0),
    .Mout_addr_ram(sig_in_vector_bus_mergerMout_addr_ram1_0),
    .Mout_Wdata_ram(sig_in_vector_bus_mergerMout_Wdata_ram0_0),
    .Mout_data_ram_size(sig_in_vector_bus_mergerMout_data_ram_size2_0),
    .clock(clock),
    .in1({32'b00000000000000000000000000000000,
      out_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0}),
    .in2({10'b0000000000,
      out_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0}),
    .in3({6'b000000,
      out_conv_out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0_7_6}),
    .in4({1'b0,
      out_const_3}),
    .sel_LOAD({1'b0,
      fuselector_BMEMORY_CTRLN_11_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_BMEMORY_CTRLN_11_i0_STORE}),
    .Min_oe_ram(Min_oe_ram),
    .Min_we_ram(Min_we_ram),
    .Min_addr_ram(Min_addr_ram),
    .M_Rdata_ram(M_Rdata_ram),
    .Min_Wdata_ram(Min_Wdata_ram),
    .Min_data_ram_size(Min_data_ram_size),
    .M_DataRdy(M_DataRdy));
  IUdata_converter_FU #(.BITSIZE_in1(8),
    .BITSIZE_out1(8)) IUdata_converter_FU_iu_conv_0 (.out1(out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0),
    .in1(out_reg_7_reg_7));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_0_BMEMORY_CTRLN_11_i0_0_0_0 (.out1(out_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0),
    .sel(selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0),
    .in1(out_conv_out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0_8_32),
    .in2(out_vb_assign_conn_obj_2_ASSIGN_VECTOR_BOOL_FU_vb_assign_3));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_15_reg_0_0_0_0 (.out1(out_MUX_15_reg_0_0_0_0),
    .sel(selector_MUX_15_reg_0_0_0_0),
    .in1(out_conv_out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2_1_32),
    .in2(out_ui_plus_expr_FU_32_0_32_16_i0_fu___internal_bambu_memcpy_329_634));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_19_reg_4_0_0_0 (.out1(out_MUX_19_reg_4_0_0_0),
    .sel(selector_MUX_19_reg_4_0_0_0),
    .in1(out_conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1_1_32),
    .in2(out_ui_plus_expr_FU_32_0_32_17_i0_fu___internal_bambu_memcpy_329_662));
  MUX_GATE #(.BITSIZE_in1(10),
    .BITSIZE_in2(10),
    .BITSIZE_out1(10)) MUX_1_BMEMORY_CTRLN_11_i0_1_0_0 (.out1(out_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0),
    .sel(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0),
    .in1(out_reg_5_reg_5),
    .in2(out_conv_out_reg_1_reg_1_32_10));
  MUX_GATE #(.BITSIZE_in1(10),
    .BITSIZE_in2(10),
    .BITSIZE_out1(10)) MUX_1_BMEMORY_CTRLN_11_i0_1_0_1 (.out1(out_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1),
    .sel(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_19_i0_fu___internal_bambu_memcpy_329_34130),
    .in2(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076_32_10));
  MUX_GATE #(.BITSIZE_in1(10),
    .BITSIZE_in2(10),
    .BITSIZE_out1(10)) MUX_1_BMEMORY_CTRLN_11_i0_1_1_0 (.out1(out_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0),
    .sel(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0),
    .in1(out_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0),
    .in2(out_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1));
  MUX_GATE #(.BITSIZE_in1(7),
    .BITSIZE_in2(7),
    .BITSIZE_out1(7)) MUX_2_BMEMORY_CTRLN_11_i0_2_0_0 (.out1(out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0),
    .sel(selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0),
    .in1(out_conv_out_const_1_5_7),
    .in2(out_const_2));
  bus_merger #(.BITSIZE_in1(64),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(64)) bus_mergerMout_Wdata_ram0_ (.out1(sig_out_bus_mergerMout_Wdata_ram0_),
    .in1({sig_in_bus_mergerMout_Wdata_ram0_0}));
  bus_merger #(.BITSIZE_in1(20),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(20)) bus_mergerMout_addr_ram1_ (.out1(sig_out_bus_mergerMout_addr_ram1_),
    .in1({sig_in_bus_mergerMout_addr_ram1_0}));
  bus_merger #(.BITSIZE_in1(12),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(12)) bus_mergerMout_data_ram_size2_ (.out1(sig_out_bus_mergerMout_data_ram_size2_),
    .in1({sig_in_bus_mergerMout_data_ram_size2_0}));
  bus_merger #(.BITSIZE_in1(2),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(2)) bus_mergerMout_oe_ram3_ (.out1(sig_out_bus_mergerMout_oe_ram3_),
    .in1({sig_in_bus_mergerMout_oe_ram3_0}));
  bus_merger #(.BITSIZE_in1(2),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(2)) bus_mergerMout_we_ram4_ (.out1(sig_out_bus_mergerMout_we_ram4_),
    .in1({sig_in_bus_mergerMout_we_ram4_0}));
  constant_value #(.BITSIZE_out1(1),
    .value(1'b0)) const_0 (.out1(out_const_0));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b01000)) const_1 (.out1(out_const_1));
  constant_value #(.BITSIZE_out1(7),
    .value(7'b0100000)) const_2 (.out1(out_const_2));
  constant_value #(.BITSIZE_out1(1),
    .value(1'b1)) const_3 (.out1(out_const_3));
  constant_value #(.BITSIZE_out1(2),
    .value(2'b10)) const_4 (.out1(out_const_4));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b100)) const_5 (.out1(out_const_5));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11110)) const_6 (.out1(out_const_6));
  constant_value #(.BITSIZE_out1(7),
    .value(7'b1111000)) const_7 (.out1(out_const_7));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) conv_in_port_dest_32_10 (.out1(out_conv_in_port_dest_32_10),
    .in1(in_port_dest));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) conv_in_port_src_32_10 (.out1(out_conv_in_port_src_32_10),
    .in1(in_port_src));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(8)) conv_out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0_32_8 (.out1(out_conv_out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0_32_8),
    .in1(out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0));
  UUdata_converter_FU #(.BITSIZE_in1(7),
    .BITSIZE_out1(6)) conv_out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0_7_6 (.out1(out_conv_out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0_7_6),
    .in1(out_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0));
  UUdata_converter_FU #(.BITSIZE_in1(5),
    .BITSIZE_out1(7)) conv_out_const_1_5_7 (.out1(out_conv_out_const_1_5_7),
    .in1(out_const_1));
  UUdata_converter_FU #(.BITSIZE_in1(8),
    .BITSIZE_out1(32)) conv_out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0_8_32 (.out1(out_conv_out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0_8_32),
    .in1(out_iu_conv_conn_obj_3_IUdata_converter_FU_iu_conv_0));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) conv_out_reg_1_reg_1_32_10 (.out1(out_conv_out_reg_1_reg_1_32_10),
    .in1(out_reg_1_reg_1));
  UUdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(32)) conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1_1_32 (.out1(out_conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1_1_32),
    .in1(out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_1));
  UUdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(32)) conv_out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2_1_32 (.out1(out_conv_out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2_1_32),
    .in1(out_u_assign_conn_obj_1_ASSIGN_UNSIGNED_FU_u_assign_2));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) conv_out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076_32_10 (.out1(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076_32_10),
    .in1(out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076));
  ui_lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu___internal_bambu_memcpy_329_34073 (.out1(out_ui_lshift_expr_FU_32_0_32_14_i0_fu___internal_bambu_memcpy_329_34073),
    .in1(out_reg_0_reg_0),
    .in2(out_const_4));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32),
    .LSB_PARAMETER(0)) fu___internal_bambu_memcpy_329_34076 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_20_i0_fu___internal_bambu_memcpy_329_34076),
    .in1(out_conv_in_port_src_32_10),
    .in2(out_ui_lshift_expr_FU_32_0_32_14_i0_fu___internal_bambu_memcpy_329_34073));
  ui_lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu___internal_bambu_memcpy_329_34080 (.out1(out_ui_lshift_expr_FU_32_0_32_14_i1_fu___internal_bambu_memcpy_329_34080),
    .in1(out_reg_0_reg_0),
    .in2(out_const_4));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32),
    .LSB_PARAMETER(0)) fu___internal_bambu_memcpy_329_34083 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_20_i1_fu___internal_bambu_memcpy_329_34083),
    .in1(out_conv_in_port_dest_32_10),
    .in2(out_ui_lshift_expr_FU_32_0_32_14_i1_fu___internal_bambu_memcpy_329_34080));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(32),
    .BITSIZE_out1(10),
    .LSB_PARAMETER(0)) fu___internal_bambu_memcpy_329_34130 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_19_i0_fu___internal_bambu_memcpy_329_34130),
    .in1(out_conv_in_port_src_32_10),
    .in2(out_reg_4_reg_4));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(32),
    .BITSIZE_out1(10),
    .LSB_PARAMETER(0)) fu___internal_bambu_memcpy_329_34134 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_19_i1_fu___internal_bambu_memcpy_329_34134),
    .in1(out_conv_in_port_dest_32_10),
    .in2(out_reg_4_reg_4));
  ui_gt_expr_FU #(.BITSIZE_in1(5),
    .BITSIZE_in2(32),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_34143 (.out1(out_ui_gt_expr_FU_0_32_32_13_i0_fu___internal_bambu_memcpy_329_34143),
    .in1(out_const_6),
    .in2(out_ui_plus_expr_FU_32_0_32_16_i0_fu___internal_bambu_memcpy_329_634));
  ui_ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(7),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_34151 (.out1(out_ui_ne_expr_FU_32_0_32_15_i0_fu___internal_bambu_memcpy_329_34151),
    .in1(out_ui_plus_expr_FU_32_0_32_17_i0_fu___internal_bambu_memcpy_329_662),
    .in2(out_const_7));
  ui_rshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(8),
    .PRECISION(32)) fu___internal_bambu_memcpy_329_34167 (.out1(out_ui_rshift_expr_FU_16_0_16_21_i0_fu___internal_bambu_memcpy_329_34167),
    .in1(out_conv_in_port_src_32_10),
    .in2(out_const_4));
  ui_rshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(8),
    .PRECISION(32)) fu___internal_bambu_memcpy_329_34173 (.out1(out_ui_rshift_expr_FU_16_0_16_22_i0_fu___internal_bambu_memcpy_329_34173),
    .in1(out_ui_pointer_plus_expr_FU_16_0_16_18_i0_fu___internal_bambu_memcpy_329_672),
    .in2(out_const_4));
  ui_rshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(8),
    .PRECISION(32)) fu___internal_bambu_memcpy_329_34177 (.out1(out_ui_rshift_expr_FU_16_0_16_22_i1_fu___internal_bambu_memcpy_329_34177),
    .in1(out_conv_in_port_dest_32_10),
    .in2(out_const_4));
  ui_rshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(8),
    .PRECISION(32)) fu___internal_bambu_memcpy_329_34180 (.out1(out_ui_rshift_expr_FU_16_0_16_21_i1_fu___internal_bambu_memcpy_329_34180),
    .in1(out_ui_pointer_plus_expr_FU_16_0_16_18_i1_fu___internal_bambu_memcpy_329_674),
    .in2(out_const_4));
  multi_read_cond_FU #(.BITSIZE_in1(1),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_34299 (.out1(out_multi_read_cond_FU_3_i0_fu___internal_bambu_memcpy_329_34299),
    .in1({out_lut_expr_FU_2_i0_fu___internal_bambu_memcpy_329_34302}));
  lut_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_34302 (.out1(out_lut_expr_FU_2_i0_fu___internal_bambu_memcpy_329_34302),
    .in1(out_const_3),
    .in2(out_ui_ge_expr_FU_8_8_8_12_i0_fu___internal_bambu_memcpy_329_673),
    .in3(out_ui_ge_expr_FU_8_8_8_12_i1_fu___internal_bambu_memcpy_329_675),
    .in4(1'b0),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  multi_read_cond_FU #(.BITSIZE_in1(1),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_34306 (.out1(out_multi_read_cond_FU_7_i0_fu___internal_bambu_memcpy_329_34306),
    .in1({out_reg_2_reg_2}));
  ui_plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1),
    .BITSIZE_out1(32)) fu___internal_bambu_memcpy_329_634 (.out1(out_ui_plus_expr_FU_32_0_32_16_i0_fu___internal_bambu_memcpy_329_634),
    .in1(out_reg_0_reg_0),
    .in2(out_const_3));
  ui_plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1),
    .BITSIZE_out1(32)) fu___internal_bambu_memcpy_329_662 (.out1(out_ui_plus_expr_FU_32_0_32_17_i0_fu___internal_bambu_memcpy_329_662),
    .in1(out_reg_4_reg_4),
    .in2(out_const_3));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(3),
    .BITSIZE_out1(10),
    .LSB_PARAMETER(0)) fu___internal_bambu_memcpy_329_672 (.out1(out_ui_pointer_plus_expr_FU_16_0_16_18_i0_fu___internal_bambu_memcpy_329_672),
    .in1(out_conv_in_port_dest_32_10),
    .in2(out_const_5));
  ui_ge_expr_FU #(.BITSIZE_in1(8),
    .BITSIZE_in2(8),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_673 (.out1(out_ui_ge_expr_FU_8_8_8_12_i0_fu___internal_bambu_memcpy_329_673),
    .in1(out_ui_rshift_expr_FU_16_0_16_21_i0_fu___internal_bambu_memcpy_329_34167),
    .in2(out_ui_rshift_expr_FU_16_0_16_22_i0_fu___internal_bambu_memcpy_329_34173));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(3),
    .BITSIZE_out1(10),
    .LSB_PARAMETER(0)) fu___internal_bambu_memcpy_329_674 (.out1(out_ui_pointer_plus_expr_FU_16_0_16_18_i1_fu___internal_bambu_memcpy_329_674),
    .in1(out_conv_in_port_src_32_10),
    .in2(out_const_5));
  ui_ge_expr_FU #(.BITSIZE_in1(8),
    .BITSIZE_in2(8),
    .BITSIZE_out1(1)) fu___internal_bambu_memcpy_329_675 (.out1(out_ui_ge_expr_FU_8_8_8_12_i1_fu___internal_bambu_memcpy_329_675),
    .in1(out_ui_rshift_expr_FU_16_0_16_22_i1_fu___internal_bambu_memcpy_329_34177),
    .in2(out_ui_rshift_expr_FU_16_0_16_21_i1_fu___internal_bambu_memcpy_329_34180));
  read_cond_FU #(.BITSIZE_in1(1)) fu___internal_bambu_memcpy_329_729 (.out1(out_read_cond_FU_10_i0_fu___internal_bambu_memcpy_329_729),
    .in1(out_reg_6_reg_6));
  join_signal #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(64)) join_signalbus_mergerMout_Wdata_ram0_0 (.out1(sig_in_bus_mergerMout_Wdata_ram0_0),
    .in1(sig_in_vector_bus_mergerMout_Wdata_ram0_0));
  join_signal #(.BITSIZE_in1(10),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(20)) join_signalbus_mergerMout_addr_ram1_0 (.out1(sig_in_bus_mergerMout_addr_ram1_0),
    .in1(sig_in_vector_bus_mergerMout_addr_ram1_0));
  join_signal #(.BITSIZE_in1(6),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(12)) join_signalbus_mergerMout_data_ram_size2_0 (.out1(sig_in_bus_mergerMout_data_ram_size2_0),
    .in1(sig_in_vector_bus_mergerMout_data_ram_size2_0));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerMout_oe_ram3_0 (.out1(sig_in_bus_mergerMout_oe_ram3_0),
    .in1(sig_in_vector_bus_mergerMout_oe_ram3_0));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerMout_we_ram4_0 (.out1(sig_in_bus_mergerMout_we_ram4_0),
    .in1(sig_in_vector_bus_mergerMout_we_ram4_0));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_0 (.out1(out_reg_0_reg_0),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_15_reg_0_0_0_0),
    .wenable(wrenable_reg_0));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_1 (.out1(out_reg_1_reg_1),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_32_32_32_20_i1_fu___internal_bambu_memcpy_329_34083),
    .wenable(wrenable_reg_1));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_2 (.out1(out_reg_2_reg_2),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_gt_expr_FU_0_32_32_13_i0_fu___internal_bambu_memcpy_329_34143),
    .wenable(wrenable_reg_2));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_3 (.out1(out_reg_3_reg_3),
    .clock(clock),
    .reset(reset),
    .in1(out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0),
    .wenable(wrenable_reg_3));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_4 (.out1(out_reg_4_reg_4),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_19_reg_4_0_0_0),
    .wenable(wrenable_reg_4));
  register_SE #(.BITSIZE_in1(10),
    .BITSIZE_out1(10)) reg_5 (.out1(out_reg_5_reg_5),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_19_i1_fu___internal_bambu_memcpy_329_34134),
    .wenable(wrenable_reg_5));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_6 (.out1(out_reg_6_reg_6),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_ne_expr_FU_32_0_32_15_i0_fu___internal_bambu_memcpy_329_34151),
    .wenable(wrenable_reg_6));
  register_STD #(.BITSIZE_in1(8),
    .BITSIZE_out1(8)) reg_7 (.out1(out_reg_7_reg_7),
    .clock(clock),
    .reset(reset),
    .in1(out_conv_out_BMEMORY_CTRLN_11_i0_BMEMORY_CTRLN_11_i0_32_8),
    .wenable(wrenable_reg_7));
  split_signal #(.BITSIZE_in1(64),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_Wdata_ram0_ (.out1(Mout_Wdata_ram),
    .in1(sig_out_bus_mergerMout_Wdata_ram0_));
  split_signal #(.BITSIZE_in1(20),
    .BITSIZE_out1(10),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_addr_ram1_ (.out1(Mout_addr_ram),
    .in1(sig_out_bus_mergerMout_addr_ram1_));
  split_signal #(.BITSIZE_in1(12),
    .BITSIZE_out1(6),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_data_ram_size2_ (.out1(Mout_data_ram_size),
    .in1(sig_out_bus_mergerMout_data_ram_size2_));
  split_signal #(.BITSIZE_in1(2),
    .BITSIZE_out1(1),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_oe_ram3_ (.out1(Mout_oe_ram),
    .in1(sig_out_bus_mergerMout_oe_ram3_));
  split_signal #(.BITSIZE_in1(2),
    .BITSIZE_out1(1),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_we_ram4_ (.out1(Mout_we_ram),
    .in1(sig_out_bus_mergerMout_we_ram4_));
  // io-signal post fix
  assign io_signal_in_port_dest_return_port = in_port_dest;
  assign return_port = io_signal_in_port_dest_return_port;
  assign OUT_CONDITION___internal_bambu_memcpy_329_729 = out_read_cond_FU_10_i0_fu___internal_bambu_memcpy_329_729;
  assign OUT_MULTIIF___internal_bambu_memcpy_329_34299 = out_multi_read_cond_FU_3_i0_fu___internal_bambu_memcpy_329_34299;
  assign OUT_MULTIIF___internal_bambu_memcpy_329_34306 = out_multi_read_cond_FU_7_i0_fu___internal_bambu_memcpy_329_34306;

endmodule

// FSM based controller description for __internal_bambu_memcpy
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module controller___internal_bambu_memcpy(done_port,
  fuselector_BMEMORY_CTRLN_11_i0_LOAD,
  fuselector_BMEMORY_CTRLN_11_i0_STORE,
  selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0,
  selector_MUX_15_reg_0_0_0_0,
  selector_MUX_19_reg_4_0_0_0,
  selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0,
  selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1,
  selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0,
  selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0,
  wrenable_reg_0,
  wrenable_reg_1,
  wrenable_reg_2,
  wrenable_reg_3,
  wrenable_reg_4,
  wrenable_reg_5,
  wrenable_reg_6,
  wrenable_reg_7,
  OUT_CONDITION___internal_bambu_memcpy_329_729,
  OUT_MULTIIF___internal_bambu_memcpy_329_34299,
  OUT_MULTIIF___internal_bambu_memcpy_329_34306,
  clock,
  reset,
  start_port);
  // IN
  input OUT_CONDITION___internal_bambu_memcpy_329_729;
  input OUT_MULTIIF___internal_bambu_memcpy_329_34299;
  input OUT_MULTIIF___internal_bambu_memcpy_329_34306;
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output fuselector_BMEMORY_CTRLN_11_i0_LOAD;
  output fuselector_BMEMORY_CTRLN_11_i0_STORE;
  output selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0;
  output selector_MUX_15_reg_0_0_0_0;
  output selector_MUX_19_reg_4_0_0_0;
  output selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0;
  output selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1;
  output selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0;
  output selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0;
  output wrenable_reg_0;
  output wrenable_reg_1;
  output wrenable_reg_2;
  output wrenable_reg_3;
  output wrenable_reg_4;
  output wrenable_reg_5;
  output wrenable_reg_6;
  output wrenable_reg_7;
  parameter [8:0] S_0 = 9'b000000001,
    S_1 = 9'b000000010,
    S_2 = 9'b000000100,
    S_3 = 9'b000001000,
    S_8 = 9'b100000000,
    S_4 = 9'b000010000,
    S_5 = 9'b000100000,
    S_6 = 9'b001000000,
    S_7 = 9'b010000000;
  reg [8:0] _present_state=S_0, _next_state;
  reg done_port;
  reg fuselector_BMEMORY_CTRLN_11_i0_LOAD;
  reg fuselector_BMEMORY_CTRLN_11_i0_STORE;
  reg selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0;
  reg selector_MUX_15_reg_0_0_0_0;
  reg selector_MUX_19_reg_4_0_0_0;
  reg selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0;
  reg selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1;
  reg selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0;
  reg selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0;
  reg wrenable_reg_0;
  reg wrenable_reg_1;
  reg wrenable_reg_2;
  reg wrenable_reg_3;
  reg wrenable_reg_4;
  reg wrenable_reg_5;
  reg wrenable_reg_6;
  reg wrenable_reg_7;
  
  always @(posedge clock)
    if (reset == 1'b0) _present_state <= S_0;
    else _present_state <= _next_state;
  
  always @(*)
  begin
    done_port = 1'b0;
    fuselector_BMEMORY_CTRLN_11_i0_LOAD = 1'b0;
    fuselector_BMEMORY_CTRLN_11_i0_STORE = 1'b0;
    selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0 = 1'b0;
    selector_MUX_15_reg_0_0_0_0 = 1'b0;
    selector_MUX_19_reg_4_0_0_0 = 1'b0;
    selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0 = 1'b0;
    selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1 = 1'b0;
    selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0 = 1'b0;
    selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0 = 1'b0;
    wrenable_reg_0 = 1'b0;
    wrenable_reg_1 = 1'b0;
    wrenable_reg_2 = 1'b0;
    wrenable_reg_3 = 1'b0;
    wrenable_reg_4 = 1'b0;
    wrenable_reg_5 = 1'b0;
    wrenable_reg_6 = 1'b0;
    wrenable_reg_7 = 1'b0;
    case (_present_state)
      S_0 :
        if(start_port == 1'b1)
        begin
          selector_MUX_15_reg_0_0_0_0 = 1'b1;
          selector_MUX_19_reg_4_0_0_0 = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_4 = 1'b1;
          casez (OUT_MULTIIF___internal_bambu_memcpy_329_34299)
            1'b1 :
              begin
                _next_state = S_4;
                selector_MUX_15_reg_0_0_0_0 = 1'b0;
                wrenable_reg_0 = 1'b0;
              end
            default:
              begin
                _next_state = S_1;
                selector_MUX_19_reg_4_0_0_0 = 1'b0;
                wrenable_reg_4 = 1'b0;
              end
          endcase
        end
        else
        begin
          _next_state = S_0;
        end
      S_1 :
        begin
          fuselector_BMEMORY_CTRLN_11_i0_LOAD = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_2 = 1'b1;
          _next_state = S_2;
        end
      S_2 :
        begin
          wrenable_reg_3 = 1'b1;
          _next_state = S_3;
        end
      S_3 :
        begin
          fuselector_BMEMORY_CTRLN_11_i0_STORE = 1'b1;
          selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0 = 1'b1;
          casez (OUT_MULTIIF___internal_bambu_memcpy_329_34306)
            1'b1 :
              begin
                _next_state = S_1;
              end
            default:
              begin
                _next_state = S_8;
                done_port = 1'b1;
              end
          endcase
        end
      S_8 :
        begin
          _next_state = S_0;
        end
      S_4 :
        begin
          fuselector_BMEMORY_CTRLN_11_i0_LOAD = 1'b1;
          selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1 = 1'b1;
          selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0 = 1'b1;
          wrenable_reg_4 = 1'b1;
          wrenable_reg_5 = 1'b1;
          wrenable_reg_6 = 1'b1;
          _next_state = S_5;
        end
      S_5 :
        begin
          wrenable_reg_7 = 1'b1;
          _next_state = S_6;
        end
      S_6 :
        begin
          fuselector_BMEMORY_CTRLN_11_i0_STORE = 1'b1;
          selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0 = 1'b1;
          selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0 = 1'b1;
          selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0 = 1'b1;
          selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0 = 1'b1;
          if (OUT_CONDITION___internal_bambu_memcpy_329_729 == 1'b1)
            begin
              _next_state = S_4;
            end
          else
            begin
              _next_state = S_7;
              done_port = 1'b1;
            end
        end
      S_7 :
        begin
          _next_state = S_0;
        end
      default :
        begin
          _next_state = S_0;
        end
    endcase
  end
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Marco Lattuada <marco.lattuada@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module flipflop_AR(clock,
  reset,
  in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input clock;
  input reset;
  input in1;
  // OUT
  output out1;
  
  reg reg_out1 =0;
  assign out1 = reg_out1;
  always @(posedge clock )
    if (reset == 1'b0)
      reg_out1 <= {BITSIZE_out1{1'b0}};
    else
      reg_out1 <= in1;
endmodule

// Top component for __internal_bambu_memcpy
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module __internal_bambu_memcpy(clock,
  reset,
  start_port,
  done_port,
  dest,
  src,
  return_port,
  M_Rdata_ram,
  M_DataRdy,
  Min_oe_ram,
  Min_we_ram,
  Min_addr_ram,
  Min_Wdata_ram,
  Min_data_ram_size,
  Mout_oe_ram,
  Mout_we_ram,
  Mout_addr_ram,
  Mout_Wdata_ram,
  Mout_data_ram_size);
  // IN
  input clock;
  input reset;
  input start_port;
  input [31:0] dest;
  input [31:0] src;
  input [63:0] M_Rdata_ram;
  input [1:0] M_DataRdy;
  input [1:0] Min_oe_ram;
  input [1:0] Min_we_ram;
  input [19:0] Min_addr_ram;
  input [63:0] Min_Wdata_ram;
  input [11:0] Min_data_ram_size;
  // OUT
  output done_port;
  output [31:0] return_port;
  output [1:0] Mout_oe_ram;
  output [1:0] Mout_we_ram;
  output [19:0] Mout_addr_ram;
  output [63:0] Mout_Wdata_ram;
  output [11:0] Mout_data_ram_size;
  // Component and signal declarations
  wire OUT_CONDITION___internal_bambu_memcpy_329_729;
  wire OUT_MULTIIF___internal_bambu_memcpy_329_34299;
  wire OUT_MULTIIF___internal_bambu_memcpy_329_34306;
  wire done_delayed_REG_signal_in;
  wire done_delayed_REG_signal_out;
  wire fuselector_BMEMORY_CTRLN_11_i0_LOAD;
  wire fuselector_BMEMORY_CTRLN_11_i0_STORE;
  wire selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0;
  wire selector_MUX_15_reg_0_0_0_0;
  wire selector_MUX_19_reg_4_0_0_0;
  wire selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0;
  wire selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1;
  wire selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0;
  wire selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0;
  wire wrenable_reg_0;
  wire wrenable_reg_1;
  wire wrenable_reg_2;
  wire wrenable_reg_3;
  wire wrenable_reg_4;
  wire wrenable_reg_5;
  wire wrenable_reg_6;
  wire wrenable_reg_7;
  
  controller___internal_bambu_memcpy Controller_i (.done_port(done_delayed_REG_signal_in),
    .fuselector_BMEMORY_CTRLN_11_i0_LOAD(fuselector_BMEMORY_CTRLN_11_i0_LOAD),
    .fuselector_BMEMORY_CTRLN_11_i0_STORE(fuselector_BMEMORY_CTRLN_11_i0_STORE),
    .selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0(selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0),
    .selector_MUX_15_reg_0_0_0_0(selector_MUX_15_reg_0_0_0_0),
    .selector_MUX_19_reg_4_0_0_0(selector_MUX_19_reg_4_0_0_0),
    .selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0),
    .selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1),
    .selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0),
    .selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0(selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0),
    .wrenable_reg_0(wrenable_reg_0),
    .wrenable_reg_1(wrenable_reg_1),
    .wrenable_reg_2(wrenable_reg_2),
    .wrenable_reg_3(wrenable_reg_3),
    .wrenable_reg_4(wrenable_reg_4),
    .wrenable_reg_5(wrenable_reg_5),
    .wrenable_reg_6(wrenable_reg_6),
    .wrenable_reg_7(wrenable_reg_7),
    .OUT_CONDITION___internal_bambu_memcpy_329_729(OUT_CONDITION___internal_bambu_memcpy_329_729),
    .OUT_MULTIIF___internal_bambu_memcpy_329_34299(OUT_MULTIIF___internal_bambu_memcpy_329_34299),
    .OUT_MULTIIF___internal_bambu_memcpy_329_34306(OUT_MULTIIF___internal_bambu_memcpy_329_34306),
    .clock(clock),
    .reset(reset),
    .start_port(start_port));
  datapath___internal_bambu_memcpy Datapath_i (.return_port(return_port),
    .Mout_oe_ram(Mout_oe_ram),
    .Mout_we_ram(Mout_we_ram),
    .Mout_addr_ram(Mout_addr_ram),
    .Mout_Wdata_ram(Mout_Wdata_ram),
    .Mout_data_ram_size(Mout_data_ram_size),
    .OUT_CONDITION___internal_bambu_memcpy_329_729(OUT_CONDITION___internal_bambu_memcpy_329_729),
    .OUT_MULTIIF___internal_bambu_memcpy_329_34299(OUT_MULTIIF___internal_bambu_memcpy_329_34299),
    .OUT_MULTIIF___internal_bambu_memcpy_329_34306(OUT_MULTIIF___internal_bambu_memcpy_329_34306),
    .clock(clock),
    .reset(reset),
    .in_port_dest(dest),
    .in_port_src(src),
    .M_Rdata_ram(M_Rdata_ram),
    .M_DataRdy(M_DataRdy),
    .Min_oe_ram(Min_oe_ram),
    .Min_we_ram(Min_we_ram),
    .Min_addr_ram(Min_addr_ram),
    .Min_Wdata_ram(Min_Wdata_ram),
    .Min_data_ram_size(Min_data_ram_size),
    .fuselector_BMEMORY_CTRLN_11_i0_LOAD(fuselector_BMEMORY_CTRLN_11_i0_LOAD),
    .fuselector_BMEMORY_CTRLN_11_i0_STORE(fuselector_BMEMORY_CTRLN_11_i0_STORE),
    .selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0(selector_MUX_0_BMEMORY_CTRLN_11_i0_0_0_0),
    .selector_MUX_15_reg_0_0_0_0(selector_MUX_15_reg_0_0_0_0),
    .selector_MUX_19_reg_4_0_0_0(selector_MUX_19_reg_4_0_0_0),
    .selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_0),
    .selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_0_1),
    .selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0(selector_MUX_1_BMEMORY_CTRLN_11_i0_1_1_0),
    .selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0(selector_MUX_2_BMEMORY_CTRLN_11_i0_2_0_0),
    .wrenable_reg_0(wrenable_reg_0),
    .wrenable_reg_1(wrenable_reg_1),
    .wrenable_reg_2(wrenable_reg_2),
    .wrenable_reg_3(wrenable_reg_3),
    .wrenable_reg_4(wrenable_reg_4),
    .wrenable_reg_5(wrenable_reg_5),
    .wrenable_reg_6(wrenable_reg_6),
    .wrenable_reg_7(wrenable_reg_7));
  flipflop_AR #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) done_delayed_REG (.out1(done_delayed_REG_signal_out),
    .clock(clock),
    .reset(reset),
    .in1(done_delayed_REG_signal_in));
  // io-signal post fix
  assign done_port = done_delayed_REG_signal_out;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ASSIGN_SIGNED_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module IIdata_converter_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  generate
  if (BITSIZE_out1 <= BITSIZE_in1)
  begin
    assign out1 = in1[BITSIZE_out1-1:0];
  end
  else
  begin
    assign out1 = {{(BITSIZE_out1-BITSIZE_in1){in1[BITSIZE_in1-1]}},in1};
  end
  endgenerate
endmodule

// Datapath RTL description for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module datapath_main(clock,
  reset,
  return_port,
  S_oe_ram,
  S_we_ram,
  S_addr_ram,
  S_Wdata_ram,
  S_data_ram_size,
  M_Rdata_ram,
  M_DataRdy,
  Sin_Rdata_ram,
  Sin_DataRdy,
  Sout_Rdata_ram,
  Sout_DataRdy,
  Min_oe_ram,
  Min_we_ram,
  Min_addr_ram,
  Min_Wdata_ram,
  Min_data_ram_size,
  Mout_oe_ram,
  Mout_we_ram,
  Mout_addr_ram,
  Mout_Wdata_ram,
  Mout_data_ram_size,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE,
  fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE,
  selector_IN_UNBOUNDED_main_33672_34057,
  selector_IN_UNBOUNDED_main_33672_34059,
  selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0,
  selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1,
  selector_MUX_206_reg_6_0_0_0,
  selector_MUX_207_reg_7_0_0_0,
  selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0,
  selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1,
  selector_MUX_92_gimple_return_FU_27_i0_0_0_0,
  wrenable_reg_0,
  wrenable_reg_1,
  wrenable_reg_10,
  wrenable_reg_11,
  wrenable_reg_12,
  wrenable_reg_13,
  wrenable_reg_14,
  wrenable_reg_15,
  wrenable_reg_16,
  wrenable_reg_17,
  wrenable_reg_18,
  wrenable_reg_19,
  wrenable_reg_2,
  wrenable_reg_20,
  wrenable_reg_21,
  wrenable_reg_3,
  wrenable_reg_4,
  wrenable_reg_5,
  wrenable_reg_6,
  wrenable_reg_7,
  wrenable_reg_8,
  wrenable_reg_9,
  OUT_CONDITION_main_33672_33808,
  OUT_CONDITION_main_33672_33847,
  OUT_UNBOUNDED_main_33672_34057,
  OUT_UNBOUNDED_main_33672_34059);
  parameter MEM_var_33690_33672=128,
    MEM_var_33691_33672=256,
    MEM_var_33764_33672=384,
    MEM_var_33765_33672=512;
  // IN
  input clock;
  input reset;
  input [1:0] S_oe_ram;
  input [1:0] S_we_ram;
  input [19:0] S_addr_ram;
  input [63:0] S_Wdata_ram;
  input [11:0] S_data_ram_size;
  input [63:0] M_Rdata_ram;
  input [1:0] M_DataRdy;
  input [63:0] Sin_Rdata_ram;
  input [1:0] Sin_DataRdy;
  input [1:0] Min_oe_ram;
  input [1:0] Min_we_ram;
  input [19:0] Min_addr_ram;
  input [63:0] Min_Wdata_ram;
  input [11:0] Min_data_ram_size;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  input fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE;
  input selector_IN_UNBOUNDED_main_33672_34057;
  input selector_IN_UNBOUNDED_main_33672_34059;
  input selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0;
  input selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1;
  input selector_MUX_206_reg_6_0_0_0;
  input selector_MUX_207_reg_7_0_0_0;
  input selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0;
  input selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1;
  input selector_MUX_92_gimple_return_FU_27_i0_0_0_0;
  input wrenable_reg_0;
  input wrenable_reg_1;
  input wrenable_reg_10;
  input wrenable_reg_11;
  input wrenable_reg_12;
  input wrenable_reg_13;
  input wrenable_reg_14;
  input wrenable_reg_15;
  input wrenable_reg_16;
  input wrenable_reg_17;
  input wrenable_reg_18;
  input wrenable_reg_19;
  input wrenable_reg_2;
  input wrenable_reg_20;
  input wrenable_reg_21;
  input wrenable_reg_3;
  input wrenable_reg_4;
  input wrenable_reg_5;
  input wrenable_reg_6;
  input wrenable_reg_7;
  input wrenable_reg_8;
  input wrenable_reg_9;
  // OUT
  output signed [31:0] return_port;
  output [63:0] Sout_Rdata_ram;
  output [1:0] Sout_DataRdy;
  output [1:0] Mout_oe_ram;
  output [1:0] Mout_we_ram;
  output [19:0] Mout_addr_ram;
  output [63:0] Mout_Wdata_ram;
  output [11:0] Mout_data_ram_size;
  output OUT_CONDITION_main_33672_33808;
  output OUT_CONDITION_main_33672_33847;
  output OUT_UNBOUNDED_main_33672_34057;
  output OUT_UNBOUNDED_main_33672_34059;
  // Component and signal declarations
  wire [31:0] null_out_signal_array_33690_0_out1_1;
  wire [31:0] null_out_signal_array_33690_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33690_0_proxy_out1_1;
  wire [31:0] null_out_signal_array_33691_0_out1_0;
  wire [31:0] null_out_signal_array_33691_0_out1_1;
  wire [31:0] null_out_signal_array_33691_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33691_0_proxy_out1_1;
  wire [31:0] null_out_signal_array_33764_0_out1_1;
  wire [31:0] null_out_signal_array_33764_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33764_0_proxy_out1_1;
  wire [31:0] null_out_signal_array_33765_0_out1_0;
  wire [31:0] null_out_signal_array_33765_0_out1_1;
  wire [31:0] null_out_signal_array_33765_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33765_0_proxy_out1_1;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_0_i0_array_33690_0;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_2_i0_array_33764_0;
  wire [6:0] out_IUdata_converter_FU_14_i0_fu_main_33672_33802;
  wire [31:0] out_IUdata_converter_FU_16_i0_fu_main_33672_33855;
  wire [31:0] out_IUdata_converter_FU_17_i0_fu_main_33672_33868;
  wire [31:0] out_IUdata_converter_FU_18_i0_fu_main_33672_33878;
  wire [31:0] out_IUdata_converter_FU_19_i0_fu_main_33672_33888;
  wire [31:0] out_IUdata_converter_FU_20_i0_fu_main_33672_33898;
  wire [31:0] out_IUdata_converter_FU_21_i0_fu_main_33672_33908;
  wire [31:0] out_IUdata_converter_FU_22_i0_fu_main_33672_33917;
  wire [31:0] out_IUdata_converter_FU_23_i0_fu_main_33672_33926;
  wire [31:0] out_IUdata_converter_FU_24_i0_fu_main_33672_33935;
  wire [31:0] out_IUdata_converter_FU_25_i0_fu_main_33672_33944;
  wire [31:0] out_MUX_19___internal_bambu_memcpy_75_i0_0_0_0;
  wire [31:0] out_MUX_19___internal_bambu_memcpy_75_i0_0_0_1;
  wire [31:0] out_MUX_206_reg_6_0_0_0;
  wire [31:0] out_MUX_207_reg_7_0_0_0;
  wire [31:0] out_MUX_20___internal_bambu_memcpy_75_i0_1_0_0;
  wire [31:0] out_MUX_20___internal_bambu_memcpy_75_i0_1_0_1;
  wire [31:0] out_MUX_92_gimple_return_FU_27_i0_0_0_0;
  wire [7:0] out_addr_expr_FU_10_i0_fu_main_33672_34013;
  wire [8:0] out_addr_expr_FU_11_i0_fu_main_33672_34023;
  wire [9:0] out_addr_expr_FU_6_i0_fu_main_33672_33982;
  wire [9:0] out_addr_expr_FU_7_i0_fu_main_33672_33987;
  wire [9:0] out_addr_expr_FU_8_i0_fu_main_33672_33991;
  wire [9:0] out_addr_expr_FU_9_i0_fu_main_33672_33995;
  wire signed [1:0] out_bit_and_expr_FU_8_0_8_37_i0_fu_main_33672_34207;
  wire signed [1:0] out_bit_and_expr_FU_8_0_8_37_i1_fu_main_33672_34221;
  wire signed [1:0] out_bit_and_expr_FU_8_0_8_37_i2_fu_main_33672_34264;
  wire signed [3:0] out_bit_and_expr_FU_8_0_8_38_i0_fu_main_33672_34238;
  wire signed [3:0] out_bit_and_expr_FU_8_0_8_38_i1_fu_main_33672_34251;
  wire signed [2:0] out_bit_and_expr_FU_8_0_8_39_i0_fu_main_33672_34280;
  wire signed [2:0] out_bit_and_expr_FU_8_0_8_39_i1_fu_main_33672_34294;
  wire signed [31:0] out_bit_ior_concat_expr_FU_40_i0_fu_main_33672_33846;
  wire signed [31:0] out_bit_ior_concat_expr_FU_40_i1_fu_main_33672_33867;
  wire signed [31:0] out_bit_ior_concat_expr_FU_40_i2_fu_main_33672_33925;
  wire signed [31:0] out_bit_ior_concat_expr_FU_41_i0_fu_main_33672_33907;
  wire signed [31:0] out_bit_ior_concat_expr_FU_41_i1_fu_main_33672_33916;
  wire signed [31:0] out_bit_ior_concat_expr_FU_42_i0_fu_main_33672_33934;
  wire signed [31:0] out_bit_ior_concat_expr_FU_42_i1_fu_main_33672_33943;
  wire signed [11:0] out_cond_expr_FU_16_16_16_16_43_i0_fu_main_33672_34318;
  wire signed [10:0] out_cond_expr_FU_16_16_16_16_43_i1_fu_main_33672_34347;
  wire signed [10:0] out_cond_expr_FU_16_16_16_16_43_i2_fu_main_33672_34355;
  wire signed [11:0] out_cond_expr_FU_16_16_16_16_43_i3_fu_main_33672_34363;
  wire signed [11:0] out_cond_expr_FU_16_16_16_16_43_i4_fu_main_33672_34371;
  wire signed [12:0] out_cond_expr_FU_16_16_16_16_43_i5_fu_main_33672_34379;
  wire signed [12:0] out_cond_expr_FU_16_16_16_16_43_i6_fu_main_33672_34387;
  wire signed [12:0] out_cond_expr_FU_16_16_16_16_43_i7_fu_main_33672_34395;
  wire signed [12:0] out_cond_expr_FU_16_16_16_16_43_i8_fu_main_33672_34403;
  wire signed [12:0] out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411;
  wire out_const_0;
  wire [1:0] out_const_1;
  wire [6:0] out_const_10;
  wire [12:0] out_const_11;
  wire [13:0] out_const_12;
  wire [2:0] out_const_13;
  wire [14:0] out_const_14;
  wire [10:0] out_const_15;
  wire [11:0] out_const_16;
  wire [12:0] out_const_17;
  wire [8:0] out_const_18;
  wire [8:0] out_const_19;
  wire [2:0] out_const_2;
  wire [11:0] out_const_20;
  wire [3:0] out_const_21;
  wire [11:0] out_const_22;
  wire [13:0] out_const_23;
  wire [12:0] out_const_24;
  wire [5:0] out_const_25;
  wire out_const_26;
  wire [1:0] out_const_27;
  wire [10:0] out_const_28;
  wire [10:0] out_const_29;
  wire [6:0] out_const_3;
  wire [10:0] out_const_30;
  wire [14:0] out_const_31;
  wire [11:0] out_const_32;
  wire [3:0] out_const_33;
  wire [10:0] out_const_34;
  wire [10:0] out_const_35;
  wire [3:0] out_const_36;
  wire [7:0] out_const_37;
  wire [7:0] out_const_38;
  wire [15:0] out_const_39;
  wire [12:0] out_const_4;
  wire [10:0] out_const_40;
  wire [11:0] out_const_41;
  wire [15:0] out_const_42;
  wire [31:0] out_const_43;
  wire [63:0] out_const_44;
  wire [7:0] out_const_45;
  wire [8:0] out_const_46;
  wire [8:0] out_const_47;
  wire [9:0] out_const_48;
  wire [9:0] out_const_5;
  wire [13:0] out_const_6;
  wire [10:0] out_const_7;
  wire [11:0] out_const_8;
  wire [12:0] out_const_9;
  wire [31:0] out_conv_out_addr_expr_FU_6_i0_fu_main_33672_33982_10_32;
  wire [31:0] out_conv_out_addr_expr_FU_7_i0_fu_main_33672_33987_10_32;
  wire signed [31:0] out_conv_out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411_I_13_I_32;
  wire signed [31:0] out_conv_out_const_10_I_7_I_32;
  wire [5:0] out_conv_out_const_3_7_6;
  wire [31:0] out_conv_out_const_45_8_32;
  wire [31:0] out_conv_out_const_46_9_32;
  wire [31:0] out_conv_out_const_47_9_32;
  wire [31:0] out_conv_out_const_48_10_32;
  wire [31:0] out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32;
  wire [31:0] out_conv_out_reg_0_reg_0_10_32;
  wire [31:0] out_conv_out_reg_1_reg_1_10_32;
  wire [31:0] out_conv_out_reg_2_reg_2_10_32;
  wire [31:0] out_conv_out_reg_3_reg_3_10_32;
  wire [9:0] out_conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016_8_10;
  wire [9:0] out_conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026_9_10;
  wire signed [0:0] out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0;
  wire signed [12:0] out_lshift_expr_FU_16_0_16_44_i0_fu_main_33672_34327;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_45_i0_fu_main_33672_34204;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_45_i1_fu_main_33672_34218;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_45_i2_fu_main_33672_34261;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_46_i0_fu_main_33672_34234;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_46_i1_fu_main_33672_34248;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_47_i0_fu_main_33672_34277;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_47_i1_fu_main_33672_34291;
  wire out_lt_expr_FU_32_0_32_48_i0_fu_main_33672_34033;
  wire out_lut_expr_FU_28_i0_fu_main_33672_34352;
  wire out_lut_expr_FU_29_i0_fu_main_33672_34360;
  wire out_lut_expr_FU_30_i0_fu_main_33672_34368;
  wire out_lut_expr_FU_31_i0_fu_main_33672_34376;
  wire out_lut_expr_FU_32_i0_fu_main_33672_34384;
  wire out_lut_expr_FU_33_i0_fu_main_33672_34424;
  wire out_lut_expr_FU_34_i0_fu_main_33672_34392;
  wire out_lut_expr_FU_35_i0_fu_main_33672_34400;
  wire out_lut_expr_FU_36_i0_fu_main_33672_34408;
  wire signed [31:0] out_mult_expr_FU_32_32_32_0_49_i0_fu_main_33672_33805;
  wire out_ne_expr_FU_32_0_32_50_i0_fu_main_33672_34031;
  wire signed [31:0] out_plus_expr_FU_32_0_32_51_i0_fu_main_33672_33807;
  wire signed [31:0] out_plus_expr_FU_32_0_32_52_i0_fu_main_33672_33854;
  wire signed [31:0] out_plus_expr_FU_32_0_32_53_i0_fu_main_33672_33877;
  wire signed [31:0] out_plus_expr_FU_32_0_32_54_i0_fu_main_33672_33887;
  wire signed [31:0] out_plus_expr_FU_32_0_32_55_i0_fu_main_33672_33897;
  wire signed [31:0] out_plus_expr_FU_32_0_32_56_i0_fu_main_33672_34199;
  wire signed [31:0] out_plus_expr_FU_32_0_32_57_i0_fu_main_33672_34215;
  wire signed [29:0] out_plus_expr_FU_32_0_32_58_i0_fu_main_33672_34231;
  wire signed [29:0] out_plus_expr_FU_32_0_32_59_i0_fu_main_33672_34245;
  wire signed [31:0] out_plus_expr_FU_32_0_32_60_i0_fu_main_33672_34258;
  wire signed [30:0] out_plus_expr_FU_32_0_32_61_i0_fu_main_33672_34274;
  wire signed [30:0] out_plus_expr_FU_32_0_32_62_i0_fu_main_33672_34288;
  wire signed [31:0] out_plus_expr_FU_32_32_32_63_i0_fu_main_33672_33806;
  wire out_read_cond_FU_15_i0_fu_main_33672_33808;
  wire out_read_cond_FU_26_i0_fu_main_33672_33847;
  wire [9:0] out_reg_0_reg_0;
  wire [31:0] out_reg_10_reg_10;
  wire [31:0] out_reg_11_reg_11;
  wire [31:0] out_reg_12_reg_12;
  wire [31:0] out_reg_13_reg_13;
  wire [31:0] out_reg_14_reg_14;
  wire [31:0] out_reg_15_reg_15;
  wire [31:0] out_reg_16_reg_16;
  wire [31:0] out_reg_17_reg_17;
  wire [31:0] out_reg_18_reg_18;
  wire [31:0] out_reg_19_reg_19;
  wire [9:0] out_reg_1_reg_1;
  wire [31:0] out_reg_20_reg_20;
  wire [31:0] out_reg_21_reg_21;
  wire [9:0] out_reg_2_reg_2;
  wire [9:0] out_reg_3_reg_3;
  wire [7:0] out_reg_4_reg_4;
  wire [8:0] out_reg_5_reg_5;
  wire [31:0] out_reg_6_reg_6;
  wire [31:0] out_reg_7_reg_7;
  wire out_reg_8_reg_8;
  wire [31:0] out_reg_9_reg_9;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_64_i0_fu_main_33672_34194;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_64_i1_fu_main_33672_34212;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_64_i2_fu_main_33672_34255;
  wire signed [28:0] out_rshift_expr_FU_32_0_32_65_i0_fu_main_33672_34226;
  wire signed [28:0] out_rshift_expr_FU_32_0_32_65_i1_fu_main_33672_34243;
  wire signed [29:0] out_rshift_expr_FU_32_0_32_66_i0_fu_main_33672_34269;
  wire signed [29:0] out_rshift_expr_FU_32_0_32_66_i1_fu_main_33672_34285;
  wire out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035;
  wire out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037;
  wire out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039;
  wire out_ui_le_expr_FU_32_0_32_69_i1_fu_main_33672_34047;
  wire out_ui_le_expr_FU_32_0_32_69_i2_fu_main_33672_34049;
  wire out_ui_le_expr_FU_32_0_32_70_i0_fu_main_33672_34041;
  wire out_ui_le_expr_FU_32_0_32_70_i1_fu_main_33672_34045;
  wire out_ui_le_expr_FU_32_0_32_70_i2_fu_main_33672_34051;
  wire out_ui_le_expr_FU_32_0_32_71_i0_fu_main_33672_34043;
  wire out_ui_le_expr_FU_32_0_32_71_i1_fu_main_33672_34053;
  wire [8:0] out_ui_lshift_expr_FU_16_0_16_72_i0_fu_main_33672_34020;
  wire [7:0] out_ui_lshift_expr_FU_8_0_8_73_i0_fu_main_33672_34010;
  wire [7:0] out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016;
  wire [8:0] out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026;
  wire s___internal_bambu_memcpy_75_i00;
  wire s_done___internal_bambu_memcpy_75_i0;
  wire [63:0] sig_in_bus_mergerMout_Wdata_ram0_0;
  wire [19:0] sig_in_bus_mergerMout_addr_ram1_0;
  wire [11:0] sig_in_bus_mergerMout_data_ram_size2_0;
  wire [1:0] sig_in_bus_mergerMout_oe_ram3_0;
  wire [1:0] sig_in_bus_mergerMout_we_ram4_0;
  wire [1:0] sig_in_bus_mergerSout_DataRdy5_0;
  wire [1:0] sig_in_bus_mergerSout_DataRdy5_1;
  wire [1:0] sig_in_bus_mergerSout_DataRdy5_2;
  wire [1:0] sig_in_bus_mergerSout_DataRdy5_3;
  wire [63:0] sig_in_bus_mergerSout_Rdata_ram6_0;
  wire [63:0] sig_in_bus_mergerSout_Rdata_ram6_1;
  wire [63:0] sig_in_bus_mergerSout_Rdata_ram6_2;
  wire [63:0] sig_in_bus_mergerSout_Rdata_ram6_3;
  wire [63:0] sig_in_vector_bus_mergerMout_Wdata_ram0_0;
  wire [19:0] sig_in_vector_bus_mergerMout_addr_ram1_0;
  wire [11:0] sig_in_vector_bus_mergerMout_data_ram_size2_0;
  wire [1:0] sig_in_vector_bus_mergerMout_oe_ram3_0;
  wire [1:0] sig_in_vector_bus_mergerMout_we_ram4_0;
  wire [1:0] sig_in_vector_bus_mergerSout_DataRdy5_0;
  wire [1:0] sig_in_vector_bus_mergerSout_DataRdy5_1;
  wire [1:0] sig_in_vector_bus_mergerSout_DataRdy5_2;
  wire [1:0] sig_in_vector_bus_mergerSout_DataRdy5_3;
  wire [63:0] sig_in_vector_bus_mergerSout_Rdata_ram6_0;
  wire [63:0] sig_in_vector_bus_mergerSout_Rdata_ram6_1;
  wire [63:0] sig_in_vector_bus_mergerSout_Rdata_ram6_2;
  wire [63:0] sig_in_vector_bus_mergerSout_Rdata_ram6_3;
  wire [63:0] sig_out_bus_mergerMout_Wdata_ram0_;
  wire [19:0] sig_out_bus_mergerMout_addr_ram1_;
  wire [11:0] sig_out_bus_mergerMout_data_ram_size2_;
  wire [1:0] sig_out_bus_mergerMout_oe_ram3_;
  wire [1:0] sig_out_bus_mergerMout_we_ram4_;
  wire [1:0] sig_out_bus_mergerSout_DataRdy5_;
  wire [63:0] sig_out_bus_mergerSout_Rdata_ram6_;
  
  ASSIGN_SIGNED_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) ASSIGN_SIGNED_FU_i_assign_0 (.out1(out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0),
    .in1(out_const_0));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_19___internal_bambu_memcpy_75_i0_0_0_0 (.out1(out_MUX_19___internal_bambu_memcpy_75_i0_0_0_0),
    .sel(selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0),
    .in1(out_conv_out_reg_3_reg_3_10_32),
    .in2(out_conv_out_reg_1_reg_1_10_32));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_19___internal_bambu_memcpy_75_i0_0_0_1 (.out1(out_MUX_19___internal_bambu_memcpy_75_i0_0_0_1),
    .sel(selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1),
    .in1(out_conv_out_addr_expr_FU_7_i0_fu_main_33672_33987_10_32),
    .in2(out_MUX_19___internal_bambu_memcpy_75_i0_0_0_0));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_206_reg_6_0_0_0 (.out1(out_MUX_206_reg_6_0_0_0),
    .sel(selector_MUX_206_reg_6_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_32_32_63_i0_fu_main_33672_33806));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_207_reg_7_0_0_0 (.out1(out_MUX_207_reg_7_0_0_0),
    .sel(selector_MUX_207_reg_7_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_0_32_51_i0_fu_main_33672_33807));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_20___internal_bambu_memcpy_75_i0_1_0_0 (.out1(out_MUX_20___internal_bambu_memcpy_75_i0_1_0_0),
    .sel(selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0),
    .in1(out_conv_out_reg_2_reg_2_10_32),
    .in2(out_conv_out_reg_0_reg_0_10_32));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_20___internal_bambu_memcpy_75_i0_1_0_1 (.out1(out_MUX_20___internal_bambu_memcpy_75_i0_1_0_1),
    .sel(selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1),
    .in1(out_conv_out_addr_expr_FU_6_i0_fu_main_33672_33982_10_32),
    .in2(out_MUX_20___internal_bambu_memcpy_75_i0_1_0_0));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_92_gimple_return_FU_27_i0_0_0_0 (.out1(out_MUX_92_gimple_return_FU_27_i0_0_0_0),
    .sel(selector_MUX_92_gimple_return_FU_27_i0_0_0_0),
    .in1(out_conv_out_const_10_I_7_I_32),
    .in2(out_conv_out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411_I_13_I_32));
  __internal_bambu_memcpy __internal_bambu_memcpy_75_i0 (.done_port(s_done___internal_bambu_memcpy_75_i0),
    .Mout_oe_ram(sig_in_vector_bus_mergerMout_oe_ram3_0),
    .Mout_we_ram(sig_in_vector_bus_mergerMout_we_ram4_0),
    .Mout_addr_ram(sig_in_vector_bus_mergerMout_addr_ram1_0),
    .Mout_Wdata_ram(sig_in_vector_bus_mergerMout_Wdata_ram0_0),
    .Mout_data_ram_size(sig_in_vector_bus_mergerMout_data_ram_size2_0),
    .clock(clock),
    .reset(reset),
    .start_port(s___internal_bambu_memcpy_75_i00),
    .dest(out_MUX_19___internal_bambu_memcpy_75_i0_0_0_1),
    .src(out_MUX_20___internal_bambu_memcpy_75_i0_1_0_1),
    .M_Rdata_ram(M_Rdata_ram),
    .M_DataRdy(M_DataRdy),
    .Min_oe_ram(Min_oe_ram),
    .Min_we_ram(Min_we_ram),
    .Min_addr_ram(Min_addr_ram),
    .Min_Wdata_ram(Min_Wdata_ram),
    .Min_data_ram_size(Min_data_ram_size));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(10),
    .PORTSIZE_in2(2),
    .BITSIZE_in3(6),
    .PORTSIZE_in3(2),
    .BITSIZE_in4(1),
    .PORTSIZE_in4(2),
    .BITSIZE_sel_LOAD(1),
    .PORTSIZE_sel_LOAD(2),
    .BITSIZE_sel_STORE(1),
    .PORTSIZE_sel_STORE(2),
    .BITSIZE_S_oe_ram(1),
    .PORTSIZE_S_oe_ram(2),
    .BITSIZE_S_we_ram(1),
    .PORTSIZE_S_we_ram(2),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2),
    .BITSIZE_S_addr_ram(10),
    .PORTSIZE_S_addr_ram(2),
    .BITSIZE_S_Wdata_ram(32),
    .PORTSIZE_S_Wdata_ram(2),
    .BITSIZE_Sin_Rdata_ram(32),
    .PORTSIZE_Sin_Rdata_ram(2),
    .BITSIZE_Sout_Rdata_ram(32),
    .PORTSIZE_Sout_Rdata_ram(2),
    .BITSIZE_S_data_ram_size(6),
    .PORTSIZE_S_data_ram_size(2),
    .BITSIZE_Sin_DataRdy(1),
    .PORTSIZE_Sin_DataRdy(2),
    .BITSIZE_Sout_DataRdy(1),
    .PORTSIZE_Sout_DataRdy(2),
    .MEMORY_INIT_file_a("array_ref_33690.mem"),
    .MEMORY_INIT_file_b("0_array_ref_33690.mem"),
    .n_elements(30),
    .data_size(32),
    .address_space_begin(MEM_var_33690_33672),
    .address_space_rangesize(128),
    .BUS_PIPELINED(1),
    .BRAM_BITSIZE(16),
    .PRIVATE_MEMORY(0),
    .USE_SPARSE_MEMORY(1),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(10),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33690_0 (.out1({null_out_signal_array_33690_0_out1_1,
      out_ARRAY_1D_STD_BRAM_NN_0_i0_array_33690_0}),
    .Sout_Rdata_ram(sig_in_vector_bus_mergerSout_Rdata_ram6_0),
    .Sout_DataRdy(sig_in_vector_bus_mergerSout_DataRdy5_0),
    .proxy_out1({null_out_signal_array_33690_0_proxy_out1_1,
      null_out_signal_array_33690_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({10'b0000000000,
      out_conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016_8_10}),
    .in3({6'b000000,
      out_conv_out_const_3_7_6}),
    .in4({1'b0,
      out_const_26}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE}),
    .S_oe_ram(S_oe_ram),
    .S_we_ram(S_we_ram),
    .S_addr_ram(S_addr_ram),
    .S_Wdata_ram(S_Wdata_ram),
    .Sin_Rdata_ram(Sin_Rdata_ram),
    .S_data_ram_size(S_data_ram_size),
    .Sin_DataRdy(Sin_DataRdy),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({10'b0000000000,
      10'b0000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(10),
    .PORTSIZE_in2(2),
    .BITSIZE_in3(6),
    .PORTSIZE_in3(2),
    .BITSIZE_in4(1),
    .PORTSIZE_in4(2),
    .BITSIZE_sel_LOAD(1),
    .PORTSIZE_sel_LOAD(2),
    .BITSIZE_sel_STORE(1),
    .PORTSIZE_sel_STORE(2),
    .BITSIZE_S_oe_ram(1),
    .PORTSIZE_S_oe_ram(2),
    .BITSIZE_S_we_ram(1),
    .PORTSIZE_S_we_ram(2),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2),
    .BITSIZE_S_addr_ram(10),
    .PORTSIZE_S_addr_ram(2),
    .BITSIZE_S_Wdata_ram(32),
    .PORTSIZE_S_Wdata_ram(2),
    .BITSIZE_Sin_Rdata_ram(32),
    .PORTSIZE_Sin_Rdata_ram(2),
    .BITSIZE_Sout_Rdata_ram(32),
    .PORTSIZE_Sout_Rdata_ram(2),
    .BITSIZE_S_data_ram_size(6),
    .PORTSIZE_S_data_ram_size(2),
    .BITSIZE_Sin_DataRdy(1),
    .PORTSIZE_Sin_DataRdy(2),
    .BITSIZE_Sout_DataRdy(1),
    .PORTSIZE_Sout_DataRdy(2),
    .MEMORY_INIT_file_a("array_ref_33691.mem"),
    .MEMORY_INIT_file_b("0_array_ref_33691.mem"),
    .n_elements(30),
    .data_size(32),
    .address_space_begin(MEM_var_33691_33672),
    .address_space_rangesize(128),
    .BUS_PIPELINED(1),
    .BRAM_BITSIZE(16),
    .PRIVATE_MEMORY(0),
    .USE_SPARSE_MEMORY(1),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(10),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33691_0 (.out1({null_out_signal_array_33691_0_out1_1,
      null_out_signal_array_33691_0_out1_0}),
    .Sout_Rdata_ram(sig_in_vector_bus_mergerSout_Rdata_ram6_1),
    .Sout_DataRdy(sig_in_vector_bus_mergerSout_DataRdy5_1),
    .proxy_out1({null_out_signal_array_33691_0_proxy_out1_1,
      null_out_signal_array_33691_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({10'b0000000000,
      10'b0000000000}),
    .in3({6'b000000,
      6'b000000}),
    .in4({1'b0,
      1'b0}),
    .sel_LOAD({1'b0,
      1'b0}),
    .sel_STORE({1'b0,
      1'b0}),
    .S_oe_ram(S_oe_ram),
    .S_we_ram(S_we_ram),
    .S_addr_ram(S_addr_ram),
    .S_Wdata_ram(S_Wdata_ram),
    .Sin_Rdata_ram(Sin_Rdata_ram),
    .S_data_ram_size(S_data_ram_size),
    .Sin_DataRdy(Sin_DataRdy),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({10'b0000000000,
      10'b0000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(10),
    .PORTSIZE_in2(2),
    .BITSIZE_in3(6),
    .PORTSIZE_in3(2),
    .BITSIZE_in4(1),
    .PORTSIZE_in4(2),
    .BITSIZE_sel_LOAD(1),
    .PORTSIZE_sel_LOAD(2),
    .BITSIZE_sel_STORE(1),
    .PORTSIZE_sel_STORE(2),
    .BITSIZE_S_oe_ram(1),
    .PORTSIZE_S_oe_ram(2),
    .BITSIZE_S_we_ram(1),
    .PORTSIZE_S_we_ram(2),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2),
    .BITSIZE_S_addr_ram(10),
    .PORTSIZE_S_addr_ram(2),
    .BITSIZE_S_Wdata_ram(32),
    .PORTSIZE_S_Wdata_ram(2),
    .BITSIZE_Sin_Rdata_ram(32),
    .PORTSIZE_Sin_Rdata_ram(2),
    .BITSIZE_Sout_Rdata_ram(32),
    .PORTSIZE_Sout_Rdata_ram(2),
    .BITSIZE_S_data_ram_size(6),
    .PORTSIZE_S_data_ram_size(2),
    .BITSIZE_Sin_DataRdy(1),
    .PORTSIZE_Sin_DataRdy(2),
    .BITSIZE_Sout_DataRdy(1),
    .PORTSIZE_Sout_DataRdy(2),
    .MEMORY_INIT_file_a("array_ref_33764.mem"),
    .MEMORY_INIT_file_b("0_array_ref_33764.mem"),
    .n_elements(30),
    .data_size(32),
    .address_space_begin(MEM_var_33764_33672),
    .address_space_rangesize(128),
    .BUS_PIPELINED(1),
    .BRAM_BITSIZE(16),
    .PRIVATE_MEMORY(0),
    .USE_SPARSE_MEMORY(1),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(10),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33764_0 (.out1({null_out_signal_array_33764_0_out1_1,
      out_ARRAY_1D_STD_BRAM_NN_2_i0_array_33764_0}),
    .Sout_Rdata_ram(sig_in_vector_bus_mergerSout_Rdata_ram6_2),
    .Sout_DataRdy(sig_in_vector_bus_mergerSout_DataRdy5_2),
    .proxy_out1({null_out_signal_array_33764_0_proxy_out1_1,
      null_out_signal_array_33764_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({10'b0000000000,
      out_conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026_9_10}),
    .in3({6'b000000,
      out_conv_out_const_3_7_6}),
    .in4({1'b0,
      out_const_26}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE}),
    .S_oe_ram(S_oe_ram),
    .S_we_ram(S_we_ram),
    .S_addr_ram(S_addr_ram),
    .S_Wdata_ram(S_Wdata_ram),
    .Sin_Rdata_ram(Sin_Rdata_ram),
    .S_data_ram_size(S_data_ram_size),
    .Sin_DataRdy(Sin_DataRdy),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({10'b0000000000,
      10'b0000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(10),
    .PORTSIZE_in2(2),
    .BITSIZE_in3(6),
    .PORTSIZE_in3(2),
    .BITSIZE_in4(1),
    .PORTSIZE_in4(2),
    .BITSIZE_sel_LOAD(1),
    .PORTSIZE_sel_LOAD(2),
    .BITSIZE_sel_STORE(1),
    .PORTSIZE_sel_STORE(2),
    .BITSIZE_S_oe_ram(1),
    .PORTSIZE_S_oe_ram(2),
    .BITSIZE_S_we_ram(1),
    .PORTSIZE_S_we_ram(2),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2),
    .BITSIZE_S_addr_ram(10),
    .PORTSIZE_S_addr_ram(2),
    .BITSIZE_S_Wdata_ram(32),
    .PORTSIZE_S_Wdata_ram(2),
    .BITSIZE_Sin_Rdata_ram(32),
    .PORTSIZE_Sin_Rdata_ram(2),
    .BITSIZE_Sout_Rdata_ram(32),
    .PORTSIZE_Sout_Rdata_ram(2),
    .BITSIZE_S_data_ram_size(6),
    .PORTSIZE_S_data_ram_size(2),
    .BITSIZE_Sin_DataRdy(1),
    .PORTSIZE_Sin_DataRdy(2),
    .BITSIZE_Sout_DataRdy(1),
    .PORTSIZE_Sout_DataRdy(2),
    .MEMORY_INIT_file_a("array_ref_33765.mem"),
    .MEMORY_INIT_file_b("0_array_ref_33765.mem"),
    .n_elements(30),
    .data_size(32),
    .address_space_begin(MEM_var_33765_33672),
    .address_space_rangesize(128),
    .BUS_PIPELINED(1),
    .BRAM_BITSIZE(16),
    .PRIVATE_MEMORY(0),
    .USE_SPARSE_MEMORY(1),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(10),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33765_0 (.out1({null_out_signal_array_33765_0_out1_1,
      null_out_signal_array_33765_0_out1_0}),
    .Sout_Rdata_ram(sig_in_vector_bus_mergerSout_Rdata_ram6_3),
    .Sout_DataRdy(sig_in_vector_bus_mergerSout_DataRdy5_3),
    .proxy_out1({null_out_signal_array_33765_0_proxy_out1_1,
      null_out_signal_array_33765_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({10'b0000000000,
      10'b0000000000}),
    .in3({6'b000000,
      6'b000000}),
    .in4({1'b0,
      1'b0}),
    .sel_LOAD({1'b0,
      1'b0}),
    .sel_STORE({1'b0,
      1'b0}),
    .S_oe_ram(S_oe_ram),
    .S_we_ram(S_we_ram),
    .S_addr_ram(S_addr_ram),
    .S_Wdata_ram(S_Wdata_ram),
    .Sin_Rdata_ram(Sin_Rdata_ram),
    .S_data_ram_size(S_data_ram_size),
    .Sin_DataRdy(Sin_DataRdy),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({10'b0000000000,
      10'b0000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  bus_merger #(.BITSIZE_in1(64),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(64)) bus_mergerMout_Wdata_ram0_ (.out1(sig_out_bus_mergerMout_Wdata_ram0_),
    .in1({sig_in_bus_mergerMout_Wdata_ram0_0}));
  bus_merger #(.BITSIZE_in1(20),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(20)) bus_mergerMout_addr_ram1_ (.out1(sig_out_bus_mergerMout_addr_ram1_),
    .in1({sig_in_bus_mergerMout_addr_ram1_0}));
  bus_merger #(.BITSIZE_in1(12),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(12)) bus_mergerMout_data_ram_size2_ (.out1(sig_out_bus_mergerMout_data_ram_size2_),
    .in1({sig_in_bus_mergerMout_data_ram_size2_0}));
  bus_merger #(.BITSIZE_in1(2),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(2)) bus_mergerMout_oe_ram3_ (.out1(sig_out_bus_mergerMout_oe_ram3_),
    .in1({sig_in_bus_mergerMout_oe_ram3_0}));
  bus_merger #(.BITSIZE_in1(2),
    .PORTSIZE_in1(1),
    .BITSIZE_out1(2)) bus_mergerMout_we_ram4_ (.out1(sig_out_bus_mergerMout_we_ram4_),
    .in1({sig_in_bus_mergerMout_we_ram4_0}));
  bus_merger #(.BITSIZE_in1(2),
    .PORTSIZE_in1(4),
    .BITSIZE_out1(2)) bus_mergerSout_DataRdy5_ (.out1(sig_out_bus_mergerSout_DataRdy5_),
    .in1({sig_in_bus_mergerSout_DataRdy5_3,
      sig_in_bus_mergerSout_DataRdy5_2,
      sig_in_bus_mergerSout_DataRdy5_1,
      sig_in_bus_mergerSout_DataRdy5_0}));
  bus_merger #(.BITSIZE_in1(64),
    .PORTSIZE_in1(4),
    .BITSIZE_out1(64)) bus_mergerSout_Rdata_ram6_ (.out1(sig_out_bus_mergerSout_Rdata_ram6_),
    .in1({sig_in_bus_mergerSout_Rdata_ram6_3,
      sig_in_bus_mergerSout_Rdata_ram6_2,
      sig_in_bus_mergerSout_Rdata_ram6_1,
      sig_in_bus_mergerSout_Rdata_ram6_0}));
  constant_value #(.BITSIZE_out1(1),
    .value(1'b0)) const_0 (.out1(out_const_0));
  constant_value #(.BITSIZE_out1(2),
    .value(2'b01)) const_1 (.out1(out_const_1));
  constant_value #(.BITSIZE_out1(7),
    .value(7'b0101001)) const_10 (.out1(out_const_10));
  constant_value #(.BITSIZE_out1(13),
    .value(13'b0101100110011)) const_11 (.out1(out_const_11));
  constant_value #(.BITSIZE_out1(14),
    .value(14'b01011001111111)) const_12 (.out1(out_const_12));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b011)) const_13 (.out1(out_const_13));
  constant_value #(.BITSIZE_out1(15),
    .value(15'b011000110110001)) const_14 (.out1(out_const_14));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b01100110011)) const_15 (.out1(out_const_15));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b011001100110)) const_16 (.out1(out_const_16));
  constant_value #(.BITSIZE_out1(13),
    .value(13'b0110011001101)) const_17 (.out1(out_const_17));
  constant_value #(.BITSIZE_out1(9),
    .value(9'b011001101)) const_18 (.out1(out_const_18));
  constant_value #(.BITSIZE_out1(9),
    .value(9'b011011001)) const_19 (.out1(out_const_19));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b010)) const_2 (.out1(out_const_2));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b011011001101)) const_20 (.out1(out_const_20));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b0111)) const_21 (.out1(out_const_21));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b011100110011)) const_22 (.out1(out_const_22));
  constant_value #(.BITSIZE_out1(14),
    .value(14'b01110011100101)) const_23 (.out1(out_const_23));
  constant_value #(.BITSIZE_out1(13),
    .value(13'b0111001111111)) const_24 (.out1(out_const_24));
  constant_value #(.BITSIZE_out1(6),
    .value(6'b011110)) const_25 (.out1(out_const_25));
  constant_value #(.BITSIZE_out1(1),
    .value(1'b1)) const_26 (.out1(out_const_26));
  constant_value #(.BITSIZE_out1(2),
    .value(2'b10)) const_27 (.out1(out_const_27));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b10011001100)) const_28 (.out1(out_const_28));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b10011010011)) const_29 (.out1(out_const_29));
  constant_value #(.BITSIZE_out1(7),
    .value(7'b0100000)) const_3 (.out1(out_const_3));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b10011011001)) const_30 (.out1(out_const_30));
  constant_value #(.BITSIZE_out1(15),
    .value(15'b101000000000000)) const_31 (.out1(out_const_31));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b101011010011)) const_32 (.out1(out_const_32));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1011)) const_33 (.out1(out_const_33));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b11001100101)) const_34 (.out1(out_const_34));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b11001100110)) const_35 (.out1(out_const_35));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1110)) const_36 (.out1(out_const_36));
  constant_value #(.BITSIZE_out1(8),
    .value(8'b11101111)) const_37 (.out1(out_const_37));
  constant_value #(.BITSIZE_out1(8),
    .value(8'b11111110)) const_38 (.out1(out_const_38));
  constant_value #(.BITSIZE_out1(16),
    .value(16'b1111111011111111)) const_39 (.out1(out_const_39));
  constant_value #(.BITSIZE_out1(13),
    .value(13'b0100000000000)) const_4 (.out1(out_const_4));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b11111111111)) const_40 (.out1(out_const_40));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b111111111110)) const_41 (.out1(out_const_41));
  constant_value #(.BITSIZE_out1(16),
    .value(16'b1111111111111110)) const_42 (.out1(out_const_42));
  constant_value #(.BITSIZE_out1(32),
    .value(32'b11111111111111111111111111111110)) const_43 (.out1(out_const_43));
  constant_value #(.BITSIZE_out1(64),
    .value(64'b1111111111111111111111111111111111111111111111111111111111111110)) const_44 (.out1(out_const_44));
  constant_value #(.BITSIZE_out1(8),
    .value(MEM_var_33690_33672)) const_45 (.out1(out_const_45));
  constant_value #(.BITSIZE_out1(9),
    .value(MEM_var_33691_33672)) const_46 (.out1(out_const_46));
  constant_value #(.BITSIZE_out1(9),
    .value(MEM_var_33764_33672)) const_47 (.out1(out_const_47));
  constant_value #(.BITSIZE_out1(10),
    .value(MEM_var_33765_33672)) const_48 (.out1(out_const_48));
  constant_value #(.BITSIZE_out1(10),
    .value(10'b0100000011)) const_5 (.out1(out_const_5));
  constant_value #(.BITSIZE_out1(14),
    .value(14'b01000011011001)) const_6 (.out1(out_const_6));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b01001100110)) const_7 (.out1(out_const_7));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b010011001101)) const_8 (.out1(out_const_8));
  constant_value #(.BITSIZE_out1(13),
    .value(13'b0100110011010)) const_9 (.out1(out_const_9));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_addr_expr_FU_6_i0_fu_main_33672_33982_10_32 (.out1(out_conv_out_addr_expr_FU_6_i0_fu_main_33672_33982_10_32),
    .in1(out_addr_expr_FU_6_i0_fu_main_33672_33982));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_addr_expr_FU_7_i0_fu_main_33672_33987_10_32 (.out1(out_conv_out_addr_expr_FU_7_i0_fu_main_33672_33987_10_32),
    .in1(out_addr_expr_FU_7_i0_fu_main_33672_33987));
  IIdata_converter_FU #(.BITSIZE_in1(13),
    .BITSIZE_out1(32)) conv_out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411_I_13_I_32 (.out1(out_conv_out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411_I_13_I_32),
    .in1(out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411));
  IIdata_converter_FU #(.BITSIZE_in1(7),
    .BITSIZE_out1(32)) conv_out_const_10_I_7_I_32 (.out1(out_conv_out_const_10_I_7_I_32),
    .in1(out_const_10));
  UUdata_converter_FU #(.BITSIZE_in1(7),
    .BITSIZE_out1(6)) conv_out_const_3_7_6 (.out1(out_conv_out_const_3_7_6),
    .in1(out_const_3));
  UUdata_converter_FU #(.BITSIZE_in1(8),
    .BITSIZE_out1(32)) conv_out_const_45_8_32 (.out1(out_conv_out_const_45_8_32),
    .in1(out_const_45));
  UUdata_converter_FU #(.BITSIZE_in1(9),
    .BITSIZE_out1(32)) conv_out_const_46_9_32 (.out1(out_conv_out_const_46_9_32),
    .in1(out_const_46));
  UUdata_converter_FU #(.BITSIZE_in1(9),
    .BITSIZE_out1(32)) conv_out_const_47_9_32 (.out1(out_conv_out_const_47_9_32),
    .in1(out_const_47));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_const_48_10_32 (.out1(out_conv_out_const_48_10_32),
    .in1(out_const_48));
  IUdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(32)) conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32 (.out1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in1(out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_reg_0_reg_0_10_32 (.out1(out_conv_out_reg_0_reg_0_10_32),
    .in1(out_reg_0_reg_0));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_reg_1_reg_1_10_32 (.out1(out_conv_out_reg_1_reg_1_10_32),
    .in1(out_reg_1_reg_1));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_reg_2_reg_2_10_32 (.out1(out_conv_out_reg_2_reg_2_10_32),
    .in1(out_reg_2_reg_2));
  UUdata_converter_FU #(.BITSIZE_in1(10),
    .BITSIZE_out1(32)) conv_out_reg_3_reg_3_10_32 (.out1(out_conv_out_reg_3_reg_3_10_32),
    .in1(out_reg_3_reg_3));
  UUdata_converter_FU #(.BITSIZE_in1(8),
    .BITSIZE_out1(10)) conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016_8_10 (.out1(out_conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016_8_10),
    .in1(out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016));
  UUdata_converter_FU #(.BITSIZE_in1(9),
    .BITSIZE_out1(10)) conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026_9_10 (.out1(out_conv_out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026_9_10),
    .in1(out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(7)) fu_main_33672_33802 (.out1(out_IUdata_converter_FU_14_i0_fu_main_33672_33802),
    .in1(out_reg_7_reg_7));
  mult_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32),
    .PIPE_PARAMETER(0)) fu_main_33672_33805 (.out1(out_mult_expr_FU_32_32_32_0_49_i0_fu_main_33672_33805),
    .clock(clock),
    .in1(out_reg_10_reg_10),
    .in2(out_reg_9_reg_9));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) fu_main_33672_33806 (.out1(out_plus_expr_FU_32_32_32_63_i0_fu_main_33672_33806),
    .in1(out_reg_6_reg_6),
    .in2(out_reg_11_reg_11));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32)) fu_main_33672_33807 (.out1(out_plus_expr_FU_32_0_32_51_i0_fu_main_33672_33807),
    .in1(out_reg_7_reg_7),
    .in2(out_const_1));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_33808 (.out1(out_read_cond_FU_15_i0_fu_main_33672_33808),
    .in1(out_reg_8_reg_8));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_in3(2),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(1)) fu_main_33672_33846 (.out1(out_bit_ior_concat_expr_FU_40_i0_fu_main_33672_33846),
    .in1(out_lshift_expr_FU_32_0_32_45_i0_fu_main_33672_34204),
    .in2(out_bit_and_expr_FU_8_0_8_37_i0_fu_main_33672_34207),
    .in3(out_const_1));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_33847 (.out1(out_read_cond_FU_26_i0_fu_main_33672_33847),
    .in1(out_lt_expr_FU_32_0_32_48_i0_fu_main_33672_34033));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(15),
    .BITSIZE_out1(32)) fu_main_33672_33854 (.out1(out_plus_expr_FU_32_0_32_52_i0_fu_main_33672_33854),
    .in1(out_reg_6_reg_6),
    .in2(out_const_14));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33855 (.out1(out_IUdata_converter_FU_16_i0_fu_main_33672_33855),
    .in1(out_plus_expr_FU_32_0_32_52_i0_fu_main_33672_33854));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_in3(2),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(1)) fu_main_33672_33867 (.out1(out_bit_ior_concat_expr_FU_40_i1_fu_main_33672_33867),
    .in1(out_lshift_expr_FU_32_0_32_45_i1_fu_main_33672_34218),
    .in2(out_bit_and_expr_FU_8_0_8_37_i1_fu_main_33672_34221),
    .in3(out_const_1));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33868 (.out1(out_IUdata_converter_FU_17_i0_fu_main_33672_33868),
    .in1(out_bit_ior_concat_expr_FU_40_i1_fu_main_33672_33867));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(14),
    .BITSIZE_out1(32)) fu_main_33672_33877 (.out1(out_plus_expr_FU_32_0_32_53_i0_fu_main_33672_33877),
    .in1(out_reg_6_reg_6),
    .in2(out_const_23));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33878 (.out1(out_IUdata_converter_FU_18_i0_fu_main_33672_33878),
    .in1(out_plus_expr_FU_32_0_32_53_i0_fu_main_33672_33877));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(14),
    .BITSIZE_out1(32)) fu_main_33672_33887 (.out1(out_plus_expr_FU_32_0_32_54_i0_fu_main_33672_33887),
    .in1(out_reg_6_reg_6),
    .in2(out_const_12));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33888 (.out1(out_IUdata_converter_FU_19_i0_fu_main_33672_33888),
    .in1(out_plus_expr_FU_32_0_32_54_i0_fu_main_33672_33887));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(13),
    .BITSIZE_out1(32)) fu_main_33672_33897 (.out1(out_plus_expr_FU_32_0_32_55_i0_fu_main_33672_33897),
    .in1(out_reg_6_reg_6),
    .in2(out_const_24));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33898 (.out1(out_IUdata_converter_FU_20_i0_fu_main_33672_33898),
    .in1(out_plus_expr_FU_32_0_32_55_i0_fu_main_33672_33897));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4),
    .BITSIZE_in3(3),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(3)) fu_main_33672_33907 (.out1(out_bit_ior_concat_expr_FU_41_i0_fu_main_33672_33907),
    .in1(out_lshift_expr_FU_32_0_32_46_i0_fu_main_33672_34234),
    .in2(out_bit_and_expr_FU_8_0_8_38_i0_fu_main_33672_34238),
    .in3(out_const_13));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33908 (.out1(out_IUdata_converter_FU_21_i0_fu_main_33672_33908),
    .in1(out_bit_ior_concat_expr_FU_41_i0_fu_main_33672_33907));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4),
    .BITSIZE_in3(3),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(3)) fu_main_33672_33916 (.out1(out_bit_ior_concat_expr_FU_41_i1_fu_main_33672_33916),
    .in1(out_lshift_expr_FU_32_0_32_46_i1_fu_main_33672_34248),
    .in2(out_bit_and_expr_FU_8_0_8_38_i1_fu_main_33672_34251),
    .in3(out_const_13));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33917 (.out1(out_IUdata_converter_FU_22_i0_fu_main_33672_33917),
    .in1(out_bit_ior_concat_expr_FU_41_i1_fu_main_33672_33916));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_in3(2),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(1)) fu_main_33672_33925 (.out1(out_bit_ior_concat_expr_FU_40_i2_fu_main_33672_33925),
    .in1(out_lshift_expr_FU_32_0_32_45_i2_fu_main_33672_34261),
    .in2(out_bit_and_expr_FU_8_0_8_37_i2_fu_main_33672_34264),
    .in3(out_const_1));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33926 (.out1(out_IUdata_converter_FU_23_i0_fu_main_33672_33926),
    .in1(out_bit_ior_concat_expr_FU_40_i2_fu_main_33672_33925));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_in3(3),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(2)) fu_main_33672_33934 (.out1(out_bit_ior_concat_expr_FU_42_i0_fu_main_33672_33934),
    .in1(out_lshift_expr_FU_32_0_32_47_i0_fu_main_33672_34277),
    .in2(out_bit_and_expr_FU_8_0_8_39_i0_fu_main_33672_34280),
    .in3(out_const_2));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33935 (.out1(out_IUdata_converter_FU_24_i0_fu_main_33672_33935),
    .in1(out_bit_ior_concat_expr_FU_42_i0_fu_main_33672_33934));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_in3(3),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(2)) fu_main_33672_33943 (.out1(out_bit_ior_concat_expr_FU_42_i1_fu_main_33672_33943),
    .in1(out_lshift_expr_FU_32_0_32_47_i1_fu_main_33672_34291),
    .in2(out_bit_and_expr_FU_8_0_8_39_i1_fu_main_33672_34294),
    .in3(out_const_2));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_33944 (.out1(out_IUdata_converter_FU_25_i0_fu_main_33672_33944),
    .in1(out_bit_ior_concat_expr_FU_42_i1_fu_main_33672_33943));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_33982 (.out1(out_addr_expr_FU_6_i0_fu_main_33672_33982),
    .in1(out_conv_out_const_46_9_32));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_33987 (.out1(out_addr_expr_FU_7_i0_fu_main_33672_33987),
    .in1(out_conv_out_const_45_8_32));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_33991 (.out1(out_addr_expr_FU_8_i0_fu_main_33672_33991),
    .in1(out_conv_out_const_48_10_32));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_33995 (.out1(out_addr_expr_FU_9_i0_fu_main_33672_33995),
    .in1(out_conv_out_const_47_9_32));
  ui_lshift_expr_FU #(.BITSIZE_in1(7),
    .BITSIZE_in2(2),
    .BITSIZE_out1(8),
    .PRECISION(32)) fu_main_33672_34010 (.out1(out_ui_lshift_expr_FU_8_0_8_73_i0_fu_main_33672_34010),
    .in1(out_IUdata_converter_FU_14_i0_fu_main_33672_33802),
    .in2(out_const_27));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(8)) fu_main_33672_34013 (.out1(out_addr_expr_FU_10_i0_fu_main_33672_34013),
    .in1(out_conv_out_const_45_8_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(8),
    .BITSIZE_in2(8),
    .BITSIZE_out1(8),
    .LSB_PARAMETER(0)) fu_main_33672_34016 (.out1(out_ui_pointer_plus_expr_FU_8_8_8_74_i0_fu_main_33672_34016),
    .in1(out_reg_4_reg_4),
    .in2(out_ui_lshift_expr_FU_8_0_8_73_i0_fu_main_33672_34010));
  ui_lshift_expr_FU #(.BITSIZE_in1(7),
    .BITSIZE_in2(2),
    .BITSIZE_out1(9),
    .PRECISION(32)) fu_main_33672_34020 (.out1(out_ui_lshift_expr_FU_16_0_16_72_i0_fu_main_33672_34020),
    .in1(out_IUdata_converter_FU_14_i0_fu_main_33672_33802),
    .in2(out_const_27));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(9)) fu_main_33672_34023 (.out1(out_addr_expr_FU_11_i0_fu_main_33672_34023),
    .in1(out_conv_out_const_47_9_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(9),
    .BITSIZE_in2(9),
    .BITSIZE_out1(9),
    .LSB_PARAMETER(0)) fu_main_33672_34026 (.out1(out_ui_pointer_plus_expr_FU_8_8_8_74_i1_fu_main_33672_34026),
    .in1(out_reg_5_reg_5),
    .in2(out_ui_lshift_expr_FU_16_0_16_72_i0_fu_main_33672_34020));
  ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(6),
    .BITSIZE_out1(1)) fu_main_33672_34031 (.out1(out_ne_expr_FU_32_0_32_50_i0_fu_main_33672_34031),
    .in1(out_plus_expr_FU_32_0_32_51_i0_fu_main_33672_33807),
    .in2(out_const_25));
  lt_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(15),
    .BITSIZE_out1(1)) fu_main_33672_34033 (.out1(out_lt_expr_FU_32_0_32_48_i0_fu_main_33672_34033),
    .in1(out_bit_ior_concat_expr_FU_40_i0_fu_main_33672_33846),
    .in2(out_const_31));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(12),
    .BITSIZE_out1(1)) fu_main_33672_34035 (.out1(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in1(out_reg_12_reg_12),
    .in2(out_const_41));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34037 (.out1(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in1(out_reg_13_reg_13),
    .in2(out_const_28));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34039 (.out1(out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039),
    .in1(out_reg_14_reg_14),
    .in2(out_const_34));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34041 (.out1(out_ui_le_expr_FU_32_0_32_70_i0_fu_main_33672_34041),
    .in1(out_reg_15_reg_15),
    .in2(out_const_40));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34043 (.out1(out_ui_le_expr_FU_32_0_32_71_i0_fu_main_33672_34043),
    .in1(out_reg_16_reg_16),
    .in2(out_const_35));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34045 (.out1(out_ui_le_expr_FU_32_0_32_70_i1_fu_main_33672_34045),
    .in1(out_reg_17_reg_17),
    .in2(out_const_40));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34047 (.out1(out_ui_le_expr_FU_32_0_32_69_i1_fu_main_33672_34047),
    .in1(out_reg_18_reg_18),
    .in2(out_const_34));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34049 (.out1(out_ui_le_expr_FU_32_0_32_69_i2_fu_main_33672_34049),
    .in1(out_reg_19_reg_19),
    .in2(out_const_34));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34051 (.out1(out_ui_le_expr_FU_32_0_32_70_i2_fu_main_33672_34051),
    .in1(out_reg_20_reg_20),
    .in2(out_const_40));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34053 (.out1(out_ui_le_expr_FU_32_0_32_71_i1_fu_main_33672_34053),
    .in1(out_reg_21_reg_21),
    .in2(out_const_35));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(31),
    .PRECISION(32)) fu_main_33672_34194 (.out1(out_rshift_expr_FU_32_0_32_64_i0_fu_main_33672_34194),
    .in1(out_reg_6_reg_6),
    .in2(out_const_1));
  plus_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(9),
    .BITSIZE_out1(32)) fu_main_33672_34199 (.out1(out_plus_expr_FU_32_0_32_56_i0_fu_main_33672_34199),
    .in1(out_rshift_expr_FU_32_0_32_64_i0_fu_main_33672_34194),
    .in2(out_const_19));
  lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34204 (.out1(out_lshift_expr_FU_32_0_32_45_i0_fu_main_33672_34204),
    .in1(out_plus_expr_FU_32_0_32_56_i0_fu_main_33672_34199),
    .in2(out_const_1));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(2)) fu_main_33672_34207 (.out1(out_bit_and_expr_FU_8_0_8_37_i0_fu_main_33672_34207),
    .in1(out_reg_6_reg_6),
    .in2(out_const_1));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(31),
    .PRECISION(32)) fu_main_33672_34212 (.out1(out_rshift_expr_FU_32_0_32_64_i1_fu_main_33672_34212),
    .in1(out_reg_6_reg_6),
    .in2(out_const_1));
  plus_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(14),
    .BITSIZE_out1(32)) fu_main_33672_34215 (.out1(out_plus_expr_FU_32_0_32_57_i0_fu_main_33672_34215),
    .in1(out_rshift_expr_FU_32_0_32_64_i1_fu_main_33672_34212),
    .in2(out_const_6));
  lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34218 (.out1(out_lshift_expr_FU_32_0_32_45_i1_fu_main_33672_34218),
    .in1(out_plus_expr_FU_32_0_32_57_i0_fu_main_33672_34215),
    .in2(out_const_1));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(2)) fu_main_33672_34221 (.out1(out_bit_and_expr_FU_8_0_8_37_i1_fu_main_33672_34221),
    .in1(out_reg_6_reg_6),
    .in2(out_const_1));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_out1(29),
    .PRECISION(32)) fu_main_33672_34226 (.out1(out_rshift_expr_FU_32_0_32_65_i0_fu_main_33672_34226),
    .in1(out_reg_6_reg_6),
    .in2(out_const_13));
  plus_expr_FU #(.BITSIZE_in1(29),
    .BITSIZE_in2(10),
    .BITSIZE_out1(30)) fu_main_33672_34231 (.out1(out_plus_expr_FU_32_0_32_58_i0_fu_main_33672_34231),
    .in1(out_rshift_expr_FU_32_0_32_65_i0_fu_main_33672_34226),
    .in2(out_const_5));
  lshift_expr_FU #(.BITSIZE_in1(30),
    .BITSIZE_in2(3),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34234 (.out1(out_lshift_expr_FU_32_0_32_46_i0_fu_main_33672_34234),
    .in1(out_plus_expr_FU_32_0_32_58_i0_fu_main_33672_34231),
    .in2(out_const_13));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4),
    .BITSIZE_out1(4)) fu_main_33672_34238 (.out1(out_bit_and_expr_FU_8_0_8_38_i0_fu_main_33672_34238),
    .in1(out_reg_6_reg_6),
    .in2(out_const_21));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_out1(29),
    .PRECISION(32)) fu_main_33672_34243 (.out1(out_rshift_expr_FU_32_0_32_65_i1_fu_main_33672_34243),
    .in1(out_reg_6_reg_6),
    .in2(out_const_13));
  plus_expr_FU #(.BITSIZE_in1(29),
    .BITSIZE_in2(3),
    .BITSIZE_out1(30)) fu_main_33672_34245 (.out1(out_plus_expr_FU_32_0_32_59_i0_fu_main_33672_34245),
    .in1(out_rshift_expr_FU_32_0_32_65_i1_fu_main_33672_34243),
    .in2(out_const_13));
  lshift_expr_FU #(.BITSIZE_in1(30),
    .BITSIZE_in2(3),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34248 (.out1(out_lshift_expr_FU_32_0_32_46_i1_fu_main_33672_34248),
    .in1(out_plus_expr_FU_32_0_32_59_i0_fu_main_33672_34245),
    .in2(out_const_13));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4),
    .BITSIZE_out1(4)) fu_main_33672_34251 (.out1(out_bit_and_expr_FU_8_0_8_38_i1_fu_main_33672_34251),
    .in1(out_reg_6_reg_6),
    .in2(out_const_21));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(31),
    .PRECISION(32)) fu_main_33672_34255 (.out1(out_rshift_expr_FU_32_0_32_64_i2_fu_main_33672_34255),
    .in1(out_reg_6_reg_6),
    .in2(out_const_1));
  plus_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(11),
    .BITSIZE_out1(32)) fu_main_33672_34258 (.out1(out_plus_expr_FU_32_0_32_60_i0_fu_main_33672_34258),
    .in1(out_rshift_expr_FU_32_0_32_64_i2_fu_main_33672_34255),
    .in2(out_const_30));
  lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34261 (.out1(out_lshift_expr_FU_32_0_32_45_i2_fu_main_33672_34261),
    .in1(out_plus_expr_FU_32_0_32_60_i0_fu_main_33672_34258),
    .in2(out_const_1));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(2)) fu_main_33672_34264 (.out1(out_bit_and_expr_FU_8_0_8_37_i2_fu_main_33672_34264),
    .in1(out_reg_6_reg_6),
    .in2(out_const_1));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_out1(30),
    .PRECISION(32)) fu_main_33672_34269 (.out1(out_rshift_expr_FU_32_0_32_66_i0_fu_main_33672_34269),
    .in1(out_reg_6_reg_6),
    .in2(out_const_2));
  plus_expr_FU #(.BITSIZE_in1(30),
    .BITSIZE_in2(11),
    .BITSIZE_out1(31)) fu_main_33672_34274 (.out1(out_plus_expr_FU_32_0_32_61_i0_fu_main_33672_34274),
    .in1(out_rshift_expr_FU_32_0_32_66_i0_fu_main_33672_34269),
    .in2(out_const_29));
  lshift_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(3),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34277 (.out1(out_lshift_expr_FU_32_0_32_47_i0_fu_main_33672_34277),
    .in1(out_plus_expr_FU_32_0_32_61_i0_fu_main_33672_34274),
    .in2(out_const_2));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_out1(3)) fu_main_33672_34280 (.out1(out_bit_and_expr_FU_8_0_8_39_i0_fu_main_33672_34280),
    .in1(out_reg_6_reg_6),
    .in2(out_const_13));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_out1(30),
    .PRECISION(32)) fu_main_33672_34285 (.out1(out_rshift_expr_FU_32_0_32_66_i1_fu_main_33672_34285),
    .in1(out_reg_6_reg_6),
    .in2(out_const_2));
  plus_expr_FU #(.BITSIZE_in1(30),
    .BITSIZE_in2(12),
    .BITSIZE_out1(31)) fu_main_33672_34288 (.out1(out_plus_expr_FU_32_0_32_62_i0_fu_main_33672_34288),
    .in1(out_rshift_expr_FU_32_0_32_66_i1_fu_main_33672_34285),
    .in2(out_const_32));
  lshift_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(3),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34291 (.out1(out_lshift_expr_FU_32_0_32_47_i1_fu_main_33672_34291),
    .in1(out_plus_expr_FU_32_0_32_62_i0_fu_main_33672_34288),
    .in2(out_const_2));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3),
    .BITSIZE_out1(3)) fu_main_33672_34294 (.out1(out_bit_and_expr_FU_8_0_8_39_i1_fu_main_33672_34294),
    .in1(out_reg_6_reg_6),
    .in2(out_const_13));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(12),
    .BITSIZE_in3(12),
    .BITSIZE_out1(12)) fu_main_33672_34318 (.out1(out_cond_expr_FU_16_16_16_16_43_i0_fu_main_33672_34318),
    .in1(out_ui_le_expr_FU_32_0_32_71_i1_fu_main_33672_34053),
    .in2(out_const_20),
    .in3(out_const_22));
  lshift_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(2),
    .BITSIZE_out1(13),
    .PRECISION(32)) fu_main_33672_34327 (.out1(out_lshift_expr_FU_16_0_16_44_i0_fu_main_33672_34327),
    .in1(out_cond_expr_FU_16_16_16_16_43_i0_fu_main_33672_34318),
    .in2(out_const_1));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(9),
    .BITSIZE_in3(11),
    .BITSIZE_out1(11)) fu_main_33672_34347 (.out1(out_cond_expr_FU_16_16_16_16_43_i1_fu_main_33672_34347),
    .in1(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in2(out_const_18),
    .in3(out_const_7));
  lut_expr_FU #(.BITSIZE_in1(4),
    .BITSIZE_out1(1)) fu_main_33672_34352 (.out1(out_lut_expr_FU_28_i0_fu_main_33672_34352),
    .in1(out_const_36),
    .in2(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in3(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in4(1'b0),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(11),
    .BITSIZE_in3(11),
    .BITSIZE_out1(11)) fu_main_33672_34355 (.out1(out_cond_expr_FU_16_16_16_16_43_i2_fu_main_33672_34355),
    .in1(out_lut_expr_FU_28_i0_fu_main_33672_34352),
    .in2(out_cond_expr_FU_16_16_16_16_43_i1_fu_main_33672_34347),
    .in3(out_const_15));
  lut_expr_FU #(.BITSIZE_in1(8),
    .BITSIZE_out1(1)) fu_main_33672_34360 (.out1(out_lut_expr_FU_29_i0_fu_main_33672_34360),
    .in1(out_const_38),
    .in2(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in3(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in4(out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(11),
    .BITSIZE_in3(12),
    .BITSIZE_out1(12)) fu_main_33672_34363 (.out1(out_cond_expr_FU_16_16_16_16_43_i3_fu_main_33672_34363),
    .in1(out_lut_expr_FU_29_i0_fu_main_33672_34360),
    .in2(out_cond_expr_FU_16_16_16_16_43_i2_fu_main_33672_34355),
    .in3(out_const_8));
  lut_expr_FU #(.BITSIZE_in1(16),
    .BITSIZE_out1(1)) fu_main_33672_34368 (.out1(out_lut_expr_FU_30_i0_fu_main_33672_34368),
    .in1(out_const_42),
    .in2(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in3(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in4(out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039),
    .in5(out_ui_le_expr_FU_32_0_32_70_i0_fu_main_33672_34041),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(12),
    .BITSIZE_in3(12),
    .BITSIZE_out1(12)) fu_main_33672_34371 (.out1(out_cond_expr_FU_16_16_16_16_43_i4_fu_main_33672_34371),
    .in1(out_lut_expr_FU_30_i0_fu_main_33672_34368),
    .in2(out_cond_expr_FU_16_16_16_16_43_i3_fu_main_33672_34363),
    .in3(out_const_16));
  lut_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(1)) fu_main_33672_34376 (.out1(out_lut_expr_FU_31_i0_fu_main_33672_34376),
    .in1(out_const_43),
    .in2(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in3(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in4(out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039),
    .in5(out_ui_le_expr_FU_32_0_32_70_i0_fu_main_33672_34041),
    .in6(out_ui_le_expr_FU_32_0_32_71_i0_fu_main_33672_34043),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(12),
    .BITSIZE_in3(13),
    .BITSIZE_out1(13)) fu_main_33672_34379 (.out1(out_cond_expr_FU_16_16_16_16_43_i5_fu_main_33672_34379),
    .in1(out_lut_expr_FU_31_i0_fu_main_33672_34376),
    .in2(out_cond_expr_FU_16_16_16_16_43_i4_fu_main_33672_34371),
    .in3(out_const_4));
  lut_expr_FU #(.BITSIZE_in1(64),
    .BITSIZE_out1(1)) fu_main_33672_34384 (.out1(out_lut_expr_FU_32_i0_fu_main_33672_34384),
    .in1(out_const_44),
    .in2(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in3(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in4(out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039),
    .in5(out_ui_le_expr_FU_32_0_32_70_i0_fu_main_33672_34041),
    .in6(out_ui_le_expr_FU_32_0_32_71_i0_fu_main_33672_34043),
    .in7(out_ui_le_expr_FU_32_0_32_70_i1_fu_main_33672_34045),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(13),
    .BITSIZE_in3(13),
    .BITSIZE_out1(13)) fu_main_33672_34387 (.out1(out_cond_expr_FU_16_16_16_16_43_i6_fu_main_33672_34387),
    .in1(out_lut_expr_FU_32_i0_fu_main_33672_34384),
    .in2(out_cond_expr_FU_16_16_16_16_43_i5_fu_main_33672_34379),
    .in3(out_const_9));
  lut_expr_FU #(.BITSIZE_in1(4),
    .BITSIZE_out1(1)) fu_main_33672_34392 (.out1(out_lut_expr_FU_34_i0_fu_main_33672_34392),
    .in1(out_const_33),
    .in2(out_ui_le_expr_FU_32_0_32_69_i1_fu_main_33672_34047),
    .in3(out_lut_expr_FU_33_i0_fu_main_33672_34424),
    .in4(1'b0),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(13),
    .BITSIZE_in3(13),
    .BITSIZE_out1(13)) fu_main_33672_34395 (.out1(out_cond_expr_FU_16_16_16_16_43_i7_fu_main_33672_34395),
    .in1(out_lut_expr_FU_34_i0_fu_main_33672_34392),
    .in2(out_cond_expr_FU_16_16_16_16_43_i6_fu_main_33672_34387),
    .in3(out_const_11));
  lut_expr_FU #(.BITSIZE_in1(8),
    .BITSIZE_out1(1)) fu_main_33672_34400 (.out1(out_lut_expr_FU_35_i0_fu_main_33672_34400),
    .in1(out_const_37),
    .in2(out_ui_le_expr_FU_32_0_32_69_i1_fu_main_33672_34047),
    .in3(out_ui_le_expr_FU_32_0_32_69_i2_fu_main_33672_34049),
    .in4(out_lut_expr_FU_33_i0_fu_main_33672_34424),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(13),
    .BITSIZE_in3(13),
    .BITSIZE_out1(13)) fu_main_33672_34403 (.out1(out_cond_expr_FU_16_16_16_16_43_i8_fu_main_33672_34403),
    .in1(out_lut_expr_FU_35_i0_fu_main_33672_34400),
    .in2(out_cond_expr_FU_16_16_16_16_43_i7_fu_main_33672_34395),
    .in3(out_const_17));
  lut_expr_FU #(.BITSIZE_in1(16),
    .BITSIZE_out1(1)) fu_main_33672_34408 (.out1(out_lut_expr_FU_36_i0_fu_main_33672_34408),
    .in1(out_const_39),
    .in2(out_ui_le_expr_FU_32_0_32_69_i1_fu_main_33672_34047),
    .in3(out_ui_le_expr_FU_32_0_32_69_i2_fu_main_33672_34049),
    .in4(out_ui_le_expr_FU_32_0_32_70_i2_fu_main_33672_34051),
    .in5(out_lut_expr_FU_33_i0_fu_main_33672_34424),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(13),
    .BITSIZE_in3(13),
    .BITSIZE_out1(13)) fu_main_33672_34411 (.out1(out_cond_expr_FU_16_16_16_16_43_i9_fu_main_33672_34411),
    .in1(out_lut_expr_FU_36_i0_fu_main_33672_34408),
    .in2(out_cond_expr_FU_16_16_16_16_43_i8_fu_main_33672_34403),
    .in3(out_lshift_expr_FU_16_0_16_44_i0_fu_main_33672_34327));
  lut_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu_main_33672_34424 (.out1(out_lut_expr_FU_33_i0_fu_main_33672_34424),
    .in1(out_const_26),
    .in2(out_ui_le_expr_FU_32_0_32_67_i0_fu_main_33672_34035),
    .in3(out_ui_le_expr_FU_32_0_32_68_i0_fu_main_33672_34037),
    .in4(out_ui_le_expr_FU_32_0_32_69_i0_fu_main_33672_34039),
    .in5(out_ui_le_expr_FU_32_0_32_70_i0_fu_main_33672_34041),
    .in6(out_ui_le_expr_FU_32_0_32_71_i0_fu_main_33672_34043),
    .in7(out_ui_le_expr_FU_32_0_32_70_i1_fu_main_33672_34045),
    .in8(1'b0),
    .in9(1'b0));
  join_signal #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(64)) join_signalbus_mergerMout_Wdata_ram0_0 (.out1(sig_in_bus_mergerMout_Wdata_ram0_0),
    .in1(sig_in_vector_bus_mergerMout_Wdata_ram0_0));
  join_signal #(.BITSIZE_in1(10),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(20)) join_signalbus_mergerMout_addr_ram1_0 (.out1(sig_in_bus_mergerMout_addr_ram1_0),
    .in1(sig_in_vector_bus_mergerMout_addr_ram1_0));
  join_signal #(.BITSIZE_in1(6),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(12)) join_signalbus_mergerMout_data_ram_size2_0 (.out1(sig_in_bus_mergerMout_data_ram_size2_0),
    .in1(sig_in_vector_bus_mergerMout_data_ram_size2_0));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerMout_oe_ram3_0 (.out1(sig_in_bus_mergerMout_oe_ram3_0),
    .in1(sig_in_vector_bus_mergerMout_oe_ram3_0));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerMout_we_ram4_0 (.out1(sig_in_bus_mergerMout_we_ram4_0),
    .in1(sig_in_vector_bus_mergerMout_we_ram4_0));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerSout_DataRdy5_0 (.out1(sig_in_bus_mergerSout_DataRdy5_0),
    .in1(sig_in_vector_bus_mergerSout_DataRdy5_0));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerSout_DataRdy5_1 (.out1(sig_in_bus_mergerSout_DataRdy5_1),
    .in1(sig_in_vector_bus_mergerSout_DataRdy5_1));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerSout_DataRdy5_2 (.out1(sig_in_bus_mergerSout_DataRdy5_2),
    .in1(sig_in_vector_bus_mergerSout_DataRdy5_2));
  join_signal #(.BITSIZE_in1(1),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(2)) join_signalbus_mergerSout_DataRdy5_3 (.out1(sig_in_bus_mergerSout_DataRdy5_3),
    .in1(sig_in_vector_bus_mergerSout_DataRdy5_3));
  join_signal #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(64)) join_signalbus_mergerSout_Rdata_ram6_0 (.out1(sig_in_bus_mergerSout_Rdata_ram6_0),
    .in1(sig_in_vector_bus_mergerSout_Rdata_ram6_0));
  join_signal #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(64)) join_signalbus_mergerSout_Rdata_ram6_1 (.out1(sig_in_bus_mergerSout_Rdata_ram6_1),
    .in1(sig_in_vector_bus_mergerSout_Rdata_ram6_1));
  join_signal #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(64)) join_signalbus_mergerSout_Rdata_ram6_2 (.out1(sig_in_bus_mergerSout_Rdata_ram6_2),
    .in1(sig_in_vector_bus_mergerSout_Rdata_ram6_2));
  join_signal #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_out1(64)) join_signalbus_mergerSout_Rdata_ram6_3 (.out1(sig_in_bus_mergerSout_Rdata_ram6_3),
    .in1(sig_in_vector_bus_mergerSout_Rdata_ram6_3));
  or or_or___internal_bambu_memcpy_75_i00( s___internal_bambu_memcpy_75_i00, selector_IN_UNBOUNDED_main_33672_34057, selector_IN_UNBOUNDED_main_33672_34059);
  register_SE #(.BITSIZE_in1(10),
    .BITSIZE_out1(10)) reg_0 (.out1(out_reg_0_reg_0),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_6_i0_fu_main_33672_33982),
    .wenable(wrenable_reg_0));
  register_SE #(.BITSIZE_in1(10),
    .BITSIZE_out1(10)) reg_1 (.out1(out_reg_1_reg_1),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_7_i0_fu_main_33672_33987),
    .wenable(wrenable_reg_1));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_10 (.out1(out_reg_10_reg_10),
    .clock(clock),
    .reset(reset),
    .in1(out_ARRAY_1D_STD_BRAM_NN_2_i0_array_33764_0),
    .wenable(wrenable_reg_10));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_11 (.out1(out_reg_11_reg_11),
    .clock(clock),
    .reset(reset),
    .in1(out_mult_expr_FU_32_32_32_0_49_i0_fu_main_33672_33805),
    .wenable(wrenable_reg_11));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_12 (.out1(out_reg_12_reg_12),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_16_i0_fu_main_33672_33855),
    .wenable(wrenable_reg_12));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_13 (.out1(out_reg_13_reg_13),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_17_i0_fu_main_33672_33868),
    .wenable(wrenable_reg_13));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_14 (.out1(out_reg_14_reg_14),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_18_i0_fu_main_33672_33878),
    .wenable(wrenable_reg_14));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_15 (.out1(out_reg_15_reg_15),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_19_i0_fu_main_33672_33888),
    .wenable(wrenable_reg_15));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_16 (.out1(out_reg_16_reg_16),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_20_i0_fu_main_33672_33898),
    .wenable(wrenable_reg_16));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_17 (.out1(out_reg_17_reg_17),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_21_i0_fu_main_33672_33908),
    .wenable(wrenable_reg_17));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_18 (.out1(out_reg_18_reg_18),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_22_i0_fu_main_33672_33917),
    .wenable(wrenable_reg_18));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_19 (.out1(out_reg_19_reg_19),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_23_i0_fu_main_33672_33926),
    .wenable(wrenable_reg_19));
  register_SE #(.BITSIZE_in1(10),
    .BITSIZE_out1(10)) reg_2 (.out1(out_reg_2_reg_2),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_8_i0_fu_main_33672_33991),
    .wenable(wrenable_reg_2));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_20 (.out1(out_reg_20_reg_20),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_24_i0_fu_main_33672_33935),
    .wenable(wrenable_reg_20));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_21 (.out1(out_reg_21_reg_21),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_25_i0_fu_main_33672_33944),
    .wenable(wrenable_reg_21));
  register_SE #(.BITSIZE_in1(10),
    .BITSIZE_out1(10)) reg_3 (.out1(out_reg_3_reg_3),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_9_i0_fu_main_33672_33995),
    .wenable(wrenable_reg_3));
  register_SE #(.BITSIZE_in1(8),
    .BITSIZE_out1(8)) reg_4 (.out1(out_reg_4_reg_4),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_10_i0_fu_main_33672_34013),
    .wenable(wrenable_reg_4));
  register_SE #(.BITSIZE_in1(9),
    .BITSIZE_out1(9)) reg_5 (.out1(out_reg_5_reg_5),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_11_i0_fu_main_33672_34023),
    .wenable(wrenable_reg_5));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_6 (.out1(out_reg_6_reg_6),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_206_reg_6_0_0_0),
    .wenable(wrenable_reg_6));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_7 (.out1(out_reg_7_reg_7),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_207_reg_7_0_0_0),
    .wenable(wrenable_reg_7));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_8 (.out1(out_reg_8_reg_8),
    .clock(clock),
    .reset(reset),
    .in1(out_ne_expr_FU_32_0_32_50_i0_fu_main_33672_34031),
    .wenable(wrenable_reg_8));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_9 (.out1(out_reg_9_reg_9),
    .clock(clock),
    .reset(reset),
    .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_33690_0),
    .wenable(wrenable_reg_9));
  split_signal #(.BITSIZE_in1(64),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_Wdata_ram0_ (.out1(Mout_Wdata_ram),
    .in1(sig_out_bus_mergerMout_Wdata_ram0_));
  split_signal #(.BITSIZE_in1(20),
    .BITSIZE_out1(10),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_addr_ram1_ (.out1(Mout_addr_ram),
    .in1(sig_out_bus_mergerMout_addr_ram1_));
  split_signal #(.BITSIZE_in1(12),
    .BITSIZE_out1(6),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_data_ram_size2_ (.out1(Mout_data_ram_size),
    .in1(sig_out_bus_mergerMout_data_ram_size2_));
  split_signal #(.BITSIZE_in1(2),
    .BITSIZE_out1(1),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_oe_ram3_ (.out1(Mout_oe_ram),
    .in1(sig_out_bus_mergerMout_oe_ram3_));
  split_signal #(.BITSIZE_in1(2),
    .BITSIZE_out1(1),
    .PORTSIZE_out1(2)) split_signalbus_mergerMout_we_ram4_ (.out1(Mout_we_ram),
    .in1(sig_out_bus_mergerMout_we_ram4_));
  split_signal #(.BITSIZE_in1(2),
    .BITSIZE_out1(1),
    .PORTSIZE_out1(2)) split_signalbus_mergerSout_DataRdy5_ (.out1(Sout_DataRdy),
    .in1(sig_out_bus_mergerSout_DataRdy5_));
  split_signal #(.BITSIZE_in1(64),
    .BITSIZE_out1(32),
    .PORTSIZE_out1(2)) split_signalbus_mergerSout_Rdata_ram6_ (.out1(Sout_Rdata_ram),
    .in1(sig_out_bus_mergerSout_Rdata_ram6_));
  // io-signal post fix
  assign return_port = out_MUX_92_gimple_return_FU_27_i0_0_0_0;
  assign OUT_CONDITION_main_33672_33808 = out_read_cond_FU_15_i0_fu_main_33672_33808;
  assign OUT_CONDITION_main_33672_33847 = out_read_cond_FU_26_i0_fu_main_33672_33847;
  assign OUT_UNBOUNDED_main_33672_34057 = s_done___internal_bambu_memcpy_75_i0;
  assign OUT_UNBOUNDED_main_33672_34059 = s_done___internal_bambu_memcpy_75_i0;

endmodule

// FSM based controller description for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module controller_main(done_port,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE,
  fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE,
  selector_IN_UNBOUNDED_main_33672_34057,
  selector_IN_UNBOUNDED_main_33672_34059,
  selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0,
  selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1,
  selector_MUX_206_reg_6_0_0_0,
  selector_MUX_207_reg_7_0_0_0,
  selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0,
  selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1,
  selector_MUX_92_gimple_return_FU_27_i0_0_0_0,
  wrenable_reg_0,
  wrenable_reg_1,
  wrenable_reg_10,
  wrenable_reg_11,
  wrenable_reg_12,
  wrenable_reg_13,
  wrenable_reg_14,
  wrenable_reg_15,
  wrenable_reg_16,
  wrenable_reg_17,
  wrenable_reg_18,
  wrenable_reg_19,
  wrenable_reg_2,
  wrenable_reg_20,
  wrenable_reg_21,
  wrenable_reg_3,
  wrenable_reg_4,
  wrenable_reg_5,
  wrenable_reg_6,
  wrenable_reg_7,
  wrenable_reg_8,
  wrenable_reg_9,
  OUT_CONDITION_main_33672_33808,
  OUT_CONDITION_main_33672_33847,
  OUT_UNBOUNDED_main_33672_34057,
  OUT_UNBOUNDED_main_33672_34059,
  clock,
  reset,
  start_port);
  // IN
  input OUT_CONDITION_main_33672_33808;
  input OUT_CONDITION_main_33672_33847;
  input OUT_UNBOUNDED_main_33672_34057;
  input OUT_UNBOUNDED_main_33672_34059;
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  output fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE;
  output selector_IN_UNBOUNDED_main_33672_34057;
  output selector_IN_UNBOUNDED_main_33672_34059;
  output selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0;
  output selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1;
  output selector_MUX_206_reg_6_0_0_0;
  output selector_MUX_207_reg_7_0_0_0;
  output selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0;
  output selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1;
  output selector_MUX_92_gimple_return_FU_27_i0_0_0_0;
  output wrenable_reg_0;
  output wrenable_reg_1;
  output wrenable_reg_10;
  output wrenable_reg_11;
  output wrenable_reg_12;
  output wrenable_reg_13;
  output wrenable_reg_14;
  output wrenable_reg_15;
  output wrenable_reg_16;
  output wrenable_reg_17;
  output wrenable_reg_18;
  output wrenable_reg_19;
  output wrenable_reg_2;
  output wrenable_reg_20;
  output wrenable_reg_21;
  output wrenable_reg_3;
  output wrenable_reg_4;
  output wrenable_reg_5;
  output wrenable_reg_6;
  output wrenable_reg_7;
  output wrenable_reg_8;
  output wrenable_reg_9;
  parameter [10:0] S_0 = 11'b00000000001,
    S_1 = 11'b00000000010,
    S_2 = 11'b00000000100,
    S_3 = 11'b00000001000,
    S_4 = 11'b00000010000,
    S_5 = 11'b00000100000,
    S_6 = 11'b00001000000,
    S_7 = 11'b00010000000,
    S_8 = 11'b00100000000,
    S_10 = 11'b10000000000,
    S_9 = 11'b01000000000;
  reg [10:0] _present_state=S_0, _next_state;
  reg done_port;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE;
  reg selector_IN_UNBOUNDED_main_33672_34057;
  reg selector_IN_UNBOUNDED_main_33672_34059;
  reg selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0;
  reg selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1;
  reg selector_MUX_206_reg_6_0_0_0;
  reg selector_MUX_207_reg_7_0_0_0;
  reg selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0;
  reg selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1;
  reg selector_MUX_92_gimple_return_FU_27_i0_0_0_0;
  reg wrenable_reg_0;
  reg wrenable_reg_1;
  reg wrenable_reg_10;
  reg wrenable_reg_11;
  reg wrenable_reg_12;
  reg wrenable_reg_13;
  reg wrenable_reg_14;
  reg wrenable_reg_15;
  reg wrenable_reg_16;
  reg wrenable_reg_17;
  reg wrenable_reg_18;
  reg wrenable_reg_19;
  reg wrenable_reg_2;
  reg wrenable_reg_20;
  reg wrenable_reg_21;
  reg wrenable_reg_3;
  reg wrenable_reg_4;
  reg wrenable_reg_5;
  reg wrenable_reg_6;
  reg wrenable_reg_7;
  reg wrenable_reg_8;
  reg wrenable_reg_9;
  
  always @(posedge clock)
    if (reset == 1'b0) _present_state <= S_0;
    else _present_state <= _next_state;
  
  always @(*)
  begin
    done_port = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE = 1'b0;
    selector_IN_UNBOUNDED_main_33672_34057 = 1'b0;
    selector_IN_UNBOUNDED_main_33672_34059 = 1'b0;
    selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0 = 1'b0;
    selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1 = 1'b0;
    selector_MUX_206_reg_6_0_0_0 = 1'b0;
    selector_MUX_207_reg_7_0_0_0 = 1'b0;
    selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0 = 1'b0;
    selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1 = 1'b0;
    selector_MUX_92_gimple_return_FU_27_i0_0_0_0 = 1'b0;
    wrenable_reg_0 = 1'b0;
    wrenable_reg_1 = 1'b0;
    wrenable_reg_10 = 1'b0;
    wrenable_reg_11 = 1'b0;
    wrenable_reg_12 = 1'b0;
    wrenable_reg_13 = 1'b0;
    wrenable_reg_14 = 1'b0;
    wrenable_reg_15 = 1'b0;
    wrenable_reg_16 = 1'b0;
    wrenable_reg_17 = 1'b0;
    wrenable_reg_18 = 1'b0;
    wrenable_reg_19 = 1'b0;
    wrenable_reg_2 = 1'b0;
    wrenable_reg_20 = 1'b0;
    wrenable_reg_21 = 1'b0;
    wrenable_reg_3 = 1'b0;
    wrenable_reg_4 = 1'b0;
    wrenable_reg_5 = 1'b0;
    wrenable_reg_6 = 1'b0;
    wrenable_reg_7 = 1'b0;
    wrenable_reg_8 = 1'b0;
    wrenable_reg_9 = 1'b0;
    case (_present_state)
      S_0 :
        if(start_port == 1'b1)
        begin
          selector_IN_UNBOUNDED_main_33672_34057 = 1'b1;
          selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1 = 1'b1;
          selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1 = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_2 = 1'b1;
          wrenable_reg_3 = 1'b1;
          wrenable_reg_4 = 1'b1;
          wrenable_reg_5 = 1'b1;
          if (OUT_UNBOUNDED_main_33672_34057 == 1'b0)
            begin
              _next_state = S_1;
            end
          else
            begin
              _next_state = S_2;
            end
        end
        else
        begin
          _next_state = S_0;
        end
      S_1 :
        begin
          if (OUT_UNBOUNDED_main_33672_34057 == 1'b0)
            begin
              _next_state = S_1;
            end
          else
            begin
              _next_state = S_2;
            end
        end
      S_2 :
        begin
          selector_IN_UNBOUNDED_main_33672_34059 = 1'b1;
          selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0 = 1'b1;
          selector_MUX_206_reg_6_0_0_0 = 1'b1;
          selector_MUX_207_reg_7_0_0_0 = 1'b1;
          selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0 = 1'b1;
          wrenable_reg_6 = 1'b1;
          wrenable_reg_7 = 1'b1;
          if (OUT_UNBOUNDED_main_33672_34059 == 1'b0)
            begin
              _next_state = S_3;
              selector_MUX_206_reg_6_0_0_0 = 1'b0;
              selector_MUX_207_reg_7_0_0_0 = 1'b0;
              wrenable_reg_6 = 1'b0;
              wrenable_reg_7 = 1'b0;
            end
          else
            begin
              _next_state = S_4;
            end
        end
      S_3 :
        begin
          selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0 = 1'b1;
          selector_MUX_206_reg_6_0_0_0 = 1'b1;
          selector_MUX_207_reg_7_0_0_0 = 1'b1;
          selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0 = 1'b1;
          wrenable_reg_6 = 1'b1;
          wrenable_reg_7 = 1'b1;
          if (OUT_UNBOUNDED_main_33672_34059 == 1'b0)
            begin
              _next_state = S_3;
              selector_MUX_206_reg_6_0_0_0 = 1'b0;
              selector_MUX_207_reg_7_0_0_0 = 1'b0;
              wrenable_reg_6 = 1'b0;
              wrenable_reg_7 = 1'b0;
            end
          else
            begin
              _next_state = S_4;
            end
        end
      S_4 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD = 1'b1;
          wrenable_reg_7 = 1'b1;
          wrenable_reg_8 = 1'b1;
          _next_state = S_5;
        end
      S_5 :
        begin
          wrenable_reg_10 = 1'b1;
          wrenable_reg_9 = 1'b1;
          _next_state = S_6;
        end
      S_6 :
        begin
          wrenable_reg_11 = 1'b1;
          _next_state = S_7;
        end
      S_7 :
        begin
          wrenable_reg_6 = 1'b1;
          if (OUT_CONDITION_main_33672_33808 == 1'b1)
            begin
              _next_state = S_4;
            end
          else
            begin
              _next_state = S_8;
            end
        end
      S_8 :
        begin
          wrenable_reg_12 = 1'b1;
          wrenable_reg_13 = 1'b1;
          wrenable_reg_14 = 1'b1;
          wrenable_reg_15 = 1'b1;
          wrenable_reg_16 = 1'b1;
          wrenable_reg_17 = 1'b1;
          wrenable_reg_18 = 1'b1;
          wrenable_reg_19 = 1'b1;
          wrenable_reg_20 = 1'b1;
          wrenable_reg_21 = 1'b1;
          if (OUT_CONDITION_main_33672_33847 == 1'b0)
            begin
              _next_state = S_9;
              done_port = 1'b1;
            end
          else
            begin
              _next_state = S_10;
              done_port = 1'b1;
              wrenable_reg_12 = 1'b0;
              wrenable_reg_13 = 1'b0;
              wrenable_reg_14 = 1'b0;
              wrenable_reg_15 = 1'b0;
              wrenable_reg_16 = 1'b0;
              wrenable_reg_17 = 1'b0;
              wrenable_reg_18 = 1'b0;
              wrenable_reg_19 = 1'b0;
              wrenable_reg_20 = 1'b0;
              wrenable_reg_21 = 1'b0;
            end
        end
      S_10 :
        begin
          selector_MUX_92_gimple_return_FU_27_i0_0_0_0 = 1'b1;
          _next_state = S_0;
        end
      S_9 :
        begin
          _next_state = S_0;
        end
      default :
        begin
          _next_state = S_0;
        end
    endcase
  end
endmodule

// Top component for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module _main(clock,
  reset,
  start_port,
  done_port,
  return_port,
  S_oe_ram,
  S_we_ram,
  S_addr_ram,
  S_Wdata_ram,
  S_data_ram_size,
  M_Rdata_ram,
  M_DataRdy,
  Sin_Rdata_ram,
  Sin_DataRdy,
  Min_oe_ram,
  Min_we_ram,
  Min_addr_ram,
  Min_Wdata_ram,
  Min_data_ram_size,
  Sout_Rdata_ram,
  Sout_DataRdy,
  Mout_oe_ram,
  Mout_we_ram,
  Mout_addr_ram,
  Mout_Wdata_ram,
  Mout_data_ram_size);
  // IN
  input clock;
  input reset;
  input start_port;
  input [1:0] S_oe_ram;
  input [1:0] S_we_ram;
  input [19:0] S_addr_ram;
  input [63:0] S_Wdata_ram;
  input [11:0] S_data_ram_size;
  input [63:0] M_Rdata_ram;
  input [1:0] M_DataRdy;
  input [63:0] Sin_Rdata_ram;
  input [1:0] Sin_DataRdy;
  input [1:0] Min_oe_ram;
  input [1:0] Min_we_ram;
  input [19:0] Min_addr_ram;
  input [63:0] Min_Wdata_ram;
  input [11:0] Min_data_ram_size;
  // OUT
  output done_port;
  output signed [31:0] return_port;
  output [63:0] Sout_Rdata_ram;
  output [1:0] Sout_DataRdy;
  output [1:0] Mout_oe_ram;
  output [1:0] Mout_we_ram;
  output [19:0] Mout_addr_ram;
  output [63:0] Mout_Wdata_ram;
  output [11:0] Mout_data_ram_size;
  // Component and signal declarations
  wire OUT_CONDITION_main_33672_33808;
  wire OUT_CONDITION_main_33672_33847;
  wire OUT_UNBOUNDED_main_33672_34057;
  wire OUT_UNBOUNDED_main_33672_34059;
  wire done_delayed_REG_signal_in;
  wire done_delayed_REG_signal_out;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE;
  wire selector_IN_UNBOUNDED_main_33672_34057;
  wire selector_IN_UNBOUNDED_main_33672_34059;
  wire selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0;
  wire selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1;
  wire selector_MUX_206_reg_6_0_0_0;
  wire selector_MUX_207_reg_7_0_0_0;
  wire selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0;
  wire selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1;
  wire selector_MUX_92_gimple_return_FU_27_i0_0_0_0;
  wire wrenable_reg_0;
  wire wrenable_reg_1;
  wire wrenable_reg_10;
  wire wrenable_reg_11;
  wire wrenable_reg_12;
  wire wrenable_reg_13;
  wire wrenable_reg_14;
  wire wrenable_reg_15;
  wire wrenable_reg_16;
  wire wrenable_reg_17;
  wire wrenable_reg_18;
  wire wrenable_reg_19;
  wire wrenable_reg_2;
  wire wrenable_reg_20;
  wire wrenable_reg_21;
  wire wrenable_reg_3;
  wire wrenable_reg_4;
  wire wrenable_reg_5;
  wire wrenable_reg_6;
  wire wrenable_reg_7;
  wire wrenable_reg_8;
  wire wrenable_reg_9;
  
  controller_main Controller_i (.done_port(done_delayed_REG_signal_in),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE),
    .fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE),
    .selector_IN_UNBOUNDED_main_33672_34057(selector_IN_UNBOUNDED_main_33672_34057),
    .selector_IN_UNBOUNDED_main_33672_34059(selector_IN_UNBOUNDED_main_33672_34059),
    .selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0(selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0),
    .selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1(selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1),
    .selector_MUX_206_reg_6_0_0_0(selector_MUX_206_reg_6_0_0_0),
    .selector_MUX_207_reg_7_0_0_0(selector_MUX_207_reg_7_0_0_0),
    .selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0(selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0),
    .selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1(selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1),
    .selector_MUX_92_gimple_return_FU_27_i0_0_0_0(selector_MUX_92_gimple_return_FU_27_i0_0_0_0),
    .wrenable_reg_0(wrenable_reg_0),
    .wrenable_reg_1(wrenable_reg_1),
    .wrenable_reg_10(wrenable_reg_10),
    .wrenable_reg_11(wrenable_reg_11),
    .wrenable_reg_12(wrenable_reg_12),
    .wrenable_reg_13(wrenable_reg_13),
    .wrenable_reg_14(wrenable_reg_14),
    .wrenable_reg_15(wrenable_reg_15),
    .wrenable_reg_16(wrenable_reg_16),
    .wrenable_reg_17(wrenable_reg_17),
    .wrenable_reg_18(wrenable_reg_18),
    .wrenable_reg_19(wrenable_reg_19),
    .wrenable_reg_2(wrenable_reg_2),
    .wrenable_reg_20(wrenable_reg_20),
    .wrenable_reg_21(wrenable_reg_21),
    .wrenable_reg_3(wrenable_reg_3),
    .wrenable_reg_4(wrenable_reg_4),
    .wrenable_reg_5(wrenable_reg_5),
    .wrenable_reg_6(wrenable_reg_6),
    .wrenable_reg_7(wrenable_reg_7),
    .wrenable_reg_8(wrenable_reg_8),
    .wrenable_reg_9(wrenable_reg_9),
    .OUT_CONDITION_main_33672_33808(OUT_CONDITION_main_33672_33808),
    .OUT_CONDITION_main_33672_33847(OUT_CONDITION_main_33672_33847),
    .OUT_UNBOUNDED_main_33672_34057(OUT_UNBOUNDED_main_33672_34057),
    .OUT_UNBOUNDED_main_33672_34059(OUT_UNBOUNDED_main_33672_34059),
    .clock(clock),
    .reset(reset),
    .start_port(start_port));
  datapath_main #(.MEM_var_33690_33672(128),
    .MEM_var_33691_33672(256),
    .MEM_var_33764_33672(384),
    .MEM_var_33765_33672(512)) Datapath_i (.return_port(return_port),
    .Sout_Rdata_ram(Sout_Rdata_ram),
    .Sout_DataRdy(Sout_DataRdy),
    .Mout_oe_ram(Mout_oe_ram),
    .Mout_we_ram(Mout_we_ram),
    .Mout_addr_ram(Mout_addr_ram),
    .Mout_Wdata_ram(Mout_Wdata_ram),
    .Mout_data_ram_size(Mout_data_ram_size),
    .OUT_CONDITION_main_33672_33808(OUT_CONDITION_main_33672_33808),
    .OUT_CONDITION_main_33672_33847(OUT_CONDITION_main_33672_33847),
    .OUT_UNBOUNDED_main_33672_34057(OUT_UNBOUNDED_main_33672_34057),
    .OUT_UNBOUNDED_main_33672_34059(OUT_UNBOUNDED_main_33672_34059),
    .clock(clock),
    .reset(reset),
    .S_oe_ram(S_oe_ram),
    .S_we_ram(S_we_ram),
    .S_addr_ram(S_addr_ram),
    .S_Wdata_ram(S_Wdata_ram),
    .S_data_ram_size(S_data_ram_size),
    .M_Rdata_ram(M_Rdata_ram),
    .M_DataRdy(M_DataRdy),
    .Sin_Rdata_ram(Sin_Rdata_ram),
    .Sin_DataRdy(Sin_DataRdy),
    .Min_oe_ram(Min_oe_ram),
    .Min_we_ram(Min_we_ram),
    .Min_addr_ram(Min_addr_ram),
    .Min_Wdata_ram(Min_Wdata_ram),
    .Min_data_ram_size(Min_data_ram_size),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE),
    .fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_2_i0_STORE),
    .selector_IN_UNBOUNDED_main_33672_34057(selector_IN_UNBOUNDED_main_33672_34057),
    .selector_IN_UNBOUNDED_main_33672_34059(selector_IN_UNBOUNDED_main_33672_34059),
    .selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0(selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_0),
    .selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1(selector_MUX_19___internal_bambu_memcpy_75_i0_0_0_1),
    .selector_MUX_206_reg_6_0_0_0(selector_MUX_206_reg_6_0_0_0),
    .selector_MUX_207_reg_7_0_0_0(selector_MUX_207_reg_7_0_0_0),
    .selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0(selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_0),
    .selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1(selector_MUX_20___internal_bambu_memcpy_75_i0_1_0_1),
    .selector_MUX_92_gimple_return_FU_27_i0_0_0_0(selector_MUX_92_gimple_return_FU_27_i0_0_0_0),
    .wrenable_reg_0(wrenable_reg_0),
    .wrenable_reg_1(wrenable_reg_1),
    .wrenable_reg_10(wrenable_reg_10),
    .wrenable_reg_11(wrenable_reg_11),
    .wrenable_reg_12(wrenable_reg_12),
    .wrenable_reg_13(wrenable_reg_13),
    .wrenable_reg_14(wrenable_reg_14),
    .wrenable_reg_15(wrenable_reg_15),
    .wrenable_reg_16(wrenable_reg_16),
    .wrenable_reg_17(wrenable_reg_17),
    .wrenable_reg_18(wrenable_reg_18),
    .wrenable_reg_19(wrenable_reg_19),
    .wrenable_reg_2(wrenable_reg_2),
    .wrenable_reg_20(wrenable_reg_20),
    .wrenable_reg_21(wrenable_reg_21),
    .wrenable_reg_3(wrenable_reg_3),
    .wrenable_reg_4(wrenable_reg_4),
    .wrenable_reg_5(wrenable_reg_5),
    .wrenable_reg_6(wrenable_reg_6),
    .wrenable_reg_7(wrenable_reg_7),
    .wrenable_reg_8(wrenable_reg_8),
    .wrenable_reg_9(wrenable_reg_9));
  flipflop_AR #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) done_delayed_REG (.out1(done_delayed_REG_signal_out),
    .clock(clock),
    .reset(reset),
    .in1(done_delayed_REG_signal_in));
  // io-signal post fix
  assign done_port = done_delayed_REG_signal_out;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module view_convert_expr_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// Minimal interface for function: main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module main(clock,
  reset,
  start_port,
  done_port,
  return_port);
  // IN
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output [31:0] return_port;
  // Component and signal declarations
  wire [1:0] M_DataRdy_INT;
  wire [63:0] M_Rdata_ram_INT;
  wire [63:0] S_Wdata_ram_INT;
  wire [19:0] S_addr_ram_INT;
  wire [11:0] S_data_ram_size_INT;
  wire [1:0] S_oe_ram_INT;
  wire [1:0] S_we_ram_INT;
  wire signed [31:0] out_return_port_view_convert_expr_FU;
  
  _main _main_i0 (.done_port(done_port),
    .return_port(out_return_port_view_convert_expr_FU),
    .Sout_Rdata_ram(M_Rdata_ram_INT),
    .Sout_DataRdy(M_DataRdy_INT),
    .Mout_oe_ram(S_oe_ram_INT),
    .Mout_we_ram(S_we_ram_INT),
    .Mout_addr_ram(S_addr_ram_INT),
    .Mout_Wdata_ram(S_Wdata_ram_INT),
    .Mout_data_ram_size(S_data_ram_size_INT),
    .clock(clock),
    .reset(reset),
    .start_port(start_port),
    .S_oe_ram(S_oe_ram_INT),
    .S_we_ram(S_we_ram_INT),
    .S_addr_ram(S_addr_ram_INT),
    .S_Wdata_ram(S_Wdata_ram_INT),
    .S_data_ram_size(S_data_ram_size_INT),
    .M_Rdata_ram(M_Rdata_ram_INT),
    .M_DataRdy(M_DataRdy_INT),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .Min_oe_ram({1'b0,
      1'b0}),
    .Min_we_ram({1'b0,
      1'b0}),
    .Min_addr_ram({10'b0000000000,
      10'b0000000000}),
    .Min_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Min_data_ram_size({6'b000000,
      6'b000000}));
  view_convert_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) return_port_view_convert_expr_FU (.out1(return_port),
    .in1(out_return_port_view_convert_expr_FU));

endmodule


