// 
// Politecnico di Milano
// Code created using PandA - Version: PandA 0.9.7 - Revision 8b59b7ac7b9ab30cd20960921ab47ea5009163f1-main - Date 2023-09-11T17:39:53
// /tmp/.mount_bambu-TIo9wF/usr/bin/bambu executed with: /tmp/.mount_bambu-TIo9wF/usr/bin/bambu --top-fname=main simple_ann.c 
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
module ARRAY_1D_STD_DISTRAM_NN_SDS(clock,
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
    MEMORY_INIT_file="array.mem",
    n_elements=1,
    data_size=32,
    address_space_begin=0,
    address_space_rangesize=4,
    BUS_PIPELINED=1,
    PRIVATE_MEMORY=0,
    READ_ONLY_MEMORY=0,
    USE_SPARSE_MEMORY=1,
    ALIGNMENT=32,
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
  parameter n_byte_on_databus = ALIGNMENT/8;
  parameter nbit_addr = BITSIZE_in2 > BITSIZE_proxy_in2 ? BITSIZE_in2 : BITSIZE_proxy_in2;
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_read_addr = n_elements == 1 ? 1 : $clog2(n_elements);
    parameter nbits_byte_offset = n_byte_on_databus<=1 ? 0 : $clog2(n_byte_on_databus);
  `else
    parameter nbit_read_addr = n_elements == 1 ? 1 : log2(n_elements);
    parameter nbits_byte_offset = n_byte_on_databus<=1 ? 0 : log2(n_byte_on_databus);
  `endif
  parameter max_n_writes = PORTSIZE_sel_STORE;
  parameter max_n_reads = PORTSIZE_sel_LOAD;
  parameter max_n_rw = max_n_writes > max_n_reads ? max_n_writes : max_n_reads;
  
  wire [max_n_writes-1:0] bram_write;
  
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_a;
  wire [nbit_read_addr-1:0] memory_addr_a_0;
  wire [nbit_read_addr-1:0] memory_addr_a_1;
  
  wire [data_size*max_n_writes-1:0] din_value_aggregated;
  wire [data_size*max_n_reads-1:0] dout_a;
  wire [nbit_addr*max_n_rw-1:0] tmp_addr;
  wire [nbit_addr*max_n_rw-1:0] relative_addr;
  wire [PORTSIZE_sel_LOAD-1:0] int_sel_LOAD;
  wire [PORTSIZE_sel_STORE-1:0] int_sel_STORE;
  integer index2;
  
  reg [data_size-1:0] memory [0:n_elements-1] /* synthesis syn_ramstyle = "no_rw_check" */;
  
  initial
  begin
    $readmemb(MEMORY_INIT_file, memory, 0, n_elements-1);
  end
  
  generate
  genvar ind2;
  for (ind2=0; ind2<max_n_rw; ind2=ind2+1)
    begin : Lind2
      assign tmp_addr[(ind2+1)*nbit_addr-1:ind2*nbit_addr] = (proxy_sel_LOAD[ind2]||proxy_sel_STORE[ind2]) ? proxy_in2[(ind2+1)*BITSIZE_proxy_in2-1:ind2*BITSIZE_proxy_in2] : in2[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2];
    end
  endgenerate
  
  generate
  genvar i6;
    for (i6=0; i6<max_n_rw; i6=i6+1)
    begin : L6
      if(USE_SPARSE_MEMORY==1)
        assign relative_addr[(i6)*nbit_addr+nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6)*nbit_addr+nbit_addr-1:i6*nbit_addr];
      else
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6+1)*nbit_addr-1:i6*nbit_addr]-address_space_begin;
    end
  endgenerate
  
  generate
  genvar i7;
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_A
      if (n_elements==1)
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr];
    end
  endgenerate
  
  generate
  genvar i14;
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14
      assign din_value_aggregated[(i14+1)*data_size-1:i14*data_size] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] : in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1];
    end
  endgenerate
  
  generate
  genvar i11;
    for (i11=0; i11<max_n_reads; i11=i11+1)
    begin : asynchronous_read
      assign dout_a[data_size*i11+:data_size] = memory[memory_addr_a[nbit_read_addr*i11+:nbit_read_addr]];
    end
  endgenerate
  
  assign memory_addr_a_0 = memory_addr_a[nbit_read_addr*0+:nbit_read_addr];
  assign memory_addr_a_1 = memory_addr_a[nbit_read_addr*1+:nbit_read_addr];
  
  generate if(READ_ONLY_MEMORY==0)
    always @(posedge clock)
    begin
      if(bram_write[0])
        memory[memory_addr_a_0] <= din_value_aggregated[data_size*0+:data_size];
      if(bram_write[1])
        memory[memory_addr_a_1] <= din_value_aggregated[data_size*1+:data_size];
    end
  endgenerate
  
  generate
  genvar i21;
    for (i21=0; i21<max_n_writes; i21=i21+1)
    begin : L21
        assign bram_write[i21] = int_sel_STORE[i21] || proxy_sel_STORE[i21];
    end
  endgenerate
  
  generate
  genvar i20;
    for (i20=0; i20<max_n_reads; i20=i20+1)
    begin : L20
      assign out1[(i20+1)*BITSIZE_out1-1:i20*BITSIZE_out1] = dout_a[(i20+1)*data_size-1:i20*data_size];
      assign proxy_out1[(i20+1)*BITSIZE_proxy_out1-1:i20*BITSIZE_proxy_out1] = dout_a[(i20+1)*data_size-1:i20*data_size];
    end
  endgenerate
  assign Sout_Rdata_ram =Sin_Rdata_ram;
  assign Sout_DataRdy = Sin_DataRdy;
  assign int_sel_LOAD = sel_LOAD & in4;
  assign int_sel_STORE = sel_STORE & in4;
  
  assign Sout_DataRdy = Sin_DataRdy;

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
// Copyright (C) 2020-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module extract_bit_expr_FU(in1,
  in2,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output out1;
  assign out1 = (in1 >>> in2)&1;
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
module UIconvert_expr_FU(in1,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
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
module ui_bit_and_expr_FU(in1,
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
  assign out1 = in1 & in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2022 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_bit_ior_concat_expr_FU(in1,
  in2,
  in3,
  out1);
  parameter BITSIZE_in1=1,
    BITSIZE_in2=1,
    BITSIZE_in3=1,
    BITSIZE_out1=1,
    OFFSET_PARAMETER=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  input [BITSIZE_in3-1:0] in3;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  parameter nbit_out = BITSIZE_out1 > OFFSET_PARAMETER ? BITSIZE_out1 : 1+OFFSET_PARAMETER;
  wire [nbit_out-1:0] tmp_in1;
  wire [OFFSET_PARAMETER-1:0] tmp_in2;
  generate
    if(BITSIZE_in1 >= nbit_out)
      assign tmp_in1=in1[nbit_out-1:0];
    else
      assign tmp_in1={{(nbit_out-BITSIZE_in1){1'b0}},in1};
  endgenerate
  generate
    if(BITSIZE_in2 >= OFFSET_PARAMETER)
      assign tmp_in2=in2[OFFSET_PARAMETER-1:0];
    else
      assign tmp_in2={{(OFFSET_PARAMETER-BITSIZE_in2){1'b0}},in2};
  endgenerate
  assign out1 = {tmp_in1[nbit_out-1:OFFSET_PARAMETER] , tmp_in2};
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

// Datapath RTL description for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module datapath_main(clock,
  reset,
  return_port,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE,
  fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE,
  selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0,
  selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0,
  selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1,
  selector_MUX_215_gimple_return_FU_143_i0_0_0_0,
  selector_MUX_383_reg_0_0_0_0,
  selector_MUX_384_reg_1_0_0_0,
  selector_MUX_389_reg_14_0_0_0,
  selector_MUX_390_reg_15_0_0_0,
  selector_MUX_404_reg_28_0_0_0,
  selector_MUX_405_reg_29_0_0_0,
  selector_MUX_407_reg_30_0_0_0,
  selector_MUX_440_reg_60_0_0_0,
  selector_MUX_441_reg_61_0_0_0,
  selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0,
  selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0,
  selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1,
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
  wrenable_reg_22,
  wrenable_reg_23,
  wrenable_reg_24,
  wrenable_reg_25,
  wrenable_reg_26,
  wrenable_reg_27,
  wrenable_reg_28,
  wrenable_reg_29,
  wrenable_reg_3,
  wrenable_reg_30,
  wrenable_reg_31,
  wrenable_reg_32,
  wrenable_reg_33,
  wrenable_reg_34,
  wrenable_reg_35,
  wrenable_reg_36,
  wrenable_reg_37,
  wrenable_reg_38,
  wrenable_reg_39,
  wrenable_reg_4,
  wrenable_reg_40,
  wrenable_reg_41,
  wrenable_reg_42,
  wrenable_reg_43,
  wrenable_reg_44,
  wrenable_reg_45,
  wrenable_reg_46,
  wrenable_reg_47,
  wrenable_reg_48,
  wrenable_reg_49,
  wrenable_reg_5,
  wrenable_reg_50,
  wrenable_reg_51,
  wrenable_reg_52,
  wrenable_reg_53,
  wrenable_reg_54,
  wrenable_reg_55,
  wrenable_reg_56,
  wrenable_reg_57,
  wrenable_reg_58,
  wrenable_reg_59,
  wrenable_reg_6,
  wrenable_reg_60,
  wrenable_reg_61,
  wrenable_reg_62,
  wrenable_reg_63,
  wrenable_reg_64,
  wrenable_reg_65,
  wrenable_reg_66,
  wrenable_reg_67,
  wrenable_reg_68,
  wrenable_reg_69,
  wrenable_reg_7,
  wrenable_reg_70,
  wrenable_reg_71,
  wrenable_reg_8,
  wrenable_reg_9,
  OUT_CONDITION_main_33672_33701,
  OUT_CONDITION_main_33672_34347,
  OUT_CONDITION_main_33672_34349,
  OUT_CONDITION_main_33672_34359,
  OUT_CONDITION_main_33672_34563,
  OUT_CONDITION_main_33672_34565,
  OUT_CONDITION_main_33672_34575,
  OUT_CONDITION_main_33672_34608);
  parameter MEM_var_33736_33672=2048,
    MEM_var_33767_33672=2048,
    MEM_var_33810_33672=2048,
    MEM_var_34309_33672=2048,
    MEM_var_34379_33672=2048,
    MEM_var_34403_33672=2048,
    MEM_var_34421_33672=2048,
    MEM_var_34585_33672=2048;
  // IN
  input clock;
  input reset;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  input fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0;
  input selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  input selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  input selector_MUX_215_gimple_return_FU_143_i0_0_0_0;
  input selector_MUX_383_reg_0_0_0_0;
  input selector_MUX_384_reg_1_0_0_0;
  input selector_MUX_389_reg_14_0_0_0;
  input selector_MUX_390_reg_15_0_0_0;
  input selector_MUX_404_reg_28_0_0_0;
  input selector_MUX_405_reg_29_0_0_0;
  input selector_MUX_407_reg_30_0_0_0;
  input selector_MUX_440_reg_60_0_0_0;
  input selector_MUX_441_reg_61_0_0_0;
  input selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0;
  input selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0;
  input selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1;
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
  input wrenable_reg_22;
  input wrenable_reg_23;
  input wrenable_reg_24;
  input wrenable_reg_25;
  input wrenable_reg_26;
  input wrenable_reg_27;
  input wrenable_reg_28;
  input wrenable_reg_29;
  input wrenable_reg_3;
  input wrenable_reg_30;
  input wrenable_reg_31;
  input wrenable_reg_32;
  input wrenable_reg_33;
  input wrenable_reg_34;
  input wrenable_reg_35;
  input wrenable_reg_36;
  input wrenable_reg_37;
  input wrenable_reg_38;
  input wrenable_reg_39;
  input wrenable_reg_4;
  input wrenable_reg_40;
  input wrenable_reg_41;
  input wrenable_reg_42;
  input wrenable_reg_43;
  input wrenable_reg_44;
  input wrenable_reg_45;
  input wrenable_reg_46;
  input wrenable_reg_47;
  input wrenable_reg_48;
  input wrenable_reg_49;
  input wrenable_reg_5;
  input wrenable_reg_50;
  input wrenable_reg_51;
  input wrenable_reg_52;
  input wrenable_reg_53;
  input wrenable_reg_54;
  input wrenable_reg_55;
  input wrenable_reg_56;
  input wrenable_reg_57;
  input wrenable_reg_58;
  input wrenable_reg_59;
  input wrenable_reg_6;
  input wrenable_reg_60;
  input wrenable_reg_61;
  input wrenable_reg_62;
  input wrenable_reg_63;
  input wrenable_reg_64;
  input wrenable_reg_65;
  input wrenable_reg_66;
  input wrenable_reg_67;
  input wrenable_reg_68;
  input wrenable_reg_69;
  input wrenable_reg_7;
  input wrenable_reg_70;
  input wrenable_reg_71;
  input wrenable_reg_8;
  input wrenable_reg_9;
  // OUT
  output signed [31:0] return_port;
  output OUT_CONDITION_main_33672_33701;
  output OUT_CONDITION_main_33672_34347;
  output OUT_CONDITION_main_33672_34349;
  output OUT_CONDITION_main_33672_34359;
  output OUT_CONDITION_main_33672_34563;
  output OUT_CONDITION_main_33672_34565;
  output OUT_CONDITION_main_33672_34575;
  output OUT_CONDITION_main_33672_34608;
  // Component and signal declarations
  wire null_out_signal_array_33736_0_Sout_DataRdy_0;
  wire null_out_signal_array_33736_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_33736_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_33736_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_33736_0_out1_1;
  wire [31:0] null_out_signal_array_33736_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33736_0_proxy_out1_1;
  wire null_out_signal_array_33767_0_Sout_DataRdy_0;
  wire null_out_signal_array_33767_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_33767_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_33767_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_33767_0_out1_1;
  wire [31:0] null_out_signal_array_33767_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33767_0_proxy_out1_1;
  wire null_out_signal_array_33810_0_Sout_DataRdy_0;
  wire null_out_signal_array_33810_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_33810_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_33810_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_33810_0_out1_1;
  wire [31:0] null_out_signal_array_33810_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_33810_0_proxy_out1_1;
  wire null_out_signal_array_34309_0_Sout_DataRdy_0;
  wire null_out_signal_array_34309_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_34309_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_34309_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_34309_0_out1_1;
  wire [31:0] null_out_signal_array_34309_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_34309_0_proxy_out1_1;
  wire null_out_signal_array_34379_0_Sout_DataRdy_0;
  wire null_out_signal_array_34379_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_34379_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_34379_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_34379_0_out1_1;
  wire [31:0] null_out_signal_array_34379_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_34379_0_proxy_out1_1;
  wire null_out_signal_array_34403_0_Sout_DataRdy_0;
  wire null_out_signal_array_34403_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_34403_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_34403_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_34403_0_out1_1;
  wire [31:0] null_out_signal_array_34403_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_34403_0_proxy_out1_1;
  wire null_out_signal_array_34421_0_Sout_DataRdy_0;
  wire null_out_signal_array_34421_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_34421_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_34421_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_34421_0_out1_1;
  wire [31:0] null_out_signal_array_34421_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_34421_0_proxy_out1_1;
  wire null_out_signal_array_34585_0_Sout_DataRdy_0;
  wire null_out_signal_array_34585_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_34585_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_34585_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_34585_0_out1_1;
  wire [31:0] null_out_signal_array_34585_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_34585_0_proxy_out1_1;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_0_i0_array_33736_0;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_4_i0_array_34379_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0;
  wire [9:0] out_IUdata_converter_FU_132_i0_fu_main_33672_34385;
  wire [9:0] out_IUdata_converter_FU_134_i0_fu_main_33672_34569;
  wire [31:0] out_IUdata_converter_FU_136_i0_fu_main_33672_34618;
  wire [31:0] out_IUdata_converter_FU_137_i0_fu_main_33672_34631;
  wire [31:0] out_IUdata_converter_FU_138_i0_fu_main_33672_34641;
  wire [31:0] out_IUdata_converter_FU_139_i0_fu_main_33672_34651;
  wire [31:0] out_IUdata_converter_FU_140_i0_fu_main_33672_34661;
  wire [31:0] out_IUdata_converter_FU_141_i0_fu_main_33672_34671;
  wire [9:0] out_IUdata_converter_FU_28_i0_fu_main_33672_33695;
  wire [9:0] out_IUdata_converter_FU_79_i0_fu_main_33672_33745;
  wire [9:0] out_IUdata_converter_FU_84_i0_fu_main_33672_34353;
  wire [31:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0;
  wire [11:0] out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  wire [11:0] out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  wire [31:0] out_MUX_215_gimple_return_FU_143_i0_0_0_0;
  wire [31:0] out_MUX_383_reg_0_0_0_0;
  wire [31:0] out_MUX_384_reg_1_0_0_0;
  wire [31:0] out_MUX_389_reg_14_0_0_0;
  wire [31:0] out_MUX_390_reg_15_0_0_0;
  wire [31:0] out_MUX_404_reg_28_0_0_0;
  wire [31:0] out_MUX_405_reg_29_0_0_0;
  wire [31:0] out_MUX_407_reg_30_0_0_0;
  wire [31:0] out_MUX_440_reg_60_0_0_0;
  wire [31:0] out_MUX_441_reg_61_0_0_0;
  wire [31:0] out_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0;
  wire [11:0] out_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0;
  wire [11:0] out_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1;
  wire signed [1:0] out_UIconvert_expr_FU_144_i0_fu_main_33672_34673;
  wire out_UUdata_converter_FU_30_i0_fu_main_33672_34941;
  wire [11:0] out_addr_expr_FU_10_i0_fu_main_33672_34722;
  wire [11:0] out_addr_expr_FU_11_i0_fu_main_33672_34732;
  wire [11:0] out_addr_expr_FU_12_i0_fu_main_33672_34746;
  wire [11:0] out_addr_expr_FU_13_i0_fu_main_33672_34776;
  wire [11:0] out_addr_expr_FU_14_i0_fu_main_33672_34762;
  wire [11:0] out_addr_expr_FU_15_i0_fu_main_33672_34797;
  wire [11:0] out_addr_expr_FU_16_i0_fu_main_33672_34807;
  wire [11:0] out_addr_expr_FU_17_i0_fu_main_33672_34821;
  wire [11:0] out_addr_expr_FU_18_i0_fu_main_33672_34849;
  wire [11:0] out_addr_expr_FU_19_i0_fu_main_33672_34835;
  wire [11:0] out_addr_expr_FU_20_i0_fu_main_33672_34869;
  wire [11:0] out_addr_expr_FU_21_i0_fu_main_33672_34879;
  wire signed [1:0] out_bit_and_expr_FU_8_0_8_146_i0_fu_main_33672_34984;
  wire signed [1:0] out_bit_and_expr_FU_8_0_8_146_i1_fu_main_33672_34998;
  wire signed [4:0] out_bit_and_expr_FU_8_0_8_147_i0_fu_main_33672_35015;
  wire signed [4:0] out_bit_and_expr_FU_8_0_8_147_i1_fu_main_33672_35029;
  wire signed [31:0] out_bit_ior_concat_expr_FU_148_i0_fu_main_33672_34617;
  wire signed [31:0] out_bit_ior_concat_expr_FU_148_i1_fu_main_33672_34640;
  wire signed [31:0] out_bit_ior_concat_expr_FU_149_i0_fu_main_33672_34650;
  wire signed [31:0] out_bit_ior_concat_expr_FU_149_i1_fu_main_33672_34660;
  wire signed [1:0] out_cond_expr_FU_8_8_8_8_150_i0_fu_main_33672_35082;
  wire signed [1:0] out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085;
  wire out_const_0;
  wire [31:0] out_const_1;
  wire [8:0] out_const_10;
  wire [12:0] out_const_11;
  wire [4:0] out_const_12;
  wire [5:0] out_const_13;
  wire [13:0] out_const_14;
  wire out_const_15;
  wire [1:0] out_const_16;
  wire [2:0] out_const_17;
  wire [3:0] out_const_18;
  wire [4:0] out_const_19;
  wire [1:0] out_const_2;
  wire [32:0] out_const_20;
  wire [4:0] out_const_21;
  wire [5:0] out_const_22;
  wire [3:0] out_const_23;
  wire [4:0] out_const_24;
  wire [4:0] out_const_25;
  wire [10:0] out_const_26;
  wire [2:0] out_const_27;
  wire [3:0] out_const_28;
  wire [4:0] out_const_29;
  wire [3:0] out_const_3;
  wire [14:0] out_const_30;
  wire [4:0] out_const_31;
  wire [3:0] out_const_32;
  wire [4:0] out_const_33;
  wire [4:0] out_const_34;
  wire [1:0] out_const_35;
  wire [2:0] out_const_36;
  wire [3:0] out_const_37;
  wire [4:0] out_const_38;
  wire [4:0] out_const_39;
  wire [4:0] out_const_4;
  wire [10:0] out_const_40;
  wire [10:0] out_const_41;
  wire [3:0] out_const_42;
  wire [4:0] out_const_43;
  wire [4:0] out_const_44;
  wire [2:0] out_const_45;
  wire [3:0] out_const_46;
  wire [4:0] out_const_47;
  wire [4:0] out_const_48;
  wire [3:0] out_const_49;
  wire [5:0] out_const_5;
  wire [4:0] out_const_50;
  wire [4:0] out_const_51;
  wire [10:0] out_const_52;
  wire [11:0] out_const_53;
  wire [15:0] out_const_54;
  wire [11:0] out_const_55;
  wire [11:0] out_const_56;
  wire [11:0] out_const_57;
  wire [11:0] out_const_58;
  wire [11:0] out_const_59;
  wire [6:0] out_const_6;
  wire [11:0] out_const_60;
  wire [11:0] out_const_61;
  wire [11:0] out_const_62;
  wire [9:0] out_const_7;
  wire [13:0] out_const_8;
  wire [11:0] out_const_9;
  wire signed [10:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0_I_32_I_11;
  wire [12:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0_32_13;
  wire [12:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0_32_13;
  wire signed [10:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0_I_32_I_11;
  wire [12:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0_32_13;
  wire [12:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0_32_13;
  wire signed [31:0] out_conv_out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085_I_2_I_32;
  wire signed [31:0] out_conv_out_const_0_I_1_I_32;
  wire [31:0] out_conv_out_const_55_12_32;
  wire [31:0] out_conv_out_const_56_12_32;
  wire [31:0] out_conv_out_const_57_12_32;
  wire [31:0] out_conv_out_const_58_12_32;
  wire [31:0] out_conv_out_const_59_12_32;
  wire [31:0] out_conv_out_const_60_12_32;
  wire [31:0] out_conv_out_const_61_12_32;
  wire [31:0] out_conv_out_const_62_12_32;
  wire [5:0] out_conv_out_const_6_7_6;
  wire [31:0] out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32;
  wire [31:0] out_conv_out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4_1_32;
  wire out_extract_bit_expr_FU_100_i0_fu_main_33672_35287;
  wire out_extract_bit_expr_FU_101_i0_fu_main_33672_35290;
  wire out_extract_bit_expr_FU_102_i0_fu_main_33672_35293;
  wire out_extract_bit_expr_FU_103_i0_fu_main_33672_35296;
  wire out_extract_bit_expr_FU_105_i0_fu_main_33672_35302;
  wire out_extract_bit_expr_FU_106_i0_fu_main_33672_35305;
  wire out_extract_bit_expr_FU_107_i0_fu_main_33672_35308;
  wire out_extract_bit_expr_FU_108_i0_fu_main_33672_35311;
  wire out_extract_bit_expr_FU_109_i0_fu_main_33672_35314;
  wire out_extract_bit_expr_FU_111_i0_fu_main_33672_35320;
  wire out_extract_bit_expr_FU_112_i0_fu_main_33672_35323;
  wire out_extract_bit_expr_FU_113_i0_fu_main_33672_35326;
  wire out_extract_bit_expr_FU_114_i0_fu_main_33672_35329;
  wire out_extract_bit_expr_FU_115_i0_fu_main_33672_35332;
  wire out_extract_bit_expr_FU_117_i0_fu_main_33672_35338;
  wire out_extract_bit_expr_FU_118_i0_fu_main_33672_35341;
  wire out_extract_bit_expr_FU_119_i0_fu_main_33672_35344;
  wire out_extract_bit_expr_FU_120_i0_fu_main_33672_35347;
  wire out_extract_bit_expr_FU_121_i0_fu_main_33672_35350;
  wire out_extract_bit_expr_FU_123_i0_fu_main_33672_35356;
  wire out_extract_bit_expr_FU_29_i0_fu_main_33672_35370;
  wire out_extract_bit_expr_FU_32_i0_fu_main_33672_35100;
  wire out_extract_bit_expr_FU_33_i0_fu_main_33672_35103;
  wire out_extract_bit_expr_FU_34_i0_fu_main_33672_35106;
  wire out_extract_bit_expr_FU_35_i0_fu_main_33672_35110;
  wire out_extract_bit_expr_FU_36_i0_fu_main_33672_35114;
  wire out_extract_bit_expr_FU_37_i0_fu_main_33672_35118;
  wire out_extract_bit_expr_FU_39_i0_fu_main_33672_35126;
  wire out_extract_bit_expr_FU_40_i0_fu_main_33672_35130;
  wire out_extract_bit_expr_FU_41_i0_fu_main_33672_35134;
  wire out_extract_bit_expr_FU_42_i0_fu_main_33672_35138;
  wire out_extract_bit_expr_FU_43_i0_fu_main_33672_35142;
  wire out_extract_bit_expr_FU_45_i0_fu_main_33672_35149;
  wire out_extract_bit_expr_FU_46_i0_fu_main_33672_35153;
  wire out_extract_bit_expr_FU_47_i0_fu_main_33672_35157;
  wire out_extract_bit_expr_FU_48_i0_fu_main_33672_35161;
  wire out_extract_bit_expr_FU_49_i0_fu_main_33672_35165;
  wire out_extract_bit_expr_FU_51_i0_fu_main_33672_35172;
  wire out_extract_bit_expr_FU_52_i0_fu_main_33672_35176;
  wire out_extract_bit_expr_FU_53_i0_fu_main_33672_35180;
  wire out_extract_bit_expr_FU_54_i0_fu_main_33672_35184;
  wire out_extract_bit_expr_FU_55_i0_fu_main_33672_35188;
  wire out_extract_bit_expr_FU_57_i0_fu_main_33672_35195;
  wire out_extract_bit_expr_FU_58_i0_fu_main_33672_35199;
  wire out_extract_bit_expr_FU_59_i0_fu_main_33672_35203;
  wire out_extract_bit_expr_FU_60_i0_fu_main_33672_35207;
  wire out_extract_bit_expr_FU_61_i0_fu_main_33672_35211;
  wire out_extract_bit_expr_FU_63_i0_fu_main_33672_35218;
  wire out_extract_bit_expr_FU_64_i0_fu_main_33672_35222;
  wire out_extract_bit_expr_FU_65_i0_fu_main_33672_35226;
  wire out_extract_bit_expr_FU_66_i0_fu_main_33672_35230;
  wire out_extract_bit_expr_FU_67_i0_fu_main_33672_35234;
  wire out_extract_bit_expr_FU_69_i0_fu_main_33672_35241;
  wire out_extract_bit_expr_FU_86_i0_fu_main_33672_35245;
  wire out_extract_bit_expr_FU_87_i0_fu_main_33672_35248;
  wire out_extract_bit_expr_FU_88_i0_fu_main_33672_35251;
  wire out_extract_bit_expr_FU_89_i0_fu_main_33672_35254;
  wire out_extract_bit_expr_FU_90_i0_fu_main_33672_35257;
  wire out_extract_bit_expr_FU_91_i0_fu_main_33672_35260;
  wire out_extract_bit_expr_FU_93_i0_fu_main_33672_35266;
  wire out_extract_bit_expr_FU_94_i0_fu_main_33672_35269;
  wire out_extract_bit_expr_FU_95_i0_fu_main_33672_35272;
  wire out_extract_bit_expr_FU_96_i0_fu_main_33672_35275;
  wire out_extract_bit_expr_FU_97_i0_fu_main_33672_35278;
  wire out_extract_bit_expr_FU_99_i0_fu_main_33672_35284;
  wire signed [0:0] out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0;
  wire [31:0] out_iu_conv_conn_obj_1_IUdata_converter_FU_iu_conv_1;
  wire [31:0] out_iu_conv_conn_obj_2_IUdata_converter_FU_iu_conv_2;
  wire [31:0] out_iu_conv_conn_obj_4_IUdata_converter_FU_iu_conv_3;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_151_i0_fu_main_33672_34981;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_151_i1_fu_main_33672_34995;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_152_i0_fu_main_33672_35011;
  wire signed [31:0] out_lshift_expr_FU_32_0_32_152_i1_fu_main_33672_35026;
  wire out_lt_expr_FU_32_0_32_153_i0_fu_main_33672_34901;
  wire out_lut_expr_FU_104_i0_fu_main_33672_35299;
  wire out_lut_expr_FU_110_i0_fu_main_33672_35317;
  wire out_lut_expr_FU_116_i0_fu_main_33672_35335;
  wire out_lut_expr_FU_122_i0_fu_main_33672_35353;
  wire out_lut_expr_FU_124_i0_fu_main_33672_34895;
  wire out_lut_expr_FU_145_i0_fu_main_33672_35072;
  wire out_lut_expr_FU_38_i0_fu_main_33672_35121;
  wire out_lut_expr_FU_44_i0_fu_main_33672_35145;
  wire out_lut_expr_FU_50_i0_fu_main_33672_35168;
  wire out_lut_expr_FU_56_i0_fu_main_33672_35191;
  wire out_lut_expr_FU_62_i0_fu_main_33672_35214;
  wire out_lut_expr_FU_68_i0_fu_main_33672_35237;
  wire out_lut_expr_FU_70_i0_fu_main_33672_34889;
  wire out_lut_expr_FU_92_i0_fu_main_33672_35263;
  wire out_lut_expr_FU_98_i0_fu_main_33672_35281;
  wire signed [25:0] out_mult_expr_FU_16_16_16_0_154_i0_fu_main_33672_33698;
  wire signed [31:0] out_mult_expr_FU_32_32_32_0_155_i0_fu_main_33672_34356;
  wire signed [31:0] out_mult_expr_FU_32_32_32_0_155_i1_fu_main_33672_34572;
  wire out_ne_expr_FU_32_0_32_156_i0_fu_main_33672_34887;
  wire out_ne_expr_FU_32_0_32_157_i0_fu_main_33672_34891;
  wire out_ne_expr_FU_32_0_32_157_i1_fu_main_33672_34893;
  wire out_ne_expr_FU_32_0_32_158_i0_fu_main_33672_34897;
  wire out_ne_expr_FU_32_0_32_158_i1_fu_main_33672_34899;
  wire signed [31:0] out_plus_expr_FU_32_0_32_159_i0_fu_main_33672_33700;
  wire signed [31:0] out_plus_expr_FU_32_0_32_159_i1_fu_main_33672_33754;
  wire signed [31:0] out_plus_expr_FU_32_0_32_159_i2_fu_main_33672_34358;
  wire signed [31:0] out_plus_expr_FU_32_0_32_159_i3_fu_main_33672_34392;
  wire signed [31:0] out_plus_expr_FU_32_0_32_159_i4_fu_main_33672_34574;
  wire signed [31:0] out_plus_expr_FU_32_0_32_160_i0_fu_main_33672_34607;
  wire signed [31:0] out_plus_expr_FU_32_0_32_161_i0_fu_main_33672_34630;
  wire signed [31:0] out_plus_expr_FU_32_0_32_162_i0_fu_main_33672_34670;
  wire signed [31:0] out_plus_expr_FU_32_0_32_163_i0_fu_main_33672_34978;
  wire signed [31:0] out_plus_expr_FU_32_0_32_164_i0_fu_main_33672_34992;
  wire signed [28:0] out_plus_expr_FU_32_0_32_165_i0_fu_main_33672_35008;
  wire signed [28:0] out_plus_expr_FU_32_0_32_166_i0_fu_main_33672_35023;
  wire signed [31:0] out_plus_expr_FU_32_32_32_167_i0_fu_main_33672_33699;
  wire signed [31:0] out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758;
  wire signed [31:0] out_plus_expr_FU_32_32_32_167_i2_fu_main_33672_34357;
  wire signed [31:0] out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395;
  wire signed [31:0] out_plus_expr_FU_32_32_32_167_i4_fu_main_33672_34573;
  wire out_read_cond_FU_125_i0_fu_main_33672_34563;
  wire out_read_cond_FU_128_i0_fu_main_33672_34565;
  wire out_read_cond_FU_135_i0_fu_main_33672_34575;
  wire out_read_cond_FU_142_i0_fu_main_33672_34608;
  wire out_read_cond_FU_31_i0_fu_main_33672_33701;
  wire out_read_cond_FU_71_i0_fu_main_33672_34347;
  wire out_read_cond_FU_74_i0_fu_main_33672_34349;
  wire out_read_cond_FU_85_i0_fu_main_33672_34359;
  wire [31:0] out_reg_0_reg_0;
  wire [11:0] out_reg_10_reg_10;
  wire [11:0] out_reg_11_reg_11;
  wire [11:0] out_reg_12_reg_12;
  wire [11:0] out_reg_13_reg_13;
  wire [31:0] out_reg_14_reg_14;
  wire [31:0] out_reg_15_reg_15;
  wire [11:0] out_reg_16_reg_16;
  wire [11:0] out_reg_17_reg_17;
  wire [11:0] out_reg_18_reg_18;
  wire out_reg_19_reg_19;
  wire [31:0] out_reg_1_reg_1;
  wire [8:0] out_reg_20_reg_20;
  wire [12:0] out_reg_21_reg_21;
  wire [12:0] out_reg_22_reg_22;
  wire out_reg_23_reg_23;
  wire [31:0] out_reg_24_reg_24;
  wire [11:0] out_reg_25_reg_25;
  wire [11:0] out_reg_26_reg_26;
  wire out_reg_27_reg_27;
  wire [31:0] out_reg_28_reg_28;
  wire [31:0] out_reg_29_reg_29;
  wire [11:0] out_reg_2_reg_2;
  wire [31:0] out_reg_30_reg_30;
  wire [11:0] out_reg_31_reg_31;
  wire [11:0] out_reg_32_reg_32;
  wire [11:0] out_reg_33_reg_33;
  wire out_reg_34_reg_34;
  wire [5:0] out_reg_35_reg_35;
  wire [12:0] out_reg_36_reg_36;
  wire out_reg_37_reg_37;
  wire [31:0] out_reg_38_reg_38;
  wire [31:0] out_reg_39_reg_39;
  wire [11:0] out_reg_3_reg_3;
  wire [31:0] out_reg_40_reg_40;
  wire [11:0] out_reg_41_reg_41;
  wire [11:0] out_reg_42_reg_42;
  wire out_reg_43_reg_43;
  wire out_reg_44_reg_44;
  wire out_reg_45_reg_45;
  wire out_reg_46_reg_46;
  wire out_reg_47_reg_47;
  wire out_reg_48_reg_48;
  wire out_reg_49_reg_49;
  wire [11:0] out_reg_4_reg_4;
  wire out_reg_50_reg_50;
  wire out_reg_51_reg_51;
  wire out_reg_52_reg_52;
  wire out_reg_53_reg_53;
  wire out_reg_54_reg_54;
  wire out_reg_55_reg_55;
  wire out_reg_56_reg_56;
  wire out_reg_57_reg_57;
  wire out_reg_58_reg_58;
  wire out_reg_59_reg_59;
  wire [11:0] out_reg_5_reg_5;
  wire [31:0] out_reg_60_reg_60;
  wire [31:0] out_reg_61_reg_61;
  wire [12:0] out_reg_62_reg_62;
  wire out_reg_63_reg_63;
  wire [31:0] out_reg_64_reg_64;
  wire [31:0] out_reg_65_reg_65;
  wire [31:0] out_reg_66_reg_66;
  wire [31:0] out_reg_67_reg_67;
  wire [31:0] out_reg_68_reg_68;
  wire [31:0] out_reg_69_reg_69;
  wire [11:0] out_reg_6_reg_6;
  wire [31:0] out_reg_70_reg_70;
  wire [31:0] out_reg_71_reg_71;
  wire [11:0] out_reg_7_reg_7;
  wire [11:0] out_reg_8_reg_8;
  wire [11:0] out_reg_9_reg_9;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_168_i0_fu_main_33672_34973;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_168_i1_fu_main_33672_34989;
  wire signed [27:0] out_rshift_expr_FU_32_0_32_169_i0_fu_main_33672_35003;
  wire signed [27:0] out_rshift_expr_FU_32_0_32_169_i1_fu_main_33672_35020;
  wire [0:0] out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4;
  wire [3:0] out_ui_bit_and_expr_FU_8_0_8_170_i0_fu_main_33672_34967;
  wire [11:0] out_ui_bit_ior_concat_expr_FU_171_i0_fu_main_33672_34719;
  wire [11:0] out_ui_bit_ior_concat_expr_FU_172_i0_fu_main_33672_34794;
  wire out_ui_gt_expr_FU_32_0_32_173_i0_fu_main_33672_34672;
  wire out_ui_le_expr_FU_32_0_32_174_i0_fu_main_33672_34903;
  wire out_ui_le_expr_FU_32_0_32_175_i0_fu_main_33672_34905;
  wire out_ui_le_expr_FU_32_0_32_176_i0_fu_main_33672_34907;
  wire out_ui_le_expr_FU_32_0_32_177_i0_fu_main_33672_34909;
  wire out_ui_le_expr_FU_32_0_32_178_i0_fu_main_33672_34911;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_179_i0_fu_main_33672_34551;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_179_i1_fu_main_33672_34963;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i0_fu_main_33672_34716;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i10_fu_main_33672_34866;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i11_fu_main_33672_34876;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i1_fu_main_33672_34729;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i2_fu_main_33672_34743;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i3_fu_main_33672_34759;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i4_fu_main_33672_34773;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i5_fu_main_33672_34791;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i6_fu_main_33672_34804;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i7_fu_main_33672_34818;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i8_fu_main_33672_34832;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_180_i9_fu_main_33672_34846;
  wire [11:0] out_ui_lshift_expr_FU_16_0_16_181_i0_fu_main_33672_34937;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_182_i0_fu_main_33672_34951;
  wire [2:0] out_ui_lshift_expr_FU_8_0_8_183_i0_fu_main_33672_35039;
  wire [5:0] out_ui_lshift_expr_FU_8_0_8_183_i1_fu_main_33672_35046;
  wire [28:0] out_ui_plus_expr_FU_32_0_32_184_i0_fu_main_33672_34948;
  wire [8:0] out_ui_plus_expr_FU_8_8_8_185_i0_fu_main_33672_34934;
  wire [5:0] out_ui_plus_expr_FU_8_8_8_185_i1_fu_main_33672_34960;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i0_fu_main_33672_34725;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i10_fu_main_33672_34872;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i11_fu_main_33672_34882;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i1_fu_main_33672_34735;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i2_fu_main_33672_34749;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i3_fu_main_33672_34765;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i4_fu_main_33672_34779;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i5_fu_main_33672_34800;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i6_fu_main_33672_34810;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i7_fu_main_33672_34824;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i8_fu_main_33672_34838;
  wire [11:0] out_ui_pointer_plus_expr_FU_16_16_16_186_i9_fu_main_33672_34852;
  wire [8:0] out_ui_rshift_expr_FU_16_0_16_187_i0_fu_main_33672_34927;
  wire [5:0] out_ui_rshift_expr_FU_16_0_16_188_i0_fu_main_33672_34954;
  wire [5:0] out_ui_rshift_expr_FU_16_0_16_188_i1_fu_main_33672_34958;
  wire [3:0] out_ui_rshift_expr_FU_16_0_16_189_i0_fu_main_33672_35042;
  wire [8:0] out_ui_rshift_expr_FU_32_0_32_190_i0_fu_main_33672_34932;
  wire [28:0] out_ui_rshift_expr_FU_32_0_32_190_i1_fu_main_33672_34946;
  
  ASSIGN_SIGNED_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) ASSIGN_SIGNED_FU_i_assign_0 (.out1(out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0),
    .in1(out_const_0));
  ASSIGN_UNSIGNED_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) ASSIGN_UNSIGNED_FU_u_assign_4 (.out1(out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4),
    .in1(out_const_0));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) IUdata_converter_FU_iu_conv_1 (.out1(out_iu_conv_conn_obj_1_IUdata_converter_FU_iu_conv_1),
    .in1(out_reg_24_reg_24));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) IUdata_converter_FU_iu_conv_2 (.out1(out_iu_conv_conn_obj_2_IUdata_converter_FU_iu_conv_2),
    .in1(out_const_1));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) IUdata_converter_FU_iu_conv_3 (.out1(out_iu_conv_conn_obj_4_IUdata_converter_FU_iu_conv_3),
    .in1(out_reg_40_reg_40));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0),
    .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0),
    .in1(out_iu_conv_conn_obj_1_IUdata_converter_FU_iu_conv_1),
    .in2(out_iu_conv_conn_obj_2_IUdata_converter_FU_iu_conv_2));
  MUX_GATE #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12)) MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 (.out1(out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0),
    .sel(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0),
    .in1(out_reg_26_reg_26),
    .in2(out_reg_25_reg_25));
  MUX_GATE #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12)) MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 (.out1(out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1),
    .sel(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i6_fu_main_33672_34810),
    .in2(out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_215_gimple_return_FU_143_i0_0_0_0 (.out1(out_MUX_215_gimple_return_FU_143_i0_0_0_0),
    .sel(selector_MUX_215_gimple_return_FU_143_i0_0_0_0),
    .in1(out_conv_out_const_0_I_1_I_32),
    .in2(out_conv_out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085_I_2_I_32));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_383_reg_0_0_0_0 (.out1(out_MUX_383_reg_0_0_0_0),
    .sel(selector_MUX_383_reg_0_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_0_32_159_i1_fu_main_33672_33754));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_384_reg_1_0_0_0 (.out1(out_MUX_384_reg_1_0_0_0),
    .sel(selector_MUX_384_reg_1_0_0_0),
    .in1(out_conv_out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4_1_32),
    .in2(out_ui_lshift_expr_FU_32_0_32_182_i0_fu_main_33672_34951));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_389_reg_14_0_0_0 (.out1(out_MUX_389_reg_14_0_0_0),
    .sel(selector_MUX_389_reg_14_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_32_32_167_i0_fu_main_33672_33699));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_390_reg_15_0_0_0 (.out1(out_MUX_390_reg_15_0_0_0),
    .sel(selector_MUX_390_reg_15_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_0_32_159_i0_fu_main_33672_33700));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_404_reg_28_0_0_0 (.out1(out_MUX_404_reg_28_0_0_0),
    .sel(selector_MUX_404_reg_28_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_0_32_159_i3_fu_main_33672_34392));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_405_reg_29_0_0_0 (.out1(out_MUX_405_reg_29_0_0_0),
    .sel(selector_MUX_405_reg_29_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_32_32_167_i2_fu_main_33672_34357));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_407_reg_30_0_0_0 (.out1(out_MUX_407_reg_30_0_0_0),
    .sel(selector_MUX_407_reg_30_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_0_32_159_i2_fu_main_33672_34358));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_440_reg_60_0_0_0 (.out1(out_MUX_440_reg_60_0_0_0),
    .sel(selector_MUX_440_reg_60_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_32_32_167_i4_fu_main_33672_34573));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_441_reg_61_0_0_0 (.out1(out_MUX_441_reg_61_0_0_0),
    .sel(selector_MUX_441_reg_61_0_0_0),
    .in1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in2(out_plus_expr_FU_32_0_32_159_i4_fu_main_33672_34574));
  MUX_GATE #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0 (.out1(out_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0),
    .sel(selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0),
    .in1(out_iu_conv_conn_obj_2_IUdata_converter_FU_iu_conv_2),
    .in2(out_iu_conv_conn_obj_4_IUdata_converter_FU_iu_conv_3));
  MUX_GATE #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12)) MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0 (.out1(out_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0),
    .sel(selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0),
    .in1(out_reg_42_reg_42),
    .in2(out_reg_41_reg_41));
  MUX_GATE #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12)) MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1 (.out1(out_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1),
    .sel(selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i11_fu_main_33672_34882),
    .in2(out_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file_a("array_ref_33736.mem"),
    .MEMORY_INIT_file_b("0_array_ref_33736.mem"),
    .n_elements(64),
    .data_size(8),
    .address_space_begin(MEM_var_33736_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .BRAM_BITSIZE(16),
    .PRIVATE_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33736_0 (.out1({null_out_signal_array_33736_0_out1_1,
      out_ARRAY_1D_STD_BRAM_NN_0_i0_array_33736_0}),
    .Sout_Rdata_ram({null_out_signal_array_33736_0_Sout_Rdata_ram_1,
      null_out_signal_array_33736_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_33736_0_Sout_DataRdy_1,
      null_out_signal_array_33736_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_33736_0_proxy_out1_1,
      null_out_signal_array_33736_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0}),
    .in2({12'b000000000000,
      out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file("array_ref_33767.mem"),
    .n_elements(16),
    .data_size(32),
    .address_space_begin(MEM_var_33767_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .PRIVATE_MEMORY(1),
    .READ_ONLY_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .ALIGNMENT(32),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33767_0 (.out1({null_out_signal_array_33767_0_out1_1,
      out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0}),
    .Sout_Rdata_ram({null_out_signal_array_33767_0_Sout_Rdata_ram_1,
      null_out_signal_array_33767_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_33767_0_Sout_DataRdy_1,
      null_out_signal_array_33767_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_33767_0_proxy_out1_1,
      null_out_signal_array_33767_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({12'b000000000000,
      out_reg_16_reg_16}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file("array_ref_33810.mem"),
    .n_elements(480),
    .data_size(32),
    .address_space_begin(MEM_var_33810_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .PRIVATE_MEMORY(1),
    .READ_ONLY_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .ALIGNMENT(32),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_33810_0 (.out1({null_out_signal_array_33810_0_out1_1,
      out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0}),
    .Sout_Rdata_ram({null_out_signal_array_33810_0_Sout_Rdata_ram_1,
      null_out_signal_array_33810_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_33810_0_Sout_DataRdy_1,
      null_out_signal_array_33810_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_33810_0_proxy_out1_1,
      null_out_signal_array_33810_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({12'b000000000000,
      out_ui_pointer_plus_expr_FU_16_16_16_186_i0_fu_main_33672_34725}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file("array_ref_34309.mem"),
    .n_elements(30),
    .data_size(32),
    .address_space_begin(MEM_var_34309_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .PRIVATE_MEMORY(1),
    .READ_ONLY_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .ALIGNMENT(32),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_34309_0 (.out1({null_out_signal_array_34309_0_out1_1,
      out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0}),
    .Sout_Rdata_ram({null_out_signal_array_34309_0_Sout_Rdata_ram_1,
      null_out_signal_array_34309_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_34309_0_Sout_DataRdy_1,
      null_out_signal_array_34309_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_34309_0_proxy_out1_1,
      null_out_signal_array_34309_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({12'b000000000000,
      out_ui_pointer_plus_expr_FU_16_16_16_186_i1_fu_main_33672_34735}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file_a("array_ref_34379.mem"),
    .MEMORY_INIT_file_b("0_array_ref_34379.mem"),
    .n_elements(32),
    .data_size(8),
    .address_space_begin(MEM_var_34379_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .BRAM_BITSIZE(16),
    .PRIVATE_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_34379_0 (.out1({null_out_signal_array_34379_0_out1_1,
      out_ARRAY_1D_STD_BRAM_NN_4_i0_array_34379_0}),
    .Sout_Rdata_ram({null_out_signal_array_34379_0_Sout_Rdata_ram_1,
      null_out_signal_array_34379_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_34379_0_Sout_DataRdy_1,
      null_out_signal_array_34379_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_34379_0_proxy_out1_1,
      null_out_signal_array_34379_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      out_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0}),
    .in2({12'b000000000000,
      out_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file("array_ref_34403.mem"),
    .n_elements(8),
    .data_size(32),
    .address_space_begin(MEM_var_34403_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .PRIVATE_MEMORY(1),
    .READ_ONLY_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .ALIGNMENT(32),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_34403_0 (.out1({null_out_signal_array_34403_0_out1_1,
      out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0}),
    .Sout_Rdata_ram({null_out_signal_array_34403_0_Sout_Rdata_ram_1,
      null_out_signal_array_34403_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_34403_0_Sout_DataRdy_1,
      null_out_signal_array_34403_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_34403_0_proxy_out1_1,
      null_out_signal_array_34403_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({12'b000000000000,
      out_ui_pointer_plus_expr_FU_16_16_16_186_i7_fu_main_33672_34824}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file("array_ref_34421.mem"),
    .n_elements(128),
    .data_size(32),
    .address_space_begin(MEM_var_34421_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .PRIVATE_MEMORY(1),
    .READ_ONLY_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .ALIGNMENT(32),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_34421_0 (.out1({null_out_signal_array_34421_0_out1_1,
      out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0}),
    .Sout_Rdata_ram({null_out_signal_array_34421_0_Sout_Rdata_ram_1,
      null_out_signal_array_34421_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_34421_0_Sout_DataRdy_1,
      null_out_signal_array_34421_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_34421_0_proxy_out1_1,
      null_out_signal_array_34421_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({12'b000000000000,
      out_ui_pointer_plus_expr_FU_16_16_16_186_i5_fu_main_33672_34800}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32),
    .PORTSIZE_in1(2),
    .BITSIZE_in2(12),
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
    .BITSIZE_S_addr_ram(12),
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
    .MEMORY_INIT_file("array_ref_34585.mem"),
    .n_elements(8),
    .data_size(32),
    .address_space_begin(MEM_var_34585_33672),
    .address_space_rangesize(2048),
    .BUS_PIPELINED(1),
    .PRIVATE_MEMORY(1),
    .READ_ONLY_MEMORY(1),
    .USE_SPARSE_MEMORY(1),
    .ALIGNMENT(32),
    .BITSIZE_proxy_in1(32),
    .PORTSIZE_proxy_in1(2),
    .BITSIZE_proxy_in2(12),
    .PORTSIZE_proxy_in2(2),
    .BITSIZE_proxy_in3(6),
    .PORTSIZE_proxy_in3(2),
    .BITSIZE_proxy_sel_LOAD(1),
    .PORTSIZE_proxy_sel_LOAD(2),
    .BITSIZE_proxy_sel_STORE(1),
    .PORTSIZE_proxy_sel_STORE(2),
    .BITSIZE_proxy_out1(32),
    .PORTSIZE_proxy_out1(2)) array_34585_0 (.out1({null_out_signal_array_34585_0_out1_1,
      out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0}),
    .Sout_Rdata_ram({null_out_signal_array_34585_0_Sout_Rdata_ram_1,
      null_out_signal_array_34585_0_Sout_Rdata_ram_0}),
    .Sout_DataRdy({null_out_signal_array_34585_0_Sout_DataRdy_1,
      null_out_signal_array_34585_0_Sout_DataRdy_0}),
    .proxy_out1({null_out_signal_array_34585_0_proxy_out1_1,
      null_out_signal_array_34585_0_proxy_out1_0}),
    .clock(clock),
    .reset(reset),
    .in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .in2({12'b000000000000,
      out_ui_pointer_plus_expr_FU_16_16_16_186_i10_fu_main_33672_34872}),
    .in3({6'b000000,
      out_conv_out_const_6_7_6}),
    .in4({1'b0,
      out_const_15}),
    .sel_LOAD({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD}),
    .sel_STORE({1'b0,
      fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE}),
    .S_oe_ram({1'b0,
      1'b0}),
    .S_we_ram({1'b0,
      1'b0}),
    .S_addr_ram({12'b000000000000,
      12'b000000000000}),
    .S_Wdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .Sin_Rdata_ram({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .S_data_ram_size({6'b000000,
      6'b000000}),
    .Sin_DataRdy({1'b0,
      1'b0}),
    .proxy_in1({32'b00000000000000000000000000000000,
      32'b00000000000000000000000000000000}),
    .proxy_in2({12'b000000000000,
      12'b000000000000}),
    .proxy_in3({6'b000000,
      6'b000000}),
    .proxy_sel_LOAD({1'b0,
      1'b0}),
    .proxy_sel_STORE({1'b0,
      1'b0}));
  constant_value #(.BITSIZE_out1(1),
    .value(1'b0)) const_0 (.out1(out_const_0));
  constant_value #(.BITSIZE_out1(32),
    .value(32'b00000000000000000000000000000000)) const_1 (.out1(out_const_1));
  constant_value #(.BITSIZE_out1(9),
    .value(9'b011001011)) const_10 (.out1(out_const_10));
  constant_value #(.BITSIZE_out1(13),
    .value(13'b0110110001011)) const_11 (.out1(out_const_11));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b01111)) const_12 (.out1(out_const_12));
  constant_value #(.BITSIZE_out1(6),
    .value(6'b011110)) const_13 (.out1(out_const_13));
  constant_value #(.BITSIZE_out1(14),
    .value(14'b01111111100011)) const_14 (.out1(out_const_14));
  constant_value #(.BITSIZE_out1(1),
    .value(1'b1)) const_15 (.out1(out_const_15));
  constant_value #(.BITSIZE_out1(2),
    .value(2'b10)) const_16 (.out1(out_const_16));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b100)) const_17 (.out1(out_const_17));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1000)) const_18 (.out1(out_const_18));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10000)) const_19 (.out1(out_const_19));
  constant_value #(.BITSIZE_out1(2),
    .value(2'b01)) const_2 (.out1(out_const_2));
  constant_value #(.BITSIZE_out1(33),
    .value(33'b100000000000000000000000000000000)) const_20 (.out1(out_const_20));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10001)) const_21 (.out1(out_const_21));
  constant_value #(.BITSIZE_out1(6),
    .value(6'b100011)) const_22 (.out1(out_const_22));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1001)) const_23 (.out1(out_const_23));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10010)) const_24 (.out1(out_const_24));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10011)) const_25 (.out1(out_const_25));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b10011001100)) const_26 (.out1(out_const_26));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b101)) const_27 (.out1(out_const_27));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1010)) const_28 (.out1(out_const_28));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10100)) const_29 (.out1(out_const_29));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b0100)) const_3 (.out1(out_const_3));
  constant_value #(.BITSIZE_out1(15),
    .value(15'b101000000000000)) const_30 (.out1(out_const_30));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10101)) const_31 (.out1(out_const_31));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1011)) const_32 (.out1(out_const_32));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10110)) const_33 (.out1(out_const_33));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b10111)) const_34 (.out1(out_const_34));
  constant_value #(.BITSIZE_out1(2),
    .value(2'b11)) const_35 (.out1(out_const_35));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b110)) const_36 (.out1(out_const_36));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1100)) const_37 (.out1(out_const_37));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11000)) const_38 (.out1(out_const_38));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11001)) const_39 (.out1(out_const_39));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b01000)) const_4 (.out1(out_const_4));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b11001100101)) const_40 (.out1(out_const_40));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b11001100110)) const_41 (.out1(out_const_41));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1101)) const_42 (.out1(out_const_42));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11010)) const_43 (.out1(out_const_43));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11011)) const_44 (.out1(out_const_44));
  constant_value #(.BITSIZE_out1(3),
    .value(3'b111)) const_45 (.out1(out_const_45));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1110)) const_46 (.out1(out_const_46));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11100)) const_47 (.out1(out_const_47));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11101)) const_48 (.out1(out_const_48));
  constant_value #(.BITSIZE_out1(4),
    .value(4'b1111)) const_49 (.out1(out_const_49));
  constant_value #(.BITSIZE_out1(6),
    .value(6'b010000)) const_5 (.out1(out_const_5));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11110)) const_50 (.out1(out_const_50));
  constant_value #(.BITSIZE_out1(5),
    .value(5'b11111)) const_51 (.out1(out_const_51));
  constant_value #(.BITSIZE_out1(11),
    .value(11'b11111111111)) const_52 (.out1(out_const_52));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b111111111110)) const_53 (.out1(out_const_53));
  constant_value #(.BITSIZE_out1(16),
    .value(16'b1111111111111110)) const_54 (.out1(out_const_54));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_33736_33672)) const_55 (.out1(out_const_55));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_33767_33672)) const_56 (.out1(out_const_56));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_33810_33672)) const_57 (.out1(out_const_57));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_34309_33672)) const_58 (.out1(out_const_58));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_34379_33672)) const_59 (.out1(out_const_59));
  constant_value #(.BITSIZE_out1(7),
    .value(7'b0100000)) const_6 (.out1(out_const_6));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_34403_33672)) const_60 (.out1(out_const_60));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_34421_33672)) const_61 (.out1(out_const_61));
  constant_value #(.BITSIZE_out1(12),
    .value(MEM_var_34585_33672)) const_62 (.out1(out_const_62));
  constant_value #(.BITSIZE_out1(10),
    .value(10'b0101001011)) const_7 (.out1(out_const_7));
  constant_value #(.BITSIZE_out1(14),
    .value(14'b01011111110001)) const_8 (.out1(out_const_8));
  constant_value #(.BITSIZE_out1(12),
    .value(12'b011001001001)) const_9 (.out1(out_const_9));
  IIdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(11)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0_I_32_I_11 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0_I_32_I_11),
    .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(13)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0_32_13 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0_32_13),
    .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(13)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0_32_13 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0_32_13),
    .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0));
  IIdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(11)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0_I_32_I_11 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0_I_32_I_11),
    .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(13)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0_32_13 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0_32_13),
    .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0));
  UUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(13)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0_32_13 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0_32_13),
    .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0));
  IIdata_converter_FU #(.BITSIZE_in1(2),
    .BITSIZE_out1(32)) conv_out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085_I_2_I_32 (.out1(out_conv_out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085_I_2_I_32),
    .in1(out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085));
  IIdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(32)) conv_out_const_0_I_1_I_32 (.out1(out_conv_out_const_0_I_1_I_32),
    .in1(out_const_0));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_55_12_32 (.out1(out_conv_out_const_55_12_32),
    .in1(out_const_55));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_56_12_32 (.out1(out_conv_out_const_56_12_32),
    .in1(out_const_56));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_57_12_32 (.out1(out_conv_out_const_57_12_32),
    .in1(out_const_57));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_58_12_32 (.out1(out_conv_out_const_58_12_32),
    .in1(out_const_58));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_59_12_32 (.out1(out_conv_out_const_59_12_32),
    .in1(out_const_59));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_60_12_32 (.out1(out_conv_out_const_60_12_32),
    .in1(out_const_60));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_61_12_32 (.out1(out_conv_out_const_61_12_32),
    .in1(out_const_61));
  UUdata_converter_FU #(.BITSIZE_in1(12),
    .BITSIZE_out1(32)) conv_out_const_62_12_32 (.out1(out_conv_out_const_62_12_32),
    .in1(out_const_62));
  UUdata_converter_FU #(.BITSIZE_in1(7),
    .BITSIZE_out1(6)) conv_out_const_6_7_6 (.out1(out_conv_out_const_6_7_6),
    .in1(out_const_6));
  IUdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(32)) conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32 (.out1(out_conv_out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0_I_1_32),
    .in1(out_i_assign_conn_obj_0_ASSIGN_SIGNED_FU_i_assign_0));
  UUdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(32)) conv_out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4_1_32 (.out1(out_conv_out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4_1_32),
    .in1(out_u_assign_conn_obj_3_ASSIGN_UNSIGNED_FU_u_assign_4));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_33695 (.out1(out_IUdata_converter_FU_28_i0_fu_main_33672_33695),
    .in1(out_reg_15_reg_15));
  mult_expr_FU #(.BITSIZE_in1(13),
    .BITSIZE_in2(13),
    .BITSIZE_out1(26),
    .PIPE_PARAMETER(0)) fu_main_33672_33698 (.out1(out_mult_expr_FU_16_16_16_0_154_i0_fu_main_33672_33698),
    .clock(clock),
    .in1(out_reg_22_reg_22),
    .in2(out_reg_21_reg_21));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(26),
    .BITSIZE_out1(32)) fu_main_33672_33699 (.out1(out_plus_expr_FU_32_32_32_167_i0_fu_main_33672_33699),
    .in1(out_reg_14_reg_14),
    .in2(out_mult_expr_FU_16_16_16_0_154_i0_fu_main_33672_33698));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32)) fu_main_33672_33700 (.out1(out_plus_expr_FU_32_0_32_159_i0_fu_main_33672_33700),
    .in1(out_reg_15_reg_15),
    .in2(out_const_2));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_33701 (.out1(out_read_cond_FU_31_i0_fu_main_33672_33701),
    .in1(out_reg_23_reg_23));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_33745 (.out1(out_IUdata_converter_FU_79_i0_fu_main_33672_33745),
    .in1(out_reg_0_reg_0));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32)) fu_main_33672_33754 (.out1(out_plus_expr_FU_32_0_32_159_i1_fu_main_33672_33754),
    .in1(out_reg_0_reg_0),
    .in2(out_const_2));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(32)) fu_main_33672_33758 (.out1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in1(out_reg_14_reg_14),
    .in2(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_array_33767_0_I_32_I_11));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34347 (.out1(out_read_cond_FU_71_i0_fu_main_33672_34347),
    .in1(out_reg_27_reg_27));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34349 (.out1(out_read_cond_FU_74_i0_fu_main_33672_34349),
    .in1(out_reg_19_reg_19));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_34353 (.out1(out_IUdata_converter_FU_84_i0_fu_main_33672_34353),
    .in1(out_reg_30_reg_30));
  mult_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(13),
    .BITSIZE_out1(32),
    .PIPE_PARAMETER(0)) fu_main_33672_34356 (.out1(out_mult_expr_FU_32_32_32_0_155_i0_fu_main_33672_34356),
    .clock(clock),
    .in1(out_reg_38_reg_38),
    .in2(out_reg_36_reg_36));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) fu_main_33672_34357 (.out1(out_plus_expr_FU_32_32_32_167_i2_fu_main_33672_34357),
    .in1(out_reg_29_reg_29),
    .in2(out_reg_39_reg_39));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32)) fu_main_33672_34358 (.out1(out_plus_expr_FU_32_0_32_159_i2_fu_main_33672_34358),
    .in1(out_reg_30_reg_30),
    .in2(out_const_2));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34359 (.out1(out_read_cond_FU_85_i0_fu_main_33672_34359),
    .in1(out_reg_37_reg_37));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_34385 (.out1(out_IUdata_converter_FU_132_i0_fu_main_33672_34385),
    .in1(out_reg_28_reg_28));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32)) fu_main_33672_34392 (.out1(out_plus_expr_FU_32_0_32_159_i3_fu_main_33672_34392),
    .in1(out_reg_28_reg_28),
    .in2(out_const_2));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(32)) fu_main_33672_34395 (.out1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in1(out_reg_29_reg_29),
    .in2(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_array_34403_0_I_32_I_11));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(3),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34551 (.out1(out_ui_lshift_expr_FU_16_0_16_179_i0_fu_main_33672_34551),
    .in1(out_IUdata_converter_FU_132_i0_fu_main_33672_34385),
    .in2(out_const_36));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34563 (.out1(out_read_cond_FU_125_i0_fu_main_33672_34563),
    .in1(out_lut_expr_FU_124_i0_fu_main_33672_34895));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34565 (.out1(out_read_cond_FU_128_i0_fu_main_33672_34565),
    .in1(out_reg_34_reg_34));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(10)) fu_main_33672_34569 (.out1(out_IUdata_converter_FU_134_i0_fu_main_33672_34569),
    .in1(out_reg_61_reg_61));
  mult_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(13),
    .BITSIZE_out1(32),
    .PIPE_PARAMETER(0)) fu_main_33672_34572 (.out1(out_mult_expr_FU_32_32_32_0_155_i1_fu_main_33672_34572),
    .clock(clock),
    .in1(out_reg_64_reg_64),
    .in2(out_reg_62_reg_62));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(32),
    .BITSIZE_out1(32)) fu_main_33672_34573 (.out1(out_plus_expr_FU_32_32_32_167_i4_fu_main_33672_34573),
    .in1(out_reg_60_reg_60),
    .in2(out_reg_65_reg_65));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32)) fu_main_33672_34574 (.out1(out_plus_expr_FU_32_0_32_159_i4_fu_main_33672_34574),
    .in1(out_reg_61_reg_61),
    .in2(out_const_2));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34575 (.out1(out_read_cond_FU_135_i0_fu_main_33672_34575),
    .in1(out_reg_63_reg_63));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(6),
    .BITSIZE_out1(32)) fu_main_33672_34607 (.out1(out_plus_expr_FU_32_0_32_160_i0_fu_main_33672_34607),
    .in1(out_reg_60_reg_60),
    .in2(out_const_22));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_33672_34608 (.out1(out_read_cond_FU_142_i0_fu_main_33672_34608),
    .in1(out_lt_expr_FU_32_0_32_153_i0_fu_main_33672_34901));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_in3(2),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(1)) fu_main_33672_34617 (.out1(out_bit_ior_concat_expr_FU_148_i0_fu_main_33672_34617),
    .in1(out_lshift_expr_FU_32_0_32_151_i0_fu_main_33672_34981),
    .in2(out_bit_and_expr_FU_8_0_8_146_i0_fu_main_33672_34984),
    .in3(out_const_2));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_34618 (.out1(out_IUdata_converter_FU_136_i0_fu_main_33672_34618),
    .in1(out_bit_ior_concat_expr_FU_148_i0_fu_main_33672_34617));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(14),
    .BITSIZE_out1(32)) fu_main_33672_34630 (.out1(out_plus_expr_FU_32_0_32_161_i0_fu_main_33672_34630),
    .in1(out_reg_60_reg_60),
    .in2(out_const_14));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_34631 (.out1(out_IUdata_converter_FU_137_i0_fu_main_33672_34631),
    .in1(out_plus_expr_FU_32_0_32_161_i0_fu_main_33672_34630));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_in3(2),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(1)) fu_main_33672_34640 (.out1(out_bit_ior_concat_expr_FU_148_i1_fu_main_33672_34640),
    .in1(out_lshift_expr_FU_32_0_32_151_i1_fu_main_33672_34995),
    .in2(out_bit_and_expr_FU_8_0_8_146_i1_fu_main_33672_34998),
    .in3(out_const_2));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_34641 (.out1(out_IUdata_converter_FU_138_i0_fu_main_33672_34641),
    .in1(out_bit_ior_concat_expr_FU_148_i1_fu_main_33672_34640));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5),
    .BITSIZE_in3(4),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(4)) fu_main_33672_34650 (.out1(out_bit_ior_concat_expr_FU_149_i0_fu_main_33672_34650),
    .in1(out_lshift_expr_FU_32_0_32_152_i0_fu_main_33672_35011),
    .in2(out_bit_and_expr_FU_8_0_8_147_i0_fu_main_33672_35015),
    .in3(out_const_3));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_34651 (.out1(out_IUdata_converter_FU_139_i0_fu_main_33672_34651),
    .in1(out_bit_ior_concat_expr_FU_149_i0_fu_main_33672_34650));
  bit_ior_concat_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5),
    .BITSIZE_in3(4),
    .BITSIZE_out1(32),
    .OFFSET_PARAMETER(4)) fu_main_33672_34660 (.out1(out_bit_ior_concat_expr_FU_149_i1_fu_main_33672_34660),
    .in1(out_lshift_expr_FU_32_0_32_152_i1_fu_main_33672_35026),
    .in2(out_bit_and_expr_FU_8_0_8_147_i1_fu_main_33672_35029),
    .in3(out_const_3));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_34661 (.out1(out_IUdata_converter_FU_140_i0_fu_main_33672_34661),
    .in1(out_bit_ior_concat_expr_FU_149_i1_fu_main_33672_34660));
  plus_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(12),
    .BITSIZE_out1(32)) fu_main_33672_34670 (.out1(out_plus_expr_FU_32_0_32_162_i0_fu_main_33672_34670),
    .in1(out_reg_60_reg_60),
    .in2(out_const_9));
  IUdata_converter_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) fu_main_33672_34671 (.out1(out_IUdata_converter_FU_141_i0_fu_main_33672_34671),
    .in1(out_plus_expr_FU_32_0_32_162_i0_fu_main_33672_34670));
  ui_gt_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34672 (.out1(out_ui_gt_expr_FU_32_0_32_173_i0_fu_main_33672_34672),
    .in1(out_reg_71_reg_71),
    .in2(out_const_52));
  UIconvert_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(2)) fu_main_33672_34673 (.out1(out_UIconvert_expr_FU_144_i0_fu_main_33672_34673),
    .in1(out_ui_gt_expr_FU_32_0_32_173_i0_fu_main_33672_34672));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34716 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i0_fu_main_33672_34716),
    .in1(out_IUdata_converter_FU_28_i0_fu_main_33672_33695),
    .in2(out_const_16));
  ui_bit_ior_concat_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(3),
    .BITSIZE_in3(2),
    .BITSIZE_out1(12),
    .OFFSET_PARAMETER(3)) fu_main_33672_34719 (.out1(out_ui_bit_ior_concat_expr_FU_171_i0_fu_main_33672_34719),
    .in1(out_ui_lshift_expr_FU_16_0_16_181_i0_fu_main_33672_34937),
    .in2(out_ui_lshift_expr_FU_8_0_8_183_i0_fu_main_33672_35039),
    .in3(out_const_35));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34722 (.out1(out_addr_expr_FU_10_i0_fu_main_33672_34722),
    .in1(out_conv_out_const_57_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(2)) fu_main_33672_34725 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i0_fu_main_33672_34725),
    .in1(out_reg_2_reg_2),
    .in2(out_ui_bit_ior_concat_expr_FU_171_i0_fu_main_33672_34719));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34729 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i1_fu_main_33672_34729),
    .in1(out_IUdata_converter_FU_28_i0_fu_main_33672_33695),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34732 (.out1(out_addr_expr_FU_11_i0_fu_main_33672_34732),
    .in1(out_conv_out_const_58_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(2)) fu_main_33672_34735 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i1_fu_main_33672_34735),
    .in1(out_reg_3_reg_3),
    .in2(out_ui_lshift_expr_FU_16_0_16_180_i1_fu_main_33672_34729));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34743 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i2_fu_main_33672_34743),
    .in1(out_IUdata_converter_FU_79_i0_fu_main_33672_33745),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34746 (.out1(out_addr_expr_FU_12_i0_fu_main_33672_34746),
    .in1(out_conv_out_const_56_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(2)) fu_main_33672_34749 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i2_fu_main_33672_34749),
    .in1(out_reg_4_reg_4),
    .in2(out_ui_lshift_expr_FU_16_0_16_180_i2_fu_main_33672_34743));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34759 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i3_fu_main_33672_34759),
    .in1(out_IUdata_converter_FU_79_i0_fu_main_33672_33745),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34762 (.out1(out_addr_expr_FU_14_i0_fu_main_33672_34762),
    .in1(out_conv_out_const_55_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(0)) fu_main_33672_34765 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i3_fu_main_33672_34765),
    .in1(out_reg_5_reg_5),
    .in2(out_reg_17_reg_17));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34773 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i4_fu_main_33672_34773),
    .in1(out_IUdata_converter_FU_79_i0_fu_main_33672_33745),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34776 (.out1(out_addr_expr_FU_13_i0_fu_main_33672_34776),
    .in1(out_conv_out_const_55_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(0)) fu_main_33672_34779 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i4_fu_main_33672_34779),
    .in1(out_reg_6_reg_6),
    .in2(out_reg_18_reg_18));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34791 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i5_fu_main_33672_34791),
    .in1(out_IUdata_converter_FU_84_i0_fu_main_33672_34353),
    .in2(out_const_16));
  ui_bit_ior_concat_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(6),
    .BITSIZE_in3(3),
    .BITSIZE_out1(12),
    .OFFSET_PARAMETER(6)) fu_main_33672_34794 (.out1(out_ui_bit_ior_concat_expr_FU_172_i0_fu_main_33672_34794),
    .in1(out_ui_lshift_expr_FU_16_0_16_179_i1_fu_main_33672_34963),
    .in2(out_ui_lshift_expr_FU_8_0_8_183_i1_fu_main_33672_35046),
    .in3(out_const_36));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34797 (.out1(out_addr_expr_FU_15_i0_fu_main_33672_34797),
    .in1(out_conv_out_const_61_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(2)) fu_main_33672_34800 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i5_fu_main_33672_34800),
    .in1(out_reg_7_reg_7),
    .in2(out_ui_bit_ior_concat_expr_FU_172_i0_fu_main_33672_34794));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34804 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i6_fu_main_33672_34804),
    .in1(out_IUdata_converter_FU_84_i0_fu_main_33672_34353),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34807 (.out1(out_addr_expr_FU_16_i0_fu_main_33672_34807),
    .in1(out_conv_out_const_55_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(0)) fu_main_33672_34810 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i6_fu_main_33672_34810),
    .in1(out_reg_8_reg_8),
    .in2(out_ui_lshift_expr_FU_16_0_16_180_i6_fu_main_33672_34804));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34818 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i7_fu_main_33672_34818),
    .in1(out_IUdata_converter_FU_132_i0_fu_main_33672_34385),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34821 (.out1(out_addr_expr_FU_17_i0_fu_main_33672_34821),
    .in1(out_conv_out_const_60_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(2)) fu_main_33672_34824 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i7_fu_main_33672_34824),
    .in1(out_reg_9_reg_9),
    .in2(out_reg_31_reg_31));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34832 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i8_fu_main_33672_34832),
    .in1(out_IUdata_converter_FU_132_i0_fu_main_33672_34385),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34835 (.out1(out_addr_expr_FU_19_i0_fu_main_33672_34835),
    .in1(out_conv_out_const_59_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(0)) fu_main_33672_34838 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i8_fu_main_33672_34838),
    .in1(out_reg_10_reg_10),
    .in2(out_reg_32_reg_32));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34846 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i9_fu_main_33672_34846),
    .in1(out_IUdata_converter_FU_132_i0_fu_main_33672_34385),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34849 (.out1(out_addr_expr_FU_18_i0_fu_main_33672_34849),
    .in1(out_conv_out_const_59_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(0)) fu_main_33672_34852 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i9_fu_main_33672_34852),
    .in1(out_reg_11_reg_11),
    .in2(out_reg_33_reg_33));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34866 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i10_fu_main_33672_34866),
    .in1(out_IUdata_converter_FU_134_i0_fu_main_33672_34569),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34869 (.out1(out_addr_expr_FU_20_i0_fu_main_33672_34869),
    .in1(out_conv_out_const_62_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(2)) fu_main_33672_34872 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i10_fu_main_33672_34872),
    .in1(out_reg_12_reg_12),
    .in2(out_ui_lshift_expr_FU_16_0_16_180_i10_fu_main_33672_34866));
  ui_lshift_expr_FU #(.BITSIZE_in1(10),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34876 (.out1(out_ui_lshift_expr_FU_16_0_16_180_i11_fu_main_33672_34876),
    .in1(out_IUdata_converter_FU_134_i0_fu_main_33672_34569),
    .in2(out_const_16));
  addr_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(12)) fu_main_33672_34879 (.out1(out_addr_expr_FU_21_i0_fu_main_33672_34879),
    .in1(out_conv_out_const_59_12_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(12),
    .BITSIZE_out1(12),
    .LSB_PARAMETER(0)) fu_main_33672_34882 (.out1(out_ui_pointer_plus_expr_FU_16_16_16_186_i11_fu_main_33672_34882),
    .in1(out_reg_13_reg_13),
    .in2(out_ui_lshift_expr_FU_16_0_16_180_i11_fu_main_33672_34876));
  ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(6),
    .BITSIZE_out1(1)) fu_main_33672_34887 (.out1(out_ne_expr_FU_32_0_32_156_i0_fu_main_33672_34887),
    .in1(out_plus_expr_FU_32_0_32_159_i0_fu_main_33672_33700),
    .in2(out_const_13));
  lut_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu_main_33672_34889 (.out1(out_lut_expr_FU_70_i0_fu_main_33672_34889),
    .in1(out_const_15),
    .in2(out_extract_bit_expr_FU_69_i0_fu_main_33672_35241),
    .in3(out_lut_expr_FU_68_i0_fu_main_33672_35237),
    .in4(1'b0),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(6),
    .BITSIZE_out1(1)) fu_main_33672_34891 (.out1(out_ne_expr_FU_32_0_32_157_i0_fu_main_33672_34891),
    .in1(out_plus_expr_FU_32_0_32_159_i1_fu_main_33672_33754),
    .in2(out_const_5));
  ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(6),
    .BITSIZE_out1(1)) fu_main_33672_34893 (.out1(out_ne_expr_FU_32_0_32_157_i1_fu_main_33672_34893),
    .in1(out_plus_expr_FU_32_0_32_159_i2_fu_main_33672_34358),
    .in2(out_const_5));
  lut_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu_main_33672_34895 (.out1(out_lut_expr_FU_124_i0_fu_main_33672_34895),
    .in1(out_const_15),
    .in2(out_reg_59_reg_59),
    .in3(out_lut_expr_FU_122_i0_fu_main_33672_35353),
    .in4(1'b0),
    .in5(1'b0),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5),
    .BITSIZE_out1(1)) fu_main_33672_34897 (.out1(out_ne_expr_FU_32_0_32_158_i0_fu_main_33672_34897),
    .in1(out_plus_expr_FU_32_0_32_159_i3_fu_main_33672_34392),
    .in2(out_const_4));
  ne_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5),
    .BITSIZE_out1(1)) fu_main_33672_34899 (.out1(out_ne_expr_FU_32_0_32_158_i1_fu_main_33672_34899),
    .in1(out_plus_expr_FU_32_0_32_159_i4_fu_main_33672_34574),
    .in2(out_const_4));
  lt_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(15),
    .BITSIZE_out1(1)) fu_main_33672_34901 (.out1(out_lt_expr_FU_32_0_32_153_i0_fu_main_33672_34901),
    .in1(out_plus_expr_FU_32_0_32_160_i0_fu_main_33672_34607),
    .in2(out_const_30));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(12),
    .BITSIZE_out1(1)) fu_main_33672_34903 (.out1(out_ui_le_expr_FU_32_0_32_174_i0_fu_main_33672_34903),
    .in1(out_reg_66_reg_66),
    .in2(out_const_53));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34905 (.out1(out_ui_le_expr_FU_32_0_32_175_i0_fu_main_33672_34905),
    .in1(out_reg_67_reg_67),
    .in2(out_const_26));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34907 (.out1(out_ui_le_expr_FU_32_0_32_176_i0_fu_main_33672_34907),
    .in1(out_reg_68_reg_68),
    .in2(out_const_40));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34909 (.out1(out_ui_le_expr_FU_32_0_32_177_i0_fu_main_33672_34909),
    .in1(out_reg_69_reg_69),
    .in2(out_const_52));
  ui_le_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(11),
    .BITSIZE_out1(1)) fu_main_33672_34911 (.out1(out_ui_le_expr_FU_32_0_32_178_i0_fu_main_33672_34911),
    .in1(out_reg_70_reg_70),
    .in2(out_const_41));
  ui_rshift_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(2),
    .BITSIZE_out1(9),
    .PRECISION(32)) fu_main_33672_34927 (.out1(out_ui_rshift_expr_FU_16_0_16_187_i0_fu_main_33672_34927),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i0_fu_main_33672_34716),
    .in2(out_const_35));
  ui_rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(9),
    .PRECISION(32)) fu_main_33672_34932 (.out1(out_ui_rshift_expr_FU_32_0_32_190_i0_fu_main_33672_34932),
    .in1(out_reg_1_reg_1),
    .in2(out_const_35));
  ui_plus_expr_FU #(.BITSIZE_in1(9),
    .BITSIZE_in2(9),
    .BITSIZE_out1(9)) fu_main_33672_34934 (.out1(out_ui_plus_expr_FU_8_8_8_185_i0_fu_main_33672_34934),
    .in1(out_ui_rshift_expr_FU_16_0_16_187_i0_fu_main_33672_34927),
    .in2(out_reg_20_reg_20));
  ui_lshift_expr_FU #(.BITSIZE_in1(9),
    .BITSIZE_in2(2),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34937 (.out1(out_ui_lshift_expr_FU_16_0_16_181_i0_fu_main_33672_34937),
    .in1(out_ui_plus_expr_FU_8_8_8_185_i0_fu_main_33672_34934),
    .in2(out_const_35));
  UUdata_converter_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu_main_33672_34941 (.out1(out_UUdata_converter_FU_30_i0_fu_main_33672_34941),
    .in1(out_extract_bit_expr_FU_29_i0_fu_main_33672_35370));
  ui_rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(29),
    .PRECISION(32)) fu_main_33672_34946 (.out1(out_ui_rshift_expr_FU_32_0_32_190_i1_fu_main_33672_34946),
    .in1(out_reg_1_reg_1),
    .in2(out_const_35));
  ui_plus_expr_FU #(.BITSIZE_in1(29),
    .BITSIZE_in2(4),
    .BITSIZE_out1(29)) fu_main_33672_34948 (.out1(out_ui_plus_expr_FU_32_0_32_184_i0_fu_main_33672_34948),
    .in1(out_ui_rshift_expr_FU_32_0_32_190_i1_fu_main_33672_34946),
    .in2(out_const_49));
  ui_lshift_expr_FU #(.BITSIZE_in1(29),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34951 (.out1(out_ui_lshift_expr_FU_32_0_32_182_i0_fu_main_33672_34951),
    .in1(out_ui_plus_expr_FU_32_0_32_184_i0_fu_main_33672_34948),
    .in2(out_const_35));
  ui_rshift_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(3),
    .BITSIZE_out1(6),
    .PRECISION(32)) fu_main_33672_34954 (.out1(out_ui_rshift_expr_FU_16_0_16_188_i0_fu_main_33672_34954),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i5_fu_main_33672_34791),
    .in2(out_const_36));
  ui_rshift_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(3),
    .BITSIZE_out1(6),
    .PRECISION(32)) fu_main_33672_34958 (.out1(out_ui_rshift_expr_FU_16_0_16_188_i1_fu_main_33672_34958),
    .in1(out_ui_lshift_expr_FU_16_0_16_179_i0_fu_main_33672_34551),
    .in2(out_const_36));
  ui_plus_expr_FU #(.BITSIZE_in1(6),
    .BITSIZE_in2(6),
    .BITSIZE_out1(6)) fu_main_33672_34960 (.out1(out_ui_plus_expr_FU_8_8_8_185_i1_fu_main_33672_34960),
    .in1(out_ui_rshift_expr_FU_16_0_16_188_i0_fu_main_33672_34954),
    .in2(out_reg_35_reg_35));
  ui_lshift_expr_FU #(.BITSIZE_in1(6),
    .BITSIZE_in2(3),
    .BITSIZE_out1(12),
    .PRECISION(32)) fu_main_33672_34963 (.out1(out_ui_lshift_expr_FU_16_0_16_179_i1_fu_main_33672_34963),
    .in1(out_ui_plus_expr_FU_8_8_8_185_i1_fu_main_33672_34960),
    .in2(out_const_36));
  ui_bit_and_expr_FU #(.BITSIZE_in1(4),
    .BITSIZE_in2(4),
    .BITSIZE_out1(4)) fu_main_33672_34967 (.out1(out_ui_bit_and_expr_FU_8_0_8_170_i0_fu_main_33672_34967),
    .in1(out_ui_rshift_expr_FU_16_0_16_189_i0_fu_main_33672_35042),
    .in2(out_const_49));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(31),
    .PRECISION(32)) fu_main_33672_34973 (.out1(out_rshift_expr_FU_32_0_32_168_i0_fu_main_33672_34973),
    .in1(out_reg_60_reg_60),
    .in2(out_const_2));
  plus_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(14),
    .BITSIZE_out1(32)) fu_main_33672_34978 (.out1(out_plus_expr_FU_32_0_32_163_i0_fu_main_33672_34978),
    .in1(out_rshift_expr_FU_32_0_32_168_i0_fu_main_33672_34973),
    .in2(out_const_8));
  lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34981 (.out1(out_lshift_expr_FU_32_0_32_151_i0_fu_main_33672_34981),
    .in1(out_plus_expr_FU_32_0_32_163_i0_fu_main_33672_34978),
    .in2(out_const_2));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(2)) fu_main_33672_34984 (.out1(out_bit_and_expr_FU_8_0_8_146_i0_fu_main_33672_34984),
    .in1(out_reg_60_reg_60),
    .in2(out_const_2));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(31),
    .PRECISION(32)) fu_main_33672_34989 (.out1(out_rshift_expr_FU_32_0_32_168_i1_fu_main_33672_34989),
    .in1(out_reg_60_reg_60),
    .in2(out_const_2));
  plus_expr_FU #(.BITSIZE_in1(31),
    .BITSIZE_in2(13),
    .BITSIZE_out1(32)) fu_main_33672_34992 (.out1(out_plus_expr_FU_32_0_32_164_i0_fu_main_33672_34992),
    .in1(out_rshift_expr_FU_32_0_32_168_i1_fu_main_33672_34989),
    .in2(out_const_11));
  lshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_34995 (.out1(out_lshift_expr_FU_32_0_32_151_i1_fu_main_33672_34995),
    .in1(out_plus_expr_FU_32_0_32_164_i0_fu_main_33672_34992),
    .in2(out_const_2));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2),
    .BITSIZE_out1(2)) fu_main_33672_34998 (.out1(out_bit_and_expr_FU_8_0_8_146_i1_fu_main_33672_34998),
    .in1(out_reg_60_reg_60),
    .in2(out_const_2));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4),
    .BITSIZE_out1(28),
    .PRECISION(32)) fu_main_33672_35003 (.out1(out_rshift_expr_FU_32_0_32_169_i0_fu_main_33672_35003),
    .in1(out_reg_60_reg_60),
    .in2(out_const_3));
  plus_expr_FU #(.BITSIZE_in1(28),
    .BITSIZE_in2(10),
    .BITSIZE_out1(29)) fu_main_33672_35008 (.out1(out_plus_expr_FU_32_0_32_165_i0_fu_main_33672_35008),
    .in1(out_rshift_expr_FU_32_0_32_169_i0_fu_main_33672_35003),
    .in2(out_const_7));
  lshift_expr_FU #(.BITSIZE_in1(29),
    .BITSIZE_in2(4),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_35011 (.out1(out_lshift_expr_FU_32_0_32_152_i0_fu_main_33672_35011),
    .in1(out_plus_expr_FU_32_0_32_165_i0_fu_main_33672_35008),
    .in2(out_const_3));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5),
    .BITSIZE_out1(5)) fu_main_33672_35015 (.out1(out_bit_and_expr_FU_8_0_8_147_i0_fu_main_33672_35015),
    .in1(out_reg_60_reg_60),
    .in2(out_const_12));
  rshift_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4),
    .BITSIZE_out1(28),
    .PRECISION(32)) fu_main_33672_35020 (.out1(out_rshift_expr_FU_32_0_32_169_i1_fu_main_33672_35020),
    .in1(out_reg_60_reg_60),
    .in2(out_const_3));
  plus_expr_FU #(.BITSIZE_in1(28),
    .BITSIZE_in2(9),
    .BITSIZE_out1(29)) fu_main_33672_35023 (.out1(out_plus_expr_FU_32_0_32_166_i0_fu_main_33672_35023),
    .in1(out_rshift_expr_FU_32_0_32_169_i1_fu_main_33672_35020),
    .in2(out_const_10));
  lshift_expr_FU #(.BITSIZE_in1(29),
    .BITSIZE_in2(4),
    .BITSIZE_out1(32),
    .PRECISION(32)) fu_main_33672_35026 (.out1(out_lshift_expr_FU_32_0_32_152_i1_fu_main_33672_35026),
    .in1(out_plus_expr_FU_32_0_32_166_i0_fu_main_33672_35023),
    .in2(out_const_3));
  bit_and_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5),
    .BITSIZE_out1(5)) fu_main_33672_35029 (.out1(out_bit_and_expr_FU_8_0_8_147_i1_fu_main_33672_35029),
    .in1(out_reg_60_reg_60),
    .in2(out_const_12));
  ui_lshift_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(2),
    .BITSIZE_out1(3),
    .PRECISION(32)) fu_main_33672_35039 (.out1(out_ui_lshift_expr_FU_8_0_8_183_i0_fu_main_33672_35039),
    .in1(out_UUdata_converter_FU_30_i0_fu_main_33672_34941),
    .in2(out_const_16));
  ui_rshift_expr_FU #(.BITSIZE_in1(12),
    .BITSIZE_in2(2),
    .BITSIZE_out1(4),
    .PRECISION(32)) fu_main_33672_35042 (.out1(out_ui_rshift_expr_FU_16_0_16_189_i0_fu_main_33672_35042),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i5_fu_main_33672_34791),
    .in2(out_const_16));
  ui_lshift_expr_FU #(.BITSIZE_in1(4),
    .BITSIZE_in2(2),
    .BITSIZE_out1(6),
    .PRECISION(32)) fu_main_33672_35046 (.out1(out_ui_lshift_expr_FU_8_0_8_183_i1_fu_main_33672_35046),
    .in1(out_ui_bit_and_expr_FU_8_0_8_170_i0_fu_main_33672_34967),
    .in2(out_const_16));
  lut_expr_FU #(.BITSIZE_in1(16),
    .BITSIZE_out1(1)) fu_main_33672_35072 (.out1(out_lut_expr_FU_145_i0_fu_main_33672_35072),
    .in1(out_const_54),
    .in2(out_ui_le_expr_FU_32_0_32_175_i0_fu_main_33672_34905),
    .in3(out_ui_le_expr_FU_32_0_32_176_i0_fu_main_33672_34907),
    .in4(out_ui_le_expr_FU_32_0_32_177_i0_fu_main_33672_34909),
    .in5(out_ui_le_expr_FU_32_0_32_178_i0_fu_main_33672_34911),
    .in6(1'b0),
    .in7(1'b0),
    .in8(1'b0),
    .in9(1'b0));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(1),
    .BITSIZE_in3(2),
    .BITSIZE_out1(2)) fu_main_33672_35082 (.out1(out_cond_expr_FU_8_8_8_8_150_i0_fu_main_33672_35082),
    .in1(out_lut_expr_FU_145_i0_fu_main_33672_35072),
    .in2(out_const_0),
    .in3(out_UIconvert_expr_FU_144_i0_fu_main_33672_34673));
  cond_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_in2(1),
    .BITSIZE_in3(2),
    .BITSIZE_out1(2)) fu_main_33672_35085 (.out1(out_cond_expr_FU_8_8_8_8_150_i1_fu_main_33672_35085),
    .in1(out_ui_le_expr_FU_32_0_32_174_i0_fu_main_33672_34903),
    .in2(out_const_0),
    .in3(out_cond_expr_FU_8_8_8_8_150_i0_fu_main_33672_35082));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1)) fu_main_33672_35100 (.out1(out_extract_bit_expr_FU_32_i0_fu_main_33672_35100),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1)) fu_main_33672_35103 (.out1(out_extract_bit_expr_FU_33_i0_fu_main_33672_35103),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_15));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2)) fu_main_33672_35106 (.out1(out_extract_bit_expr_FU_34_i0_fu_main_33672_35106),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_16));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2)) fu_main_33672_35110 (.out1(out_extract_bit_expr_FU_35_i0_fu_main_33672_35110),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_35));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35114 (.out1(out_extract_bit_expr_FU_36_i0_fu_main_33672_35114),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_17));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35118 (.out1(out_extract_bit_expr_FU_37_i0_fu_main_33672_35118),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_27));
  lut_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu_main_33672_35121 (.out1(out_lut_expr_FU_38_i0_fu_main_33672_35121),
    .in1(out_const_15),
    .in2(out_extract_bit_expr_FU_32_i0_fu_main_33672_35100),
    .in3(out_extract_bit_expr_FU_33_i0_fu_main_33672_35103),
    .in4(out_extract_bit_expr_FU_34_i0_fu_main_33672_35106),
    .in5(out_extract_bit_expr_FU_35_i0_fu_main_33672_35110),
    .in6(out_extract_bit_expr_FU_36_i0_fu_main_33672_35114),
    .in7(out_extract_bit_expr_FU_37_i0_fu_main_33672_35118),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35126 (.out1(out_extract_bit_expr_FU_39_i0_fu_main_33672_35126),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_36));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35130 (.out1(out_extract_bit_expr_FU_40_i0_fu_main_33672_35130),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_45));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35134 (.out1(out_extract_bit_expr_FU_41_i0_fu_main_33672_35134),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_18));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35138 (.out1(out_extract_bit_expr_FU_42_i0_fu_main_33672_35138),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_23));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35142 (.out1(out_extract_bit_expr_FU_43_i0_fu_main_33672_35142),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_28));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35145 (.out1(out_lut_expr_FU_44_i0_fu_main_33672_35145),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_39_i0_fu_main_33672_35126),
    .in3(out_extract_bit_expr_FU_40_i0_fu_main_33672_35130),
    .in4(out_extract_bit_expr_FU_41_i0_fu_main_33672_35134),
    .in5(out_extract_bit_expr_FU_42_i0_fu_main_33672_35138),
    .in6(out_extract_bit_expr_FU_43_i0_fu_main_33672_35142),
    .in7(out_lut_expr_FU_38_i0_fu_main_33672_35121),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35149 (.out1(out_extract_bit_expr_FU_45_i0_fu_main_33672_35149),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_32));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35153 (.out1(out_extract_bit_expr_FU_46_i0_fu_main_33672_35153),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_37));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35157 (.out1(out_extract_bit_expr_FU_47_i0_fu_main_33672_35157),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_42));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35161 (.out1(out_extract_bit_expr_FU_48_i0_fu_main_33672_35161),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_46));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35165 (.out1(out_extract_bit_expr_FU_49_i0_fu_main_33672_35165),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_49));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35168 (.out1(out_lut_expr_FU_50_i0_fu_main_33672_35168),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_45_i0_fu_main_33672_35149),
    .in3(out_extract_bit_expr_FU_46_i0_fu_main_33672_35153),
    .in4(out_extract_bit_expr_FU_47_i0_fu_main_33672_35157),
    .in5(out_extract_bit_expr_FU_48_i0_fu_main_33672_35161),
    .in6(out_extract_bit_expr_FU_49_i0_fu_main_33672_35165),
    .in7(out_lut_expr_FU_44_i0_fu_main_33672_35145),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35172 (.out1(out_extract_bit_expr_FU_51_i0_fu_main_33672_35172),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_19));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35176 (.out1(out_extract_bit_expr_FU_52_i0_fu_main_33672_35176),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_21));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35180 (.out1(out_extract_bit_expr_FU_53_i0_fu_main_33672_35180),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_24));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35184 (.out1(out_extract_bit_expr_FU_54_i0_fu_main_33672_35184),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_25));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35188 (.out1(out_extract_bit_expr_FU_55_i0_fu_main_33672_35188),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_29));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35191 (.out1(out_lut_expr_FU_56_i0_fu_main_33672_35191),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_51_i0_fu_main_33672_35172),
    .in3(out_extract_bit_expr_FU_52_i0_fu_main_33672_35176),
    .in4(out_extract_bit_expr_FU_53_i0_fu_main_33672_35180),
    .in5(out_extract_bit_expr_FU_54_i0_fu_main_33672_35184),
    .in6(out_extract_bit_expr_FU_55_i0_fu_main_33672_35188),
    .in7(out_lut_expr_FU_50_i0_fu_main_33672_35168),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35195 (.out1(out_extract_bit_expr_FU_57_i0_fu_main_33672_35195),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_31));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35199 (.out1(out_extract_bit_expr_FU_58_i0_fu_main_33672_35199),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_33));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35203 (.out1(out_extract_bit_expr_FU_59_i0_fu_main_33672_35203),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_34));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35207 (.out1(out_extract_bit_expr_FU_60_i0_fu_main_33672_35207),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_38));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35211 (.out1(out_extract_bit_expr_FU_61_i0_fu_main_33672_35211),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_39));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35214 (.out1(out_lut_expr_FU_62_i0_fu_main_33672_35214),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_57_i0_fu_main_33672_35195),
    .in3(out_extract_bit_expr_FU_58_i0_fu_main_33672_35199),
    .in4(out_extract_bit_expr_FU_59_i0_fu_main_33672_35203),
    .in5(out_extract_bit_expr_FU_60_i0_fu_main_33672_35207),
    .in6(out_extract_bit_expr_FU_61_i0_fu_main_33672_35211),
    .in7(out_lut_expr_FU_56_i0_fu_main_33672_35191),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35218 (.out1(out_extract_bit_expr_FU_63_i0_fu_main_33672_35218),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_43));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35222 (.out1(out_extract_bit_expr_FU_64_i0_fu_main_33672_35222),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_44));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35226 (.out1(out_extract_bit_expr_FU_65_i0_fu_main_33672_35226),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_47));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35230 (.out1(out_extract_bit_expr_FU_66_i0_fu_main_33672_35230),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_48));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35234 (.out1(out_extract_bit_expr_FU_67_i0_fu_main_33672_35234),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_50));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35237 (.out1(out_lut_expr_FU_68_i0_fu_main_33672_35237),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_63_i0_fu_main_33672_35218),
    .in3(out_extract_bit_expr_FU_64_i0_fu_main_33672_35222),
    .in4(out_extract_bit_expr_FU_65_i0_fu_main_33672_35226),
    .in5(out_extract_bit_expr_FU_66_i0_fu_main_33672_35230),
    .in6(out_extract_bit_expr_FU_67_i0_fu_main_33672_35234),
    .in7(out_lut_expr_FU_62_i0_fu_main_33672_35214),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35241 (.out1(out_extract_bit_expr_FU_69_i0_fu_main_33672_35241),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .in2(out_const_51));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1)) fu_main_33672_35245 (.out1(out_extract_bit_expr_FU_86_i0_fu_main_33672_35245),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1)) fu_main_33672_35248 (.out1(out_extract_bit_expr_FU_87_i0_fu_main_33672_35248),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_15));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2)) fu_main_33672_35251 (.out1(out_extract_bit_expr_FU_88_i0_fu_main_33672_35251),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_16));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(2)) fu_main_33672_35254 (.out1(out_extract_bit_expr_FU_89_i0_fu_main_33672_35254),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_35));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35257 (.out1(out_extract_bit_expr_FU_90_i0_fu_main_33672_35257),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_17));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35260 (.out1(out_extract_bit_expr_FU_91_i0_fu_main_33672_35260),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_27));
  lut_expr_FU #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) fu_main_33672_35263 (.out1(out_lut_expr_FU_92_i0_fu_main_33672_35263),
    .in1(out_const_15),
    .in2(out_extract_bit_expr_FU_86_i0_fu_main_33672_35245),
    .in3(out_extract_bit_expr_FU_87_i0_fu_main_33672_35248),
    .in4(out_extract_bit_expr_FU_88_i0_fu_main_33672_35251),
    .in5(out_extract_bit_expr_FU_89_i0_fu_main_33672_35254),
    .in6(out_extract_bit_expr_FU_90_i0_fu_main_33672_35257),
    .in7(out_extract_bit_expr_FU_91_i0_fu_main_33672_35260),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35266 (.out1(out_extract_bit_expr_FU_93_i0_fu_main_33672_35266),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_36));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(3)) fu_main_33672_35269 (.out1(out_extract_bit_expr_FU_94_i0_fu_main_33672_35269),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_45));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35272 (.out1(out_extract_bit_expr_FU_95_i0_fu_main_33672_35272),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_18));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35275 (.out1(out_extract_bit_expr_FU_96_i0_fu_main_33672_35275),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_23));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35278 (.out1(out_extract_bit_expr_FU_97_i0_fu_main_33672_35278),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_28));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35281 (.out1(out_lut_expr_FU_98_i0_fu_main_33672_35281),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_93_i0_fu_main_33672_35266),
    .in3(out_extract_bit_expr_FU_94_i0_fu_main_33672_35269),
    .in4(out_extract_bit_expr_FU_95_i0_fu_main_33672_35272),
    .in5(out_extract_bit_expr_FU_96_i0_fu_main_33672_35275),
    .in6(out_extract_bit_expr_FU_97_i0_fu_main_33672_35278),
    .in7(out_lut_expr_FU_92_i0_fu_main_33672_35263),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35284 (.out1(out_extract_bit_expr_FU_99_i0_fu_main_33672_35284),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_32));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35287 (.out1(out_extract_bit_expr_FU_100_i0_fu_main_33672_35287),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_37));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35290 (.out1(out_extract_bit_expr_FU_101_i0_fu_main_33672_35290),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_42));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35293 (.out1(out_extract_bit_expr_FU_102_i0_fu_main_33672_35293),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_46));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(4)) fu_main_33672_35296 (.out1(out_extract_bit_expr_FU_103_i0_fu_main_33672_35296),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_49));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35299 (.out1(out_lut_expr_FU_104_i0_fu_main_33672_35299),
    .in1(out_const_20),
    .in2(out_extract_bit_expr_FU_99_i0_fu_main_33672_35284),
    .in3(out_extract_bit_expr_FU_100_i0_fu_main_33672_35287),
    .in4(out_extract_bit_expr_FU_101_i0_fu_main_33672_35290),
    .in5(out_extract_bit_expr_FU_102_i0_fu_main_33672_35293),
    .in6(out_extract_bit_expr_FU_103_i0_fu_main_33672_35296),
    .in7(out_lut_expr_FU_98_i0_fu_main_33672_35281),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35302 (.out1(out_extract_bit_expr_FU_105_i0_fu_main_33672_35302),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_19));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35305 (.out1(out_extract_bit_expr_FU_106_i0_fu_main_33672_35305),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_21));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35308 (.out1(out_extract_bit_expr_FU_107_i0_fu_main_33672_35308),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_24));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35311 (.out1(out_extract_bit_expr_FU_108_i0_fu_main_33672_35311),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_25));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35314 (.out1(out_extract_bit_expr_FU_109_i0_fu_main_33672_35314),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_29));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35317 (.out1(out_lut_expr_FU_110_i0_fu_main_33672_35317),
    .in1(out_const_20),
    .in2(out_reg_44_reg_44),
    .in3(out_reg_45_reg_45),
    .in4(out_reg_46_reg_46),
    .in5(out_reg_47_reg_47),
    .in6(out_reg_48_reg_48),
    .in7(out_reg_43_reg_43),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35320 (.out1(out_extract_bit_expr_FU_111_i0_fu_main_33672_35320),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_31));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35323 (.out1(out_extract_bit_expr_FU_112_i0_fu_main_33672_35323),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_33));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35326 (.out1(out_extract_bit_expr_FU_113_i0_fu_main_33672_35326),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_34));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35329 (.out1(out_extract_bit_expr_FU_114_i0_fu_main_33672_35329),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_38));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35332 (.out1(out_extract_bit_expr_FU_115_i0_fu_main_33672_35332),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_39));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35335 (.out1(out_lut_expr_FU_116_i0_fu_main_33672_35335),
    .in1(out_const_20),
    .in2(out_reg_49_reg_49),
    .in3(out_reg_50_reg_50),
    .in4(out_reg_51_reg_51),
    .in5(out_reg_52_reg_52),
    .in6(out_reg_53_reg_53),
    .in7(out_lut_expr_FU_110_i0_fu_main_33672_35317),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35338 (.out1(out_extract_bit_expr_FU_117_i0_fu_main_33672_35338),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_43));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35341 (.out1(out_extract_bit_expr_FU_118_i0_fu_main_33672_35341),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_44));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35344 (.out1(out_extract_bit_expr_FU_119_i0_fu_main_33672_35344),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_47));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35347 (.out1(out_extract_bit_expr_FU_120_i0_fu_main_33672_35347),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_48));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35350 (.out1(out_extract_bit_expr_FU_121_i0_fu_main_33672_35350),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_50));
  lut_expr_FU #(.BITSIZE_in1(33),
    .BITSIZE_out1(1)) fu_main_33672_35353 (.out1(out_lut_expr_FU_122_i0_fu_main_33672_35353),
    .in1(out_const_20),
    .in2(out_reg_54_reg_54),
    .in3(out_reg_55_reg_55),
    .in4(out_reg_56_reg_56),
    .in5(out_reg_57_reg_57),
    .in6(out_reg_58_reg_58),
    .in7(out_lut_expr_FU_116_i0_fu_main_33672_35335),
    .in8(1'b0),
    .in9(1'b0));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(5)) fu_main_33672_35356 (.out1(out_extract_bit_expr_FU_123_i0_fu_main_33672_35356),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .in2(out_const_51));
  extract_bit_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_in2(1)) fu_main_33672_35370 (.out1(out_extract_bit_expr_FU_29_i0_fu_main_33672_35370),
    .in1(out_reg_15_reg_15),
    .in2(out_const_0));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_0 (.out1(out_reg_0_reg_0),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_383_reg_0_0_0_0),
    .wenable(wrenable_reg_0));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_1 (.out1(out_reg_1_reg_1),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_384_reg_1_0_0_0),
    .wenable(wrenable_reg_1));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_10 (.out1(out_reg_10_reg_10),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_19_i0_fu_main_33672_34835),
    .wenable(wrenable_reg_10));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_11 (.out1(out_reg_11_reg_11),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_18_i0_fu_main_33672_34849),
    .wenable(wrenable_reg_11));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_12 (.out1(out_reg_12_reg_12),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_20_i0_fu_main_33672_34869),
    .wenable(wrenable_reg_12));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_13 (.out1(out_reg_13_reg_13),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_21_i0_fu_main_33672_34879),
    .wenable(wrenable_reg_13));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_14 (.out1(out_reg_14_reg_14),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_389_reg_14_0_0_0),
    .wenable(wrenable_reg_14));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_15 (.out1(out_reg_15_reg_15),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_390_reg_15_0_0_0),
    .wenable(wrenable_reg_15));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_16 (.out1(out_reg_16_reg_16),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i2_fu_main_33672_34749),
    .wenable(wrenable_reg_16));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_17 (.out1(out_reg_17_reg_17),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i3_fu_main_33672_34759),
    .wenable(wrenable_reg_17));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_18 (.out1(out_reg_18_reg_18),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i4_fu_main_33672_34773),
    .wenable(wrenable_reg_18));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_19 (.out1(out_reg_19_reg_19),
    .clock(clock),
    .reset(reset),
    .in1(out_ne_expr_FU_32_0_32_157_i0_fu_main_33672_34891),
    .wenable(wrenable_reg_19));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_2 (.out1(out_reg_2_reg_2),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_10_i0_fu_main_33672_34722),
    .wenable(wrenable_reg_2));
  register_SE #(.BITSIZE_in1(9),
    .BITSIZE_out1(9)) reg_20 (.out1(out_reg_20_reg_20),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_rshift_expr_FU_32_0_32_190_i0_fu_main_33672_34932),
    .wenable(wrenable_reg_20));
  register_STD #(.BITSIZE_in1(13),
    .BITSIZE_out1(13)) reg_21 (.out1(out_reg_21_reg_21),
    .clock(clock),
    .reset(reset),
    .in1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_array_33810_0_32_13),
    .wenable(wrenable_reg_21));
  register_STD #(.BITSIZE_in1(13),
    .BITSIZE_out1(13)) reg_22 (.out1(out_reg_22_reg_22),
    .clock(clock),
    .reset(reset),
    .in1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_34309_0_32_13),
    .wenable(wrenable_reg_22));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_23 (.out1(out_reg_23_reg_23),
    .clock(clock),
    .reset(reset),
    .in1(out_ne_expr_FU_32_0_32_156_i0_fu_main_33672_34887),
    .wenable(wrenable_reg_23));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_24 (.out1(out_reg_24_reg_24),
    .clock(clock),
    .reset(reset),
    .in1(out_plus_expr_FU_32_32_32_167_i1_fu_main_33672_33758),
    .wenable(wrenable_reg_24));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_25 (.out1(out_reg_25_reg_25),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i3_fu_main_33672_34765),
    .wenable(wrenable_reg_25));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_26 (.out1(out_reg_26_reg_26),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i4_fu_main_33672_34779),
    .wenable(wrenable_reg_26));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_27 (.out1(out_reg_27_reg_27),
    .clock(clock),
    .reset(reset),
    .in1(out_lut_expr_FU_70_i0_fu_main_33672_34889),
    .wenable(wrenable_reg_27));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_28 (.out1(out_reg_28_reg_28),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_404_reg_28_0_0_0),
    .wenable(wrenable_reg_28));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_29 (.out1(out_reg_29_reg_29),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_405_reg_29_0_0_0),
    .wenable(wrenable_reg_29));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_3 (.out1(out_reg_3_reg_3),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_11_i0_fu_main_33672_34732),
    .wenable(wrenable_reg_3));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_30 (.out1(out_reg_30_reg_30),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_407_reg_30_0_0_0),
    .wenable(wrenable_reg_30));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_31 (.out1(out_reg_31_reg_31),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i7_fu_main_33672_34818),
    .wenable(wrenable_reg_31));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_32 (.out1(out_reg_32_reg_32),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i8_fu_main_33672_34832),
    .wenable(wrenable_reg_32));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_33 (.out1(out_reg_33_reg_33),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_lshift_expr_FU_16_0_16_180_i9_fu_main_33672_34846),
    .wenable(wrenable_reg_33));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_34 (.out1(out_reg_34_reg_34),
    .clock(clock),
    .reset(reset),
    .in1(out_ne_expr_FU_32_0_32_158_i0_fu_main_33672_34897),
    .wenable(wrenable_reg_34));
  register_SE #(.BITSIZE_in1(6),
    .BITSIZE_out1(6)) reg_35 (.out1(out_reg_35_reg_35),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_rshift_expr_FU_16_0_16_188_i1_fu_main_33672_34958),
    .wenable(wrenable_reg_35));
  register_SE #(.BITSIZE_in1(13),
    .BITSIZE_out1(13)) reg_36 (.out1(out_reg_36_reg_36),
    .clock(clock),
    .reset(reset),
    .in1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_array_34421_0_32_13),
    .wenable(wrenable_reg_36));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_37 (.out1(out_reg_37_reg_37),
    .clock(clock),
    .reset(reset),
    .in1(out_ne_expr_FU_32_0_32_157_i1_fu_main_33672_34893),
    .wenable(wrenable_reg_37));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_38 (.out1(out_reg_38_reg_38),
    .clock(clock),
    .reset(reset),
    .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_33736_0),
    .wenable(wrenable_reg_38));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_39 (.out1(out_reg_39_reg_39),
    .clock(clock),
    .reset(reset),
    .in1(out_mult_expr_FU_32_32_32_0_155_i0_fu_main_33672_34356),
    .wenable(wrenable_reg_39));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_4 (.out1(out_reg_4_reg_4),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_12_i0_fu_main_33672_34746),
    .wenable(wrenable_reg_4));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_40 (.out1(out_reg_40_reg_40),
    .clock(clock),
    .reset(reset),
    .in1(out_plus_expr_FU_32_32_32_167_i3_fu_main_33672_34395),
    .wenable(wrenable_reg_40));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_41 (.out1(out_reg_41_reg_41),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i8_fu_main_33672_34838),
    .wenable(wrenable_reg_41));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_42 (.out1(out_reg_42_reg_42),
    .clock(clock),
    .reset(reset),
    .in1(out_ui_pointer_plus_expr_FU_16_16_16_186_i9_fu_main_33672_34852),
    .wenable(wrenable_reg_42));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_43 (.out1(out_reg_43_reg_43),
    .clock(clock),
    .reset(reset),
    .in1(out_lut_expr_FU_104_i0_fu_main_33672_35299),
    .wenable(wrenable_reg_43));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_44 (.out1(out_reg_44_reg_44),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_105_i0_fu_main_33672_35302),
    .wenable(wrenable_reg_44));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_45 (.out1(out_reg_45_reg_45),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_106_i0_fu_main_33672_35305),
    .wenable(wrenable_reg_45));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_46 (.out1(out_reg_46_reg_46),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_107_i0_fu_main_33672_35308),
    .wenable(wrenable_reg_46));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_47 (.out1(out_reg_47_reg_47),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_108_i0_fu_main_33672_35311),
    .wenable(wrenable_reg_47));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_48 (.out1(out_reg_48_reg_48),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_109_i0_fu_main_33672_35314),
    .wenable(wrenable_reg_48));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_49 (.out1(out_reg_49_reg_49),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_111_i0_fu_main_33672_35320),
    .wenable(wrenable_reg_49));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_5 (.out1(out_reg_5_reg_5),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_14_i0_fu_main_33672_34762),
    .wenable(wrenable_reg_5));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_50 (.out1(out_reg_50_reg_50),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_112_i0_fu_main_33672_35323),
    .wenable(wrenable_reg_50));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_51 (.out1(out_reg_51_reg_51),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_113_i0_fu_main_33672_35326),
    .wenable(wrenable_reg_51));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_52 (.out1(out_reg_52_reg_52),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_114_i0_fu_main_33672_35329),
    .wenable(wrenable_reg_52));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_53 (.out1(out_reg_53_reg_53),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_115_i0_fu_main_33672_35332),
    .wenable(wrenable_reg_53));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_54 (.out1(out_reg_54_reg_54),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_117_i0_fu_main_33672_35338),
    .wenable(wrenable_reg_54));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_55 (.out1(out_reg_55_reg_55),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_118_i0_fu_main_33672_35341),
    .wenable(wrenable_reg_55));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_56 (.out1(out_reg_56_reg_56),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_119_i0_fu_main_33672_35344),
    .wenable(wrenable_reg_56));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_57 (.out1(out_reg_57_reg_57),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_120_i0_fu_main_33672_35347),
    .wenable(wrenable_reg_57));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_58 (.out1(out_reg_58_reg_58),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_121_i0_fu_main_33672_35350),
    .wenable(wrenable_reg_58));
  register_STD #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_59 (.out1(out_reg_59_reg_59),
    .clock(clock),
    .reset(reset),
    .in1(out_extract_bit_expr_FU_123_i0_fu_main_33672_35356),
    .wenable(wrenable_reg_59));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_6 (.out1(out_reg_6_reg_6),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_13_i0_fu_main_33672_34776),
    .wenable(wrenable_reg_6));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_60 (.out1(out_reg_60_reg_60),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_440_reg_60_0_0_0),
    .wenable(wrenable_reg_60));
  register_SE #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_61 (.out1(out_reg_61_reg_61),
    .clock(clock),
    .reset(reset),
    .in1(out_MUX_441_reg_61_0_0_0),
    .wenable(wrenable_reg_61));
  register_SE #(.BITSIZE_in1(13),
    .BITSIZE_out1(13)) reg_62 (.out1(out_reg_62_reg_62),
    .clock(clock),
    .reset(reset),
    .in1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_array_34585_0_32_13),
    .wenable(wrenable_reg_62));
  register_SE #(.BITSIZE_in1(1),
    .BITSIZE_out1(1)) reg_63 (.out1(out_reg_63_reg_63),
    .clock(clock),
    .reset(reset),
    .in1(out_ne_expr_FU_32_0_32_158_i1_fu_main_33672_34899),
    .wenable(wrenable_reg_63));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_64 (.out1(out_reg_64_reg_64),
    .clock(clock),
    .reset(reset),
    .in1(out_ARRAY_1D_STD_BRAM_NN_4_i0_array_34379_0),
    .wenable(wrenable_reg_64));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_65 (.out1(out_reg_65_reg_65),
    .clock(clock),
    .reset(reset),
    .in1(out_mult_expr_FU_32_32_32_0_155_i1_fu_main_33672_34572),
    .wenable(wrenable_reg_65));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_66 (.out1(out_reg_66_reg_66),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_136_i0_fu_main_33672_34618),
    .wenable(wrenable_reg_66));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_67 (.out1(out_reg_67_reg_67),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_137_i0_fu_main_33672_34631),
    .wenable(wrenable_reg_67));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_68 (.out1(out_reg_68_reg_68),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_138_i0_fu_main_33672_34641),
    .wenable(wrenable_reg_68));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_69 (.out1(out_reg_69_reg_69),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_139_i0_fu_main_33672_34651),
    .wenable(wrenable_reg_69));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_7 (.out1(out_reg_7_reg_7),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_15_i0_fu_main_33672_34797),
    .wenable(wrenable_reg_7));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_70 (.out1(out_reg_70_reg_70),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_140_i0_fu_main_33672_34661),
    .wenable(wrenable_reg_70));
  register_STD #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) reg_71 (.out1(out_reg_71_reg_71),
    .clock(clock),
    .reset(reset),
    .in1(out_IUdata_converter_FU_141_i0_fu_main_33672_34671),
    .wenable(wrenable_reg_71));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_8 (.out1(out_reg_8_reg_8),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_16_i0_fu_main_33672_34807),
    .wenable(wrenable_reg_8));
  register_SE #(.BITSIZE_in1(12),
    .BITSIZE_out1(12)) reg_9 (.out1(out_reg_9_reg_9),
    .clock(clock),
    .reset(reset),
    .in1(out_addr_expr_FU_17_i0_fu_main_33672_34821),
    .wenable(wrenable_reg_9));
  // io-signal post fix
  assign return_port = out_MUX_215_gimple_return_FU_143_i0_0_0_0;
  assign OUT_CONDITION_main_33672_33701 = out_read_cond_FU_31_i0_fu_main_33672_33701;
  assign OUT_CONDITION_main_33672_34347 = out_read_cond_FU_71_i0_fu_main_33672_34347;
  assign OUT_CONDITION_main_33672_34349 = out_read_cond_FU_74_i0_fu_main_33672_34349;
  assign OUT_CONDITION_main_33672_34359 = out_read_cond_FU_85_i0_fu_main_33672_34359;
  assign OUT_CONDITION_main_33672_34563 = out_read_cond_FU_125_i0_fu_main_33672_34563;
  assign OUT_CONDITION_main_33672_34565 = out_read_cond_FU_128_i0_fu_main_33672_34565;
  assign OUT_CONDITION_main_33672_34575 = out_read_cond_FU_135_i0_fu_main_33672_34575;
  assign OUT_CONDITION_main_33672_34608 = out_read_cond_FU_142_i0_fu_main_33672_34608;

endmodule

// FSM based controller description for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module controller_main(done_port,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE,
  fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD,
  fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD,
  fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE,
  selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0,
  selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0,
  selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1,
  selector_MUX_215_gimple_return_FU_143_i0_0_0_0,
  selector_MUX_383_reg_0_0_0_0,
  selector_MUX_384_reg_1_0_0_0,
  selector_MUX_389_reg_14_0_0_0,
  selector_MUX_390_reg_15_0_0_0,
  selector_MUX_404_reg_28_0_0_0,
  selector_MUX_405_reg_29_0_0_0,
  selector_MUX_407_reg_30_0_0_0,
  selector_MUX_440_reg_60_0_0_0,
  selector_MUX_441_reg_61_0_0_0,
  selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0,
  selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0,
  selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1,
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
  wrenable_reg_22,
  wrenable_reg_23,
  wrenable_reg_24,
  wrenable_reg_25,
  wrenable_reg_26,
  wrenable_reg_27,
  wrenable_reg_28,
  wrenable_reg_29,
  wrenable_reg_3,
  wrenable_reg_30,
  wrenable_reg_31,
  wrenable_reg_32,
  wrenable_reg_33,
  wrenable_reg_34,
  wrenable_reg_35,
  wrenable_reg_36,
  wrenable_reg_37,
  wrenable_reg_38,
  wrenable_reg_39,
  wrenable_reg_4,
  wrenable_reg_40,
  wrenable_reg_41,
  wrenable_reg_42,
  wrenable_reg_43,
  wrenable_reg_44,
  wrenable_reg_45,
  wrenable_reg_46,
  wrenable_reg_47,
  wrenable_reg_48,
  wrenable_reg_49,
  wrenable_reg_5,
  wrenable_reg_50,
  wrenable_reg_51,
  wrenable_reg_52,
  wrenable_reg_53,
  wrenable_reg_54,
  wrenable_reg_55,
  wrenable_reg_56,
  wrenable_reg_57,
  wrenable_reg_58,
  wrenable_reg_59,
  wrenable_reg_6,
  wrenable_reg_60,
  wrenable_reg_61,
  wrenable_reg_62,
  wrenable_reg_63,
  wrenable_reg_64,
  wrenable_reg_65,
  wrenable_reg_66,
  wrenable_reg_67,
  wrenable_reg_68,
  wrenable_reg_69,
  wrenable_reg_7,
  wrenable_reg_70,
  wrenable_reg_71,
  wrenable_reg_8,
  wrenable_reg_9,
  OUT_CONDITION_main_33672_33701,
  OUT_CONDITION_main_33672_34347,
  OUT_CONDITION_main_33672_34349,
  OUT_CONDITION_main_33672_34359,
  OUT_CONDITION_main_33672_34563,
  OUT_CONDITION_main_33672_34565,
  OUT_CONDITION_main_33672_34575,
  OUT_CONDITION_main_33672_34608,
  clock,
  reset,
  start_port);
  // IN
  input OUT_CONDITION_main_33672_33701;
  input OUT_CONDITION_main_33672_34347;
  input OUT_CONDITION_main_33672_34349;
  input OUT_CONDITION_main_33672_34359;
  input OUT_CONDITION_main_33672_34563;
  input OUT_CONDITION_main_33672_34565;
  input OUT_CONDITION_main_33672_34575;
  input OUT_CONDITION_main_33672_34608;
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  output fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0;
  output selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  output selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  output selector_MUX_215_gimple_return_FU_143_i0_0_0_0;
  output selector_MUX_383_reg_0_0_0_0;
  output selector_MUX_384_reg_1_0_0_0;
  output selector_MUX_389_reg_14_0_0_0;
  output selector_MUX_390_reg_15_0_0_0;
  output selector_MUX_404_reg_28_0_0_0;
  output selector_MUX_405_reg_29_0_0_0;
  output selector_MUX_407_reg_30_0_0_0;
  output selector_MUX_440_reg_60_0_0_0;
  output selector_MUX_441_reg_61_0_0_0;
  output selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0;
  output selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0;
  output selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1;
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
  output wrenable_reg_22;
  output wrenable_reg_23;
  output wrenable_reg_24;
  output wrenable_reg_25;
  output wrenable_reg_26;
  output wrenable_reg_27;
  output wrenable_reg_28;
  output wrenable_reg_29;
  output wrenable_reg_3;
  output wrenable_reg_30;
  output wrenable_reg_31;
  output wrenable_reg_32;
  output wrenable_reg_33;
  output wrenable_reg_34;
  output wrenable_reg_35;
  output wrenable_reg_36;
  output wrenable_reg_37;
  output wrenable_reg_38;
  output wrenable_reg_39;
  output wrenable_reg_4;
  output wrenable_reg_40;
  output wrenable_reg_41;
  output wrenable_reg_42;
  output wrenable_reg_43;
  output wrenable_reg_44;
  output wrenable_reg_45;
  output wrenable_reg_46;
  output wrenable_reg_47;
  output wrenable_reg_48;
  output wrenable_reg_49;
  output wrenable_reg_5;
  output wrenable_reg_50;
  output wrenable_reg_51;
  output wrenable_reg_52;
  output wrenable_reg_53;
  output wrenable_reg_54;
  output wrenable_reg_55;
  output wrenable_reg_56;
  output wrenable_reg_57;
  output wrenable_reg_58;
  output wrenable_reg_59;
  output wrenable_reg_6;
  output wrenable_reg_60;
  output wrenable_reg_61;
  output wrenable_reg_62;
  output wrenable_reg_63;
  output wrenable_reg_64;
  output wrenable_reg_65;
  output wrenable_reg_66;
  output wrenable_reg_67;
  output wrenable_reg_68;
  output wrenable_reg_69;
  output wrenable_reg_7;
  output wrenable_reg_70;
  output wrenable_reg_71;
  output wrenable_reg_8;
  output wrenable_reg_9;
  parameter [25:0] S_0 = 26'b00000000000000000000000001,
    S_8 = 26'b00000000000000000100000000,
    S_1 = 26'b00000000000000000000000010,
    S_2 = 26'b00000000000000000000000100,
    S_3 = 26'b00000000000000000000001000,
    S_4 = 26'b00000000000000000000010000,
    S_6 = 26'b00000000000000000001000000,
    S_5 = 26'b00000000000000000000100000,
    S_7 = 26'b00000000000000000010000000,
    S_18 = 26'b00000001000000000000000000,
    S_9 = 26'b00000000000000001000000000,
    S_10 = 26'b00000000000000010000000000,
    S_11 = 26'b00000000000000100000000000,
    S_12 = 26'b00000000000001000000000000,
    S_13 = 26'b00000000000010000000000000,
    S_14 = 26'b00000000000100000000000000,
    S_16 = 26'b00000000010000000000000000,
    S_15 = 26'b00000000001000000000000000,
    S_17 = 26'b00000000100000000000000000,
    S_19 = 26'b00000010000000000000000000,
    S_20 = 26'b00000100000000000000000000,
    S_21 = 26'b00001000000000000000000000,
    S_22 = 26'b00010000000000000000000000,
    S_23 = 26'b00100000000000000000000000,
    S_25 = 26'b10000000000000000000000000,
    S_24 = 26'b01000000000000000000000000;
  reg [25:0] _present_state=S_0, _next_state;
  reg done_port;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0;
  reg selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  reg selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  reg selector_MUX_215_gimple_return_FU_143_i0_0_0_0;
  reg selector_MUX_383_reg_0_0_0_0;
  reg selector_MUX_384_reg_1_0_0_0;
  reg selector_MUX_389_reg_14_0_0_0;
  reg selector_MUX_390_reg_15_0_0_0;
  reg selector_MUX_404_reg_28_0_0_0;
  reg selector_MUX_405_reg_29_0_0_0;
  reg selector_MUX_407_reg_30_0_0_0;
  reg selector_MUX_440_reg_60_0_0_0;
  reg selector_MUX_441_reg_61_0_0_0;
  reg selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0;
  reg selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0;
  reg selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1;
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
  reg wrenable_reg_22;
  reg wrenable_reg_23;
  reg wrenable_reg_24;
  reg wrenable_reg_25;
  reg wrenable_reg_26;
  reg wrenable_reg_27;
  reg wrenable_reg_28;
  reg wrenable_reg_29;
  reg wrenable_reg_3;
  reg wrenable_reg_30;
  reg wrenable_reg_31;
  reg wrenable_reg_32;
  reg wrenable_reg_33;
  reg wrenable_reg_34;
  reg wrenable_reg_35;
  reg wrenable_reg_36;
  reg wrenable_reg_37;
  reg wrenable_reg_38;
  reg wrenable_reg_39;
  reg wrenable_reg_4;
  reg wrenable_reg_40;
  reg wrenable_reg_41;
  reg wrenable_reg_42;
  reg wrenable_reg_43;
  reg wrenable_reg_44;
  reg wrenable_reg_45;
  reg wrenable_reg_46;
  reg wrenable_reg_47;
  reg wrenable_reg_48;
  reg wrenable_reg_49;
  reg wrenable_reg_5;
  reg wrenable_reg_50;
  reg wrenable_reg_51;
  reg wrenable_reg_52;
  reg wrenable_reg_53;
  reg wrenable_reg_54;
  reg wrenable_reg_55;
  reg wrenable_reg_56;
  reg wrenable_reg_57;
  reg wrenable_reg_58;
  reg wrenable_reg_59;
  reg wrenable_reg_6;
  reg wrenable_reg_60;
  reg wrenable_reg_61;
  reg wrenable_reg_62;
  reg wrenable_reg_63;
  reg wrenable_reg_64;
  reg wrenable_reg_65;
  reg wrenable_reg_66;
  reg wrenable_reg_67;
  reg wrenable_reg_68;
  reg wrenable_reg_69;
  reg wrenable_reg_7;
  reg wrenable_reg_70;
  reg wrenable_reg_71;
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
    fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0 = 1'b0;
    selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 = 1'b0;
    selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 = 1'b0;
    selector_MUX_215_gimple_return_FU_143_i0_0_0_0 = 1'b0;
    selector_MUX_383_reg_0_0_0_0 = 1'b0;
    selector_MUX_384_reg_1_0_0_0 = 1'b0;
    selector_MUX_389_reg_14_0_0_0 = 1'b0;
    selector_MUX_390_reg_15_0_0_0 = 1'b0;
    selector_MUX_404_reg_28_0_0_0 = 1'b0;
    selector_MUX_405_reg_29_0_0_0 = 1'b0;
    selector_MUX_407_reg_30_0_0_0 = 1'b0;
    selector_MUX_440_reg_60_0_0_0 = 1'b0;
    selector_MUX_441_reg_61_0_0_0 = 1'b0;
    selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0 = 1'b0;
    selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0 = 1'b0;
    selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1 = 1'b0;
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
    wrenable_reg_22 = 1'b0;
    wrenable_reg_23 = 1'b0;
    wrenable_reg_24 = 1'b0;
    wrenable_reg_25 = 1'b0;
    wrenable_reg_26 = 1'b0;
    wrenable_reg_27 = 1'b0;
    wrenable_reg_28 = 1'b0;
    wrenable_reg_29 = 1'b0;
    wrenable_reg_3 = 1'b0;
    wrenable_reg_30 = 1'b0;
    wrenable_reg_31 = 1'b0;
    wrenable_reg_32 = 1'b0;
    wrenable_reg_33 = 1'b0;
    wrenable_reg_34 = 1'b0;
    wrenable_reg_35 = 1'b0;
    wrenable_reg_36 = 1'b0;
    wrenable_reg_37 = 1'b0;
    wrenable_reg_38 = 1'b0;
    wrenable_reg_39 = 1'b0;
    wrenable_reg_4 = 1'b0;
    wrenable_reg_40 = 1'b0;
    wrenable_reg_41 = 1'b0;
    wrenable_reg_42 = 1'b0;
    wrenable_reg_43 = 1'b0;
    wrenable_reg_44 = 1'b0;
    wrenable_reg_45 = 1'b0;
    wrenable_reg_46 = 1'b0;
    wrenable_reg_47 = 1'b0;
    wrenable_reg_48 = 1'b0;
    wrenable_reg_49 = 1'b0;
    wrenable_reg_5 = 1'b0;
    wrenable_reg_50 = 1'b0;
    wrenable_reg_51 = 1'b0;
    wrenable_reg_52 = 1'b0;
    wrenable_reg_53 = 1'b0;
    wrenable_reg_54 = 1'b0;
    wrenable_reg_55 = 1'b0;
    wrenable_reg_56 = 1'b0;
    wrenable_reg_57 = 1'b0;
    wrenable_reg_58 = 1'b0;
    wrenable_reg_59 = 1'b0;
    wrenable_reg_6 = 1'b0;
    wrenable_reg_60 = 1'b0;
    wrenable_reg_61 = 1'b0;
    wrenable_reg_62 = 1'b0;
    wrenable_reg_63 = 1'b0;
    wrenable_reg_64 = 1'b0;
    wrenable_reg_65 = 1'b0;
    wrenable_reg_66 = 1'b0;
    wrenable_reg_67 = 1'b0;
    wrenable_reg_68 = 1'b0;
    wrenable_reg_69 = 1'b0;
    wrenable_reg_7 = 1'b0;
    wrenable_reg_70 = 1'b0;
    wrenable_reg_71 = 1'b0;
    wrenable_reg_8 = 1'b0;
    wrenable_reg_9 = 1'b0;
    case (_present_state)
      S_0 :
        if(start_port == 1'b1)
        begin
          selector_MUX_383_reg_0_0_0_0 = 1'b1;
          selector_MUX_384_reg_1_0_0_0 = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_10 = 1'b1;
          wrenable_reg_11 = 1'b1;
          wrenable_reg_12 = 1'b1;
          wrenable_reg_13 = 1'b1;
          wrenable_reg_2 = 1'b1;
          wrenable_reg_3 = 1'b1;
          wrenable_reg_4 = 1'b1;
          wrenable_reg_5 = 1'b1;
          wrenable_reg_6 = 1'b1;
          wrenable_reg_7 = 1'b1;
          wrenable_reg_8 = 1'b1;
          wrenable_reg_9 = 1'b1;
          _next_state = S_8;
        end
        else
        begin
          _next_state = S_0;
        end
      S_8 :
        begin
          selector_MUX_389_reg_14_0_0_0 = 1'b1;
          selector_MUX_390_reg_15_0_0_0 = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_14 = 1'b1;
          wrenable_reg_15 = 1'b1;
          wrenable_reg_16 = 1'b1;
          wrenable_reg_17 = 1'b1;
          wrenable_reg_18 = 1'b1;
          wrenable_reg_19 = 1'b1;
          wrenable_reg_20 = 1'b1;
          _next_state = S_1;
        end
      S_1 :
        begin
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD = 1'b1;
          wrenable_reg_15 = 1'b1;
          wrenable_reg_21 = 1'b1;
          wrenable_reg_22 = 1'b1;
          wrenable_reg_23 = 1'b1;
          _next_state = S_2;
        end
      S_2 :
        begin
          wrenable_reg_14 = 1'b1;
          if (OUT_CONDITION_main_33672_33701 == 1'b1)
            begin
              _next_state = S_1;
            end
          else
            begin
              _next_state = S_3;
            end
        end
      S_3 :
        begin
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD = 1'b1;
          wrenable_reg_24 = 1'b1;
          wrenable_reg_25 = 1'b1;
          wrenable_reg_26 = 1'b1;
          wrenable_reg_27 = 1'b1;
          _next_state = S_4;
        end
      S_4 :
        begin
          if (OUT_CONDITION_main_33672_34347 == 1'b1)
            begin
              _next_state = S_5;
            end
          else
            begin
              _next_state = S_6;
            end
        end
      S_6 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE = 1'b1;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 = 1'b1;
          _next_state = S_7;
        end
      S_5 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0 = 1'b1;
          _next_state = S_7;
        end
      S_7 :
        begin
          selector_MUX_404_reg_28_0_0_0 = 1'b1;
          wrenable_reg_28 = 1'b1;
          if (OUT_CONDITION_main_33672_34349 == 1'b1)
            begin
              _next_state = S_8;
              selector_MUX_404_reg_28_0_0_0 = 1'b0;
              wrenable_reg_28 = 1'b0;
            end
          else
            begin
              _next_state = S_18;
            end
        end
      S_18 :
        begin
          selector_MUX_405_reg_29_0_0_0 = 1'b1;
          selector_MUX_407_reg_30_0_0_0 = 1'b1;
          wrenable_reg_28 = 1'b1;
          wrenable_reg_29 = 1'b1;
          wrenable_reg_30 = 1'b1;
          wrenable_reg_31 = 1'b1;
          wrenable_reg_32 = 1'b1;
          wrenable_reg_33 = 1'b1;
          wrenable_reg_34 = 1'b1;
          wrenable_reg_35 = 1'b1;
          _next_state = S_9;
        end
      S_9 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD = 1'b1;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 = 1'b1;
          wrenable_reg_30 = 1'b1;
          wrenable_reg_36 = 1'b1;
          wrenable_reg_37 = 1'b1;
          _next_state = S_10;
        end
      S_10 :
        begin
          wrenable_reg_38 = 1'b1;
          _next_state = S_11;
        end
      S_11 :
        begin
          wrenable_reg_39 = 1'b1;
          _next_state = S_12;
        end
      S_12 :
        begin
          wrenable_reg_29 = 1'b1;
          if (OUT_CONDITION_main_33672_34359 == 1'b1)
            begin
              _next_state = S_9;
            end
          else
            begin
              _next_state = S_13;
            end
        end
      S_13 :
        begin
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD = 1'b1;
          wrenable_reg_40 = 1'b1;
          wrenable_reg_41 = 1'b1;
          wrenable_reg_42 = 1'b1;
          wrenable_reg_43 = 1'b1;
          wrenable_reg_44 = 1'b1;
          wrenable_reg_45 = 1'b1;
          wrenable_reg_46 = 1'b1;
          wrenable_reg_47 = 1'b1;
          wrenable_reg_48 = 1'b1;
          wrenable_reg_49 = 1'b1;
          wrenable_reg_50 = 1'b1;
          wrenable_reg_51 = 1'b1;
          wrenable_reg_52 = 1'b1;
          wrenable_reg_53 = 1'b1;
          wrenable_reg_54 = 1'b1;
          wrenable_reg_55 = 1'b1;
          wrenable_reg_56 = 1'b1;
          wrenable_reg_57 = 1'b1;
          wrenable_reg_58 = 1'b1;
          wrenable_reg_59 = 1'b1;
          _next_state = S_14;
        end
      S_14 :
        begin
          if (OUT_CONDITION_main_33672_34563 == 1'b1)
            begin
              _next_state = S_15;
            end
          else
            begin
              _next_state = S_16;
            end
        end
      S_16 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE = 1'b1;
          selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0 = 1'b1;
          selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0 = 1'b1;
          _next_state = S_17;
        end
      S_15 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE = 1'b1;
          _next_state = S_17;
        end
      S_17 :
        begin
          selector_MUX_440_reg_60_0_0_0 = 1'b1;
          selector_MUX_441_reg_61_0_0_0 = 1'b1;
          wrenable_reg_60 = 1'b1;
          wrenable_reg_61 = 1'b1;
          if (OUT_CONDITION_main_33672_34565 == 1'b1)
            begin
              _next_state = S_18;
              selector_MUX_440_reg_60_0_0_0 = 1'b0;
              selector_MUX_441_reg_61_0_0_0 = 1'b0;
              wrenable_reg_60 = 1'b0;
              wrenable_reg_61 = 1'b0;
            end
          else
            begin
              _next_state = S_19;
            end
        end
      S_19 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD = 1'b1;
          selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1 = 1'b1;
          wrenable_reg_61 = 1'b1;
          wrenable_reg_62 = 1'b1;
          wrenable_reg_63 = 1'b1;
          _next_state = S_20;
        end
      S_20 :
        begin
          wrenable_reg_64 = 1'b1;
          _next_state = S_21;
        end
      S_21 :
        begin
          wrenable_reg_65 = 1'b1;
          _next_state = S_22;
        end
      S_22 :
        begin
          wrenable_reg_60 = 1'b1;
          if (OUT_CONDITION_main_33672_34575 == 1'b1)
            begin
              _next_state = S_19;
            end
          else
            begin
              _next_state = S_23;
            end
        end
      S_23 :
        begin
          wrenable_reg_66 = 1'b1;
          wrenable_reg_67 = 1'b1;
          wrenable_reg_68 = 1'b1;
          wrenable_reg_69 = 1'b1;
          wrenable_reg_70 = 1'b1;
          wrenable_reg_71 = 1'b1;
          if (OUT_CONDITION_main_33672_34608 == 1'b0)
            begin
              _next_state = S_24;
              done_port = 1'b1;
            end
          else
            begin
              _next_state = S_25;
              done_port = 1'b1;
              wrenable_reg_66 = 1'b0;
              wrenable_reg_67 = 1'b0;
              wrenable_reg_68 = 1'b0;
              wrenable_reg_69 = 1'b0;
              wrenable_reg_70 = 1'b0;
              wrenable_reg_71 = 1'b0;
            end
        end
      S_25 :
        begin
          selector_MUX_215_gimple_return_FU_143_i0_0_0_0 = 1'b1;
          _next_state = S_0;
        end
      S_24 :
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

// Top component for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module _main(clock,
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
  output signed [31:0] return_port;
  // Component and signal declarations
  wire OUT_CONDITION_main_33672_33701;
  wire OUT_CONDITION_main_33672_34347;
  wire OUT_CONDITION_main_33672_34349;
  wire OUT_CONDITION_main_33672_34359;
  wire OUT_CONDITION_main_33672_34563;
  wire OUT_CONDITION_main_33672_34565;
  wire OUT_CONDITION_main_33672_34575;
  wire OUT_CONDITION_main_33672_34608;
  wire done_delayed_REG_signal_in;
  wire done_delayed_REG_signal_out;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0;
  wire selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  wire selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  wire selector_MUX_215_gimple_return_FU_143_i0_0_0_0;
  wire selector_MUX_383_reg_0_0_0_0;
  wire selector_MUX_384_reg_1_0_0_0;
  wire selector_MUX_389_reg_14_0_0_0;
  wire selector_MUX_390_reg_15_0_0_0;
  wire selector_MUX_404_reg_28_0_0_0;
  wire selector_MUX_405_reg_29_0_0_0;
  wire selector_MUX_407_reg_30_0_0_0;
  wire selector_MUX_440_reg_60_0_0_0;
  wire selector_MUX_441_reg_61_0_0_0;
  wire selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0;
  wire selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0;
  wire selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1;
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
  wire wrenable_reg_22;
  wire wrenable_reg_23;
  wire wrenable_reg_24;
  wire wrenable_reg_25;
  wire wrenable_reg_26;
  wire wrenable_reg_27;
  wire wrenable_reg_28;
  wire wrenable_reg_29;
  wire wrenable_reg_3;
  wire wrenable_reg_30;
  wire wrenable_reg_31;
  wire wrenable_reg_32;
  wire wrenable_reg_33;
  wire wrenable_reg_34;
  wire wrenable_reg_35;
  wire wrenable_reg_36;
  wire wrenable_reg_37;
  wire wrenable_reg_38;
  wire wrenable_reg_39;
  wire wrenable_reg_4;
  wire wrenable_reg_40;
  wire wrenable_reg_41;
  wire wrenable_reg_42;
  wire wrenable_reg_43;
  wire wrenable_reg_44;
  wire wrenable_reg_45;
  wire wrenable_reg_46;
  wire wrenable_reg_47;
  wire wrenable_reg_48;
  wire wrenable_reg_49;
  wire wrenable_reg_5;
  wire wrenable_reg_50;
  wire wrenable_reg_51;
  wire wrenable_reg_52;
  wire wrenable_reg_53;
  wire wrenable_reg_54;
  wire wrenable_reg_55;
  wire wrenable_reg_56;
  wire wrenable_reg_57;
  wire wrenable_reg_58;
  wire wrenable_reg_59;
  wire wrenable_reg_6;
  wire wrenable_reg_60;
  wire wrenable_reg_61;
  wire wrenable_reg_62;
  wire wrenable_reg_63;
  wire wrenable_reg_64;
  wire wrenable_reg_65;
  wire wrenable_reg_66;
  wire wrenable_reg_67;
  wire wrenable_reg_68;
  wire wrenable_reg_69;
  wire wrenable_reg_7;
  wire wrenable_reg_70;
  wire wrenable_reg_71;
  wire wrenable_reg_8;
  wire wrenable_reg_9;
  
  controller_main Controller_i (.done_port(done_delayed_REG_signal_in),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE),
    .fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE),
    .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0),
    .selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0),
    .selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1),
    .selector_MUX_215_gimple_return_FU_143_i0_0_0_0(selector_MUX_215_gimple_return_FU_143_i0_0_0_0),
    .selector_MUX_383_reg_0_0_0_0(selector_MUX_383_reg_0_0_0_0),
    .selector_MUX_384_reg_1_0_0_0(selector_MUX_384_reg_1_0_0_0),
    .selector_MUX_389_reg_14_0_0_0(selector_MUX_389_reg_14_0_0_0),
    .selector_MUX_390_reg_15_0_0_0(selector_MUX_390_reg_15_0_0_0),
    .selector_MUX_404_reg_28_0_0_0(selector_MUX_404_reg_28_0_0_0),
    .selector_MUX_405_reg_29_0_0_0(selector_MUX_405_reg_29_0_0_0),
    .selector_MUX_407_reg_30_0_0_0(selector_MUX_407_reg_30_0_0_0),
    .selector_MUX_440_reg_60_0_0_0(selector_MUX_440_reg_60_0_0_0),
    .selector_MUX_441_reg_61_0_0_0(selector_MUX_441_reg_61_0_0_0),
    .selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0(selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0),
    .selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0(selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0),
    .selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1(selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1),
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
    .wrenable_reg_22(wrenable_reg_22),
    .wrenable_reg_23(wrenable_reg_23),
    .wrenable_reg_24(wrenable_reg_24),
    .wrenable_reg_25(wrenable_reg_25),
    .wrenable_reg_26(wrenable_reg_26),
    .wrenable_reg_27(wrenable_reg_27),
    .wrenable_reg_28(wrenable_reg_28),
    .wrenable_reg_29(wrenable_reg_29),
    .wrenable_reg_3(wrenable_reg_3),
    .wrenable_reg_30(wrenable_reg_30),
    .wrenable_reg_31(wrenable_reg_31),
    .wrenable_reg_32(wrenable_reg_32),
    .wrenable_reg_33(wrenable_reg_33),
    .wrenable_reg_34(wrenable_reg_34),
    .wrenable_reg_35(wrenable_reg_35),
    .wrenable_reg_36(wrenable_reg_36),
    .wrenable_reg_37(wrenable_reg_37),
    .wrenable_reg_38(wrenable_reg_38),
    .wrenable_reg_39(wrenable_reg_39),
    .wrenable_reg_4(wrenable_reg_4),
    .wrenable_reg_40(wrenable_reg_40),
    .wrenable_reg_41(wrenable_reg_41),
    .wrenable_reg_42(wrenable_reg_42),
    .wrenable_reg_43(wrenable_reg_43),
    .wrenable_reg_44(wrenable_reg_44),
    .wrenable_reg_45(wrenable_reg_45),
    .wrenable_reg_46(wrenable_reg_46),
    .wrenable_reg_47(wrenable_reg_47),
    .wrenable_reg_48(wrenable_reg_48),
    .wrenable_reg_49(wrenable_reg_49),
    .wrenable_reg_5(wrenable_reg_5),
    .wrenable_reg_50(wrenable_reg_50),
    .wrenable_reg_51(wrenable_reg_51),
    .wrenable_reg_52(wrenable_reg_52),
    .wrenable_reg_53(wrenable_reg_53),
    .wrenable_reg_54(wrenable_reg_54),
    .wrenable_reg_55(wrenable_reg_55),
    .wrenable_reg_56(wrenable_reg_56),
    .wrenable_reg_57(wrenable_reg_57),
    .wrenable_reg_58(wrenable_reg_58),
    .wrenable_reg_59(wrenable_reg_59),
    .wrenable_reg_6(wrenable_reg_6),
    .wrenable_reg_60(wrenable_reg_60),
    .wrenable_reg_61(wrenable_reg_61),
    .wrenable_reg_62(wrenable_reg_62),
    .wrenable_reg_63(wrenable_reg_63),
    .wrenable_reg_64(wrenable_reg_64),
    .wrenable_reg_65(wrenable_reg_65),
    .wrenable_reg_66(wrenable_reg_66),
    .wrenable_reg_67(wrenable_reg_67),
    .wrenable_reg_68(wrenable_reg_68),
    .wrenable_reg_69(wrenable_reg_69),
    .wrenable_reg_7(wrenable_reg_7),
    .wrenable_reg_70(wrenable_reg_70),
    .wrenable_reg_71(wrenable_reg_71),
    .wrenable_reg_8(wrenable_reg_8),
    .wrenable_reg_9(wrenable_reg_9),
    .OUT_CONDITION_main_33672_33701(OUT_CONDITION_main_33672_33701),
    .OUT_CONDITION_main_33672_34347(OUT_CONDITION_main_33672_34347),
    .OUT_CONDITION_main_33672_34349(OUT_CONDITION_main_33672_34349),
    .OUT_CONDITION_main_33672_34359(OUT_CONDITION_main_33672_34359),
    .OUT_CONDITION_main_33672_34563(OUT_CONDITION_main_33672_34563),
    .OUT_CONDITION_main_33672_34565(OUT_CONDITION_main_33672_34565),
    .OUT_CONDITION_main_33672_34575(OUT_CONDITION_main_33672_34575),
    .OUT_CONDITION_main_33672_34608(OUT_CONDITION_main_33672_34608),
    .clock(clock),
    .reset(reset),
    .start_port(start_port));
  datapath_main #(.MEM_var_33736_33672(2048),
    .MEM_var_33767_33672(2048),
    .MEM_var_33810_33672(2048),
    .MEM_var_34309_33672(2048),
    .MEM_var_34379_33672(2048),
    .MEM_var_34403_33672(2048),
    .MEM_var_34421_33672(2048),
    .MEM_var_34585_33672(2048)) Datapath_i (.return_port(return_port),
    .OUT_CONDITION_main_33672_33701(OUT_CONDITION_main_33672_33701),
    .OUT_CONDITION_main_33672_34347(OUT_CONDITION_main_33672_34347),
    .OUT_CONDITION_main_33672_34349(OUT_CONDITION_main_33672_34349),
    .OUT_CONDITION_main_33672_34359(OUT_CONDITION_main_33672_34359),
    .OUT_CONDITION_main_33672_34563(OUT_CONDITION_main_33672_34563),
    .OUT_CONDITION_main_33672_34565(OUT_CONDITION_main_33672_34565),
    .OUT_CONDITION_main_33672_34575(OUT_CONDITION_main_33672_34575),
    .OUT_CONDITION_main_33672_34608(OUT_CONDITION_main_33672_34608),
    .clock(clock),
    .reset(reset),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE),
    .fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_LOAD),
    .fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_4_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_1_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_2_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_5_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_6_i0_STORE),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_LOAD),
    .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_7_i0_STORE),
    .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_0_0_0),
    .selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0),
    .selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1),
    .selector_MUX_215_gimple_return_FU_143_i0_0_0_0(selector_MUX_215_gimple_return_FU_143_i0_0_0_0),
    .selector_MUX_383_reg_0_0_0_0(selector_MUX_383_reg_0_0_0_0),
    .selector_MUX_384_reg_1_0_0_0(selector_MUX_384_reg_1_0_0_0),
    .selector_MUX_389_reg_14_0_0_0(selector_MUX_389_reg_14_0_0_0),
    .selector_MUX_390_reg_15_0_0_0(selector_MUX_390_reg_15_0_0_0),
    .selector_MUX_404_reg_28_0_0_0(selector_MUX_404_reg_28_0_0_0),
    .selector_MUX_405_reg_29_0_0_0(selector_MUX_405_reg_29_0_0_0),
    .selector_MUX_407_reg_30_0_0_0(selector_MUX_407_reg_30_0_0_0),
    .selector_MUX_440_reg_60_0_0_0(selector_MUX_440_reg_60_0_0_0),
    .selector_MUX_441_reg_61_0_0_0(selector_MUX_441_reg_61_0_0_0),
    .selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0(selector_MUX_4_ARRAY_1D_STD_BRAM_NN_4_i0_0_0_0),
    .selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0(selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_0),
    .selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1(selector_MUX_5_ARRAY_1D_STD_BRAM_NN_4_i0_1_0_1),
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
    .wrenable_reg_22(wrenable_reg_22),
    .wrenable_reg_23(wrenable_reg_23),
    .wrenable_reg_24(wrenable_reg_24),
    .wrenable_reg_25(wrenable_reg_25),
    .wrenable_reg_26(wrenable_reg_26),
    .wrenable_reg_27(wrenable_reg_27),
    .wrenable_reg_28(wrenable_reg_28),
    .wrenable_reg_29(wrenable_reg_29),
    .wrenable_reg_3(wrenable_reg_3),
    .wrenable_reg_30(wrenable_reg_30),
    .wrenable_reg_31(wrenable_reg_31),
    .wrenable_reg_32(wrenable_reg_32),
    .wrenable_reg_33(wrenable_reg_33),
    .wrenable_reg_34(wrenable_reg_34),
    .wrenable_reg_35(wrenable_reg_35),
    .wrenable_reg_36(wrenable_reg_36),
    .wrenable_reg_37(wrenable_reg_37),
    .wrenable_reg_38(wrenable_reg_38),
    .wrenable_reg_39(wrenable_reg_39),
    .wrenable_reg_4(wrenable_reg_4),
    .wrenable_reg_40(wrenable_reg_40),
    .wrenable_reg_41(wrenable_reg_41),
    .wrenable_reg_42(wrenable_reg_42),
    .wrenable_reg_43(wrenable_reg_43),
    .wrenable_reg_44(wrenable_reg_44),
    .wrenable_reg_45(wrenable_reg_45),
    .wrenable_reg_46(wrenable_reg_46),
    .wrenable_reg_47(wrenable_reg_47),
    .wrenable_reg_48(wrenable_reg_48),
    .wrenable_reg_49(wrenable_reg_49),
    .wrenable_reg_5(wrenable_reg_5),
    .wrenable_reg_50(wrenable_reg_50),
    .wrenable_reg_51(wrenable_reg_51),
    .wrenable_reg_52(wrenable_reg_52),
    .wrenable_reg_53(wrenable_reg_53),
    .wrenable_reg_54(wrenable_reg_54),
    .wrenable_reg_55(wrenable_reg_55),
    .wrenable_reg_56(wrenable_reg_56),
    .wrenable_reg_57(wrenable_reg_57),
    .wrenable_reg_58(wrenable_reg_58),
    .wrenable_reg_59(wrenable_reg_59),
    .wrenable_reg_6(wrenable_reg_6),
    .wrenable_reg_60(wrenable_reg_60),
    .wrenable_reg_61(wrenable_reg_61),
    .wrenable_reg_62(wrenable_reg_62),
    .wrenable_reg_63(wrenable_reg_63),
    .wrenable_reg_64(wrenable_reg_64),
    .wrenable_reg_65(wrenable_reg_65),
    .wrenable_reg_66(wrenable_reg_66),
    .wrenable_reg_67(wrenable_reg_67),
    .wrenable_reg_68(wrenable_reg_68),
    .wrenable_reg_69(wrenable_reg_69),
    .wrenable_reg_7(wrenable_reg_7),
    .wrenable_reg_70(wrenable_reg_70),
    .wrenable_reg_71(wrenable_reg_71),
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
  wire signed [31:0] out_return_port_view_convert_expr_FU;
  
  _main _main_i0 (.done_port(done_port),
    .return_port(out_return_port_view_convert_expr_FU),
    .clock(clock),
    .reset(reset),
    .start_port(start_port));
  view_convert_expr_FU #(.BITSIZE_in1(32),
    .BITSIZE_out1(32)) return_port_view_convert_expr_FU (.out1(return_port),
    .in1(out_return_port_view_convert_expr_FU));

endmodule


