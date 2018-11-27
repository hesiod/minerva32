#!/usr/bin/env bash

CPUDIR=verilog/Top/cpu
TBDIR=verilog/Top/tb
VFLAGS="-Wno-fatal --default-language 1364-2005 --cc --exe sim_main.cpp -v /usr/share/yosys/ice40/cells_sim.v --trace"

#verilator $VFLAGS --clk clk -I$TBDIR $TBDIR/tb.v
verilator $VFLAGS --clk ref_clk -I$CPUDIR $CPUDIR/cpu.v

#make -j -C obj_dir -f Vtb.mk Vtb
make -j -C obj_dir -f Vcpu.mk Vcpu
