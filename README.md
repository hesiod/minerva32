# minerva32, a RV32I CPU

minerva32 is a RISC-V CPU targeting the RV32I ISA featuring a 4-stage
RISC-ish pipeline (IF, ID, EX and WB). The target architecture is
the ICE40UP5K FPGA from Lattice. Because it uses the on-chip Block RAM (EBR)
as instruction/data memory, no cache is required since EBR Reads require
only one clock cycle. The ALU utilizes the UP5K's DSP blocks for adding and
subtracting.

The core is written in Clash, a Haskell dialect that compiles Haskell code directly
to VHDL or Verilog. The tree contains synthesis scripts to synthesize Clash-generated
Verilog using Yosys and place-and-route the netlist using arachne-pnr or nextpnr.

minerva32 is not yet RV32I-compliant. The following features are missing:
 - Support for the CSR instructions
 - Support for ECALL/EBREAK
 - Support for LOADs/STOREs with non-word widths (would only require writing a primitive for the UP5K's SPRAM which includes a write mask)
 - Support for the RISC-V Priviledged Standard (i.e. required Machine-level CSRs, most importantly Interrupts)

# Running
## Prerequisites
You'll need to install [Clash](https://clash-lang.org/).
Follow the installation instructions provided by the Clash project [here](https://github.com/clash-lang/clash-compiler/wiki/FAQ).

For an integrated synthesis and implementation flow, it is recommended to use
[Yosys](http://www.clifford.at/yosys/) and [nextpnr](https://github.com/YosysHQ/nextpnr).

## Compilation, Synthesis and Place-and-Route

1. Compile minerva32 to Verilog: `clash --verilog -ihdl hdl/Top.hs`
2. Voilà! You'll find the generated Verilog in a new directory called `verilog` (surprise!)
2. Run the synthesis script (this will run Yosys and nextpnr as required): `./synth.sh`
4. Run icepack/iceprog to upload the design to an ICE40UP5K FPGA or run icetime to show the timing results

The tree contains Clash-generated Verilog (in the `verilog` directory)
in case you don't want to install Clash.

