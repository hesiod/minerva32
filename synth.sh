#!/usr/bin/env bash

#cd verilog/MAC
yosys -s mac.sy
arachne-pnr -o mac.asc -d 5k -P sg48 mac.blif

#icepack mac.asc mac.bin
#iceprog mac.bin

#icetime -tmd up5k mac.asc
