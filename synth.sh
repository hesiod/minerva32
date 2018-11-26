#!/usr/bin/env bash

yosys -t -l mrv.synth.log -s mrv.sy
nextpnr-ice40 --up5k --pcf mrv.pcf --asc mrv.asc --json mrv.json --freq 30
#arachne-pnr -p mrv.pcf -r -o mrv.asc -d 5k -P sg48 mrv.blif

#icepack mrv.asc mrv.bin
#iceprog mrv.bin

#icetime -tmd up5k mrv.asc
