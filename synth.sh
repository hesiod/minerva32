#!/usr/bin/env bash

yosys -q -s mac.sy
arachne-pnr -q -r -o mac.asc -d 5k -P sg48 mac.blif

#icepack mac.asc mac.bin
#iceprog mac.bin

#icetime -tmd up5k mac.asc
