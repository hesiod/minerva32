PREFIX=riscv64-linux-gnu-
CC=$(PREFIX)gcc
LD=$(PREFIX)gcc
OBJCOPY=$(PREFIX)objcopy

.PHONY: all

all: dummy.lit
dummy: dummy.o

dummy.lit: dummy.bin
	xxd -g 0 -b $< | grep -E -o -e '[01]{48}' > $@
dummy.bin: dummy
	$(OBJCOPY) -j .text -O binary $< $@

