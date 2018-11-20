PREFIX=riscv64-linux-gnu-
CC=$(PREFIX)gcc
LD=$(PREFIX)gcc
OBJCOPY=$(PREFIX)objcopy

.PHONY: all

all: dummy.bin
dummy: dummy.o

dummy.bin: dummy
	$(OBJCOPY) -j .text -O binary $< $@

