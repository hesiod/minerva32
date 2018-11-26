PREFIX=/opt/riscv/bin/riscv32-unknown-elf-
CC=$(PREFIX)gcc
LD=$(PREFIX)gcc
OBJCOPY=$(PREFIX)objcopy
ASFLAGS=-march=rv32e -mabi=ilp32e -ffreestanding
CFLAGS=-march=rv32e -mabi=ilp32e -ffreestanding
LDFLAGS=-march=rv32e -mabi=ilp32e -nostdlib -lgcc -Tlinker.ld -Wl,--build-id=none

.PHONY: all clean

all: dummy.lit crt0.o
dummy: dummy.o

dummy.lit: dummy.bin
	xxd -e $< | xxd -r | xxd -c 4 -g 0 -b - | grep -E -o -e '[01]{32}' > $@
dummy.bin: dummy
	$(OBJCOPY) -j .text -O binary $< $@
	dd if=/dev/null of=$@ bs=1 count=1 seek=16384

clean:
	$(RM) dummy.o dummy dummy.lit crt0.o
