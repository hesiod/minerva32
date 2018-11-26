ENTRY(_start)
STARTUP(crt0.o)

MEMORY
{
    rom (rxi): ORIGIN = 0x00000000, LENGTH = 4k
    data (rw): ORIGIN = 0x00100000, LENGTH = 4k
}

SECTIONS
{
    .text : ALIGN(4) {
        . = 0x0;
        *(.text)
        *(.data)
        *(.rodata)
    } > rom

    .bss : ALIGN(4) {
        _bss_start = .;
        *(.bss)
        . = ALIGN(4);
        _bss_end = .;
    } > data
    
    .stack (NOLOAD): ALIGN(4) {
        _stack_start = .;
        . = . + 0x1000;
        _stack_end = .;
    } > data
}


