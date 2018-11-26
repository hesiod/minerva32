.section .text

reset_handler:

  /* set 0 in mtvec (base for IVT) */
  //csrrw x0, mtvec, x0

  /* set all registers to zero */
  mv  x1, x0
  mv  x2, x1
  mv  x3, x1
  mv  x4, x1
  mv  x5, x1
  mv  x6, x1
  mv  x7, x1
  mv  x8, x1
  mv  x9, x1
  mv x10, x1
  mv x11, x1
  mv x12, x1
  mv x13, x1
  mv x14, x1
  mv x15, x1
/*mv x16, x1
  mv x17, x1
  mv x18, x1
  mv x19, x1
  mv x20, x1
  mv x21, x1
  mv x22, x1
  mv x23, x1
  mv x24, x1
  mv x25, x1
  mv x26, x1
  mv x27, x1
  mv x28, x1
  mv x29, x1
  mv x30, x1
  mv x31, x1
*/

  /* stack initilization */
  la   x2, _stack_start

_start:
  .global _start

  /* clear BSS */
  la x1, _bss_start
  la x2, _bss_end

  bge x1, x2, zero_loop_end

zero_loop:
  sw x0, 0(x1)
  addi x1, x1, 4
  ble x1, x2, zero_loop
zero_loop_end:

  mv x1, x0
  mv x2, x0

  /* Run global initialization functions */
//  call    __libc_init_array

main_entry:
//  addi   x10, x0, 0
//  addi   x11, x0, 0x1
//  jal  uart_set_cfg;

  /* jump to main program entry point (argc = argv = 0) */
  addi x10, x0, 0
  addi x11, x0, 0
  jal x1, main
//  mv s0, a0
//  jal  uart_wait_tx_done;
//  mv a0, s0

hang:
  jal x1, hang

  /* if program exits call exit routine from library */
//  jal  x1, exit

