#include <cstdlib>
#include <iostream>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vcpu.h"

vluint64_t main_time = 0;

double sc_time_stamp() { return main_time; }

int main(int argc, char** argv, char** env) {
    Verilated::commandArgs(argc, argv);
    Verilated::traceEverOn(true);

    std::cout << std::boolalpha;

    Vcpu* top = new Vcpu;
    VerilatedVcdC* trace = new VerilatedVcdC;
    top->trace(trace, 99);
    trace->open("cpu.vcd");

    top->ext_reset = 1;
    while (!Verilated::gotFinish() && main_time < 1000000) {
        if (main_time > 10) {
            top->ext_reset = 0;
        }
        if ((main_time % 10) == 1) {
            top->ref_clk = 1;
        }
        if ((main_time % 10) == 6) {
            top->ref_clk = 0;
        }
        top->eval();
        trace->dump(main_time);
        std::cout << main_time << ": " << (bool)top->uart_tx << std::endl;
        main_time++;
    }
    trace->close();
    delete trace;
    delete top;

    return EXIT_SUCCESS;
}
