#include "machinecode_simulator.h"

void run_system_operation(machinecode *program, int *pc, int *halt) {
    switch(program[*pc].opcode) {
    case 0: /* nop */
        (*pc)++;
        break;
    case 1: /* halt */
        *halt = 1;
        break;
    default:
        fprintf(stderr, "%2x : unknown instruction\n", (program[*pc].info)+(program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

