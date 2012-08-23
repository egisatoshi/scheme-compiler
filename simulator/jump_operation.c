#include "machinecode_simulator.h"

void run_jump_operation(machinecode *program, int *pc, int *convert, union bit *reg) {
    switch(program[*pc].opcode) {
    case 0: /* if reg */
        do_if(reg_ref(reg, program[*pc].sr2, convert), pc);
        break;
    case (0 + IMD_SIZE): /* if imd */
        do_if(program[*pc].imd, pc);
        break;
    case 1: /* goto reg */
        do_goto(reg_ref(reg, program[*pc].sr2, convert), pc);
        break;
    case (1 + IMD_SIZE): /* goto imd */
        do_goto(program[*pc].imd, pc);
        break;
    case 2: /* call reg */
        do_call(reg_ref(reg, program[*pc].sr2, convert), pc, convert);
        break;
    case (2 + IMD_SIZE): /* call imd */
        do_call(program[*pc].imd, pc, convert);
        break;
    default:
        fprintf(stderr, "%2x : unknown instruction\n", (program[*pc].info)+(program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

void do_if(union bit x, int *pc) {
    if(x.i == FALSE) {
        *pc += 2;
    }else {
        *pc += 1;
    }
}

void do_goto(union bit x, int *pc) {
    *pc = x.i / 2;
}

void do_call(union bit x, int *pc, int *convert) {
    *pc = x.i / 2;
    if(*convert == 0) {
        *convert = 1;
    }else {
        *convert = 0;
    }
}
