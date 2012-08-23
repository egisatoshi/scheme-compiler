#include "machinecode_simulator.h"

void run_scu_operation(machinecode *program, int *pc, int *convert, union bit *reg) {
    switch(program[*pc].opcode) {
    case 0: /* set reg */
        reg_set(reg, program[*pc].dr, reg_ref(reg,program[*pc].sr2,convert), convert);
        (*pc)++;
        break;
    case (0 + IMD_SIZE): /* set imd */
        reg_set(reg, program[*pc].dr, program[*pc].imd, convert);
        (*pc)++;
        break;
     case 1: /* + reg */
        reg_set(reg, program[*pc].dr, do_plus(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
     case (1 + IMD_SIZE): /* + imd */
        reg_set(reg, program[*pc].dr, do_plus(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
     case 2: /* - reg */
        reg_set(reg, program[*pc].dr, do_minus(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
     case (2 + IMD_SIZE): /* - imd */
        reg_set(reg, program[*pc].dr, do_minus(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
     case 3: /* xor reg */
        reg_set(reg, program[*pc].dr, do_xor(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
     case (3 + IMD_SIZE): /* xor imd */
        reg_set(reg, program[*pc].dr, do_xor(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
     case 4: /* or reg */
        reg_set(reg, program[*pc].dr, do_or(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
     case (4 + IMD_SIZE): /* or imd */
        reg_set(reg, program[*pc].dr, do_or(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
     case 5: /* and reg */
        reg_set(reg, program[*pc].dr, do_and(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
     case (5 + IMD_SIZE): /* adn imd */
        reg_set(reg, program[*pc].dr, do_and(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
     case 6: /* not reg */
        reg_set(reg, program[*pc].dr, do_not(reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
     case (6 + IMD_SIZE): /* not imd */
        reg_set(reg, program[*pc].dr, do_not(program[*pc].imd), convert);
        (*pc)++;
        break;
     default:
        fprintf(stderr, "%2x : unknown instruction\n", (program[*pc].info)+(program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

union bit do_plus(union bit x, union bit y) {
    union bit z;
    z.i = x.i + y.i;
    return z;
}

union bit do_minus(union bit x, union bit y) {
    union bit z;
    z.i = x.i - y.i;
    return z;
}

union bit do_xor(union bit x, union bit y) {
    union bit z;
    z.i = x.i ^ y.i;
    return z;
}

union bit do_or(union bit x, union bit y) {
    union bit z;
    z.i = x.i | y.i;
    return z;
}

union bit do_and(union bit x, union bit y) {
    union bit z;
    z.i = x.i & y.i;
    return z;
}

union bit do_not(union bit x) {
    union bit y;
    y.i = ~(x.i);
    return y;
}

