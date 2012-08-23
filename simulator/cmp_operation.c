#include "machinecode_simulator.h"

void run_cmp_operation(machinecode *program, int *pc, int *convert, union bit *reg) {
    switch(program[*pc].opcode) {
    case 0: /* = reg */
        reg_set(reg, program[*pc].dr, do_equal(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (0 + IMD_SIZE): /* = imd */
        reg_set(reg, program[*pc].dr, do_equal(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 1: /* =. reg */
        reg_set(reg, program[*pc].dr, do_equal_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (1 + IMD_SIZE): /* =. imd */
        reg_set(reg, program[*pc].dr, do_equal_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 2: /* > reg */
        reg_set(reg, program[*pc].dr, do_greater(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (2 + IMD_SIZE): /* > imd */
        reg_set(reg, program[*pc].dr, do_greater(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 3: /* >. reg */
        reg_set(reg, program[*pc].dr, do_greater_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (3 + IMD_SIZE): /* >. imd */
        reg_set(reg, program[*pc].dr, do_greater_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 4: /* < reg */
        reg_set(reg, program[*pc].dr, do_less(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (4 + IMD_SIZE): /* < imd */
        reg_set(reg, program[*pc].dr, do_less(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 5: /* <. reg */
        reg_set(reg, program[*pc].dr, do_less_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (5 + IMD_SIZE): /* <. imd */
        reg_set(reg, program[*pc].dr, do_less_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 6: /* >= reg */
        reg_set(reg, program[*pc].dr, do_greater_equal(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (6 + IMD_SIZE): /* >= imd */
        reg_set(reg, program[*pc].dr, do_greater_equal(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 7: /* >=. reg */
        reg_set(reg, program[*pc].dr, do_greater_equal_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (7 + IMD_SIZE): /* >=. imd */
        reg_set(reg, program[*pc].dr, do_greater_equal_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 8: /* <= reg */
        reg_set(reg, program[*pc].dr, do_less_equal(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (8 + IMD_SIZE): /* <= imd */
        reg_set(reg, program[*pc].dr, do_less_equal(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 9: /* <=. reg */
        reg_set(reg, program[*pc].dr, do_less_equal_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (9 + IMD_SIZE): /* <=. imd */
        reg_set(reg, program[*pc].dr, do_less_equal_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    default:
        fprintf(stderr, "%2x : unknown instruction\n", (program[*pc].info+program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

union bit do_equal(union bit x, union bit y) {
    union bit z;
    if(x.i == y.i) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_equal_dot(union bit x, union bit y) {
    union bit z;
    if(x.f == y.f) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_greater(union bit x, union bit y) {
    union bit z;
    if(x.i > y.i) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_greater_dot(union bit x, union bit y) {
    union bit z;
    if(x.f > y.f) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_less(union bit x, union bit y) {
    union bit z;
    if(x.i < y.i) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_less_dot(union bit x, union bit y) {
    union bit z;
    if(x.f < y.f) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_greater_equal(union bit x, union bit y) {
    union bit z;
    if(x.i >= y.i) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_greater_equal_dot(union bit x, union bit y) {
    union bit z;
    if(x.f >= y.f) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_less_equal(union bit x, union bit y) {
    union bit z;
    if(x.i <= y.i) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

union bit do_less_equal_dot(union bit x, union bit y) {
    union bit z;
    if(x.f <= y.f) {
        z.i = TRUE;
        return z;
    }else {
        z.i = FALSE;
        return z;
    }
}

