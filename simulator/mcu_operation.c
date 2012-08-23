#include "machinecode_simulator.h"

void run_mcu_operation(machinecode *program, int *pc, int *convert, union bit *reg) {
    switch(program[*pc].opcode) {
    case 0: /* sll reg */
        reg_set(reg, program[*pc].dr, do_sll(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break; /* sll imd */
    case (0 + IMD_SIZE):
        reg_set(reg, program[*pc].dr, do_sll(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 1: /* srl reg */
        reg_set(reg, program[*pc].dr, do_srl(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (1 + IMD_SIZE): /* srl imd */
        reg_set(reg, program[*pc].dr, do_srl(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 2: /* * reg */
        reg_set(reg, program[*pc].dr, do_mult(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (2 + IMD_SIZE): /* * imd */
        reg_set(reg, program[*pc].dr, do_mult(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 3: /* / reg */
        reg_set(reg, program[*pc].dr, do_div(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (3 + IMD_SIZE): /* / imd */
        reg_set(reg, program[*pc].dr, do_div(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 4: /* modulo reg */
        reg_set(reg, program[*pc].dr, do_modulo(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (4 + IMD_SIZE): /* modulo imd */
        reg_set(reg, program[*pc].dr, do_modulo(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 5: /* +. reg */
        reg_set(reg, program[*pc].dr, do_plus_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (5 + IMD_SIZE): /* +. imd */
        reg_set(reg, program[*pc].dr, do_plus_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 6: /* -. reg */
        reg_set(reg, program[*pc].dr, do_minus_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (6 + IMD_SIZE): /* -. imd */
        reg_set(reg, program[*pc].dr, do_minus_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 7: /* *. reg */
        // reg_print(reg, 2, convert);
        // reg_print(reg, 2, convert);
        reg_set(reg, program[*pc].dr, do_mult_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        // reg_print(reg, 128, convert);
        // printf("\n");
        (*pc)++;
        break;
    case (7 + IMD_SIZE): /* *. imd */
        reg_set(reg, program[*pc].dr, do_mult_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 8: /* /. reg */
        reg_set(reg, program[*pc].dr, do_div_dot(reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (8 + IMD_SIZE): /* /. imd */
        reg_set(reg, program[*pc].dr, do_div_dot(reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case 9: /* itof reg */
        reg_set(reg, program[*pc].dr, do_itof(reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (9 + IMD_SIZE): /* itof imd */
        reg_set(reg, program[*pc].dr, do_itof(program[*pc].imd), convert);
        (*pc)++;
        break;
    case 10: /* ftoi reg */
        reg_set(reg, program[*pc].dr, do_ftoi(reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (10 + IMD_SIZE): /* ftoi imd */
        reg_set(reg, program[*pc].dr, do_ftoi(program[*pc].imd), convert);
        (*pc)++;
        break;
    default:
        fprintf(stderr, "%2x : unknown instruction\n", (program[*pc].info)+(program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

union bit do_sll(union bit x, union bit y) {
    union bit z;
    z.i = x.i << y.i;
    return z;
}

union bit do_srl(union bit x, union bit y) {
    union bit z;
    z.i = x.i >> y.i;
    return z;
}

union bit do_mult(union bit x, union bit y) {
    union bit z;
    z.i = x.i * y.i;
    return z;
}

union bit do_div(union bit x, union bit y) {
    union bit z;
    z.i = x.i / y.i;
    return z;
}

union bit do_modulo(union bit x, union bit y) {
    union bit z;
    z.i = x.i % y.i;
    return z;
}

union bit do_plus_dot(union bit x, union bit y) {
    union bit z;
    z.f = x.f + y.f;
    return z;
}

union bit do_minus_dot(union bit x, union bit y) {
    union bit z;
    z.f = x.f - y.f;
    return z;
}

union bit do_mult_dot(union bit x, union bit y) {
    union bit z;
    z.f = x.f * y.f;
    return z;
}

union bit do_div_dot(union bit x, union bit y) {
    union bit z;
    z.f = x.f / y.f;
    return z;
}

union bit do_itof(union bit x) {
    union bit y;
    y.f = (float)x.i;
    return y;
}

union bit do_ftoi(union bit x) {
    union bit y;

    y.i = (int)(x.f + 0.5);

    return y;
}

