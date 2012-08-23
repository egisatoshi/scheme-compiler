#include "machinecode_simulator.h"

void run_io_operation(machinecode *program, int *pc, int *convert, union bit *reg) {
    switch(program[*pc].opcode) {
    case 0: /* read-byte */
        reg_set(reg, program[*pc].dr, do_read_byte(), convert);
        (*pc)++;
        break;
    case 1: /* read-word */
        reg_set(reg, program[*pc].dr, do_read_word(), convert);
        (*pc)++;
        break;
    case 2: /* write-byte reg */
        do_write_byte(reg_ref(reg,program[*pc].sr2,convert));
        (*pc)++;
        break;
    case (2 + IMD_SIZE): /* write-byte imd */
        do_write_byte(program[*pc].imd);
        (*pc)++;
        break;
    case 3: /* write-word reg */
        do_write_word(reg_ref(reg,program[*pc].sr2,convert));
        (*pc)++;
        break;
    case (3 + IMD_SIZE): /* write-word imd */
        do_write_word(program[*pc].imd);
        (*pc)++;
        break;
    default:
        fprintf(stderr, "%2x : unknown opcode\n", (program[*pc].info)+(program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

union bit do_read_byte(void) {
    union bit x;
    x.i = read_byte();
    return x;
}

union bit do_read_word(void) {
    union bit x;
    x.i = read_word();
    return x;
}

void do_write_byte(union bit x) {
//    printf("%d\n  ", x.i);
    write_byte(x.i);
}

void do_write_word(union bit x) {
    write_word(x.i);
}

