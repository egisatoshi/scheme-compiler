#include "machinecode_simulator.h"

void run_memory_operation(machinecode *program, int *pc, int *convert, union bit *reg, union bit *sram) {
    switch(program[*pc].opcode) {
    case (0 + IMD_SIZE):  /* load reg */
        reg_set(reg, program[*pc].dr, sram_ref(sram,reg_ref(reg,program[*pc].sr1,convert),reg_ref(reg,program[*pc].sr2,convert)), convert);
        (*pc)++;
        break;
    case (1 + IMD_SIZE): /* load imd */
       reg_set(reg, program[*pc].dr, sram_ref(sram,reg_ref(reg,program[*pc].sr1,convert),program[*pc].imd), convert);
        (*pc)++;
        break;
    case (2 + IMD_SIZE): /* store */
        sram_set(sram, reg_ref(reg,program[*pc].sr1,convert), program[*pc].imd, reg_ref(reg,program[*pc].sr2,convert));
        (*pc)++;
        break;
    default:
        fprintf(stderr, "%2x : unknown instruction\n", (program[*pc].info)+(program[*pc].opcode));
        exit(EXIT_FAILURE);
    }
}

