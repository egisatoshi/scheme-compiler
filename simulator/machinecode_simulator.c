#include "machinecode_simulator.h"

int main(int argc, char **argv) {
    machinecode *program = xmalloc(sizeof(machinecode) * PROGRAM_SIZE);
    int pc = 0;
    int convert = 0;
    int halt = 0;
    union bit *reg = xmalloc(sizeof(union bit) * REG_SIZE);
    union bit *sram = xmalloc(sizeof(union bit) * SRAM_SIZE);
    FILE *fp;
    int i;
    int flag = 0;
    int count = 0;
    char *value = xmalloc(WORD_SIZE);

    for(i=0; i<REG_SIZE; i++) {
        reg[i].f = 0.0;
    }

    for(i=0; i<SRAM_SIZE; i++) {
        sram[i].f = 0.0;
    }

    if((fp = fopen(argv[1], "rb"))) {
        while(fread(value, 1, 4, fp) > 0) {
            if(flag == 0) {
                load_program(value, program, count, 0);
                flag = 1;
            }else {
                load_program(value, program, count, 1);
                count++;
                flag = 0;
            }
        }
    }else {
        fprintf(stderr, "cannot open : %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    fclose(fp);

    run_machinecode(program, &pc, &convert, &halt, reg, sram);
    
    exit(EXIT_SUCCESS);
}

void load_program(char *value, machinecode *program, int count, int flag) {
    unsigned int tmp;

    if(flag == 0) {
        program[count].info = (value[0] & 0xe0) / 0x20;
        program[count].opcode = value[0] & 0x1f;
        program[count].dr = value[1] & 0x0000000ff;
        program[count].sr1 = value[2] & 0xff;
        program[count].sr2 = value[3] & 0xff;
    }else {
        /* imd is not always positive */
        tmp = (unsigned char)value[0] * 0x01000000 +
              (unsigned char)value[1] * 0x00010000 +
              (unsigned char)value[2] * 0x00000100 +
              (unsigned char)value[3];
        program[count].imd.i = (int)tmp;
    }

}

void run_machinecode(machinecode *program, int *pc, int *convert, int *halt, union bit *reg, union bit *sram) {

    unsigned long long int total_command_count = 0;
    unsigned long long int system_command_count = 0;
    unsigned long long int io_command_count = 0;
    unsigned long long int memory_command_count = 0;
    unsigned long long int jump_command_count = 0;
    unsigned long long int cmp_command_count = 0;
    unsigned long long int scu_command_count = 0;
    unsigned long long int mcu_command_count = 0;

    for(;;) {
        switch(program[*pc].info) {
        case 0: /* SYS */
            run_system_operation(program, pc, halt);
            system_command_count++;
            break;
        case 1: /* IO */
            run_io_operation(program, pc, convert, reg);
            io_command_count++;
            break;
        case 2: /* MEM */
            run_memory_operation(program, pc, convert, reg, sram);
            memory_command_count++;
            break;
        case 3: /* JUMP */
            run_jump_operation(program, pc, convert, reg);
            jump_command_count++;
            break;
        case 4: /* CMP */
            run_cmp_operation(program, pc, convert, reg);
            cmp_command_count++;
            break;
        case 5: /* SCU */
            run_scu_operation(program, pc, convert, reg);
            scu_command_count++;
            break;
        case 6: /* MCU */
            run_mcu_operation(program, pc, convert, reg);
            mcu_command_count++;
           break;
        default: 
            fprintf(stderr, "%d%d : unknown instruction\n", program[*pc].info, program[*pc].opcode);
            exit(EXIT_FAILURE);
        }

        total_command_count++;
       
        if(*halt == 1) {
            fprintf(stderr, "\n=== Simulator Report ===\n");
            fprintf(stderr, "Command Count        : %llu\n", total_command_count);
            fprintf(stderr, "SYStem Command Count : %llu\n", system_command_count);
            fprintf(stderr, "IO Command Count     : %llu\n", io_command_count);
            fprintf(stderr, "MEMory Command Count : %llu\n", memory_command_count);
            fprintf(stderr, "JUMP Command Count   : %llu\n", jump_command_count);
            fprintf(stderr, "CMP Command Count    : %llu\n", cmp_command_count);
            fprintf(stderr, "SCU Command Count    : %llu\n", scu_command_count);
            fprintf(stderr, "MCU Command Count    : %llu\n", mcu_command_count);
            fprintf(stderr, "Used Memory          : %d\n", (reg_ref(reg, 240, convert).i-65536));
            break;
        }
    }
}

