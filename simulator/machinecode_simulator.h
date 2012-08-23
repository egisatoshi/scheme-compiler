#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#define PROGRAM_SIZE 524288
#define REG_SIZE 256
#define SRAM_SIZE 1048576
#define WORD_SIZE 4
#define IMD_SIZE 16
#define TRUE 0xffffffff
#define FALSE 0


/* structure */

union bit{
    int i;
    float f;
};

typedef struct {
    unsigned char info;
    unsigned char opcode;
    unsigned char dr;
    unsigned char sr1;
    unsigned char sr2;
    union bit imd;
}machinecode;

/* utility.c */

int read_byte(void);

void write_byte(int x);

int read_word(void);

void write_word(int x);

void reg_set(union bit *reg, int reg_num, union bit value, int *convert);

union bit reg_ref(union bit *reg, int reg_num, int *convert);

void reg_print(union bit *reg, int reg_num, int *convert);

void sram_set(union bit *sram, union bit sram_num1, union bit sram_num2, union bit value);

union bit sram_ref(union bit *sram, union bit sram_num1, union bit sram_num2);

void *xmalloc(size_t size);

/* system_operation.c */

void run_system_operation(machinecode *program, int *pc, int *halt);

/* io_operation.c */

void run_io_operation(machinecode *program, int *pc, int *convert, union bit *reg);

union bit do_read_byte(void);

union bit do_read_word(void);

void do_write_byte(union bit x);

void do_write_word(union bit x);

/* memory_operation.c */

void run_memory_operation(machinecode *program, int *pc, int *convert, union bit *reg, union bit *sram);

/* jump_operation.c */

void run_jump_operation(machinecode *program, int *pc, int *convert, union bit *reg);

void do_if(union bit x, int *pc);

void do_goto(union bit x, int *pc);

void do_call(union bit x, int *pc, int *convert);

/* cmp_operation.c */

void run_cmp_operation(machinecode *program, int *pc, int *convert, union bit *reg);

union bit do_equal(union bit x, union bit y);

union bit do_equal_dot(union bit x, union bit y);

union bit do_greater(union bit x, union bit y);

union bit do_greater_dot(union bit x, union bit y);

union bit do_less(union bit x, union bit y);

union bit do_less_dot(union bit x, union bit y);

union bit do_greater_equal(union bit x, union bit y);

union bit do_greater_equal_dot(union bit x, union bit y);

union bit do_less_equal(union bit x, union bit y);

union bit do_less_equal_dot(union bit x, union bit y);

/* scu_operation.c */

void run_scu_operation(machinecode *program, int *pc, int *convert, union bit *reg);

union bit do_plus(union bit x, union bit y);

union bit do_minus(union bit x, union bit y);

union bit do_xor(union bit x, union bit y);

union bit do_or(union bit x, union bit y);

union bit do_and(union bit x, union bit  y);

union bit do_not(union bit x);

/* mcu_operation.c */

void run_mcu_operation(machinecode *program, int *pc, int *convert, union bit *reg);

union bit do_sll(union bit x, union bit y);

union bit do_srl(union bit x, union bit y);

union bit do_mult(union bit x, union bit y);

union bit do_div(union bit x, union bit y);

union bit do_modulo(union bit x, union bit y);

union bit do_plus_dot(union bit x, union bit y);

union bit do_minus_dot(union bit x, union bit y);

union bit do_mult_dot(union bit x, union bit y);

union bit do_div_dot(union bit x, union bit y);

union bit do_itof(union bit x);

union bit do_ftoi(union bit x);

/* simulator.c */

void load_program(char *value, machinecode *program, int count, int flag);

void run_machinecode(machinecode *program, int *pc, int *convert, int *halt, union bit *reg, union bit *sram);

