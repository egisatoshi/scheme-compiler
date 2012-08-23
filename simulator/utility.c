#include "machinecode_simulator.h"

int read_byte(void) {
    return getchar();
}

void write_byte(int x) {
    putchar(x);
    fflush(stdout);
}

int read_word(void) {
    int b0, b1, b2, b3;

    b0 = read_byte();
    b1 = 0x00000100 * read_byte();
    b2 = 0x00010000 * read_byte();
    b3 = 0x01000000 * read_byte();

    return b3 + b2 + b1 + b0;
}

void write_word(int x) {
    int b0, b1, b2, b3;

    b0 = x & 0x000000ff;
    b1 = (x & 0x0000ff00) / 0x00000100;
    b2 = (x & 0x00ff0000) / 0x00010000;
    b2 = (x & 0xff000000) / 0x01000000;

    write_byte(b3);
    fflush(stdout);
    write_byte(b2);
    fflush(stdout);
    write_byte(b1);
    fflush(stdout);
    write_byte(b0);
    fflush(stdout);
}

void reg_set(union bit *reg, int reg_num, union bit value, int *convert) {
    if(((reg_num >= 0) && (reg_num < 32)) && (*convert == 0)) {
        reg[reg_num] = value;
    }else if(((reg_num >= 32) && (reg_num < 64)) && (*convert == 1)) {
        reg[reg_num-32] = value;
    }else if(((reg_num >= 0) && (reg_num < 32)) && (*convert == 1)) {
        reg[reg_num+32] = value;
    }else if(((reg_num >= 32) && (reg_num < 64)) && (*convert == 0)) {
        reg[reg_num] = value;
    }else if((reg_num >= 64) && (reg_num < 256)) {
        reg[reg_num] = value;
    }else {
        fprintf(stderr, "%d : reg_set error\n", reg_num);
        exit(EXIT_FAILURE);
    }
}

union bit reg_ref(union bit *reg, int reg_num, int *convert) {
    if(((reg_num >= 0) && (reg_num < 32)) && (*convert == 0)) {
        return reg[reg_num];
    }else if(((reg_num >= 32) && (reg_num < 64)) && (*convert == 1)) {
        return reg[reg_num-32];
    }else if(((reg_num >= 0) && (reg_num < 32)) && (*convert == 1)) {
        return reg[reg_num+32];
    }else if(((reg_num >= 32) && (reg_num < 64)) && (*convert == 0)) {
        return reg[reg_num];
    }else if((reg_num >= 64) && (reg_num < 256)) {
        return reg[reg_num];
    }else {
        fprintf(stderr, "%d : reg_ref error\n", reg_num);
        exit(EXIT_FAILURE);
    }
}

void reg_print(union bit *reg, int reg_num, int *convert) {
    if(((reg_num >= 0) && (reg_num < 32)) && (*convert == 0)) {
        printf("i%d : %d|%f ", reg_num, reg[reg_num].i, reg[reg_num].f);
    }else if(((reg_num >= 32) && (reg_num < 64)) && (*convert == 1)) {
        printf("i%d : %d|%f ", (reg_num-32), reg[reg_num-32].i, reg[reg_num-32].f);
    }else if(((reg_num >= 0) && (reg_num < 32)) && (*convert == 1)) {
        printf("o%d : %d|%f ", reg_num, reg[reg_num+32].i, reg[reg_num+32].f);
    }else if(((reg_num >= 32) && (reg_num < 64)) && (*convert == 0)) {
        printf("o%d : %d|%f ", (reg_num-32), reg[reg_num].i, reg[reg_num].f);
    }else if((reg_num >= 64) && (reg_num < 128)) {
        printf("g%d : %d|%f ", (reg_num-64), reg[reg_num].i, reg[reg_num].f);
    }else if((reg_num >= 128) && (reg_num < 224)) {
        printf("v%d : %d|%f ", (reg_num-128), reg[reg_num].i, reg[reg_num].f);
    }else if((reg_num >= 224) && (reg_num < 240)) {
        printf("h%d : %d|%f ", (reg_num-224), reg[reg_num].i, reg[reg_num].f);
    }else if((reg_num >= 240) && (reg_num < 256)) {
        printf("s%d : %d|%f ", (reg_num-240), reg[reg_num].i, reg[reg_num].f);
    }else {
        fprintf(stderr, "%d : reg_print error\n", reg_num);
        exit(EXIT_FAILURE);
    }
}

void sram_set(union bit *sram, union bit sram_num1, union bit sram_num2, union bit value) {
    int sram_num = sram_num1.i + sram_num2.i;
    if(sram_num < SRAM_SIZE) {
        sram[sram_num] = value;
    }else {
        fprintf(stderr, "%d : sram_set error\n", sram_num);
        exit(EXIT_FAILURE);
    }
}

union bit sram_ref(union bit *sram, union bit sram_num1, union bit sram_num2) {
    int sram_num = sram_num1.i + sram_num2.i;
    if(sram_num < SRAM_SIZE) {
        union bit z = sram[sram_num];
        return z;
    }else {
        fprintf(stderr, "%d : sram_ref error\n", sram_num);
        exit(EXIT_FAILURE);
    }
}

void *xmalloc(size_t size) {
    void *p = malloc(size);

    if(!p) {
        fprintf(stderr, "cannot allocate memory\n");
        exit(EXIT_FAILURE);
    }

    return p;
}

