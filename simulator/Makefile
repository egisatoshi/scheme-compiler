#!/bin/sh

CC = gcc
CFLAGS = -g -Wall
TARGET = machinecode_simulator
OBJS = utility.o system_operation.o io_operation.o memory_operation.o jump_operation.o cmp_operation.o scu_operation.o mcu_operation.o machinecode_simulator.o


.SUFFIXES: .c .o

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $^

.c.o:
	$(CC) $(CFLAGS) -c $<

.PHONY: clean
clean:
	$(RM) $(TARGET) $(OBJS)


utility.o: machinecode_simulator.h
system_operation.o: machinecode_simulator.h
io_operation.o: machinecode_simulator.h
memory_operation.o: machinecode_simulator.h
jump_operation.o: machinecode_simulator.h
cmp_operation.o: machinecode_simulator.h
scu_operation.o: machinecode_simulator.h
mcu_operation.o: machinecode_simulator.h
machinecode_simulator.o: machinecode_simulator.h



