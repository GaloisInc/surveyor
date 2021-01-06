CC=clang
CFLAGS=-emit-llvm -c -g -I../crux-dbg/include

bc = $(patsubst %.c,%.bc,$(wildcard *.c))

all: $(bc)

%.bc : %.c
	$(CC) $(CFLAGS) $< -o $@
