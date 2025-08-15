clox: main.o chunk.o memory.o debug.o value.o vm.o
	gcc -o clox main.o chunk.o memory.o debug.o value.o vm.o

main.o: main.c common.h chunk.h
	gcc -c main.c

chunk.o: chunk.c chunk.h memory.h common.h
	gcc -c chunk.c

memory.o: memory.c memory.h common.h
	gcc -c memory.c

debug.o: debug.c debug.h value.h
	gcc -c debug.c

value.o: value.c value.h memory.h common.h
	gcc -c value.c

vm.o: vm.c vm.h common.h debug.h
	gcc -c vm.c