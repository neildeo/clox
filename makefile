clox: main.o chunk.o memory.o debug.o value.o vm.o scanner.o compiler.o
	gcc -o clox main.o chunk.o memory.o debug.o value.o vm.o scanner.o compiler.o

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

vm.o: vm.c vm.h common.h compiler.h debug.h 
	gcc -c vm.c

scanner.o: scanner.c scanner.h common.h
	gcc -c scanner.c

compiler.o: compiler.c compiler.h common.h scanner.h debug.h
	gcc -c compiler.c