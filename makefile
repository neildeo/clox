clox: main.o chunk.o memory.o debug.o
	gcc -o clox main.o chunk.o memory.o debug.o

main.o: main.c common.h chunk.h
	gcc -c main.c

chunk.o: chunk.c chunk.h memory.h
	gcc -c chunk.c

memory.o: memory.c memory.h
	gcc -c memory.c

debug.o: debug.c debug.h
	gcc -c debug.c