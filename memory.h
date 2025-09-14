#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"

#define ALLOCATE(type, count) \
    (type *)reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount)      \
    (type *)reallocate(pointer, sizeof(type) * (oldCount), \
                       sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

/*
Reallocates a block of memory starting at the given pointer based on the `oldSize`
and `newSize` parameters.
- If newSize == 0, it frees the block
- Otherwise, it uses realloc to resize the block. If the reallocation fails, the function
exits the program with exit code 1.
*/
void *reallocate(void *pointer, size_t oldSize, size_t newSize);

/*
Mark a Lox object as a GC root
*/
void markObject(Obj *object);

/*
Mark a Lox value as a GC root
*/
void markValue(Value value);

/*
Run garbage collection
*/
void collectGarbage();

/*
Frees the linked list of objects stored in the VM.
*/
void freeObjects();

#endif