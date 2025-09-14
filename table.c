#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table)
{
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table *table)
{
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

/*
Find an entry in the table
- If the key is in the table, this function return the entry corresponding to the key.
- Otherwise, it returns an empty or tombstone entry, whose key is `NULL`.

Note that this function always returns an entry since there is always an empty entry
due to the load factor.
*/
static Entry *findEntry(Entry *entries, int capacity, ObjString *key)
{
    uint32_t index = key->hash % capacity;
    Entry *tombstone = NULL;

    for (;;)
    {
        Entry *entry = &entries[index];
        if (entry->key == NULL)
        {
            if (IS_NIL(entry->value))
            {
                // Empty entry
                return tombstone != NULL ? tombstone : entry;
            }
            else
            {
                // Tombstone entry
                if (tombstone == NULL)
                    tombstone = entry;
            }
        }
        else if (entry->key == key)
        {
            // Found the key
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

/*
Resizes the table and re-enters all existing key-value pairs
*/
static void adjustCapacity(Table *table, int capacity)
{
    Entry *entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // Copy existing k-v pairs into new array (except tombstones)
    table->count = 0;
    for (int i = 0; i < table->capacity; i++)
    {
        Entry *entry = &table->entries[i];
        if (entry->key == NULL)
            continue;

        Entry *dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

/*
Retrieve a value from the table
- Returns false if the requested key was not found, true otherwise.
- If true, the retrieved value is stored in the pointer passed to the function.
*/
bool tableGet(Table *table, ObjString *key, Value *value)
{
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    *value = entry->value;
    return true;
}

/*
Insert a value into the table

Returns `true` if the key was newly inserted, false otherwise.
*/
bool tableSet(Table *table, ObjString *key, Value value)
{
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD)
    {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry *entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // We only want to increment when filling an empty entry, not reusing a tombstone
    if (isNewKey && IS_NIL(entry->value))
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

/*
Delete a key-value pair from the table
- We use a tombstoning approach: the tombstone entry is `(NULL, true)` (
recall that the empty entry is `(NULL, nil)`).
- Returns false if key was not found.
- Returns true if key was successfully deleted.
*/
bool tableDelete(Table *table, ObjString *key)
{
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    // Place tombstone in deleted entry
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

/*
Copy all key-value pairs from one table to another
*/
void tableAddAll(Table *from, Table *to)
{
    for (int i = 0; i < from->capacity; i++)
    {
        Entry *entry = &from->entries[i];
        if (entry->key != NULL)
        {
            tableSet(to, entry->key, entry->value);
        }
    }
}

/*
Find an entry whose key is a particular string
- Returns `NULL` if the key is not found.
*/
ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash)
{
    if (table->count == 0)
        return NULL;

    uint32_t index = hash % table->capacity;
    for (;;)
    {
        Entry *entry = &table->entries[index];
        if (entry->key == NULL)
        {
            // If the entry is empty, we've failed to find the string.
            // Otherwise, it's a tombstone which we can skip over.
            if (IS_NIL(entry->value))
                return NULL;
        }
        else if (entry->key->length == length && entry->key->hash == hash && memcmp(entry->key->chars, chars, length) == 0)
        {
            // We've found the string.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}

/*
Mark entries of a table still in use as GC roots
*/
void markTable(Table *table)
{
    for (int i = 0; i < table->capacity; i++)
    {
        Entry *entry = &table->entries[i];
        markObject((Obj *)entry->key);
        markValue(entry->value);
    }
}