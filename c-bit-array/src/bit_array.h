#ifndef BIT_ARRAY_H
#define BIT_ARRAY_H

#include <stdint.h>

typedef struct bit_array *bit_array;

typedef unsigned int bit_array_size;

bit_array bit_array_new(bit_array_size size);

uint8_t bit_array_get(bit_array this, bit_array_size index);

void bit_array_set(bit_array this, bit_array_size index, uint8_t value);

void bit_array_destroy(bit_array this);

#endif
