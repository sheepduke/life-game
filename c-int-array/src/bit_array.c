#include "bit_array.h"

#include <stdlib.h>
#include <assert.h>

struct bit_array {
  unsigned int size;
  uint8_t *content;
};

bit_array_size size_to_trunk_id(bit_array_size size) {
  return (size / 8) + (size % 8 == 0 ? 0 : 1);
}

bit_array bit_array_new(bit_array_size size) {
  bit_array this = malloc(sizeof(struct bit_array));
  this->size = size;
  this->content = malloc(size_to_trunk_id(size));
  
  return this;
}

void bit_array_destroy(bit_array this) {
  bit_array array = this;
  if (array) {
    free(array->content);
    free(array);
  }
}

uint8_t bit_array_get(bit_array this, bit_array_size index) {
  assert(index < this->size);
  bit_array_size trunk_id = size_to_trunk_id(index);
  return (this->content[trunk_id] >> (index % 8)) & 1;
}

void bit_array_set(bit_array this, bit_array_size index, uint8_t value) {
  assert(index < this->size);
  assert(value == 1 || value == 0);
  bit_array_size trunk_id = size_to_trunk_id(index);
  uint8_t trunk = this->content[trunk_id];
  uint8_t trunk_index = index % 8;
  trunk ^= (-value ^ trunk) & (1UL << trunk_index);
  this->content[trunk_id] = trunk;
}
