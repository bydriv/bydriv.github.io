#include <stdint.h>

typedef struct closure closure_t;
typedef struct stack stack_t;
typedef struct env env_t;

struct stack {
  uintptr_t count;
  closure_t *values;
};

struct env {
  uintptr_t count;
  closure_t *values;
};

struct closure {
  closure_t *(*code)(env_t, stack_t);
  env_t env;
};
