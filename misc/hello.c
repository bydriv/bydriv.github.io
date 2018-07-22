#include "implementing-lambda-calculus.h"

closure_t *hello(env_t, stack_t);

closure_t *grab(env_t *env, stack_t *stack) {
  stack->count -= sizeof(closure_t);
  closure_t arg = stack->values[stack->count / sizeof(closure_t)];
  return arg.code(arg.env, *stack);
}

closure_t *seq(env_t env, stack_t stack) {
  grab(&env, &stack);
  return grab(&env, &stack);
}

closure_t *putc(env_t env, stack_t stack) {
  long n = grab(&env, &stack);
  putchar(n);
  return 0;
}

closure_t *O(env_t env, stack_t stack) {
  return 0;
}

closure_t *S(env_t env, stack_t stack) {
  long n = grab(&env, &stack);
  return n + 1;
}

int main(int argc, char *argv[]) {
  /* States */

  env_t env;
  stack_t stack;

  env.count = 0;
  env.values = malloc(256);
  stack.count = 0;
  stack.values = malloc(256);

  /* Primitives */

  closure_t prim_seq;
  prim_seq.code = seq;
  prim_seq.env = env;

  closure_t prim_putc;
  prim_putc.code = putc;
  prim_putc.env = env;

  closure_t prim_O;
  prim_O.code = O;
  prim_O.env = env;

  closure_t prim_S;
  prim_S.code = S;
  prim_S.env = env;

  stack.count = 4 * sizeof(closure_t);
  stack.values[0] = prim_S;
  stack.values[1] = prim_O;
  stack.values[2] = prim_putc;
  stack.values[3] = prim_seq;

  hello(env, stack);
}
