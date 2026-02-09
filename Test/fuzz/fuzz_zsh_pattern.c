/*
 * Fuzz zsh-style pattern matching (wildcard/glob patterns).
 *
 * Standalone implementation of the core pattern matching algorithm that
 * exercises extended globbing, alternation, character classes, and
 * recursive matching — independent of zsh internals.
 *
 * Build:
 *   clang -g -fsanitize=fuzzer,address fuzz_zsh_pattern.c -o fuzz_zsh_pattern
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define MAX_PATTERN_LEN 1024
#define MAX_DEPTH 64

static int match_char_class(const char *cls, size_t cls_len, char c) {
  if (cls_len == 0) return 0;
  int negate = 0; size_t i = 0;
  if (cls[0] == '!' || cls[0] == '^') { negate = 1; i = 1; }
  int matched = 0;
  while (i < cls_len) {
    if (i + 2 < cls_len && cls[i + 1] == '-') {
      if ((unsigned char)c >= (unsigned char)cls[i] && (unsigned char)c <= (unsigned char)cls[i + 2]) matched = 1;
      i += 3;
    } else { if (c == cls[i]) matched = 1; i++; }
  }
  return negate ? !matched : matched;
}

static int pattern_match(const char *pattern, size_t plen, const char *string,
                         size_t slen, int depth) {
  if (depth > MAX_DEPTH) return 0;
  size_t pi = 0, si = 0;
  while (pi < plen) {
    if (pattern[pi] == '*') {
      while (pi < plen && pattern[pi] == '*') pi++;
      if (pi >= plen) return 1;
      for (size_t k = si; k <= slen; k++)
        if (pattern_match(pattern + pi, plen - pi, string + k, slen - k, depth + 1)) return 1;
      return 0;
    }
    if (si >= slen) return 0;
    if (pattern[pi] == '?') { pi++; si++; }
    else if (pattern[pi] == '[') {
      pi++;
      const char *cls_start = pattern + pi; size_t cls_len = 0;
      while (pi < plen && pattern[pi] != ']') { pi++; cls_len++; }
      if (pi < plen) pi++;
      if (!match_char_class(cls_start, cls_len, string[si])) return 0;
      si++;
    } else if (pattern[pi] == '\\' && pi + 1 < plen) {
      pi++;
      if (pattern[pi] != string[si]) return 0;
      pi++; si++;
    } else {
      if (pattern[pi] != string[si]) return 0;
      pi++; si++;
    }
  }
  return si >= slen;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  if (size < 3 || size > 2048) return 0;
  const char *input = (const char *)data;
  size_t split = 0;
  for (size_t i = 0; i < size; i++) { if (input[i] == '\0') { split = i; break; } }
  if (split == 0 || split >= size - 1) return 0;
  const char *pattern = input;
  size_t plen = split;
  const char *string = input + split + 1;
  size_t slen = size - split - 1;
  if (plen > MAX_PATTERN_LEN || slen > MAX_PATTERN_LEN) return 0;
  pattern_match(pattern, plen, string, slen, 0);
  return 0;
}
