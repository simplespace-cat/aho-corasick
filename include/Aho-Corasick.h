#ifndef AHOCORASICK_CAPI_H
#define AHOCORASICK_CAPI_H

#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum {
  NoncontiguousNFA = 0,
  ContiguousNFA = 1,
  DFA = 2,
} ACAutomatonKind;

typedef enum {
  Standard = 0,
  LeftmostFirst = 1,
  LeftmostLongest = 2,
} ACMatchKind;

typedef enum {
  AC_OK = 0,
  AC_BUILD_ERROR = 1,
  AC_MATCH_ERROR = 2,
  AC_INVALID_ARGUMENT = 3,
  AC_PANIC = 100,
} ACResult;

typedef enum {
  Unanchored = 0,
  Anchored = 1,
  Both = 2,
} ACStartKind;

typedef struct AC AC;

typedef struct ACBuilder ACBuilder;

typedef struct ACOverlappingState ACOverlappingState;

typedef struct {
  uint32_t pattern;
  size_t start;
  size_t end;
} ACMatch;

/**
 * Create a new builder with default configuration.
 */
ACBuilder *ac_builder_create(void);

/**
 * Free a builder previously returned by `ac_builder_create`.
 */
void ac_builder_free(ACBuilder *builder);

/**
 * Enable or disable ASCII-aware case-insensitivity.
 */
void ac_builder_set_ascii_case_insensitive(ACBuilder *builder, bool yes);

/**
 * Set the match semantics (standard/leftmost-first/leftmost-longest).
 */
void ac_builder_set_match_kind(ACBuilder *builder, ACMatchKind kind);

/**
 * Set the supported start kind (unanchored by default).
 */
void ac_builder_set_start_kind(ACBuilder *builder, ACStartKind kind);

/**
 * Build an automaton using the given builder and pattern set.
 *
 * On success, returns `AC_OK` and writes a non-null handle to `*out_ac`.
 * On failure, returns a non-zero error code and optionally writes an
 * error message buffer to `*err_msg`/`*err_len` (must be freed with `ac_str_free`).
 */
ACResult ac_builder_build(ACBuilder *builder,
                          const uint8_t *const *patterns,
                          const size_t *pattern_lens,
                          size_t patterns_len,
                          AC **out_ac,
                          char **err_msg,
                          size_t *err_len);

/**
 * Convenience: build an automaton with default configuration.
 */
ACResult ac_new(const uint8_t *const *patterns,
                const size_t *pattern_lens,
                size_t patterns_len,
                AC **out_ac,
                char **err_msg,
                size_t *err_len);

/**
 * Free an automaton handle created by `ac_new` or `ac_builder_build`.
 */
void ac_free(AC *ac);

/**
 * Return which Aho-Corasick implementation was chosen internally.
 */
ACAutomatonKind ac_kind(const AC *ac);

/**
 * Return the match kind used by this automaton.
 */
ACMatchKind ac_match_kind(const AC *ac);

/**
 * Return the start kind supported by this automaton (as configured).
 */
ACStartKind ac_start_kind(const AC *ac);

/**
 * Return the number of patterns compiled into this automaton.
 */
size_t ac_patterns_len(const AC *ac);

/**
 * Return the minimum pattern length in this automaton.
 */
size_t ac_min_pattern_len(const AC *ac);

/**
 * Return the maximum pattern length in this automaton.
 */
size_t ac_max_pattern_len(const AC *ac);

/**
 * Return the approximate heap memory usage (bytes).
 */
size_t ac_memory_usage(const AC *ac);

/**
 * Test whether any match exists in `haystack`.
 *
 * Writes true/false to `*out_found`. On configuration errors, returns
 * `AC_MATCH_ERROR` with a message.
 */
ACResult ac_is_match(const AC *ac,
                     const uint8_t *haystack,
                     size_t haystack_len,
                     bool *out_found,
                     char **err_msg,
                     size_t *err_len);

/**
 * Find the first match. If found, writes it to `*out_match` and sets
 * `*out_found = true`. Otherwise, `*out_found = false`.
 *
 * `earliest` can be set to true to return as soon as any match is known
 * to occur (useful with leftmost semantics). `anchored` selects whether
 * to run an anchored search.
 */
ACResult ac_find(const AC *ac,
                 const uint8_t *haystack,
                 size_t haystack_len,
                 bool earliest,
                 bool anchored,
                 ACMatch *out_match,
                 bool *out_found,
                 char **err_msg,
                 size_t *err_len);

/**
 * Find all non-overlapping matches and return them as an array of ACMatch.
 * The array must be freed with `ac_matches_free`.
 */
ACResult ac_find_all(const AC *ac,
                     const uint8_t *haystack,
                     size_t haystack_len,
                     bool anchored,
                     ACMatch **out_matches,
                     size_t *out_len,
                     char **err_msg,
                     size_t *err_len);

/**
 * Prefix check: does any pattern match at the beginning of `haystack`?
 *
 * This runs an anchored search from the start (offset 0) and sets
 * `*out_found` accordingly. This respects the automaton's configured
 * match semantics:
 * - Standard: first match seen is returned
 * - LeftmostFirst: first by pattern priority
 * - LeftmostLongest: longest among those starting at 0
 */
ACResult ac_is_prefix(const AC *ac,
                      const uint8_t *haystack,
                      size_t haystack_len,
                      bool *out_found,
                      char **err_msg,
                      size_t *err_len);

/**
 * Prefix match: if a match starts at 0, write it to `*out_match` and set `*out_found = true`.
 */
ACResult ac_find_prefix(const AC *ac,
                        const uint8_t *haystack,
                        size_t haystack_len,
                        ACMatch *out_match,
                        bool *out_found,
                        char **err_msg,
                        size_t *err_len);

/**
 * Suffix check: does any pattern match and end at the end of `haystack`?
 *
 * Notes:
 * - Works for all match kinds.
 * - With MatchKind::Standard, we enumerate overlapping matches anchored at
 *   candidate starts near the end to ensure we can detect longer suffixes.
 * - With LeftmostLongest, this naturally finds the longest suffix.
 * - With LeftmostFirst, suffix existence depends on pattern priority when
 *   multiple matches start at the same position.
 */
ACResult ac_is_suffix(const AC *ac,
                      const uint8_t *haystack,
                      size_t haystack_len,
                      bool *out_found,
                      char **err_msg,
                      size_t *err_len);

/**
 * Suffix match: if a match ends at the end of `haystack`, write it to `*out_match`.
 *
 * Preference:
 * - When MatchKind::Standard, returns the longest suffix (by iterating candidate
 *   start positions from earlier to later and enumerating overlapping matches).
 * - When MatchKind::LeftmostLongest, returns the longest suffix.
 * - When MatchKind::LeftmostFirst, returns the suffix chosen by pattern priority
 *   at its start position (if any).
 */
ACResult ac_find_suffix(const AC *ac,
                        const uint8_t *haystack,
                        size_t haystack_len,
                        ACMatch *out_match,
                        bool *out_found,
                        char **err_msg,
                        size_t *err_len);

/**
 * Create a new overlapping state object.
 */
ACOverlappingState *ac_overlapping_state_create(void);

/**
 * Reset an overlapping state to its initial value.
 */
void ac_overlapping_state_reset(ACOverlappingState *state);

/**
 * Free an overlapping state object.
 */
void ac_overlapping_state_free(ACOverlappingState *state);

/**
 * Perform one step of overlapping search. On success, sets `*out_found`
 * to true and writes the match to `*out_match` if a match is available.
 * Otherwise, sets `*out_found = false`.
 *
 * Only supported when the automaton uses Standard semantics.
 */
ACResult ac_find_overlapping_step(const AC *ac,
                                  const uint8_t *haystack,
                                  size_t haystack_len,
                                  ACOverlappingState *state,
                                  ACMatch *out_match,
                                  bool *out_found,
                                  char **err_msg,
                                  size_t *err_len);

/**
 * Replace all matches with the corresponding replacement at the same index.
 * The replacements array length must equal `ac_patterns_len(ac)`.
 * On success, writes an owned buffer to `*out_ptr` and length to `*out_len`.
 * Free the buffer with `ac_buf_free`.
 */
ACResult ac_replace_all_bytes(const AC *ac,
                              const uint8_t *haystack,
                              size_t haystack_len,
                              const uint8_t *const *replacements,
                              const size_t *repl_lens,
                              size_t replacements_len,
                              uint8_t **out_ptr,
                              size_t *out_len,
                              char **err_msg,
                              size_t *err_len);

/**
 * Free a buffer returned by this API (e.g., from `ac_replace_all_bytes`).
 */
void ac_buf_free(uint8_t *ptr, size_t len);

/**
 * Free an error/message buffer returned by this API.
 */
void ac_str_free(char *ptr, size_t len);

/**
 * Free a match array returned by this API.
 */
void ac_matches_free(ACMatch *ptr, size_t len);

#endif  /* AHOCORASICK_CAPI_H */
