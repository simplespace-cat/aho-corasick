// Minimal smoke test for Aho-Corasick C ABI (new FFI).
// Assumes the header was generated to include/Aho-Corasick.h via cbindgen.
// Compile with: cc -Iinclude smoke.c -L<path-to-lib> -laho_corasick -o smoke
// (or link the static lib directly: -l:libaho_corasick.a)

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include "Aho-Corasick.h"

static void clear_err(char **err_msg, size_t *err_len) {
    if (err_msg && *err_msg) {
        ac_str_free(*err_msg, *err_len);
        *err_msg = NULL;
        if (err_len) *err_len = 0;
    }
}

static void expect_ok(ACResult rc, const char *ctx, char **err_msg, size_t *err_len) {
    if (rc != AC_OK) {
        if (err_msg && *err_msg) {
            fprintf(stderr, "%s failed: rc=%d msg=%.*s\n",
                    ctx, rc, (int)(*err_len), *err_msg);
        } else {
            fprintf(stderr, "%s failed: rc=%d\n", ctx, rc);
        }
        clear_err(err_msg, err_len);
        // Fail fast for a smoke test
        // You can change this to return non-zero instead of exit.
        exit(2);
    }
}

int main(void) {
    char *err_msg = NULL;
    size_t err_len = 0;

    // 1) Case-sensitive handle with patterns: "log", "keep"
    const uint8_t *pats1[] = {
        (const uint8_t*)"log",
        (const uint8_t*)"keep"
    };
    const size_t lens1[] = { 3, 4 };

    AC *ac = NULL;
    ACResult rc = ac_new(pats1, lens1, 2, &ac, &err_msg, &err_len);
    expect_ok(rc, "ac_new(cs)", &err_msg, &err_len);

    bool found = false;

    const char *s1 = "hello.log";
    rc = ac_is_match(ac, (const uint8_t*)s1, strlen(s1), &found, &err_msg, &err_len);
    expect_ok(rc, "ac_is_match(s1)", &err_msg, &err_len);
    printf("%s -> found=%d (expect 1)\n", s1, found ? 1 : 0);

    const char *s2 = "KEEP.TXT";
    rc = ac_is_match(ac, (const uint8_t*)s2, strlen(s2), &found, &err_msg, &err_len);
    expect_ok(rc, "ac_is_match(s2)", &err_msg, &err_len);
    printf("%s -> found=%d (expect 0 case-sensitive)\n", s2, found ? 1 : 0);

    const char *s3 = "nothing-here";
    rc = ac_is_match(ac, (const uint8_t*)s3, strlen(s3), &found, &err_msg, &err_len);
    expect_ok(rc, "ac_is_match(s3)", &err_msg, &err_len);
    printf("%s -> found=%d (expect 0)\n", s3, found ? 1 : 0);

    ac_free(ac);
    ac = NULL;

    // 2) Case-insensitive handle with pattern: "snapple"
    ACBuilder *b = ac_builder_create();
    if (!b) {
        fprintf(stderr, "ac_builder_create failed\n");
        return 3;
    }
    ac_builder_set_ascii_case_insensitive(b, true);

    const uint8_t *pats2[] = { (const uint8_t*)"snapple" };
    const size_t lens2[] = { 7 };

    AC *ac_ci = NULL;
    rc = ac_builder_build(b, pats2, lens2, 1, &ac_ci, &err_msg, &err_len);
    ac_builder_free(b);
    expect_ok(rc, "ac_builder_build(ci)", &err_msg, &err_len);

    const char *s4 = "Snapple Zero";
    rc = ac_is_match(ac_ci, (const uint8_t*)s4, strlen(s4), &found, &err_msg, &err_len);
    expect_ok(rc, "ac_is_match(ci)", &err_msg, &err_len);
    printf("%s -> found=%d (expect 1)\n", s4, found ? 1 : 0);

    ac_free(ac_ci);
    ac_ci = NULL;

    clear_err(&err_msg, &err_len);
    return 0;
}