package main

import (
    "errors"
    "fmt"
    "io"
    "log"
    "os"
    "os/exec"
    "path/filepath"
)

const (
    target     = "aarch64-apple-darwin"
    crateName  = "liba_ac"
    headerName = "ac.h"
    libName    = "libac.a"
)

func main() {
    log.SetFlags(0)

    // Check required tools.
    mustHave("cargo")
    mustHave("cbindgen")
    mustHave("clang")

    // Ensure include/ exists and clean previous artifacts.
    include := "include"
    must(MkdirAll(include, 0o755))
    clean(include)

    // Build static library for macOS arm64.
    say("Building Rust static library (release, %s)...", target)
    must(run("cargo", "build", "--release", "--target", target))

    // Copy lib into include/.
    srcLib := filepath.Join("target", target, "release", libName)
    dstLib := filepath.Join(include, libName)
    must(copyFile(srcLib, dstLib))

    // Generate header via cbindgen into include/.
    headerOut := filepath.Join(include, headerName)
    say("Generating C header via cbindgen...")
    must(run("cbindgen", "--crate", crateName, "--config", "cbindgen.toml", "--output", headerOut))

    // Ensure exe/ and build the smoke test there.
    exeDir := "exe"
    must(MkdirAll(exeDir, 0o755))
    smokeOut := filepath.Join(exeDir, "smoke")
    _ = os.Remove(smokeOut)

    smokeSrc := smokePath()
    say("Compiling smoke test (%s) -> %s...", smokeSrc, smokeOut)
    must(run(
        "clang",
        "-std=c23",
        "-arch", "arm64",
        "-I", include,
        smokeSrc,
        dstLib,
        "-o", smokeOut,
    ))

    // Run the smoke test
    say("Running smoke test...")
    if err := run(smokeOut); err != nil {
        log.Fatalf("smoke test failed: %v", err)
    }
    say("Smoke test PASSED.")

    // Build the real-test tool (CoreFoundation needed). Do not auto-run by default.
    realSrc := realTestPath()
    realOut := filepath.Join(exeDir, "real-test")
    _ = os.Remove(realOut)
    say("Compiling real-test (%s) -> %s...", realSrc, realOut)
    must(run(
        "clang",
        "-std=c23",
        "-arch", "arm64",
        "-I", include,
        realSrc,
        dstLib,
        "-framework", "CoreFoundation",
        "-o", realOut,
    ))

    say("Done.")
    fmt.Printf("Header: %s\n", headerOut)
    fmt.Printf("Library: %s\n", dstLib)
}

func say(format string, a ...any) { log.Printf(format, a...) }

func must(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func mustHave(tool string) {
    if _, err := exec.LookPath(tool); err != nil {
        log.Fatalf("required tool not found: %s (install it and retry)", tool)
    }
}

func run(name string, args ...string) error {
    cmd := exec.Command(name, args...)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    return cmd.Run()
}

func MkdirAll(path string, perm os.FileMode) error {
    if path == "" {
        return errors.New("empty path")
    }
    return os.MkdirAll(path, perm)
}

func clean(include string) {
    // Remove any previous generated artifacts.
    _ = os.Remove(filepath.Join(include, headerName))
    _ = os.Remove(filepath.Join(include, libName))
}

func copyFile(src, dst string) error {
    in, err := os.Open(src)
    if err != nil {
        return fmt.Errorf("open %s: %w", src, err)
    }
    defer in.Close()

    out, err := os.Create(dst)
    if err != nil {
        return fmt.Errorf("create %s: %w", dst, err)
    }
    defer func() {
        _ = out.Close()
    }()

    if _, err := io.Copy(out, in); err != nil {
        return fmt.Errorf("copy %s -> %s: %w", src, dst, err)
    }
    if err := out.Chmod(0o644); err != nil {
        return fmt.Errorf("chmod %s: %w", dst, err)
    }
    return nil
}

// smokePath returns the existing smoke test source path, preferring
// smoke/c_smoke.c, falling back to smoke-test/smoke.c for older layouts.
func smokePath() string {
    p1 := filepath.Join("smoke", "c_smoke.c")
    if _, err := os.Stat(p1); err == nil {
        return p1
    }
    p2 := filepath.Join("smoke-test", "smoke.c")
    if _, err := os.Stat(p2); err == nil {
        return p2
    }
    log.Fatalf("smoke test source not found (looked for %s and %s)", p1, p2)
    return "" // unreachable
}

// realTestPath returns the existing real test source path.
func realTestPath() string {
    p1 := filepath.Join("smoke", "real-test.c")
    if _, err := os.Stat(p1); err == nil {
        return p1
    }
    p2 := filepath.Join("smoke-test", "real-test.c")
    if _, err := os.Stat(p2); err == nil {
        return p2
    }
    log.Fatalf("real test source not found (looked for %s and %s)", p1, p2)
    return "" // unreachable
}
