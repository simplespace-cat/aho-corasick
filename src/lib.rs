// lib.rs
/*!
A library for finding occurrences of many patterns at once. This library
provides multiple pattern search principally through an implementation of the
[Aho-Corasick algorithm](https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm),
which builds a fast finite state machine for executing searches in linear time.

Additionally, this library provides a number of configuration options for
building the automaton that permit controlling the space versus time trade
off. Other features include simple ASCII case insensitive matching, finding
overlapping matches, replacements, searching streams and even searching and
replacing text in streams.

Finally, unlike most other Aho-Corasick implementations, this one
supports enabling [leftmost-first](MatchKind::LeftmostFirst) or
[leftmost-longest](MatchKind::LeftmostLongest) match semantics, using a
(seemingly) novel alternative construction algorithm. For more details on what
match semantics means, see the [`MatchKind`] type.

# Overview

This section gives a brief overview of the primary types in this crate:

* [`AhoCorasick`] is the primary type and represents an Aho-Corasick automaton.
This is the type you use to execute searches.
* [`AhoCorasickBuilder`] can be used to build an Aho-Corasick automaton, and
supports configuring a number of options.
* [`Match`] represents a single match reported by an Aho-Corasick automaton.
Each match has two pieces of information: the pattern that matched and the
start and end byte offsets corresponding to the position in the haystack at
which it matched.

# Example: basic searching

This example shows how to search for occurrences of multiple patterns
simultaneously. Each match includes the pattern that matched along with the
byte offsets of the match.

```
use aho_corasick::{AhoCorasick, PatternID};

let patterns = &["apple", "maple", "Snapple"];
let haystack = "Nobody likes maple in their apple flavored Snapple.";

let ac = AhoCorasick::new(patterns).unwrap();
let mut matches = vec![];
for mat in ac.find_iter(haystack) {
    matches.push((mat.pattern(), mat.start(), mat.end()));
}
assert_eq!(matches, vec![
    (PatternID::must(1), 13, 18),
    (PatternID::must(0), 28, 33),
    (PatternID::must(2), 43, 50),
]);
```

# Example: case insensitivity

This is like the previous example, but matches `Snapple` case insensitively
using `AhoCorasickBuilder`:

```
use aho_corasick::{AhoCorasick, PatternID};

let patterns = &["apple", "maple", "snapple"];
let haystack = "Nobody likes maple in their apple flavored Snapple.";

let ac = AhoCorasick::builder()
    .ascii_case_insensitive(true)
    .build(patterns)
    .unwrap();
let mut matches = vec![];
for mat in ac.find_iter(haystack) {
    matches.push((mat.pattern(), mat.start(), mat.end()));
}
assert_eq!(matches, vec![
    (PatternID::must(1), 13, 18),
    (PatternID::must(0), 28, 33),
    (PatternID::must(2), 43, 50),
]);
```

# Example: replacing matches in a stream

This example shows how to execute a search and replace on a stream without
loading the entire stream into memory first.

```
# #[cfg(feature = "std")] {
use aho_corasick::AhoCorasick;

# fn example() -> Result<(), std::io::Error> {
let patterns = &["fox", "brown", "quick"];
let replace_with = &["sloth", "grey", "slow"];

// In a real example, these might be `std::fs::File`s instead. All you need to
// do is supply a pair of `std::io::Read` and `std::io::Write` implementations.
let rdr = "The quick brown fox.";
let mut wtr = vec![];

let ac = AhoCorasick::new(patterns).unwrap();
ac.try_stream_replace_all(rdr.as_bytes(), &mut wtr, replace_with)?;
assert_eq!(b"The slow grey sloth.".to_vec(), wtr);
# Ok(()) }; example().unwrap()
# }
```

# Example: finding the leftmost first match

In the textbook description of Aho-Corasick, its formulation is typically
structured such that it reports all possible matches, even when they overlap
with another. In many cases, overlapping matches may not be desired, such as
the case of finding all successive non-overlapping matches like you might with
a standard regular expression.

Unfortunately the "obvious" way to modify the Aho-Corasick algorithm to do
this doesn't always work in the expected way, since it will report matches as
soon as they are seen. For example, consider matching the regex `Samwise|Sam`
against the text `Samwise`. Most regex engines (that are Perl-like, or
non-POSIX) will report `Samwise` as a match, but the standard Aho-Corasick
algorithm modified for reporting non-overlapping matches will report `Sam`.

A novel contribution of this library is the ability to change the match
semantics of Aho-Corasick (without additional search time overhead) such that
`Samwise` is reported instead. For example, here's the standard approach:

```
use aho_corasick::AhoCorasick;

let patterns = &["Samwise", "Sam"];
let haystack = "Samwise";

let ac = AhoCorasick::new(patterns).unwrap();
let mat = ac.find(haystack).expect("should have a match");
assert_eq!("Sam", &haystack[mat.start()..mat.end()]);
```

And now here's the leftmost-first version, which matches how a Perl-like
regex will work:

```
use aho_corasick::{AhoCorasick, MatchKind};

let patterns = &["Samwise", "Sam"];
let haystack = "Samwise";

let ac = AhoCorasick::builder()
    .match_kind(MatchKind::LeftmostFirst)
    .build(patterns)
    .unwrap();
let mat = ac.find(haystack).expect("should have a match");
assert_eq!("Samwise", &haystack[mat.start()..mat.end()]);
```

In addition to leftmost-first semantics, this library also supports
leftmost-longest semantics, which match the POSIX behavior of a regular
expression alternation. See [`MatchKind`] for more details.

# Prefilters

While an Aho-Corasick automaton can perform admirably when compared to more
naive solutions, it is generally slower than more specialized algorithms that
are accelerated using vector instructions such as SIMD.

For that reason, this library will internally use a "prefilter" to attempt
to accelerate searches when possible. Currently, this library has several
different algorithms it might use depending on the patterns provided. Once the
number of patterns gets too big, prefilters are no longer used.

While a prefilter is generally good to have on by default since it works
well in the common case, it can lead to less predictable or even sub-optimal
performance in some cases. For that reason, prefilters can be explicitly
disabled via [`AhoCorasickBuilder::prefilter`].

# Lower level APIs

This crate also provides several sub-modules that collectively expose many of
the implementation details of the main [`AhoCorasick`] type. Most users of this
library can completely ignore the submodules and their contents, but if you
needed finer grained control, some parts of them may be useful to you. Here is
a brief overview of each and why you might want to use them:

* The [`packed`] sub-module contains a lower level API for using fast
vectorized routines for finding a small number of patterns in a haystack.
You might want to use this API when you want to completely side-step using
Aho-Corasick automata. Otherwise, the fast vectorized routines are used
automatically as prefilters for `AhoCorasick` searches whenever possible.
* The [`automaton`] sub-module provides a lower level finite state
machine interface that the various Aho-Corasick implementations in
this crate implement. This sub-module's main contribution is the
[`Automaton`](automaton::Automaton) trait, which permits manually walking the
state transitions of an Aho-Corasick automaton.
* The [`dfa`] and [`nfa`] sub-modules provide DFA and NFA implementations of
the aforementioned `Automaton` trait. The main reason one might want to use
these sub-modules is to get access to a type that implements the `Automaton`
trait. (The top-level `AhoCorasick` type does not implement the `Automaton`
trait.)

As mentioned above, if you aren't sure whether you need these sub-modules,
you should be able to safely ignore them and just focus on the [`AhoCorasick`]
type.

# Crate features

This crate exposes a few features for controlling dependency usage and whether
this crate can be used without the standard library.

* **std** -
  Enables support for the standard library. This feature is enabled by
  default. When disabled, only `core` and `alloc` are used. At an API
  level, enabling `std` enables `std::error::Error` trait impls for the
  various error types, and higher level stream search routines such as
  [`AhoCorasick::try_stream_find_iter`]. But the `std` feature is also required
  to enable vectorized prefilters. Prefilters can greatly accelerate searches,
  but generally only apply when the number of patterns is small (less than
  ~100).
* **perf-literal** -
  Enables support for literal prefilters that use vectorized routines from
  external crates. This feature is enabled by default. If you're only using
  Aho-Corasick for large numbers of patterns or otherwise can abide lower
  throughput when searching with a small number of patterns, then it is
  reasonable to disable this feature.
* **logging** -
  Enables a dependency on the `log` crate and emits messages to aide in
  diagnostics. This feature is disabled by default.
*/

#![no_std]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]

extern crate alloc;
#[cfg(any(test, feature = "std"))]
extern crate std;

#[cfg(doctest)]
doc_comment::doctest!("../README.md");

#[cfg(feature = "std")]
pub use crate::ahocorasick::StreamFindIter;
pub use crate::{
    ahocorasick::{
        AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, FindIter,
        FindOverlappingIter,
    },
    util::{
        error::{BuildError, MatchError, MatchErrorKind},
        primitives::{PatternID, PatternIDError},
        search::{Anchored, Input, Match, MatchKind, Span, StartKind},
    },
};

#[macro_use]
mod macros;

mod ahocorasick;
pub mod automaton;
pub mod dfa;
pub mod nfa;
pub mod packed;
#[cfg(test)]
mod tests;
// I wrote out the module for implementing fst::Automaton only to later realize
// that this would make fst a public dependency and fst is not at 1.0 yet. I
// decided to just keep the code in tree, but build it only during tests.
//
// TODO: I think I've changed my mind again. I'm considering pushing it out
// into either a separate crate or into 'fst' directly as an optional feature.
// #[cfg(test)]
// #[allow(dead_code)]
// mod transducer;
pub(crate) mod util;

#[cfg(test)]
mod testoibits {
    use std::panic::{RefUnwindSafe, UnwindSafe};

    use super::*;

    fn assert_all<T: Send + Sync + UnwindSafe + RefUnwindSafe>() {}

    #[test]
    fn oibits_main() {
        assert_all::<AhoCorasick>();
        assert_all::<AhoCorasickBuilder>();
        assert_all::<AhoCorasickKind>();
        assert_all::<FindIter>();
        assert_all::<FindOverlappingIter>();

        assert_all::<BuildError>();
        assert_all::<MatchError>();
        assert_all::<MatchErrorKind>();

        assert_all::<Anchored>();
        assert_all::<Input>();
        assert_all::<Match>();
        assert_all::<MatchKind>();
        assert_all::<Span>();
        assert_all::<StartKind>();
    }

    #[test]
    fn oibits_automaton() {
        use crate::{automaton, dfa::DFA};

        assert_all::<automaton::FindIter<DFA>>();
        assert_all::<automaton::FindOverlappingIter<DFA>>();
        #[cfg(feature = "std")]
        assert_all::<automaton::StreamFindIter<DFA, std::io::Stdin>>();
        assert_all::<automaton::OverlappingState>();

        assert_all::<automaton::Prefilter>();
        assert_all::<automaton::Candidate>();
    }

    #[test]
    fn oibits_packed() {
        use crate::packed;

        assert_all::<packed::Config>();
        assert_all::<packed::Builder>();
        assert_all::<packed::Searcher>();
        assert_all::<packed::FindIter>();
        assert_all::<packed::MatchKind>();
    }
}

#[cfg(feature = "std")]
#[allow(missing_docs)]
pub mod ffi {
    //! A small, refined C API for the Aho-Corasick library.
    //!
    //! Design goals:
    //! - Static library output (crate-type = "staticlib")
    //! - A minimal but mature interface for common scenarios
    //! - Automatic selection of the best underlying implementation (DFA/NFA)
    //! - No panics unwind across the FFI boundary (errors are returned)
    //!
    //! Memory management:
    //! - All buffers returned by this API must be freed with the provided
    //!   free functions in this module (they use Rust's allocator).
    use std::boxed::Box;
    use std::string::ToString;
    use std::vec::Vec;

    use std::{
        ffi::c_char,
        panic::{catch_unwind, AssertUnwindSafe},
        slice,
    };

    use crate::{
        automaton::OverlappingState, Anchored, AhoCorasick, AhoCorasickBuilder,
        AhoCorasickKind, Input, Match, MatchError, MatchKind, StartKind,
    };

    #[repr(C)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum ACResult {
        AC_OK = 0,
        AC_BUILD_ERROR = 1,
        AC_MATCH_ERROR = 2,
        AC_INVALID_ARGUMENT = 3,
        AC_PANIC = 100,
    }

    #[repr(C)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum ACMatchKind {
        Standard = 0,
        LeftmostFirst = 1,
        LeftmostLongest = 2,
    }

    impl From<ACMatchKind> for MatchKind {
        fn from(v: ACMatchKind) -> Self {
            match v {
                ACMatchKind::Standard => MatchKind::Standard,
                ACMatchKind::LeftmostFirst => MatchKind::LeftmostFirst,
                ACMatchKind::LeftmostLongest => MatchKind::LeftmostLongest,
            }
        }
    }

    #[repr(C)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum ACStartKind {
        Unanchored = 0,
        Anchored = 1,
        Both = 2,
    }

    impl From<ACStartKind> for StartKind {
        fn from(v: ACStartKind) -> Self {
            match v {
                ACStartKind::Unanchored => StartKind::Unanchored,
                ACStartKind::Anchored => StartKind::Anchored,
                ACStartKind::Both => StartKind::Both,
            }
        }
    }

    #[repr(C)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum ACAutomatonKind {
        NoncontiguousNFA = 0,
        ContiguousNFA = 1,
        DFA = 2,
    }

    impl From<AhoCorasickKind> for ACAutomatonKind {
        fn from(k: AhoCorasickKind) -> Self {
            match k {
                AhoCorasickKind::NoncontiguousNFA => {
                    ACAutomatonKind::NoncontiguousNFA
                }
                AhoCorasickKind::ContiguousNFA => ACAutomatonKind::ContiguousNFA,
                AhoCorasickKind::DFA => ACAutomatonKind::DFA,
            }
        }
    }

    #[repr(C)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct ACMatch {
        pub pattern: u32,
        pub start: usize,
        pub end: usize,
    }

    impl From<Match> for ACMatch {
        fn from(m: Match) -> Self {
            ACMatch {
                pattern: m.pattern().as_usize() as u32,
                start: m.start(),
                end: m.end(),
            }
        }
    }

    
    pub struct AC {
        inner: AhoCorasick,
    }

    
    pub struct ACBuilder {
        inner: AhoCorasickBuilder,
    }

    
    pub struct ACOverlappingState {
        inner: OverlappingState,
    }

    // ===================
    // Utilities
    // ===================

    unsafe fn read_patterns<'a>(
        patterns: *const *const u8,
        lens: *const usize,
        count: usize,
    ) -> Result<Vec<&'a [u8]>, ACResult> {
        if count == 0 {
            return Ok(Vec::new());
        }
        if patterns.is_null() || lens.is_null() {
            return Err(ACResult::AC_INVALID_ARGUMENT);
        }
        let mut out: Vec<&'a [u8]> = Vec::with_capacity(count);
        for i in 0..count {
            let p = *patterns.add(i);
            let l = *lens.add(i);
            if l == 0 {
                out.push(&[]);
            } else if p.is_null() {
                return Err(ACResult::AC_INVALID_ARGUMENT);
            } else {
                out.push(slice::from_raw_parts(p, l));
            }
        }
        Ok(out)
    }

    unsafe fn read_slices<'a>(
        ptrs: *const *const u8,
        lens: *const usize,
        count: usize,
    ) -> Result<Vec<&'a [u8]>, ACResult> {
        read_patterns(ptrs, lens, count)
    }

    unsafe fn read_slice<'a>(
        ptr: *const u8,
        len: usize,
    ) -> Result<&'a [u8], ACResult> {
        if len == 0 {
            Ok(&[])
        } else if ptr.is_null() {
            Err(ACResult::AC_INVALID_ARGUMENT)
        } else {
            Ok(slice::from_raw_parts(ptr, len))
        }
    }

    unsafe fn set_error(err_msg: *mut *mut c_char, err_len: *mut usize, msg: &str) {
        if err_msg.is_null() {
            return;
        }
        let bytes = msg.as_bytes().to_vec();
        let boxed: Box<[u8]> = bytes.into_boxed_slice();
        let len = (&*boxed).len();
        let ptr = Box::into_raw(boxed) as *mut c_char;
        *err_msg = ptr;
        if !err_len.is_null() {
            *err_len = len;
        }
    }

    fn map_build_err(
        e: crate::util::error::BuildError,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        unsafe { set_error(err_msg, err_len, &e.to_string()) }
        ACResult::AC_BUILD_ERROR
    }

    fn map_match_err(
        e: MatchError,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        unsafe { set_error(err_msg, err_len, &e.to_string()) }
        ACResult::AC_MATCH_ERROR
    }

    // ===================
    // Builder
    // ===================

    /// Create a new builder with default configuration.
    #[no_mangle]
    pub extern "C" fn ac_builder_create() -> *mut ACBuilder {
        let b = ACBuilder { inner: AhoCorasickBuilder::new() };
        Box::into_raw(Box::new(b))
    }

    /// Free a builder previously returned by `ac_builder_create`.
    #[no_mangle]
    pub extern "C" fn ac_builder_free(builder: *mut ACBuilder) {
        if !builder.is_null() {
            unsafe { drop(Box::from_raw(builder)) };
        }
    }

    /// Enable or disable ASCII-aware case-insensitivity.
    #[no_mangle]
    pub extern "C" fn ac_builder_set_ascii_case_insensitive(
        builder: *mut ACBuilder,
        yes: bool,
    ) {
        if builder.is_null() {
            return;
        }
        let b = unsafe { &mut *builder };
        b.inner.ascii_case_insensitive(yes);
    }

    /// Set the match semantics (standard/leftmost-first/leftmost-longest).
    #[no_mangle]
    pub extern "C" fn ac_builder_set_match_kind(
        builder: *mut ACBuilder,
        kind: ACMatchKind,
    ) {
        if builder.is_null() {
            return;
        }
        let b = unsafe { &mut *builder };
        b.inner.match_kind(kind.into());
    }

    /// Set the supported start kind (unanchored by default).
    #[no_mangle]
    pub extern "C" fn ac_builder_set_start_kind(
        builder: *mut ACBuilder,
        kind: ACStartKind,
    ) {
        if builder.is_null() {
            return;
        }
        let b = unsafe { &mut *builder };
        b.inner.start_kind(kind.into());
    }

    /// Build an automaton using the given builder and pattern set.
    ///
    /// On success, returns `AC_OK` and writes a non-null handle to `*out_ac`.
    /// On failure, returns a non-zero error code and optionally writes an
    /// error message buffer to `*err_msg`/`*err_len` (must be freed with `ac_str_free`).
    #[no_mangle]
    pub extern "C" fn ac_builder_build(
        builder: *mut ACBuilder,
        patterns: *const *const u8,
        pattern_lens: *const usize,
        patterns_len: usize,
        out_ac: *mut *mut AC,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if builder.is_null() || out_ac.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let b = unsafe { &mut *builder };
            let pats = match unsafe { read_patterns(patterns, pattern_lens, patterns_len) } {
                Ok(p) => p,
                Err(code) => return code,
            };
            match b.inner.build(pats) {
                Ok(ac) => {
                    unsafe { *out_ac = Box::into_raw(Box::new(AC { inner: ac })) };
                    ACResult::AC_OK
                }
                Err(e) => map_build_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    // ===================
    // Automaton: creation and info
    // ===================

    /// Convenience: build an automaton with default configuration.
    #[no_mangle]
    pub extern "C" fn ac_new(
        patterns: *const *const u8,
        pattern_lens: *const usize,
        patterns_len: usize,
        out_ac: *mut *mut AC,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if out_ac.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let pats = match unsafe { read_patterns(patterns, pattern_lens, patterns_len) } {
                Ok(p) => p,
                Err(code) => return code,
            };
            match AhoCorasick::new(pats) {
                Ok(ac) => {
                    unsafe { *out_ac = Box::into_raw(Box::new(AC { inner: ac })) };
                    ACResult::AC_OK
                }
                Err(e) => map_build_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    /// Free an automaton handle created by `ac_new` or `ac_builder_build`.
    #[no_mangle]
    pub extern "C" fn ac_free(ac: *mut AC) {
        if !ac.is_null() {
            unsafe { drop(Box::from_raw(ac)) };
        }
    }

    /// Return which Aho-Corasick implementation was chosen internally.
    #[no_mangle]
    pub extern "C" fn ac_kind(ac: *const AC) -> ACAutomatonKind {
        if ac.is_null() {
            return ACAutomatonKind::ContiguousNFA;
        }
        let k = unsafe { (&*ac).inner.kind() };
        k.into()
    }

    /// Return the match kind used by this automaton.
    #[no_mangle]
    pub extern "C" fn ac_match_kind(ac: *const AC) -> ACMatchKind {
        if ac.is_null() {
            return ACMatchKind::Standard;
        }
        let k = unsafe { (&*ac).inner.match_kind() };
        match k {
            MatchKind::Standard => ACMatchKind::Standard,
            MatchKind::LeftmostFirst => ACMatchKind::LeftmostFirst,
            MatchKind::LeftmostLongest => ACMatchKind::LeftmostLongest,
        }
    }

    /// Return the start kind supported by this automaton (as configured).
    #[no_mangle]
    pub extern "C" fn ac_start_kind(ac: *const AC) -> ACStartKind {
        if ac.is_null() {
            return ACStartKind::Unanchored;
        }
        let k = unsafe { (&*ac).inner.start_kind() };
        match k {
            StartKind::Unanchored => ACStartKind::Unanchored,
            StartKind::Anchored => ACStartKind::Anchored,
            StartKind::Both => ACStartKind::Both,
        }
    }

    /// Return the number of patterns compiled into this automaton.
    #[no_mangle]
    pub extern "C" fn ac_patterns_len(ac: *const AC) -> usize {
        if ac.is_null() {
            return 0;
        }
        unsafe { (&*ac).inner.patterns_len() }
    }

    /// Return the minimum pattern length in this automaton.
    #[no_mangle]
    pub extern "C" fn ac_min_pattern_len(ac: *const AC) -> usize {
        if ac.is_null() {
            return 0;
        }
        unsafe { (&*ac).inner.min_pattern_len() }
    }

    /// Return the maximum pattern length in this automaton.
    #[no_mangle]
    pub extern "C" fn ac_max_pattern_len(ac: *const AC) -> usize {
        if ac.is_null() {
            return 0;
        }
        unsafe { (&*ac).inner.max_pattern_len() }
    }

    /// Return the approximate heap memory usage (bytes).
    #[no_mangle]
    pub extern "C" fn ac_memory_usage(ac: *const AC) -> usize {
        if ac.is_null() {
            return 0;
        }
        unsafe { (&*ac).inner.memory_usage() }
    }

    // ===================
    // Searching
    // ===================

    /// Test whether any match exists in `haystack`.
    ///
    /// Writes true/false to `*out_found`. On configuration errors, returns
    /// `AC_MATCH_ERROR` with a message.
    #[no_mangle]
    pub extern "C" fn ac_is_match(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_found.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            // earliest(true) for is_match with leftmost semantics.
            match ac.inner.try_find(Input::new(hay).earliest(true)) {
                Ok(opt) => {
                    unsafe { *out_found = opt.is_some() };
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    /// Find the first match. If found, writes it to `*out_match` and sets
    /// `*out_found = true`. Otherwise, `*out_found = false`.
    ///
    /// `earliest` can be set to true to return as soon as any match is known
    /// to occur (useful with leftmost semantics). `anchored` selects whether
    /// to run an anchored search.
    #[no_mangle]
    pub extern "C" fn ac_find(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        earliest: bool,
        anchored: bool,
        out_match: *mut ACMatch,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_found.is_null() || out_match.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            let input = Input::new(hay)
                .earliest(earliest)
                .anchored(if anchored { Anchored::Yes } else { Anchored::No });
            match ac.inner.try_find(input) {
                Ok(opt) => {
                    if let Some(m) = opt {
                        unsafe {
                            *out_match = ACMatch::from(m);
                            *out_found = true;
                        }
                    } else {
                        unsafe { *out_found = false };
                    }
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    /// Find all non-overlapping matches and return them as an array of ACMatch.
    /// The array must be freed with `ac_matches_free`.
    #[no_mangle]
    pub extern "C" fn ac_find_all(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        anchored: bool,
        out_matches: *mut *mut ACMatch,
        out_len: *mut usize,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_matches.is_null() || out_len.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            let input =
                Input::new(hay).anchored(if anchored { Anchored::Yes } else { Anchored::No });
            match ac.inner.try_find_iter(input) {
                Ok(mut it) => {
                    let mut out = Vec::<ACMatch>::new();
                    while let Some(m) = it.next() {
                        out.push(ACMatch::from(m));
                    }
                    let len = out.len();
                    let boxed = out.into_boxed_slice();
                    let ptr = Box::into_raw(boxed) as *mut ACMatch;
                    unsafe {
                        *out_matches = ptr;
                        *out_len = len;
                    }
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    // ===================
    // New: Prefix and Suffix matching
    // ===================

    /// Prefix check: does any pattern match at the beginning of `haystack`?
    ///
    /// This runs an anchored search from the start (offset 0) and sets
    /// `*out_found` accordingly. This respects the automaton's configured
    /// match semantics:
    /// - Standard: first match seen is returned
    /// - LeftmostFirst: first by pattern priority
    /// - LeftmostLongest: longest among those starting at 0
    #[no_mangle]
    pub extern "C" fn ac_is_prefix(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_found.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            let input = Input::new(hay).anchored(Anchored::Yes).earliest(true);
            match ac.inner.try_find(input) {
                Ok(opt) => {
                    unsafe { *out_found = opt.is_some() };
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(c) => c,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    /// Prefix match: if a match starts at 0, write it to `*out_match` and set `*out_found = true`.
    #[no_mangle]
    pub extern "C" fn ac_find_prefix(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        out_match: *mut ACMatch,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_match.is_null() || out_found.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            let input = Input::new(hay).anchored(Anchored::Yes);
            match ac.inner.try_find(input) {
                Ok(opt) => {
                    if let Some(m) = opt {
                        unsafe {
                            *out_match = ACMatch::from(m);
                            *out_found = true;
                        }
                    } else {
                        unsafe { *out_found = false };
                    }
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(c) => c,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    /// Helper used by suffix routines. Tries to find a match that ends at
    /// `hay.len()`. Returns Ok(Some(m)) if found.
    fn find_suffix_match(ac: &AhoCorasick, hay: &[u8]) -> Result<Option<Match>, MatchError> {
        let n = hay.len();
        let min_len = ac.min_pattern_len();
        let max_len = ac.max_pattern_len();

        if min_len > n {
            return Ok(None);
        }
        // Candidate start positions: any start s such that some pattern length L
        // satisfies s + L == n, i.e., s in [n - max_len, n - min_len].
        let start_lo = n.saturating_sub(max_len);
        let start_hi = n - min_len;

        match ac.match_kind() {
            MatchKind::Standard => {
                // Enumerate anchored overlapping matches at each start position
                // to see if any ends at the haystack end. Iterate from earliest
                // start to favor the longest suffix.
                for s in start_lo..=start_hi {
                    let mut state = OverlappingState::start();
                    let input = Input::new(hay).range(s..).anchored(Anchored::Yes);
                    loop {
                        ac.try_find_overlapping(input.clone(), &mut state)?;
                        match state.get_match() {
                            Some(m) => {
                                if m.end() == n {
                                    return Ok(Some(m));
                                } else {
                                    // Keep enumerating other matches at this start position.
                                    continue;
                                }
                            }
                            None => break,
                        }
                    }
                }
                Ok(None)
            }
            // For leftmost-first/longest, we cannot enumerate overlapping matches.
            // Rely on anchored `find` at each start position. With LeftmostLongest this
            // chooses the longest match starting at `s`. With LeftmostFirst it chooses by
            // pattern priority. We accept it and filter by those ending at `n`.
            MatchKind::LeftmostFirst | MatchKind::LeftmostLongest => {
                for s in start_lo..=start_hi {
                    let input = Input::new(hay).range(s..).anchored(Anchored::Yes);
                    if let Some(m) = ac.try_find(input)? {
                        if m.end() == n {
                            return Ok(Some(m));
                        }
                    }
                }
                Ok(None)
            }
        }
    }

    /// Suffix check: does any pattern match and end at the end of `haystack`?
    ///
    /// Notes:
    /// - Works for all match kinds.
    /// - With MatchKind::Standard, we enumerate overlapping matches anchored at
    ///   candidate starts near the end to ensure we can detect longer suffixes.
    /// - With LeftmostLongest, this naturally finds the longest suffix.
    /// - With LeftmostFirst, suffix existence depends on pattern priority when
    ///   multiple matches start at the same position.
    #[no_mangle]
    pub extern "C" fn ac_is_suffix(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_found.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            match find_suffix_match(&ac.inner, hay) {
                Ok(opt) => {
                    unsafe { *out_found = opt.is_some() };
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(c) => c,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    /// Suffix match: if a match ends at the end of `haystack`, write it to `*out_match`.
    ///
    /// Preference:
    /// - When MatchKind::Standard, returns the longest suffix (by iterating candidate
    ///   start positions from earlier to later and enumerating overlapping matches).
    /// - When MatchKind::LeftmostLongest, returns the longest suffix.
    /// - When MatchKind::LeftmostFirst, returns the suffix chosen by pattern priority
    ///   at its start position (if any).
    #[no_mangle]
    pub extern "C" fn ac_find_suffix(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        out_match: *mut ACMatch,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_match.is_null() || out_found.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            match find_suffix_match(&ac.inner, hay) {
                Ok(opt) => {
                    if let Some(m) = opt {
                        unsafe {
                            *out_match = ACMatch::from(m);
                            *out_found = true;
                        }
                    } else {
                        unsafe { *out_found = false };
                    }
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(c) => c,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    // ===================
    // Overlapping search (step-wise)
    // ===================

    /// Create a new overlapping state object.
    #[no_mangle]
    pub extern "C" fn ac_overlapping_state_create() -> *mut ACOverlappingState {
        Box::into_raw(Box::new(ACOverlappingState { inner: OverlappingState::start() }))
    }

    /// Reset an overlapping state to its initial value.
    #[no_mangle]
    pub extern "C" fn ac_overlapping_state_reset(state: *mut ACOverlappingState) {
        if state.is_null() {
            return;
        }
        let s = unsafe { &mut *state };
        s.inner = OverlappingState::start();
    }

    /// Free an overlapping state object.
    #[no_mangle]
    pub extern "C" fn ac_overlapping_state_free(state: *mut ACOverlappingState) {
        if !state.is_null() {
            unsafe { drop(Box::from_raw(state)) };
        }
    }

    /// Perform one step of overlapping search. On success, sets `*out_found`
    /// to true and writes the match to `*out_match` if a match is available.
    /// Otherwise, sets `*out_found = false`.
    ///
    /// Only supported when the automaton uses Standard semantics.
    #[no_mangle]
    pub extern "C" fn ac_find_overlapping_step(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        state: *mut ACOverlappingState,
        out_match: *mut ACMatch,
        out_found: *mut bool,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null()
                || state.is_null()
                || out_match.is_null()
                || out_found.is_null()
            {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let s = unsafe { &mut *state };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            match ac.inner.try_find_overlapping(hay, &mut s.inner) {
                Ok(()) => {
                    let opt = s.inner.get_match();
                    if let Some(m) = opt {
                        unsafe {
                            *out_match = ACMatch::from(m);
                            *out_found = true;
                        }
                    } else {
                        unsafe { *out_found = false };
                    }
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    // ===================
    // Replacement
    // ===================

    /// Replace all matches with the corresponding replacement at the same index.
    /// The replacements array length must equal `ac_patterns_len(ac)`.
    /// On success, writes an owned buffer to `*out_ptr` and length to `*out_len`.
    /// Free the buffer with `ac_buf_free`.
    #[no_mangle]
    pub extern "C" fn ac_replace_all_bytes(
        ac: *const AC,
        haystack: *const u8,
        haystack_len: usize,
        replacements: *const *const u8,
        repl_lens: *const usize,
        replacements_len: usize,
        out_ptr: *mut *mut u8,
        out_len: *mut usize,
        err_msg: *mut *mut c_char,
        err_len: *mut usize,
    ) -> ACResult {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if ac.is_null() || out_ptr.is_null() || out_len.is_null() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            let ac = unsafe { &*ac };
            let hay = match unsafe { read_slice(haystack, haystack_len) } {
                Ok(h) => h,
                Err(code) => return code,
            };
            let repls = match unsafe { read_slices(replacements, repl_lens, replacements_len) } {
                Ok(r) => r,
                Err(code) => return code,
            };
            if repls.len() != ac.inner.patterns_len() {
                return ACResult::AC_INVALID_ARGUMENT;
            }
            match ac.inner.try_replace_all_bytes(hay, &repls) {
                Ok(bytes) => {
                    let len = bytes.len();
                    let boxed = bytes.into_boxed_slice();
                    let ptr = Box::into_raw(boxed) as *mut u8;
                    unsafe {
                        *out_ptr = ptr;
                        *out_len = len;
                    }
                    ACResult::AC_OK
                }
                Err(e) => map_match_err(e, err_msg, err_len),
            }
        }));
        match result {
            Ok(code) => code,
            Err(_) => ACResult::AC_PANIC,
        }
    }

    // ===================
    // Free functions for owned buffers
    // ===================

    /// Free a buffer returned by this API (e.g., from `ac_replace_all_bytes`).
    #[no_mangle]
    pub extern "C" fn ac_buf_free(ptr: *mut u8, len: usize) {
        if ptr.is_null() {
            return;
        }
        unsafe {
            let _ = Box::from_raw(slice::from_raw_parts_mut(ptr, len));
        }
    }

    /// Free an error/message buffer returned by this API.
    #[no_mangle]
    pub extern "C" fn ac_str_free(ptr: *mut c_char, len: usize) {
        if ptr.is_null() {
            return;
        }
        unsafe {
            let _ = Box::from_raw(slice::from_raw_parts_mut(ptr as *mut u8, len));
        }
    }

    /// Free a match array returned by this API.
    #[no_mangle]
    pub extern "C" fn ac_matches_free(ptr: *mut ACMatch, len: usize) {
        if ptr.is_null() {
            return;
        }
        unsafe {
            let _ = Box::from_raw(slice::from_raw_parts_mut(ptr, len));
        }
    }
}
