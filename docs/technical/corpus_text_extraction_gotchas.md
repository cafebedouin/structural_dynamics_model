# Corpus Text Extraction Gotchas (testset `.pl` files as text)

Implementation note. Scope: parsing-level traps hit while extracting commentary and fact
terms from `prolog/testsets/*.pl` **as text** (grep/regex/Python), each of which produced a
silently wrong count before being caught. Read this before writing any script that counts,
extracts, or classifies over testset commentary, omega declarations, or beneficiary/victim
facts. All instances witnessed during the OQ-65 detector-bait census (2026-06-04); reference
implementation for every fix: `python/audits/oq65_bait_census.py`; full provenance:
`audits/2026-06-04_oq65_bait_census/writeup.md` and KNOWN_STATE same date.

These are instances of the build-discipline spine (an absence presenting as a presence): a
regex that silently stops early returns a *plausible nonempty result*, so the read site
cannot tell truncated from complete. The positive-control rule applies with no exemption —
every extractor below was "working" until run against a case it had to find.

## 1. `omega_variable\([^)]*\)` truncates at the first INNER `)` — use balanced capture

The long-form omega term is multi-line prose with parentheses *inside* the quoted strings:

```prolog
omega_variable( name,
    'Question text (often with a parenthetical like this) ...',
    'Resolution mechanism ... (more parens) ...',
    'Impact ... f(d) ... σ(S) ...',
    confidence_without_resolution(medium)
).
```

`[^)]*` stops at the first inner `)` — typically mid-question — and returns a plausible
fragment. **Witnessed blast radius:** the OQ-65 recon channel table was measured with the
truncating regex; balanced capture found **345 omega terms whose `beneficiar*` mention the
truncating regex had cut off** (the beneficiary-mentioning-omega population grew 162→381
files, ~2.3×). Any census keyed on "does the omega mention X" silently undercounts with the
naive regex, and the undercount *looks like a clean result*.

The same applies to `intractable_uncertainty(...)` and to `constraint_beneficiary(...)`
when the value side can contain structure. One-level-nesting patterns like
`\((?:[^()]|\([^()]*\))*\)` fail on two levels of nesting — go straight to a scanner.

**Fix:** depth-counting scanner that is **quote-aware** — parens inside quoted atoms must
not move the depth counter. SWI quoted atoms escape by doubling (`''`) and may contain
`\'`-style escapes; handle both (in-quote: backslash skips next char; `''` is a literal
quote, a single `'` closes). See `capture_terms()` in `oq65_bait_census.py`. Pair it with a
**count-parity assertion** (number of `functor(` occurrences == number of captured terms)
and a truncation assertion (every captured term is balanced-complete) so a regression fails
loud instead of returning fragments.

## 2. Comment text: consecutive `%` lines fragment sentences; one "line" can be a paragraph

Two opposite traps in the same channel:

- **Sentence-level extraction over `%` comments must JOIN consecutive `%` lines first.**
  Generated files wrap sentences across `%`-line runs; treating each `%` line as its own
  segment splits sentences mid-clause, so a co-occurrence test ("sentence contains A and B")
  silently misses pairs that straddle the wrap. Witnessed: the partition counts of the OQ-65
  census shifted materially between per-line and joined-run extraction. Block comments need
  the analogous join: strip ` * ` continuations (`\n\s*\*\s*` → space) before splitting.
  Sentence-split on punctuation (`(?<=[.!?])\s+`), not on newlines — generated prose
  newlines are wrapping, not sentence boundaries.
- **Line-based extraction budgets explode:** generated files routinely put an entire
  paragraph on ONE physical line (PERSPECTIVE comments, DIRECTIONALITY LOGIC blocks, omega
  resolution strings). "537 matched lines" was 420k chars (~780 chars/line median, with
  multi-KB outliers). If you plan to read or display "matching lines," measure chars, not
  lines, before promising a budget — and prefer sentence- or window-level units.

## 3. Proximity windows must anchor on BOTH token families

A "tokens A and B within ±N chars" test implemented as *windows centered on A-matches,
searched for B* is **boundary-asymmetric** with the mirrored test (centered on B, searched
for A): the reachable span differs by up to `len(match)` per side, so pairs sitting right at
the ±N edge pass one direction and fail the other. Witnessed: 2 files
(`common_article_3_scope__state_centric_reading.pl`,
`quantum_formalism__many_worlds_reading.pl`) passed the beneficiar-anchored window scan but
failed the FSM-anchored consistency assertion at ~200-char distances — an asymmetry that
surfaced only because the census asserted the complement (a window channel and an
"absence" channel must partition exactly; the E-channel beneficiary-free assertion failed
loud). **Fix:** scan anchors from both token families and dedup near-identical spans; if
your design has a "covered" channel and a "residue" channel over the same proximity
relation, assert their partition rather than trusting either side alone.

## 4. Smaller traps from the same run (one line each)

- **Count equality is not set equality.** Two independent probes both returning 461 files
  had symmetric difference 83/83. Diff the sets before claiming two methods agree.
- Some testsets wrap IDs in lists (`constraint_claim([id], ...)`) and beneficiary values may
  be lists — split fact args on `[,\s\[\]]+`, not on `,` alone (long-standing; reconfirmed).
- Corpus commentary is saturated with engine-predicting prose ("the engine will flag
  this..."). A classifier over commentary must separate *descriptive predictions of correct
  firing* from *expectation-authored* text, or it will count the template's boilerplate as
  signal — operational rules with witnessed contrast pairs are in
  `audits/2026-06-04_oq65_bait_census/writeup.md` §Classification rules.
