# OQ-137 Phase 5 — reading-layer classification sweep (rubric + probe design)

Date: 2026-07-02. Follows the OQ-137 enabling slice (registry + suite, commits
`a81d4c83`/`2453b922`). The rubric below is the one pre-registered in the OQ-137 ISSUES.md
entry (2026-06-16); this file pins the probe mechanics and the fix discipline.

## Rubric (from the OQ entry, unchanged)

Scope: predicates an aggregate could read as a measurement — verdict/reading predicates over a
corpus-enumerable key (constraint, seat, story-UID, kernel). Each is classified exactly one of:

- **total_on_domain** — returns a typed token on every domain member (compliant; registered and
  suite-guarded);
- **partial_by_design(reason)** — relational / fires-on-detection; off-domain or non-fire
  silence is correct, and a TOTAL surface exists (or failure-as-false is the consumed
  semantics); registered with the reason;
- **silently-failing-defect** — fails on its own domain where an aggregate/report would misread
  the silence → FIX to a typed token (§5), then register.

Out of scope (recorded, not registered): engine-internal classification machinery
(`integrate_signature_with_modal/3`, `resolve_modal_signature_conflict/3`), pure term accessors
(`verdict_unknown_count/2`), dynamic probe surfaces (`stakeholder_d_override/3`), type-atom
tables (`extractive_type/1`, `signature_grade/2`, `signature_severity/2`).

## Probe

`probe_oq137_sweep.pl` (this dir; results in `sweep_results.txt`): for every candidate, the
solution-count distribution (zero / exactly-one / multi) over its natural domain on the live
corpus (n=119; 661 seats; 89 story-UIDs; 83 drift-UIDs; 45 kernels), with exceptions caught as
failure. **Diagnostic controls, both fired**: `ctl_planted_fail` reads zero=119 and
`ctl_planted_multi` reads multi=119 — witnessed AFTER the first sweep run passed VACUOUSLY
(every row 0/0/0: an unparenthesized `[K]-m:g(...)` template parses as `([K]-m):g(...)` since
`:` is priority 600 > `-` 500, so `copy_term(T, Key-Goal)` failed silently). The controls are
what caught it; the partition-sum invariant (zero+one+multi == n) is now asserted per row.

## Fix discipline

- Fixes follow the file-local conventions (report_generator's OQ-99 fail-loud OPEN marker;
  signature_detection's honest-abstain `unknown`).
- Output-changing fixes (report text) land in their own commit with before/after diffs;
  behavior-preserving fixes carry a same-session old-vs-new enumeration diff.
- HARD STOP if any fix moves `per_constraint` classification (commentary-grade must not); a
  full-pipeline diff at the end witnesses it.
- Every classified predicate lands in `prolog/reading_registry.pl`; total_on_domain entries are
  thereby suite-guarded (`prolog/tests/test_reading_totality.pl`).

## Honesty note on ordering

The empirical sweep ran BEFORE this file was written (recon-first; the sweep is a mechanical
census, not a hypothesis test — the pre-registered element is the rubric, which predates it in
ISSUES.md). Classifications were made from the sweep table + code reads, per predicate, in
`classification_table.md`.
