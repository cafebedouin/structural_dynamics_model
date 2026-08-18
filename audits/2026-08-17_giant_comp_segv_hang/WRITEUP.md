# giant_comp_segv_hang — PARTIAL: round-2 preregistered, arms unrun; owner session closed mid-flight

**Executed:** 2026-08-17 (= directory date; marked PARTIAL same day)
**OQ:** OQ-77 (reopen candidate), OQ-182 (attribution challenged)
**Verdict:** PARTIAL — no verdict issued; the round-2 prereg and arm driver are staged, no
arm has recorded results in this directory; the owning instance hung (waiting on shells)
and was closed by the operator, who will return to execute the arms.
**Substrate:** no pipeline run. The round-1 baseline cited in PREREGISTRATION.md (7/100
failures — 6 hang, 1 SIGSEGV — serial, live corpus n=279) was measured in the owner
session; its raw output is not in this directory.
**Fired:** latent — PROVISIONAL, to be re-issued when the arms run: round 1 recorded a
real failure population and a power objection to OQ-182's N=10 cure battery
(P(10 clean | p₀=0.07) = 0.48, so "cured" vs "got lucky" was not distinguished);
adjudication is conditional on the unrun round-2 arms.
**Evidence map:**
- `PREREGISTRATION.md` — round-2 design: n=150 power table, arms A–F, B×C decision table
  committed before any run, arm-D buffering caveat, falsifier for the one-bug reading.
  Its freeze is asserted in its own header but NOT witnessed (no `audit_log.md` exists,
  so no md5 was logged) — log the md5 before the first result line when work resumes.
- `round2_arms.sh` — the arm driver (A–D; F via `SWIPL=`), per-invocation rc/byte-count
  records, corpus fingerprint taken around each arm (fingerprint drift voids the arm).

## State at marking

Marked PARTIAL by a different instance (operator ruling, 2026-08-17: the owning session
hung and was closed). Nothing in this directory was touched except adding this file.
The working tree at marking time carried an uncommitted `python/run_pipeline.py`
modification of unknown ownership — left alone, not committed with this file.

**Re-entry for the operator:** run `./round2_arms.sh A B C D` from this directory
(n=150 per arm, `timeout 25`, serial, idle machine per the prereg); create
`audit_log.md` with the prereg md5 FIRST. Arm F additionally needs a 9.3.x swipl;
the debug-symbol package for naming the crashing frame needs interactive `sudo`
(prereg §Out of scope).
