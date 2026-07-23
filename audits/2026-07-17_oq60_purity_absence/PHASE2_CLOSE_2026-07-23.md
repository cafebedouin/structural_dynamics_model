# Phase-2 consolidation + Phase-3 close (2026-07-23)

## Phase-2 witness — final engine (commit `d051d06c` + gc/canary repairs), all four legs

Full canonical `run_pipeline.py`: **exit 0**, `pipeline_output.json` mtime advanced
(18:31), manifest n=199 commit d051d06; opening ISSUES status-grammar gate and OQ-137
reading-totality gate both passed in-run; validation stage ok (16/16 TANGLED, suite
regenerated with 199 tests); `diagnostic.purity_n_scored/n_total = 153/199`.

Final-engine `classify_corpus` on all four legs (serialized, corpus md5 stable around each
run) + mechanical census join (`phase2_4leg.log`):

```
[testsets]   flips=11 over/under/valdiff = none  screen n=153 mean OK   JOIN CLEAN
[haiku]      flips=2  over/under/valdiff = none  screen n=492 mean OK   JOIN CLEAN
[flash]      flips=80 over/under/valdiff = none  screen n=668 mean OK   JOIN CLEAN
[kernel_v1]  flips=2  over/under/valdiff = none  screen n=1102 mean OK  JOIN CLEAN
money pair: 0.972 -> None / 0.948 -> None
```

**Unjoined residue: NONE** (the Phase-2 deliverable).

## Gates

- plunit: **22/22** — test_purity_absence (17: preflight injection, golden, precedence,
  token totality, fpn/gc ingest, m1–m4 producers, R3 polarity, C-FLOOR m5 + authored paths)
  + test_coexists_fpn_canary (5, incl. corpus-level no-leak). FPN canary needed a FIXTURE
  repair (not an engine defect): its synthetic donors authored no `coordination_type`, so
  post-C-FLOOR their purity is legitimately `unknown` and the test-local `pair_eligible`
  arithmetic threw — fixtures now author `resource_allocation` + the helper number-guards
  (unknown ⇒ ineligible, fail-closed). Same class as the preflight non-target control repair.
- 0b report-line graduation: `maxent_report.md` "Coverage: 153/181 ...", 
  `giant_component_analysis.md` "Intrinsic/Effective Purity (153/181 constraints with valid
  scores)" (the RENDERED section at :517-540 gained the denominator; the plan-anchored
  section at :904 was edited but is not exercised by the canonical run). Grothendieck's
  per-band `n_scored=n/N` remains **OPEN — graduation: next manual grothendieck report run**
  (the module is not wired into run_pipeline; Unwired ≠ worthless).
- `./scripts/gate.sh` run at close (output pasted in session/close commit).

## Phase-3 close ledger

ISSUES.md: OQ-60 → **resolved** (compressed per footer rule; R1–R4 kept as still-operative
rulings); follow-ups minted **OQ-236** (cross-leg comparability, P3), **OQ-237**
(print-what-you-dropped, P4), **OQ-238** (SI N≥6 floor undershoot, P4), all
`splits_from OQ-60`; dated updates on OQ-61 (new `undetermined` stability token) and OQ-62
(−1.0 exposure now engine-internal only). `issues_status --check`: 238 parsed, 0 malformed;
`omega_resolver check`: 0 problems; index regenerated. KNOWN_STATE 2026-07-23 tripwire entry;
CLAUDE.md Architecture Invariants gained the two-absence-token purity tripwire. Auto-memory:
`feedback_oq60_witnessing_lessons`, `project_oq60_resolution_and_ripple`.
