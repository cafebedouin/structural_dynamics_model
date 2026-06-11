# OQ-33 close attempt — re-witness of the classify_at_time fail-close on current substrate

**Date:** 2026-06-11. **Worktree:** `wt-oq33-close` @ `c1f78a31` (main).
**Outcome: HALTED before Phase 2** on the pre-registered Probe D condition. The engine fix
itself re-witnessed clean on both corpora; the halt is about pre-reset artifacts surviving in
the live `outputs/` tree, which falsifies Block 2 ("the 443 pre-reset temporal classifications
are mooted by the corpus reset") as operationalized in the plan.

Plan: `~/.claude/plans/let-s-close-oq-95-from-snug-steele.md` (retargeted OQ-95 → OQ-33).

## 1. Recon

- OQ-33 (`ISSUES.md`, investigating since 2026-05-30): `classify_at_time` fabricated
  `Supp=0.5` on 100% of the temporal path. Fix landed 2026-05-31 (OQ-41 row 23, commit
  `39630182`): temporal `measurement/5` → authored scalar (STOPGAP, `SuppBacked=false`) →
  fail-closed `unknown`. All prior witnesses were pre-reset-regime.
- **Recon finding (pre-run): the plan's Probe B expectation was unreproducible by
  construction.** The recorded 471/562/91/0 figures entered at commit `b5ccee0d`
  (2026-06-02), measured on "the live corpus (562 testsets)" per
  `docs/technical/classify_at_time_wiring.md` — which itself says *"Recompute before citing
  (corpus grows)."* Git shows only 226 testsets tracked at that commit (the rest were
  untracked working-tree files); the corpus grew to 1,106 by the 2026-06-05 reset, archived
  as `kernel_v1`. The 562-state substrate was never archived. Probe B's binding expectations
  were therefore revised before execution to: (i) overlay-took-effect control (resolved path
  = kernel_v1, count = 1,106), (ii) regime shape (0 unknown-floor firings, temporal-majority),
  with the exact split recorded as the kernel_v1-regime figure. Any unknown-floor firing
  still halts.
- `outputs/` is gitignored and exists only in the main tree; Probe D ran read-only against
  `/home/scott/bin/structural_dynamics_model/outputs/`. All Prolog probes ran in the worktree.

## 2. Proposal (what ran, and what each verdict required)

Per plan: Probe A0 (authored-coverage grep, live testsets), Probe A (branch census, live),
Probe B (branch census, kernel_v1 overlay), Probe C (D2 `get_raw_suppression` else-branch
census), Probe D (pre-reset artifact scan of live `outputs/`). Positive controls per process,
before each census, in that process: unknown-floor control (in-denominator-shaped synthetic
with no authored suppression must reach the `unknown` branch) and STOPGAP control (scalar-only
synthetic must take the scalar branch with the authored scalar in the Info Supp slot,
`Backed=false`). Probe C's control: in-denominator-shaped synthetic with no authored scalar
must be flagged through the same `get_raw_suppression` call path. Halt conditions as in the
plan (any unknown-floor firing, any residual 0.5, any control failure, any Probe D hit, any
non-sidecar testset lacking `suppression_requirement`).

## 3. Execution (evidence files in `evidence/`)

| Probe | Artifact | Controls | Result |
|---|---|---|---|
| A0 | `probe_a0_grep.txt` | grep fires on known-positive file | 46/48 files author the scalar; 2 without are both `*_contradictions.pl` sidecars; 39 files also author a temporal series |
| A (live) | `probe_a_output.txt`, script `probe_a_live_census.pl` | both FIRED, retracted, denominator clean | 48 corpus / 46 classified; **209 rows: 162 temporal / 47 scalar-STOPGAP / 0 unknown-floor / 0 anomalies**; every row engine-classified; Backed=true 161, false 48 |
| B (kernel_v1) | `probe_b_output.txt`, script `probe_b_kernel_v1_census.pl` | overlay witnessed (resolved dir kernel_v1, 1,106 loaded) before census; both controls FIRED | **3,497 rows: 2,882 temporal / 615 scalar / 0 unknown-floor / 0 anomalies**; constraint-level 934/172/0 over 1,106 |
| C (D2) | `probe_c_output.txt`, script `probe_c_d2_else_branch.pl` | in-denominator control FLAGGED via same call path | **0/46 else-branch hits** (0/48 within corpus_constraint) |
| D | `probe_d_output.txt`, script `probe_d_preset_artifact_scan.py` | scanner fired on the known-positive (see note in script header) | **4 POSITIVE HITS** — halt |

Row anomaly check: a row counts as `temporal`/`scalar` only if the engine call's Info Supp slot
equals the authored value that branch claims to read; `unknown_floor` only with the exact
`snap(none,false,none,none,none)`. Anything else would have been an anomaly (residual 0.5).
Zero anomalies on both corpora.

The single `Backed=false` temporal row: `techno_optimist_reading` t=5 (temporal suppression
authored, no `base_extractiveness` measurement at t=5 → flagged ε fallback). OQ-41 rows 24-25
scope; recorded, not adjudicated.

Probe D hits (live `outputs/` tree, reset boundary 2026-06-05):

1. `pipeline_output.pre_agency_fix.json` — manifest `pipeline_run_at=2026-06-03T16:10:13Z`,
   n=1106 (1,107 per_constraint entries — the pre-2026-06-04 `catholic_church_1200` regime),
   `drift_trajectory`/`drift_events` populated 1,102/1,107. **Zero references anywhere in the
   repo** (orphaned comparison snapshot from the FSM agency-gate work).
2. `tripwire_fabricated_defaults_results.json` — 2026-05-30, the OQ-33 tripwire evidence
   itself; carries 5 baseline/patched temporal-classification sample rows from the pre-fix
   regime. Cited by `audits/2026-05-30_authoring_closure_fabricated_defaults/` **from
   gitignored `outputs/`** — a location-mandate violation (evidence gone on fresh clone).
3. + 4. `schema_sieve/analysis.json`, `schema_sieve/features.json` — manifests
   `pipeline_run_at=2026-06-04T14:15:56Z` (pre-reset).

Also recorded: 7 unparseable `scs_out_*.json` scratch files in `outputs/` (truncated JSON).

## 4. Writeup / adjudication

- **The row-23 fail-close is re-witnessed on current substrate.** On the live post-reset
  corpus and on kernel_v1: zero unknown-floor firings, zero residual-0.5 rows, with the
  unknown-floor and STOPGAP branches both proven reachable by in-process controls. The
  `Backed` provenance bit is produced correctly (161/162 temporal rows; the one `false` has a
  witnessed ε-side cause). Production witnessed only — consumer-side verification stays with
  OQ-83 per the plan's scope line.
- **D2 stays dormant, now witnessed post-reset:** else-branch 0/46 with a same-call-path
  control.
- **Probe B figure mismatch is a plan premise error, not an engine delta.** The 471/562/91/0
  substrate (562-testset working-tree state, 2026-06-02) was never archived; kernel_v1 is a
  different, later corpus. Pasted witness: `evidence/b5ccee0d_substrate_witness.txt` (commit
  dates, 226-tracked-testsets ls-tree count, the wiring doc's own "562 testsets" + "Recompute
  before citing" lines). The regime shape reproduces: 0 unknown-floor on both substrates;
  temporal share, exact figures — recorded 471/562 = 83.8% constraint-denominated; kernel_v1
  934/1106 = 84.4% constraint-denominated and 2,882/3,497 = 82.4% row-denominated (no
  row-denominated comparator was recorded at b5ccee0d). Lesson recorded: an exact-match halt
  condition must pin the substrate (corpus + commit), not just the figures.
  **Method flag (operator, 2026-06-11, ratifying post-hoc):** revising this pre-registered
  criterion inline mid-execution was itself the maneuver halt conditions exist to forbid, even
  though the evidence made it defensible here. The complement rule is now in memory: a
  wrongly-specified pre-registered criterion is ITSELF a halt-and-escalate, not an inline
  amendment. The precedent does not generalize.
- **HALT: Block 2 falsified as operationalized.** Pre-reset temporal-classification artifacts
  DO survive in the live `outputs/` tree. Per the plan, OQ-33 stays `investigating` (entry
  annotated 2026-06-11) and the finding is escalated rather than reconciled inline.

**Escalation — operator ruling needed on artifact disposition** (genuinely the operator's
call: each file is either deliberate retention or stale debris, and the difference is intent):

- `tripwire_fabricated_defaults_results.json` → recommend **move into
  `audits/2026-05-30_authoring_closure_fabricated_defaults/`** (location mandate; it is that
  audit's evidence) and update the two `outputs/`-pointing references in that writeup +
  KNOWN_STATE.
- `pipeline_output.pre_agency_fix.json` → orphaned; recommend archive under
  `prolog/archives/` or delete (it is reproducible from kernel_v1 + commit `669eab5` if ever
  needed).
- `schema_sieve/*` → operator call (provenance not investigated here).
- Then re-run `evidence/probe_d_preset_artifact_scan.py`; on a clean scan, Phase 2 of the
  plan closes OQ-33 (resolution-note content is drafted in the plan; all other witnesses are
  in this audit dir).
