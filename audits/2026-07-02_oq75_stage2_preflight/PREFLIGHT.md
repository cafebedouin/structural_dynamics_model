# OQ-75 Stage-2 go — preflight checklist, verified live (2026-07-02)

**Purpose:** the operator's Stage-2 "go" is not a binary — this preflight re-confirms every
precondition line with same-turn output so the go can be scoped honestly. Code state: HEAD
`a2f6edc7` (2026-07-02). All counts below are same-turn shell/probe output, not remembered
numbers.

## MET

**1. OQ-109 resolved; stability-table instrument exists.**
`ISSUES.md` OQ-109 `Status: resolved — 2026-06-13 (operator ruling)`.
`audits/2026-06-12_cohort_zero/stability_table.json` present (93,814 bytes; keys:
`stories_total`, `stories_with_replicates`, `min_population_for_sigma_seat_verdict`,
`per_story`, `within_vs_between`).
*Note (flag, not blocker):* the table's interpretation contract ("compare only draw-stable
fields") has an open successor — **OQ-118** (priority 1, open: draw-stability tracks
field-construction-type, not the σ/seat line). Stage-2 analysis design should read the table
under OQ-118's contract, not OQ-109's original σ/seat framing.

**2. Power tiers cleared ~3×.**
Twin legs: 960 + 960 `.pl` files (`ls | wc -l`, same-turn). Distinct `cs_kernel_id` values:
**331 per leg** (960 files-with-fact each; grep over
`^narrative_ontology:cs_kernel_id(`, distinct arg-2). Tier 2 needs ~250–300 stories → 960
each ✓. Tier 3 needs ~100–150 kernels → 331 ✓.
*(First grep attempt returned 0 — quoted-atom pattern error, corrected; the 331 is from the
fixed pattern whose positive control is the 960 files-with-fact count.)*

**3. Fresh twin classifications exist.**
`outputs/pipeline_output_haiku.json`: `pipeline_run_at=2026-07-02T06:06:27Z`,
`n_constraints=960`, `code_commit_short=b733b23`, **`code_dirty=true`**.
`outputs/pipeline_output_flash.json`: `2026-07-02T06:08:21Z`, 960, `b733b23`,
**`code_dirty=true`**.

**4. Twin-comparison harness + pre-registered run exist.**
`audits/2026-06-13_twin_comparison/`: `PRE_REGISTRATION.md`, `RESULTS.md`, `FINDINGS.md`,
`twin_comparison.json`.

## NOT MET (with two corrections to the exploration record)

**1. Flat-control stratum EMPTY in the twins — CONFIRMED.**
`testsets_haiku`: 0 flat_control-named files, 0 files carrying `flat_control_of(`, 0 fact
lines. `testsets_flash`: 0 / 0 / 0.
**Positive control (nonzero on the leg that has them):** `testsets/` = 9 flat_control-named
files, **10 files carrying `flat_control_of/2`, 10 fact lines** — same-turn count. (The
exploration hypothesis "10 files / 20 facts" was half right: 10 files ✓, facts are 10, not
20.) The OQ-76-mandated construction-pair stratum cannot be reported from the twins without
regeneration (a spend ruling) or an explicit N/A declaration.

**2. Part (b) tooling not built — CONFIRMED, but the dependency statement was STALE.**
`check_axis_boundary.py` header confirms it is a fail-loud reachability **guard** (v8 §8
invariant), not a correlation statistic; no §7.1 cross-axis correlation tool exists.
**Correction: OQ-15 is RESOLVED (2026-06-24), not open** — `ISSUES.md` OQ-15
`Status: resolved — load-bearing core, 2026-06-24: Phase 2 ruled policed-in-place (v8); the taint
guard is the structure. Synthesis (v7 named mediator) preserved as a triggered upgrade`; ruling
commit `279d7c24` ("docs(OQ-15): Phase 2 RULED policed-in-place (v8); core resolved"). The taint
guard IS the structure, and the v7 mediator/synthesis layer is preserved only as a *triggered
upgrade*. So part (b) is not "gated on OQ-15" — there is, by ruling, no mediator layer coming to
consume. Building the §7.1 correlation statistic is its own build item (and would be a candidate
trigger for the preserved upgrade path). Honest menu wording: "part (b) tooling unbuilt; its
former named dependency has resolved to a design posture that leaves the statistic as a standalone
build."

**3. Part (a) tooling — PARTIAL CORRECTION: the core counter EXISTS.**
The exploration claim "no `false_*`/`dr_claim_mismatch` corpus-prevalence counter exists" is
an over-claim. **`python/audits/oq49_override_remeasure.py`** (run 2026-06-14,
`audits/2026-06-14_oq49_remeasure/`) is a read-only per-corpus counter emitting a per-reading
`ROW <id> <MT> <FT> <Sig> <eff> <fnl_source>` table — signature prevalence (unbound cascade
idiom), override effectiveness (MT≠FT), FNL source tagging — with per-process positive
controls (PC_CLAUSE878, PC_SOURCE1, PC_LIVECHANGE), any corpus via argv.
What it does NOT yet do for the Tier-2 diff-distribution readout: **the gap is specific and must
be labeled, not waved at.** `oq49_override_remeasure.py` counts *override firing* (MT≠FT) and
tags FNL source — it does NOT count `false_*` / `dr_claim_mismatch` PREVALENCE per-corpus,
per-claimed-type cells, or the "too-small-a-diff = author held the key" health-check framing that
IS OQ-75 part (a). So "part (a) is an extension of a witnessed tool" is an ESTIMATE of the build
shape (the corpus-loop, per-process controls, and overlay recipe are reusable), not a confirmed
drop-in — someone must confirm the counter can be pointed at `dr_claim_mismatch`/`false_*`
prevalence rather than per-override effectiveness before the cost line ("materially cheaper") is
stated as fact. The correction stands (it is NOT from-scratch); its confidence is "reuses the
harness," not "counts the right thing already."

**4. `code_dirty: true` on both twin manifests — CONFIRMED** (item 3 above, both
`b733b23`+dirty). A clean-tree `classify_corpus` re-run on each twin is owed before any
citable Stage-2 numbers.

## What this leaves for the ruling

(a) **scoped go** — part (a) only: extend `oq49_override_remeasure.py` to the diff-distribution
readout + clean-tree twin re-classification; construction-pair stratum declared N/A for this
cohort (record in OQ-75/OQ-76). **Under (a), OQ-75's headline staked prediction — the
cross-axis correlation falsifier — stays UNTESTED**; "go" must not be misread as "the OQ's
falsifiable core is in motion."
(b) **full go** — additionally fund flat-control regeneration for the 331 twin kernels
(~331 stories × 2 models; prior Haiku full run ≈ $27 / 1,005 stories → roughly $9–18 total,
price precisely at ruling time) and sequence the §7.1 correlation-statistic build.
(c) **hold** — this preflight is the blocking witness.
