# C-gen — haiku↔flash same-kernel family recovery — **FAIL at locked bar**

**Date:** 2026-06-26. **Legs:** `testsets_haiku/` + `testsets_flash/` (960 each, 1:1
matched). **Plan step:** Part A / A2 (LIVE FALSIFIER). Pre-registration:
`c_gen_PREREGISTRATION.md` (locked before any result was seen).

## Substrate witnesses (overlay took effect — not the 104 default)
- haiku: `corpus_constraint = 960`, `family_assignment = 960` (all clustered), 34 families.
- flash: `corpus_constraint = 960`, `family_assignment = 960` (all clustered), 20 families.
- Counts pasted in `c_gen_haiku.stderr.log` / `c_gen_flash.stderr.log` (the
  `[cgen] corpus_constraint count=960` lines confirm the `asserta` overlay loaded
  the twin, not the default `testsets`).
- Runtime ~30s (haiku) / ~37s (flash). No engine edits; `trajectory_enabled` stayed 0.

## Result (against the locked pre-registration)
- **Headline ARI (all 960 shared clustered constraints): 0.1171.**
  Locked bar was ARI ≥ 0.60 PASS; < 0.50 FAIL. **⇒ FAIL.**
- Robustness ARI (957 constraints in the 328 multi-reading kernels): 0.1173 — same.
- **PRES (within-kernel reading-pair co-familiality preserved across legs):
  792/954 = 0.8302.** (This was pre-registered only as the *near-miss* resolver,
  which the verdict path did not reach — recorded here because it is the
  granularity-insensitive companion the global ARI cannot see.)
- haiku family sizes `[1×5, 2,4,4,5,6,6,8,11,12,13,14,15,17,18,18,18,20,21,25,26,30,34,46,47,56,68,99,146,166]`
  vs flash `[1×5, 4,7,10,12,23,24,25,38,41,53,85,105,155,166,207]`.

## Reading (for the A4 operator re-scope — NOT an agent ruling)
The two facts pull in opposite directions and the gap *is* the content:
1. **Global partition is generation-expressive, not generation-invariant** (ARI
   0.12). The two models produce different *granularities* (34 vs 20 families) and
   different global structure for the same 960 kernel-readings. ARI penalizes the
   granularity mismatch heavily.
2. **Local within-kernel relations are ~83% preserved** (PRES). Whether two
   readings of one kernel land together is substantially stable across the
   generation draw.

This is consistent with the framework's own posture (OQ-26: generation is
stochastic; "draw-stable ≈ situation-fixed, draw-unstable ≈ seat-expressive";
a redraw occupies a new seat). A low cross-generation ARI may mean the *global*
family structure is seat-expressive rather than that the subsystem is invalid —
but that is a re-scope ruling, not an agent call.

## Halt-and-escalate metric note (NOT inline-amended to a pass)
I locked ARI before seeing results and do **not** move it post-hoc. I flag — for
the operator only — that ARI conflates two failure modes the C-gen question may
want to separate: *cut-height/granularity* divergence vs *structural* divergence.
The cut-height escalates per-corpus (C1), so two corpora naturally land at
different granularities, mechanically depressing ARI even when dendrograms agree.
PRES=0.83 is the granularity-insensitive read. **Per audit-plan discipline, a
possibly-mis-specified pre-registered criterion is halt-and-escalate, not
silently re-specified.** The strict verdict stands: **C-gen FAILS at the locked
bar.** Whether the right cross-generation invariance metric is partition-ARI or a
granularity-insensitive measure (PRES / dendrogram-cophenetic correlation) is the
operator's re-scope call at A4.

## Consequence
Per plan A4: **do not flip the gate.** C-gen is a live falsifier and it failed at
the registered bar. Recorded as the verdict; the re-scope (re-specify the metric,
accept generation-expressiveness as a finding, or hold the family product OPEN on
the cross-generation axis) is surfaced for the operator.
