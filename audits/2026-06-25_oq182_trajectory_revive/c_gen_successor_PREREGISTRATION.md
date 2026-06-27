# C-gen SUCCESSOR pre-registration — substrate read of the ARI failure

**Locked 2026-06-26, BEFORE the TRACK quantity is computed.** This is a NEW,
separately pre-registered question — NOT a do-over that launders the C-gen fail
into a pass (operator ruling, this session). The original C-gen ARI verdict
stands and is reported regardless of this successor's outcome.

## Why a successor question (not a re-run of the same test)
The original C-gen locked metric was partition-ARI; it FAILED (ARI=0.117 < 0.50).
That verdict is final and reported. PRES=0.83 was observed POST-HOC as a near-miss
resolver — it is therefore **contaminated for gating** and may only be reported as
a descriptive statistic, never promoted to the thing that passes C-gen. The
honest successor uses a quantity **not yet observed** and asks a genuinely
different question.

## The successor question
The ARI failure has two possible causes the partition coefficient cannot
separate, and which one holds is settled by the SUBSTRATE, not by any coefficient:
- **Generation-EXPRESSIVE (real structure):** haiku and flash genuinely produce
  different trajectory structure for the same kernel-readings, so the families
  legitimately differ. The ARI failure is then a true finding about the engine's
  inputs, not a clustering defect.
- **Granularity / tie-break NOISE (artifact):** the readings have identical
  structure across legs but land in different families only because the
  data-dependent cut-height / HAC tie-break differs. The ARI failure is then a
  clustering artifact.

## Operationalization (granularity-insensitive; uses `fingerprint_shift`)
`logical_fingerprint:fingerprint_shift(C, Shift)` is identity-derived (a pure
function of C's authored data, independent of clustering — confirmed). It is the
structural signal upstream of family assignment (`group_by_shift`).

For every `cs_kernel_id` with ≥2 readings co-clustered in both legs, enumerate
reading-pairs (r1,r2). A pair is **SPLIT** if co-familial in exactly one leg. For
each split pair, in the leg where they are NOT co-familial:
- **TRACKS-STRUCTURE:** `fingerprint_shift(r1) ≠ fingerprint_shift(r2)` in that
  leg — the family split reflects a real per-reading structural divergence.
- **GRANULARITY-NOISE:** `fingerprint_shift(r1) == fingerprint_shift(r2)` in that
  leg yet they landed in different families — cut-height / tie-break artifact.

Let **TRACK = (# SPLIT pairs that TRACK-STRUCTURE) / (# SPLIT pairs).**

## Pre-registered gate (TRACK is unobserved at lock time)
- **TRACK ≥ 0.70 ⇒ the ARI failure is GENERATION-EXPRESSIVENESS, not artifact.**
- **TRACK < 0.70 ⇒ a material share of the ARI failure is clustering artifact.**

## Pre-committed DUAL-FINDING close-language (written before the result)
Report BOTH, neither overwriting the other:

- **If TRACK ≥ 0.70:** "C-gen FAILED at the partition level (ARI=0.117, locked
  bar < 0.50). The disagreement is generation-EXPRESSIVE: ≥70% of inter-leg family
  splits track real haiku-vs-flash `fingerprint_shift` differences, not cut-height
  noise. The HAC family product is **locally generation-stable** (PRES=0.83,
  descriptive) but **globally generation-expressive** — consistent with the
  framework's draw-stable/draw-unstable posture. Two findings, both stand."

- **If TRACK < 0.70:** "C-gen FAILED at the partition level (ARI=0.117). The
  failure is at least partly a clustering artifact (cut-height / tie-break): a
  material share of inter-leg splits join readings of identical structure. Family
  stability across the generation boundary is genuinely weak; the family product's
  cross-generation axis remains OPEN/weak, not merely 'expressive'."

## Positive controls (analysis logic — to be pasted before the verdict)
1. Confirm **same-family ⟹ same-`fingerprint_shift` within a leg** (families do not
   cross shift-groups, since `group_by_shift` precedes clustering). If this fails,
   the TRACK partition is ill-posed — halt.
2. Confirm the SPLIT set is non-empty (else TRACK is vacuous — report as vacuous).
3. Cross-check `fingerprint_shift` differs across legs for at least one known
   redrawn reading (generation actually changed structure — the premise of the
   whole test).
