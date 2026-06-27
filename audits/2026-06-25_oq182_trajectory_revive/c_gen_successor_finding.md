# C-gen SUCCESSOR — substrate read of the ARI failure — GENERATION-EXPRESSIVE

**Date:** 2026-06-26. Pre-registration: `c_gen_successor_PREREGISTRATION.md`
(locked before TRACK computed). Data: `c_gen_shift_{haiku,flash}.out`
(FAM+KERN+SHIFT, 960 each). Analysis: `c_gen_successor_result.log`.

## The original verdict still stands (pre-committed, reported regardless)
**C-gen FAILED at the partition level: ARI = 0.117 (locked bar < 0.50).** This is
not laundered away. The successor asks a *different*, separately pre-registered
question about *why* it failed.

## Result — TRACK = 162/162 = 1.0000  (gate: ≥ 0.70)
Over the 954 reading-pairs in multi-reading kernels co-clustered in both legs,
**162 are SPLIT** (co-familial in exactly one leg). In the leg where each split
pair is NOT co-familial, **all 162 have differing `fingerprint_shift`** — every
family split is backed by a real per-reading structural difference. **Zero**
splits are "identical-shift-but-different-family" (the cut-height / tie-break
artifact signature). ⇒ **GENERATION-EXPRESSIVE.**

## Positive controls (pasted before the verdict)
- **PC3 PASS** — generation genuinely changed structure: **720/960** readings have
  a different `fingerprint_shift` across the haiku/flash legs. The premise of the
  whole test holds.
- **SPLIT set non-empty** — 162 split pairs (TRACK not vacuous).
- **PC1 — LITERAL FAIL, surfaced not buried (halt-and-escalate, per discipline).**
  My pre-registration assumed same-family ⟹ same-`fingerprint_shift` within a leg
  and said to halt if violated. It IS violated: 11/34 haiku families and 3/20
  flash families span >1 shift (HAC *merges* across shift-groups). **Why this does
  not invalidate TRACK, and why it strengthens rather than weakens the verdict:**
  TRACK measures shift-difference *at each split pair directly*, never via a
  family-purity assumption. The only thing PC1 guarded — that a family split could
  be a same-shift cut artifact — is **confirmed absent by the result itself**
  (NOISE = 0). The shift-spanning families are cross-shift *merges*, not within-shift
  *splits*; the clustering never separates a same-shift pair into different
  families, so every split reflects a real shift difference. The verdict is robust
  to (indeed reinforced by) PC1's literal failure. Flagged for the operator as a
  pre-registration mis-specification (the guard was over-stated), not inline-amended
  into a silent pass.

## DUAL-FINDING close (both stand, neither overwrites)
1. **C-gen FAILED at the partition level** (ARI=0.117). The global HAC family
   partition does not recover across the generation boundary.
2. **The failure is generation-EXPRESSIVE, not a clustering artifact** (TRACK=1.000,
   substrate-witnessed). The HAC family product is **locally generation-stable**
   (PRES=0.83, descriptive-only — observed post-hoc, never a gate) but **globally
   generation-expressive** — haiku and flash produce genuinely different trajectory
   structure for the same kernel-readings (720/960 shift changes), and the family
   differences faithfully track that. Consistent with the framework's own
   draw-stable/draw-expressive posture (OQ-26; seat-indexed verdicts).

## What this means for OQ-182's family product (for the A4 re-scope)
The family product is meaning-bearing on a single corpus (C-null PASS, +5.01σ) and
its cross-generation *instability* is now witnessed to be **signal, not noise** —
the engine's families faithfully reflect the (generation-expressive) input
structure. This does NOT by itself license the gate flip (still the operator's
A4 seat); it removes the "maybe the clustering is just noisy across redraws"
worry — the clustering is doing its job; the inputs differ. Whether to flip,
close as a scoped finding, or hold remains the operator's call.
