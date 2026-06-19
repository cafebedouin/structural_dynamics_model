# Findings — within-kernel reading perturbation on the twins (OQ-56 D1)

Adjudication of `results.json` against `PRE_REGISTRATION.md`. Substrate:
`outputs/pipeline_output.{haiku,flash}.json` (n=960 each, classified at `8126231`,
2026-06-13). 328 multi-reading kernels per twin. Analysis is deterministic Python over the
4-perspective-seat schema; permutation null N=1000, seed 20260618.

## Positive controls (all pass — reported before the M-numbers)

- **Extraction matches the engine:** twin `claimed_type` agreement = **0.7208**, reproducing
  the independently-computed 0.721 of `2026-06-13_twin_comparison`. The JSON fields are read
  correctly.
- **Non-vacuity:** max per-seat depth rate = **0.881** ≫ 0 — the live schema richly expresses
  within-kernel depth (unlike the stranded `reading_diff`, which returned 0; see
  PRE_REGISTRATION). The reading_diff trap is **not** silently repeated here.
- **Null non-degeneracy:** permute band95 = 0.265 < 1.0.

## The recorded blocker: `reading_diff` was schema-stranded — FIXED 2026-06-19

All 1106 pre-reset `kernel_v1` files author `constraint_classification/3`; the twins author
**0**. The within-kernel census on the haiku twin returned **954/954 pairs
`robustly_undersampled`, 0 disparities** — vacuous. The de-leak rebuild dropped the authored
(P,T,E,S) cell map the operator consumes and replaced it with named stakeholder seats.

**Resolved (commit `01cff6a7`):** `reading_cells/2` now unions the live stakeholder-seat schema
(`stakeholder_seats:stakeholder_context/3` emits the same `context/4` tuple +
`dr_type_for_stakeholder/3`), additive and non-regressive on archives. The haiku census now
returns **136 robustly_binocular / 111 key_fragile / 707 robustly_undersampled** (191 exact
disparities); the 707 are now MEASURED coverage gaps (a reading authored no stakeholders), not
a vacuous no-cells pass — both-stakeholder pair coverage is 26% (haiku) / 61% (flash). So D1's
perspective-seat analysis below can now be cross-checked against a working stakeholder-seat
`reading_diff` (a follow-on, not re-run here).

## M1 — the seat-depth gradient (headline descriptive product)

Varying the reading within a kernel moves the type at *some* seat for **98.2% (haiku) /
95.7% (flash)** of multi-reading kernels. The perturbation is highly live. Per-seat rate of
"this seat sees ≥2 types across the kernel's readings":

| seat | haiku | flash |
|---|---|---|
| powerless | 0.820 | 0.878 |
| moderate | 0.854 | 0.881 |
| **institutional** | **0.643** | **0.369** |
| analytical | 0.747 | 0.863 |

**The institutional seat is the flattening observer:** it reads a kernel's divergent readings
as the *same* type far more often than the powerless/moderate seats do — most starkly in flash
(0.369). Within-kernel reading depth is largely invisible from the institutional position.
This composes with OQ-123 (institutional seat = highest raw twin-agreement but narrowest
margin): institutional is the flattest seat on **both** the model-perturbation and the
reading-perturbation axes. *(Reported alongside OQ-123, not merged — different perturbation
axes.)*

## M2 — model-invariance of the depth-vector (pre-registered test)

Over the 328 kernels multi-reading in both twins, agreement on the 4-bit depth-vector:

- observed **0.3293**, Wilson-95 lo **0.2806**, permute band95 **0.2652**, permute-mean 0.234.
- **Verdict (per the committed rule): PASS_model_invariant** — lo (0.281) > band95 (0.265).

**But the margin is thin (+0.015) and the structure is uneven**, so the honest claim is
*"which seats see a kernel's reading-depth is a weak, mostly-model-invariant property of the
kernel — except at the institutional seat, where it is nearly draw-expressive."* Per-seat
depth-bit agreement:

| seat | depth-bit agreement |
|---|---|
| moderate | 0.869 |
| analytical | 0.817 |
| powerless | 0.759 |
| **institutional** | **0.567** |

The institutional depth-bit is the least model-reproducible — consistent with flash flattening
it. Given the thin M2 margin, this PASS warrants its own registered replication on a fresh
twin pair before it is load-bearing (the falsifier: re-run on a new draw; if lo drops into the
band, depth is draw-expressive, OQ-118).

## M3 — kernels DO group into orbits, but membership is draw-sensitive

- 125 (haiku) / 111 (flash) distinct full signatures `(n_readings, depth_vector,
  claimed_type-multiset)` over 328 kernels; **largest single orbit = 29 kernels.** The
  distribution is structured, not uniform — orbits are real.
- The coarse depth-vector class is dominated by **(1,1,1,1) = all seats see depth = 140/328
  (43%)** (full binocularity), then (1,1,0,1)=64 (institutional flat), (0,1,1,1)=24
  (powerless flat).
- **Cross-model orbit membership: full signature 0.134; coarse depth-vector class 0.329.**
  Orbit *structure* (the distribution shape) is robust; individual kernel *membership* is
  largely model-relative.

## What this means for OQ-56 (the taxonomy this audit was the prerequisite for)

A reading-stance taxonomy keyed on within-kernel depth-vectors would be **mostly
model-relative**: only ~33% of kernels keep their depth-class — and only ~13% their full
signature — across an independent model redraw. The cross-kernel clustering OQ-56 needs
therefore **cannot be read off one model's corpus as if discovered**; the clustering is itself
seat/draw-expressive (an Ω_P selection-seat, exactly as OQ-56 already declares). The robust,
declarable invariants are coarser: the **seat gradient** (institutional flattens; powerless/
moderate see depth — M1) and the **orbit-distribution shape** (M3), not per-kernel membership.

## Cross-references

- Composes with OQ-123 (institutional seat model-sensitivity), OQ-118 (σ/seat draw-stability).
- The `reading_relation` model-divergence (0.392) + `overridden` 51-vs-4 finding is from the
  recon census, tracked under the newly minted committer-axis OQ.
- Kernel-orbit grouping (M3) tracked under the newly minted kernel-orbit OQ.
