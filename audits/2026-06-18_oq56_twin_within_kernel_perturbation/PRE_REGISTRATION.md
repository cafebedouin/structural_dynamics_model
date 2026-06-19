# Pre-registration — within-kernel reading perturbation on the twins (OQ-56 D1)

**Committed BEFORE the analysis run.** Fixes the design, binning, and falsifiers so the
result cannot be narrated into a conclusion after the fact.

## The question (operator, 2026-06-18)

Hold the **model** and the **observer-seat** fixed; **vary the reading within a kernel**.
What does that perturbation reveal? Can kernels be grouped into orbits? Is any structure
found **model-invariant** (the twin check)?

## Substrate and why it changed mid-recon (a recorded finding)

The natural operator for "hold observer, vary reading" is `reading_diff`/`reading_diff_census`
(OQ-59). It is **schema-stranded** against the live/twin corpus and may not be used here:

- `reading_diff` reads authored `constraint_indexing:constraint_classification/3` cells (the
  per-(P,T,E,S) type map). **Witnessed:** all 1106 pre-reset `kernel_v1` files author it;
  the twins author **0**. Running the census on the haiku twin returns **954/954 pairs
  `robustly_undersampled`, 0 disparities** — a vacuous Pattern-5 pass. Positive control: the
  pre-reset census (`audits/2026-06-03_reading_diff_census/`, 615 pairs, 39.5% binocular)
  shows the census *code* produces real depth when cells exist; the twin *data* lacks them.

The de-leak rebuild replaced the authored (P,T,E,S) cell map with **4 computed perspective
seats** (`powerless / moderate / institutional / analytical`), serialized per constraint in
the existing twin outputs `outputs/pipeline_output.{haiku,flash}.json` (n=960 each, classified
at commit `8126231`, 2026-06-13; the same artifacts the twin_comparison audit used). The
perturbation runs on **that live schema**: observer-seat = one of the 4 perspectives.

## Unit and definitions

- **Unit:** a kernel with ≥2 readings (`cs_kernel_id` groups; ~328 per twin).
- **Per (kernel K, seat s):** `seat_disparity(K,s)` = number of *distinct* perspective-types
  taken across K's readings at seat s. `=1` ⇒ the seat reads every reading of K the same
  (situation-fixed at that seat); `≥2` ⇒ within-kernel **depth** is visible at seat s.
- **Depth-vector(K)** = the 4-bit vector (does seat s see ≥2 types?) over the 4 seats.

## Metrics

- **M1 — seat-depth gradient.** For each seat, the fraction of multi-reading kernels with
  `seat_disparity ≥ 2`. Which observer position most often *sees* within-kernel reading depth?
  (Descriptive; the seat gradient is the product. Composes with OQ-123's powerless-seat
  model-sensitivity — a different axis, reported alongside, not merged.)
- **M2 — model-invariance of the depth pattern (the load-bearing pre-registered test).**
  Over kernels present as multi-reading in BOTH twins: agreement = fraction with identical
  `depth-vector` across haiku/flash. Null: permute flash kernel→depth-vector assignments,
  N=1000, seed 20260618; band = 95th pct of permuted agreement.
- **M3 — orbit grouping (exploratory).** Group kernels by a structure signature
  (n_readings, depth-vector, claimed_type multiset across readings); report the orbit-size
  distribution and how stable orbit membership is across the two models.

## Pre-registered decision rules (M2)

- **PASS — within-kernel depth is a model-invariant property of the kernel:** observed
  agreement's Wilson-95% lower bound **>** permute band (95th pct).
  *Meaning, stated now:* which seats see a kernel's reading-depth is a fact about the kernel,
  reproduced across independent model draws.
- **FAIL / OPEN — depth is authoring-seat-expressive:** observed agreement **within** the
  band. *Meaning, stated now:* the within-kernel depth pattern is itself a draw-property (a
  σ/seat expression, OQ-118), not a fixed kernel property — which would make any reading-stance
  taxonomy keyed on it model-relative.

## Positive controls (required; witnessed before any M-number is reported)

1. **Data-load:** both outputs n=960, `perspectives` present with all 4 seats. *(Already
   witnessed in recon.)*
2. **Extraction matches the engine:** twin `claimed_type` agreement reproduces the
   independently-computed **0.721** from `2026-06-13_twin_comparison/FINDINGS.md`. *(Already
   witnessed in recon — confirms the JSON fields are read correctly.)*
3. **Non-vacuity (guards against repeating the reading_diff trap):** in EACH twin, at least
   some multi-reading kernels have `seat_disparity ≥ 2` at some seat. **HALT-and-escalate if
   the max per-seat depth rate is 0 in either model** — that would mean the live schema, too,
   does not express within-kernel depth, and the question needs reframing rather than a number.
4. **Null non-degeneracy:** permute band (M2) `< 1.0`. If chance agreement is ~1.0 the test
   is vacuous → report as OPEN, not PASS.

## What this is / is not

- **Is:** the shape of within-kernel reading-depth across observer seats, and whether that
  shape survives a model redraw. The product is the SHAPE (per CLAUDE.md determinism frontier).
- **Is not:** a re-measurement. A haiku reading and a flash reading of the same kernel slot are
  two different draws on two seats; a depth disagreement can be the engine correctly typing two
  genuinely different authored situations, not an error.

## Reproduce

```
python3 audits/2026-06-18_oq56_twin_within_kernel_perturbation/within_kernel_perturbation.py
```
Inputs: `outputs/pipeline_output.{haiku,flash}.json`. Output:
`audits/2026-06-18_oq56_twin_within_kernel_perturbation/results.json` + console.
