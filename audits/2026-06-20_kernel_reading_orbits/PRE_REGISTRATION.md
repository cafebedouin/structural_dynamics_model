# Pre-registration — Phase 1 cross-twin orbit-key diagnosticity (OQ-150)

**Committed BEFORE the agreement statistics are computed.** Phase 0 recon (`RECON.md`) is done;
positive controls pass; the agreement numbers have NOT been computed. This file fixes the design,
binning, decision rule, and falsifiers so the result cannot be narrated after the fact. Reuses
the 2026-06-18 within-kernel method (permutation null N=1000, fixed seed, Wilson-95 vs band95).

## The question (OQ-150)

Do readings (across kernels) and kernels form **orbits** under candidate grouping keys, and is
any such grouping **draw-robust** (reproducible across an independent model redraw) — hence
usable as a diagnostic vocabulary for OQ-56 — versus **draw-sensitive** (model-relative,
report-only)? The headline is draw-stability, NOT which orbits exist on one model.

## Unit, keys, statistic

- **Reading-orbit unit:** one constraint id `kernel__reading` (all 960 common across twins).
  A reading-orbit key assigns each id a label; the orbit = the set of ids sharing a label
  (across all kernels).
- **Kernel-orbit unit:** one kernel (331; 328 multi-reading). A kernel-orbit key assigns each
  kernel a structure label.
- **Membership-agreement statistic** `A(key)` = fraction of common units whose key label is
  **identical** across the two twins. A per-unit Bernoulli proportion ⇒ Wilson-95% lower bound
  is correctly typed (RECON Statistic-family check).
- **Permutation null:** shuffle one twin's unit→label assignment (N=1000, seed **20260620**),
  recompute agreement; `band95` = 95th percentile of the permuted agreement distribution.

## Keys tested (JSON-computable pass — Phase 1)

Reading-orbit keys (unit = id):
- **R1 observer orbit** — `signature` string (gauge_orbit proxy; `classifications` empty).
- **R2 commitment-apparatus orbit [operator axis 1]** — `cs_pattern` (9 classes).
- **R3 terminal-projection orbit (committer) [operator axis 2]** — `cs_drift_terminal`.
- **R4 terminal-projection orbit (observer)** — dominant dr_type across the 4 `perspectives`
  seats (mode; ties broken by the priority cascade order in CLAUDE.md). Reported separately
  from R3 (committer vs presheaf sense — never merged).
- **R5 seat-signature / role-vector orbit** — the 4-tuple `perspectives`
  (powerless,moderate,institutional,analytical) dr_types. The OQ-56 reading-stance unit.

Kernel-orbit keys (unit = kernel, multi-reading only):
- **K1 structure-signature** — `(n_readings, depth_vector, claimed_type-multiset)` (lifts the
  2026-06-18 M3 signature; prior cross-model membership 0.134 full / 0.329 depth-class — a
  *different object* from the reading-orbit keys, cited as prior not as this result).

Deferred to Phase 1b Prolog probe (cost-gated): commitment-apparatus via `cs_pattern` is already
in JSON, but **axiom-grounding-profile** (`cs_axiom_grounding`) and **obstruction-class**
(`cs_kernel_obstruction/4`) are not serialized.

## Pre-registered decision rule (per key)

A key is **DRAW-ROBUST (diagnostic, declarable)** iff ALL hold:
1. **Reproducible:** Wilson-95 lower bound of `A(key)` **>** `band95`.
2. **Non-degenerate null:** `band95 < 1.0` (else chance agreement ~1 ⇒ test vacuous ⇒ OPEN).
3. **Non-degenerate partition on BOTH twins:** the key takes **≥2 distinct labels** on each
   twin AND the largest single orbit holds **< 95%** of units on each twin (a key that lumps
   nearly everything into one label is trivially "agreeing" and carries no partition
   information — this is the per-twin leg of the plan's "both-twins" gate).

Otherwise the key is **DRAW-SENSITIVE (model-relative, report-only)**.

## Multiplicity (named, per the plan)

~6 keys this pass, each tested at α=0.05 via its own permutation `band95` ⇒ ~6 shots at a 5%
false-positive. A spurious menu entry (a draw-sensitive key dressed robust) is the **costlier**
error than an empty menu. Controls: (a) `band95` is reported **per-key-unadjusted** — a single
borderline pass is read as "candidate," never "robust on its own"; (b) the cross-twin agreement
test is itself stringent (a key must reproduce across two *independent model draws* AND survive
the differing-commit confound). No across-key α correction is applied numerically; instead the
thin-margin keys are flagged for the registered replication the 2026-06-18 audit also owed.

## Pre-stated expectations (commit the priors, let the measurement rule)

- **R2 (apparatus, axis 1) is predicted MOST LIKELY to FAIL** the robustness bar — the committer
  family is the most model-divergent layer (OQ-149: `cs_reading_relation` agreement 0.392 vs
  `claimed_type` 0.721). If R2 comes back draw-sensitive, **that is the finding** ("the
  commitment system you most want to group by is model-relative") — a substantive Ω_E result,
  not a method failure. Caveat: 0.392 is `cs_reading_relation`, not `cs_pattern` directly — same
  family, not the same measurement, so this is an *expectation to test*, not a result.
- R5 (seat-vector) is the richest key (4 seats) ⇒ likely the lowest raw agreement (more ways to
  differ); judged by the rule, not by raw agreement.
- R1 (observer `signature`) expected most robust (closest to the `claimed_type` 0.721 layer).

## Kill condition (pre-committed, the empty-menu terminus)

**If ZERO reading-orbit keys (R1–R5) clear the draw-robust rule**, OQ-56's candidate
stance-vocabulary menu is **empty**, and OQ-53's transpose closes as
**`foreclosed by finding — model-relative only`** (typed **Ω_E** — the measurement forecloses
the draw-robust build; no operator declines, so "declined"/Ω_P is the wrong type), NOT "gated
pending ruling." This branch is committed here at design time, not left as a Phase-4 silence.

## Outputs

- `phase1_orbit_keys.py` (under the audit dir) → `phase1_results.json` + console.
- Adjudication in `FINDINGS.md` against this file.

## Reproduce

```
python3 audits/2026-06-20_kernel_reading_orbits/phase1_orbit_keys.py
```
