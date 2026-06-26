# C-null protocol — FROZEN (pre-registered 2026-06-25, before any spend-tier separation number is seen)

**Purpose.** C-null is the OQ-182 **scope-setter**, not a flip-blocker. It answers: do the HAC
structural families MEAN anything, or could a shuffle produce equally "tight" families? It sets
OQ-182's close verdict — *validated meaning-bearing* (pass) vs *validated safe+stable, meaning OPEN*
(fail). This file is frozen BEFORE the spend tier runs; pre-registration only counts if it precedes
the data. Do not edit the statistic, null model, N, or threshold after a separation number is seen.

---

## Statistic — mean silhouette over the HAC family partition

Distance-matrix-native (no coordinate re-derivation), bounded `[-1, 1]`. Computed directly from the
precomputed pairwise distance facts `pair_dist/3` over the HAC family partition (`family_assignment/2`):

For each constraint `c`:
- `a(c)` = mean distance from `c` to every OTHER member of `c`'s own family (intra-family).
  (Singleton family ⇒ `a(c)` undefined; exclude singletons from the mean, and report the count
  excluded — singletons are anomalies, not silhouette-scorable.)
- `b(c)` = min over other families `F` of [mean distance from `c` to all members of `F`]
  (nearest-other-family).
- `s(c) = (b(c) - a(c)) / max(a(c), b(c))`.

**Report: mean `s` over all non-singleton constraints** (the real statistic), per corpus
(`testsets/` and `kernel_v1`).

---

## Null model — per-component-independent permutation

The 4 `trajectory_distance` components (weights `config.pl:572–575`):
1. **shift** (type-sequence) — weight 0.35
2. **metric** (chi / entropy / conf sequence) — weight 0.25
3. **stability** (coupling, purity tuple) — weight 0.25
4. **pathology** (drift, voids tuple) — weight 0.15

Export each constraint's 4 component feature-bundles. For each of N permutations:
- **Independently** permute the constraint→bundle map FOR EACH of the 4 components SEPARATELY
  (4 independent shuffles per permutation, not one joint shuffle).
  - *Why per-component, not joint:* a joint shuffle merely relabels intact feature-vectors —
    distances are preserved up to relabeling and the silhouette is unchanged ⇒ a false PASS. The
    per-component shuffle destroys the cross-component co-occurrence structure that is exactly what a
    "structural family" claims to capture, while preserving each component's marginal distribution.
- Recompute the weighted distance matrix from the shuffled bundles (same 4 weights).
- Re-run HAC at the **SAME cut height** (`trajectory_family_cut_level = 0.30`, `config.pl:576`).
- Recompute mean `s` (same statistic, same singleton-exclusion rule).

---

## N and threshold (frozen)

- **N = 200** permutations.
- **Threshold (one-sided):** real mean `s` must **exceed the 95th percentile** of the null mean-`s`
  distribution. Chosen now; NOT after seeing the first shuffle.

## Mandatory reporting add (does not change the threshold)

**Report the null FAMILY-COUNT distribution alongside null `s`.** Silhouette is not scale-invariant
across cluster counts: a shuffled (less-structured) input at a *fixed* cut tends to produce more,
smaller families, which can inflate the mean nearest-other-family distance `b` and thus inflate null
`s` — biasing **toward a false FAIL** (real families real, but scoped to "meaning OPEN"), never a
false PASS. Reporting the null partition granularity lets a fail be read against whether the null
partitions are even comparable in family-count to the real partition. Report-only; threshold unchanged.

---

## Outcome rule (frozen)

- **C-null PASS** (real mean `s` > null 95th pct): OQ-182 may close as
  *"validated meaning-bearing product."*
- **C-null FAIL**: do NOT ship families as meaning-bearing. OQ-182's close scopes to
  *"validated = safe + stable, NOT semantically verified; family meaning OPEN,"* recorded with this
  shuffle test named as exactly what would close it.

---

## ADDENDUM — POSITIVE CONTROL IS STEP ONE (added 2026-06-25, operator-driven; strengthening, not a change)

C-null is an **absence claim** ("structure is gone after the shuffle"), so by the project's
positive-control rule it is a fact about the *probe* until the probe is shown to *find*. A joint-shuffle
false-pass is a probe that cannot find: it preserves the within-component correlation it is meant to
destroy, so "families survive the null" decodes to "my null had no teeth," not "families are real."
**Therefore the FIRST witnessed step of the C-null pass is the positive control, pasted, gating the
null — before any survival number is trusted.** This does NOT change the statistic, null model, N, or
threshold above; it gates whether they may be reported.

**The control must distinguish per-component (destroying) from joint (toothless) — and that distinction
appears ONLY under re-clustering.** Derived 2026-06-25: scoring the *real partition* under shuffled
distances (no re-cluster) collapses the silhouette for BOTH shuffles (relabeling breaks the i↔family
correspondence either way), so it cannot tell per-component from joint. So the control re-clusters under
each shuffle and shows:
- **per-component** re-clustered silhouette **collapses below** the real silhouette (teeth), AND
- **joint** re-clustered silhouette **≈ real** silhouette (the false-pass, demonstrated).

Only with that pasted does the real null run. **Quantile flag (load-bearing):** the 95th-percentile
threshold must be computed over the family-quality statistic under the **per-component (destroying)**
shuffle draws — NOT joint draws — or the whole quantile inherits the toothless null and the threshold is
counterfeit one level up.

**Chimera surgery map (for the harness).** The 4 components do not map to separable trajectory sub-terms:
`shift_distance`+`metric_distance` both read `Points` (per-point types vs chi/entropy/confidence);
`stability_distance`+`pathology_distance` both read `Summary` = `trajectory_summary/12`
(preservation/coupling/purity/boltzmann vs signature/drift_count/drift_max_severity/voids). A per-component
chimera `trajectory_cached(C, Ctx, trajectory(C, Points', Summary'))` needs FIELD-level reassembly so that
`group_by_shift` sees the π_shift-shuffled shift patterns AND the within-group HAC sees the fully
component-shuffled distances. Build the chimera, re-assert `trajectory_cached`, then call
`run_hierarchical_clustering/1` (it re-reads `trajectory_cached` + `pair_dist`; recompute `pair_dist` from
the chimera too).

**Frozen by:** OQ-182 cheap tier, 2026-06-25; positive-control addendum same day. Spend tier runs this verbatim.

---

## ERRATUM — Chimera surgery map is mechanically wrong (2026-06-25, at spend-tier execution)

The "Chimera surgery map" above is **incorrect** and the harness (`c_null_harness.pl`) deviates from it.
The **frozen quantities are untouched** (statistic = mean silhouette; per-component-independent shuffle;
N=200; threshold = real mean silhouette > 95th pct of the per-component null). Only the *mechanism* changed.

**Why the map fails.** `group_by_shift/2` (`context_profile_mining.pl:559`) recomputes the shift
pre-grouping key via `logical_fingerprint:fingerprint_shift/2`, which classifies from the **constraint
identity** (`logical_fingerprint.pl:113`, `dr_type` at four standard power levels) and **ignores
`trajectory_cached` entirely**. So building a chimera `trajectory_cached` and calling
`run_hierarchical_clustering/1` leaves the shift pre-grouping pinned to the *real* shift boundaries
regardless of σ_shift — a toothless / false-PASS bias, and it breaks the joint control's validity.

**What the harness does instead.** It builds the shift-groups itself (`make_groups/4`, keyed on
`fingerprint_shift(C[σ_shift(i)])` so σ_shift actually moves the grouping) and reuses only
`cluster_all_groups/2` + `assign_families/1`. No chimera `trajectory_cached` is constructed: the four
component distances are precomputed ONCE over the real trajectories into symmetric matrices, and each
draw is a pure index recombination `Σ_k w_k·comp_k(σ_k(i), σ_k(j))` (FIDELITY at identity reproduces the
engine `pair_dist` to 0.0; GROUPING-FIDELITY at identity reproduces `group_by_shift` exactly).

**Result (testsets/ leg, 2026-06-25):** PASS. RealSil 0.161119 > P95(null) −0.026436; standardized gap
+5.01σ; 0/200 null draws reach real; null family-count centers at 15 vs real 11 (conservative direction).
Reproducible under seed 20260625 (SWI 9.2.9). Full record: `c_null_results.log`, `c_null_distribution.json`.
