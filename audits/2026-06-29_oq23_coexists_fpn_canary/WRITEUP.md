# OQ-23 canary — coexists_with FPN exclusion: VERDICT RED (exclusion already violated)

**Date:** 2026-06-29
**Ruling under test:** operator (a+) — ratify documented-only, convert "holds by
absence" into a positive-controlled *checked* invariant (canary).
**Outcome:** the canary did its job and **falsified the premise it was built to
backstop.** The `coexists_with` contamination exclusion does **not** hold on the
corpus — it is *already violated* via the `affects_constraint` side channel,
wherever coexists siblings co-exist with non-sentinel differing purity. This is
the Step-3 **RED** branch: STOP, do not self-resolve, re-escalate to the operator.

## Artifact

`prolog/tests/test_coexists_fpn_canary.pl` — positive-controlled canary sited at
the construction point (`constraint_neighbors/3`). Leak detection is faithful to
the engine: it reads `effective_purity/4`'s `contamination_detail` and counts a
pair as leaked iff the real consumer attributes `Contam > 0` from one sibling to
the other (covering both directions, the sentinel short-circuit, and the donor's
`dr_type` contamination strength).

Run: `cd prolog && swipl -g "[stack],[tests/test_coexists_fpn_canary], run_tests, halt" -t "halt(1)"`
Per-leg census via `corpus_path` overlay (see `census_*.log` in this dir).

### Controls (all green — the probe can SEE a leak; build_discipline Pattern 5 discharged)
- **positive (fact-level)** — inject a coexists sibling pair as the recon-identified
  construction produces it: two co-present readings, differing non-sentinel purity,
  low member a contaminating snare, joined by an `affects_constraint` side-channel
  edge. Canary flags it (`Contam > 0`). *Not* a direct neighbor/edge assertion (that
  proves only arithmetic) — the engine builds the edge from the facts.
- **negative (equal purity)** — same metrics → Δ=0 → `Contam=0`, not collected.
- **negative (sentinel donor)** — donor with no classifications → `purity_score = -1.0`
  → short-circuit → `Contam = 0`. Directly witnesses the funnel-guard #5 assumption
  (a coupled-but-sentinel-ineligible pair computes 0, never silently leaks).
- **tripwire** — a `cs_reading_relation` typed edge *alone* (no `affects_constraint`)
  produces no contamination neighbor: `constraint_neighbors_existing/2` reads no
  typed label. Confirms the DIRECT typed-edge channel is unwired (data-independent).
  The live risk is the **side channel**, which the census below measures.

## Census (the measurement — distinguishes the three greens / shows the red)

Schema pinned pre-run: denominator (co-present pairs) / eligible (purity≥0 both,
Δ>0) / coupled (affects_constraint side channel present) / leaked (effective_purity
Contam>0) / coupled-but-ineligible (funnel guard #5).

| Leg | denom | eligible | coupled | **leaked** | coupled-ineligible |
|---|---|---|---|---|---|
| `testsets/` (live, 109) | 3 | 2 | 3 | **2** | 1 |
| `testsets_haiku/` (twin, 109/770pairs) | 770 | 181 | 762 | **178** | 584 |
| `testsets_flash/` (twin) | _see census_flash.log_ | | | | |
| `archives/datasets/kernel_v1` (1,106) | 695 | 676 | 680 | **662** | 18 |

**kernel_v1 load control (#6):** denom=695 > 0 on the most-sibling-rich leg — no
load/schema-drift fault.

**Count is cache-order-soft (see HOLD_FINDINGS.md D2):** the *exact* leaked count
drifts across runs (flash 361 vs 310; kernel_v1 662 vs 645) because
`purity_score`/`dr_type` read the Boltzmann memo in traversal-dependent order
(OQ-112 class). Within any single run it is self-consistent (`leak ⟹ eligible`, 0
anomalies). The RED verdict is robust to the drift; treat the numerator as ±~15%.

**Every leg with co-present coexists siblings leaks**, and `leaked ≈ eligible`
(testsets 2/2, haiku 178/181, kernel_v1 662/676). The `testsets/` exclusion is NOT
sparse-and-clean as the OQ-23 entry's recon claimed — the entry sampled one kernel
(`basic_law…parliamentary_sovereignty`, sibling ungenerated). Other live kernels
(`press_reformation_causation`, `jewish_sovereignty_palestine`,
`zero_mathematical_status`) DO have co-present siblings, and 2 of them leak.

### Live testsets/ leaking pairs (pasted witness)
```
press_reformation_causation__mutual_shaping  <-> press_reformation_causation__strategic_deployment  Contam=0.256167
jewish_sovereignty_palestine__cultural_zionist_reading <-> jewish_sovereignty_palestine__settler_colonial_reading  Contam=0.198500
```

## Mechanism (why it leaks)

The generation template authors an `affects_constraint` edge between sibling
readings ("to enable contamination propagation across readings"), and
`constraint_neighbors_existing/2` admits those as `explicit` edges. The intra-kernel
filter at `drl_purity_network.pl:105` applies **only to shared-agent edges, not to
explicit `affects_constraint`**. So a coexists sibling pair contaminates via its
parallel `affects_constraint` edge whenever both siblings are co-present and the
higher-purity member's intrinsic purity exceeds the lower's (Δ>0), with the donor a
contaminating type. The "zero by definition" claim never bound: nothing zeroes the
side channel.

The exclusion held *on the sampled kernel* only by two Pattern-5 absences (singleton
sparsity → phantom-dropped siblings; `-1.0` purity sentinel) — **neither is a
`coexists_with` filter.** Where neither absence obtains, it leaks.

## OQ-24 cross-check (forecloses) — flag, do NOT fix here

The same side channel is label-blind to `forecloses` too. On `testsets/` the
forecloses census is denom=1 / eligible=1 / coupled=1 / **leaked=1**. So OQ-24's
doc-only close ("forecloses excluded by gradient-orthogonality") is **also living on
the sentinel**: gradient-orthogonality protects only the *typed* channel; the
authored `affects_constraint` side door carries contamination in the
causation-inverted direction the OQ-24 argument relied on being inert. **OQ-24
reopening candidate** — logged, not folded into OQ-23.

## Verdict & routing (Step 3)

**RED** — eligible pairs leak on every populated leg. The exclusion is violated, not
latent. Per the pre-registered RED branch: **STOP and re-escalate to the operator.**
Do not self-resolve. OQ-23 stays **open**; the canary + census are recorded as its
evidence. The operator's ruling is between:

1. **Filter intra-kernel typed-relation siblings out of the explicit edge set** in
   `constraint_neighbors_existing/2` (extend the line-105 intra-kernel filter to
   `affects_constraint` edges between same-kernel readings, or specifically between
   `cs_reading_relation`-typed siblings) — a real engine change with its own
   old-vs-new pipeline diff. Closes the leak; makes "zero by definition" true.
2. **Accept dual-channel authoring and rescope the claim** — the architecture
   acknowledges siblings DO contaminate each other via authored `affects_constraint`,
   and the "zero contamination weight by definition" language is retired as wrong.

Option 1 also bears on OQ-24 (same filter would close the forecloses side-channel
leak); Option 2 would require rescoping both claims.
