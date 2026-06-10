# OQ-95 fix: phantom network nodes — fail-closed neighbor filter + scoped giant_comp edges

**Date:** 2026-06-10. **Branch:** `worktree-oq95-phantom-nodes` (worktree off main @ `c7084464`).
**Resolves:** ISSUES.md OQ-95 (filed from the OQ-77 kill-condition audit,
`audits/2026-06-10_oq77_serial_kill_condition/`).

## 1. Defect (as witnessed in the filing audit)

Node enumeration in `giant_component_analysis.pl` is corpus-scoped
(`all_corpus_constraints/1`), but edge discovery
(`drl_purity_network:constraint_neighbors/3`) was unscoped: 26 authored
`affects_constraint/2` facts name 25 targets with **zero ontology presence** (LLM-authored
cross-references to testsets that don't exist post-reset). The component BFS counted the
dangling atoms as nodes: largest component 44/37 = **118.9%** of the network on the live
corpus (manifest 2026-06-09T23:19:26Z); **259.9%** on `archives/datasets/original_v6`.

## 2. Consumer census (the OQ's gating question)

Every consumer of `constraint_neighbors/3` inherited the phantom endpoints:

| Consumer | Phantom effect pre-fix |
|---|---|
| `giant_component_analysis.pl:108` | phantoms counted as BFS nodes (the witnessed >100%) |
| `drl_purity_network.pl` `bfs_path`/`contamination_path` (:366) | phantoms **traversable** — paths route through nonexistent nodes (a phantom acquires neighbors via the `ExplicitIn` reverse-edge clause; witnessed `phantom_..._neighbor_count=1`) |
| `drl_fpn.pl:118` | phantom entries cached in `fpn_neighbors_cache` (inert in math, dead weight) |
| `network_dynamics.pl:85,128,235` | phantom entries in contagion/velocity scans (inert in math) |
| `json_report.pl:1025` | phantom entries written into per-constraint neighbor output |

Contamination *values* were already phantom-inert everywhere: `purity_score/2` returns the
`-1.0` sentinel on missing data and every edge-contamination clause requires `>= 0.0`. The
distortion was purely **topological** (node counts, component membership, path existence,
neighbor lists). Census verdict: fix belongs at the shared source, `drl_purity_network.pl`.

## 3. Filter-predicate probe (`evidence/filter_probe.pl`, pre-fix run)

```
corpus=39 enumerated=37 aff_endpoints=43 with_claim=18 no_claim=25 not_in_enumerated=25
claim_test_matches_enumeration_gap=YES
enumerated_without_claim=[]              ← positive control: filter passes every real node
claim_fires_on_real_example=ai_governance_accountability
catholic_church_1200_has_claim=no
phantom_algorithmic_bias_in_targeting_neighbor_count=1   ← traversability witness
```

The 25 no-claim endpoints exactly equal the OQ's witnessed phantom list, and none carries
any `constraint_metric` (`phantoms=25 with_any_metric=[]`). Existence test chosen:
**claim OR any metric** — fail-closed on zero-fact atoms, robust to partially-authored
constraints, and it admits engine demos/probsets (which author metrics), so it is an
*existence* test, not a corpus-membership test. The two `*_contradictions.pl` sidecars
author only `cs_axiom_contradiction/2` — they can never be edge endpoints.

## 4. Fix (resolution option (a), two layers, + the scheme-level answer)

1. **`drl_purity_network.pl`** — `phantom_subject/1` (no claim AND no metric);
   `constraint_neighbors/3` is now symmetric fail-closed: phantom endpoints are excluded
   (`exclude(phantom_neighbor, ...)`) and a phantom *subject* gets `Neighbors = []`
   (pre-fix the reverse-edge clause manufactured node-ness in both directions).
   Fixes all five consumers at the source.
2. **`giant_component_analysis.pl`** — `precompute_edges_loop` now guards
   `assert_edge_canonical` with `ord_memberchk(Other, AllNodes)`: edges are scoped to the
   enumerated node set, so component size can never exceed node count **by construction**
   (defense in depth against claimed-but-unenumerated endpoints, e.g. engine demos).
3. **Scheme level:** this is the answer that outlives any one cleanup — every regenerated
   corpus mints new dangling refs (an already-measured property: `python/dangle_curve.py`,
   OQ-58 frontier model). Option (b) (fail-loud at generation) was **rejected**: dangling
   cross-references are expected during incremental generation, and
   `python/audits/reading_reference_linter.py` already censuses them separately,
   documenting why they are not a clean integrity signal. Option (c) (provenance bit) is
   unnecessary: after the filter there is no phantom read-site left, and value-level
   inertness was already guaranteed by the `-1.0` sentinel.

## 5. Witnesses (all in `evidence/`)

- **Live corpus before/after** (`giant_comp_BEFORE.md` / `giant_comp_AFTER.md`):
  `44 nodes (118.9%)` → `21 nodes (56.8%)`; total nodes 37 unchanged. The component
  shrank by more than the 25 phantoms because real nodes connected only *through*
  phantoms correctly fall out of the giant component.
- **original_v6 archive after** (`giant_comp_v6_AFTER.md`): pre-fix 8,785 nodes
  (259.9%); post-fix headline grepped in §6 below.
- **Endpoint probe after** (`endpoint_probe_AFTER.txt`):
  `enumerated=37 edges=49 endpoint_nodes=23 phantom=0` — the 26 dropped edge records
  (75→49) exactly equal the 26 dangling `affects_constraint/2` facts; probe still fires
  (positive control: 49 edges counted).
- **New regression suite** `prolog/tests/test_phantom_neighbor_filter.pl` (4/4 pass):
  positive control (synthetic edge to a real target IS returned), forward exclusion,
  reverse non-traversability, corpus-wide zero-phantom census.
- **Consumer regression** `test_forecloses_fpn_injection` 6/6 pass. The suite caught a
  real contract change first: its synthetic fixtures (`tw_*`) authored no
  claim/metric and became phantoms; fixtures now assert `constraint_claim/2` in
  setup (retracted in teardown). **Contract note for future synthetic constraints:**
  anything that should participate in the network must author a claim or a metric.
- **Embedded testset tests**: identical failure sets before vs after the fix
  (9 pre-existing story-authored threshold failures; 77→81 passes = the 4 new tests).
- **Validation suite**: `run_dynamic_suite` exit 0, 39/39 PASS (`test_runs.txt`).

## 6. original_v6 post-fix headline

From `evidence/giant_comp_v6_AFTER.md` (corpus_path overlay, 3380 testsets loaded,
25,757 non-inferred edge records):

```
| Total nodes (constraints) | 3380 |
**Largest component**: 3014 nodes (89.2% of network)
```

Pre-fix this corpus reported an 8,785-node largest component (259.9%). Post-fix the
fraction is ≤ 100% and the giant component is a meaningful corpus-connectivity statistic.

## 7. Side-finding → filed as OQ-96

`prolog/domain_registry.pl` is **gitignored** (`.gitignore:8`) yet load-bearing:
`domain_priors.pl:71` calls `domain_registry:domain_category/2`, and on a fresh
clone/worktree the module is absent, so the documented validation-suite command
(`[stack], [validation_suite], run_dynamic_suite`) aborts with an existence error on the
first testset. It is regenerated only by `python/run_pipeline.py:268`
(`domain_priors.generate_domain_registry`). Loud failure, but the canonical test command
is broken until the first pipeline run; `validation_suite.pl` (same auto-generated
category) IS committed — inconsistent treatment. Note: `python/domain_priors.py`'s CLI
`--output` default is an **absolute path into the main checkout** — a worktree user must
override it or silently write into the other tree.
