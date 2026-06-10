# OQ-77 kill-condition execution: giant_component_analysis SIGSEGV is not serially reproducible

**Date:** 2026-06-10. **Operator request:** review OQ-77 and resolve it.
**Verdict:** kill-condition arm 1 satisfied — **no serial recurrence** at the crash's exact
corpus size (n=39, 10/10 rc=0), under 12-way co-residency (12/12 rc=0), or at 28×/87× the
crash size on archived corpora (kernel_v1 n=1106; original_v6 n=3380 — see rc_table.md).
Per the pre-registered kill-condition (`ISSUES.md` OQ-77, filed commit `9676ed82`), this
closes the OQ as a concurrency artifact; resolution is the operational rule, not a Prolog fix.

## 1. What OQ-77 pinned

2026-06-06 slow-build session: three topic runs launched concurrently against shared
`prolog/testsets/` + `outputs/`. The giant_comp step (`run_pipeline.py:416`) segfaulted
(rc=-11) on exactly one of three — the maximally-overlapping run (leiden, n=39, manifest
00:10:49Z, between 00:08:36Z and 00:12:46Z) — and passed at n=43 and n=33. Pre-registered
kill-condition: serial non-recurrence at comparable size ⇒ concurrency artifact ⇒ operational
rule; serial recurrence ⇒ real defect in `giant_component_analysis.pl`.

## 2. Recon facts (code inspection, this session)

- `_prolog_giant_comp` runs `swipl -l stack.pl -l giant_component_analysis.pl -g
  "run_giant_component_analysis, halt."`; Prolog writes nothing to disk (no `open/tell` in
  the module) — stdout only; Python writes the `.md`. So giant_comp itself cannot be a file-race
  *writer*; any race is against other processes mutating what it *reads*.
- Even a single pipeline runs the 11 Prolog analyses in a ThreadPoolExecutor
  (`run_pipeline.py:471`) — giant_comp has always co-resided with ~10 sibling swipl processes.
  The suspect regime is therefore *pipeline-vs-pipeline*: another run's prep phase rewriting
  `domain_registry.pl`/`validation_suite.pl` and its generate step appending testsets while
  this run's swipl loads.
- The exact crashing corpus is NOT reconstructible: the leiden run loaded a transient
  mid-generation testsets state (three runs appending concurrently); the nearest corpus
  snapshot commit (`b3383dd6`) is n=91, hours later. The "same N topics" replay of the
  kill-condition text is therefore unavailable; "comparable corpus sizes" is the strongest
  runnable arm — and the live corpus happens to be exactly n=39 today (manifest
  2026-06-09T23:19:26Z).

## 3. Execution (all arms in evidence/rc_table.md; raw outputs alongside)

1. **Serial, live n=39 (exact crash size): 10/10 rc=0**, all outputs byte-identical
   (deterministic). Detection channel = process rc, the same channel that caught the
   original rc=-11; a recurrence is caught by construction.
2. **12 concurrent co-resident invocations, n=39: 12/12 rc=0**, outputs byte-identical to
   serial. Pure CPU/RAM co-residency (12-core, 7.7 GB WSL2 box) does not reproduce it.
3. **Serial, archived corpora** (topology/stack-depth stressor, per operator suggestion to
   use `prolog/archives/datasets/`): kernel_v1 n=1106 rc=0 (6.0s, 66 MB); original_v6
   n=3380 ×3 — runs 1–2 rc=0 (~6 min, ~198 MB each); run 3's rc was lost to a harness
   timeout on the wrapper shell, but its output is complete (end marker) and
   byte-identical to runs 1–2 (rc_table.md carries the full caveat). At n=3380 the
   component BFS traverses an 8,785-node component — a far deeper recursion than
   anything the n=39 crash corpus could present.

## 4. What remains unexplained (scoped honestly)

The *mechanism* inside the concurrent regime is unidentified. Ruled out: deterministic
topology/stack-depth defect at ≤3380 nodes (arm 3); pure co-residency at 12× (arm 2).
Not simulated: the mutating interleave (another pipeline's prep rewriting `.pl` files
mid-consult) — though buffered consult of a truncated file yields a syntax error (rc≠0,
loud), not plausibly SIGSEGV. The crash remains a one-observation event in a regime the
operational rule now forbids. If it ever recurs under serial operation, OQ-77's
"recurs serially" branch reopens with this audit as the baseline.

## 5. Side-finding → filed as OQ-94 (phantom network nodes)

Witnessed while validating the probe: the giant_comp report prints
`Total nodes (constraints) | 37` and `Largest component: 44 nodes (118.9% of network)` —
an internal inconsistency. Probe (`evidence/phantom_node_probe.txt`, edge count matches the
report's 75 = positive control): node enumeration is corpus-scoped
(`all_corpus_constraints/1`, 37 = 39 corpus files minus 2 `*_contradictions.pl` sidecars —
that exclusion is documented-intended), but edge discovery
(`drl_purity_network:constraint_neighbors/3`) is unscoped: **26 authored
`affects_constraint/2` facts point at 25 constraint IDs that do not exist in the live
corpus**, and the component BFS counts those dangling targets as nodes (49 endpoint nodes,
25 phantom). Build-discipline spine instance: a dangling reference presents as a network
node; >100% percentages print without error.

## 6. Disposition

- ISSUES.md OQ-77 → **resolved** (compressed per compress-on-close), operational rule recorded.
- ISSUES.md **OQ-94 filed** (phantom nodes / unscoped edge endpoints in giant_component_analysis).
- Operational rule promoted to CLAUDE.md "Running the System" (silent-denominator-corruption
  arm of the failure justifies promotion; the segfault arm alone is loud).
- KNOWN_STATE.md entry 2026-06-10.
