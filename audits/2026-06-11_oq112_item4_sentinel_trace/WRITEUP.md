# OQ-112 item 4 — `unknown`-sentinel trace: reachability, sink, and absorption boundaries

**Date:** 2026-06-11. **Substrate:** worktree `oq112-item4-trace` from `009c793a`, live corpus
62 testsets. **Probes:** `probe_sentinel_trace.pl` (v1), `_v2.pl`, `_v3_indexed.pl`, `_v3b.pl`;
raw outputs alongside. Read-only on the engine.

**Verdict (OQ-112 vocabulary): SILENT** — three distinct mechanisms, none requiring the other.
Re-rank applied to the OQ-112 entry. The loud throw exists but no production path lets it reach
the operator.

## Witness chain

**Reachability (point 1).**

- W1: exactly 2 absent-suppression constraints on the live corpus
  (`employment_boundary_contradictions`, `human_dignity_ai_governance_contradictions` — the
  OQ-44 non-story `cs_axiom_contradiction` pair).
- W2/W3: sentinel path taken in-process (`get_raw_suppression → unknown`; per-process positive
  control returns 0.72 on a present-suppression constraint).
  `get_constraint_metrics` returns `supp=unknown` → **the `; Supp = 0.0` branches
  (`maxent_classifier.pl:255/:761`) are DEAD, confirmed**.
- W8: **both absent-suppression constraints lack `constraint_claim`** → excluded from
  `maxent_run`/`maxent_indexed_run`'s findall (60 of 62 enter). On the live corpus the atom
  reaches the Gaussian sink by direct call only — the standard drivers never route it there.

**The sink (point 2) — driven, all three outcomes observed.**

- W10 (standard path, profiles present after `maxent_run`):
  `maxent_type_log_likelihood(employment_boundary_contradictions, snare, DefaultCtx, …)` →
  **`error(type_error(evaluable, unknown/0), context(system:(is)/2, _))`** — the `is/2` sink at
  `gaussian_log_likelihood/4` (`maxent_classifier.pl:123`, `Diff is X - Mu`). LOUD in isolation.
- W15 (indexed path, profiles present after `maxent_run` + `maxent_indexed_run`): **same
  type_error** — `metric_value_indexed(:771)` passes the atom with no default branch at all.
- W12a (threshold proximity, **uncaught**): `maxent_threshold_proximity(A, …)` →
  **quiet_failure, no exception** — an earlier subgoal fails for the claim-less constraint
  before the suppression arithmetic is reached. **The third sink: clause-failure absorption
  with no catch anywhere** — invisible to a catch-grep, witnessed only by driving the goal.
- W13 (positive control): present-suppression constraint → numeric LL (7.31), no error.

**False-clean traps the probe itself hit (kept as method witnesses):** v1's W4 ran before
`maxent_run`, so the dynamic `maxent_profile/4` table was empty and the sink returned a
plausible LL=-10.0 (prior+bool) without ever touching the atom — a success-shaped non-witness.
v3's W15 repeated the shape on the indexed table. Both were caught by checking
profile-present before trusting the sink result (v2 W10 / v3b W14b).

**Loud or silent (point 2) — every production boundary absorbs.**

catch-grep over the call-path files, pasted:

```
maxent_classifier.pl:317   catch(signature_detection:constraint_signature(C, Sig), _, fail)   [unrelated]
json_report.pl:72          catch(maxent_classifier:maxent_multi_run(WCtxs, _), _, true)
json_report.pl:76          catch(maxent_classifier:maxent_indexed_run(MaxEntCtx, _), _, true)
maxent_report.pl:211       catch(maxent_classifier:maxent_threshold_proximity(...), _, fail)
maxent_diagnostic.pl:395   catch(maxent_classifier:maxent_threshold_proximity(...), _, fail)
trajectory_mining.pl:912   catch(maxent_classifier:maxent_run(Context, _), _, true)
```

- W16: the json_report-style `catch(_, true)` wrapper **succeeds vacuously** over the W10 throw
  — a mid-corpus throw inside `maxent_multi_run` would silently void the maxent stage for the
  whole run (dist facts missing for every constraint after the throw; JSON fields go
  null/absent).
- W12b: the `catch(_, fail)` wrapper converts the proximity row to a silent drop.
- Plus the no-catch path (W12a) and `maxent_classify_one`'s `; true` on empty pairs
  (`maxent_classifier.pl:624`).

**Bonus defect (census-external, witnessed v3 vs v3b):** `maxent_indexed_run` **quiet-fails
when run standalone** (W14 v3: `quiet_failure`, indexed profiles never asserted) and succeeds
only after `maxent_run` in the same process (W14 v3b: success, 60 constraints) — a hidden order
dependency. In json_report the standalone-failure case would be absorbed by the same
`catch(_, true)` at `:76`. Filed on OQ-112 item 2 (absorber boundaries), since the boundary is
what makes it invisible.

## Verdict detail and re-rank

SILENT, by three mechanisms in priority order of breadth:

1. **Stage-level `catch(_, true)`** (`json_report.pl:72/:76`, `trajectory_mining.pl:912`) —
   absorbs *every* maxent failure mode, not just this atom (witnessed: W16; also the W14 order
   dependency). This is the composition boundary defect — Pattern-6 at the channel level.
2. **`catch(_, fail)` row drops** (`maxent_report.pl:211`, `maxent_diagnostic.pl:395`) — W12b.
3. **Clause-failure-into-quiet-omission with no catch** (W12a) — joins the absorbed-error class
   as the operator predicted: absorption without exception machinery.

Per the operator's conditional: SILENT → re-rank. Applied to ISSUES.md OQ-112: the widened
absorber-boundary class (old item 7, A10) is elevated to **item 2** with the new members; item
4 (A3) keeps its idiom-cleanup scope but its interaction sub-item is now TRACED — dead branches
confirmed, sink confirmed loud-in-isolation, live firing set EMPTY (claim-less constraints
gated out upstream), hazard latent: the first claim-bearing story missing
`suppression_requirement` voids the maxent stage silently through boundary 1.
