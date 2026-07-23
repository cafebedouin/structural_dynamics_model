# OQ-60 item-0: ordering-predicate audit (2026-07-23)

**Hazard:** Prolog standard order puts atoms BEFORE numbers, so an `unknown` purity value
reaching `msort`/`sort`/`max_member`/`min_member`/`predsort`/top-N machinery silently heads
the list — plausible output, no throw. Commit 0a guarded arithmetic only (loud throws);
this audit covers the silent ordering paths.

## Sweep method + positive control

Candidate files = every `.pl` (excl. testsets/archives/tests) reading
`purity_score|effective_purity|fpn_intrinsic|gc_node_purity` (20 files, enumerated in the
session paste). Sweep = grep for `msort|sort(|sort/4|max_member|min_member|keysort|predsort|
max_list|min_list|last(` over those files.

**Positive control:** the sweep must hit a KNOWN purity-ordering site — it does:
`giant_component_analysis.pl:299-303` (`distribution_stats/2`: `msort(Values)` + `nth1`/
`last` quartiles) is fed purity values at `:524/540/906/912` (`IPs`/`EPs` from
`gc_node_purity/3`). The method finds real purity-ordering sites; the corpus-wide result
below is therefore a measured-clean, not a didn't-look.

## Findings (full sweep output pasted in session; classification of every candidate)

| site | what is sorted | purity-ordering risk |
|---|---|---|
| giant_component `distribution_stats` (299-303) fed at 524/540/906/912 | intrinsic/effective purity values | **REAL SITE — protected at ingest**: gc precompute (`:353-370`, 0a.2) collapses `unknown`→`-1.0`; every feeder filters `IP >= 0.0`/`EP >= 0.0`, excluding the sentinel before the msort |
| giant_component spreaders (613-620, 943, 974) | pairs sorted by Potential (=Deg×CS, numeric); EP display-only | EP from `gc_node_purity` = numeric by ingest guard; `-1.0` can render in the table (OQ-62 territory, not unknown-ordering) |
| fpn_report movers (176-181) | `Shift is OH - FP` then msort | OH/FP from FPN state; fpn ingest (`drl_fpn.pl:105-116`, 0a.2) collapses `unknown`→`-1.0` and the iterator skips −1.0 nodes |
| drl_purity_network `deduplicate_neighbors` (172) | `neighbor(C, Strength, Src)` — edge strengths | not purity values |
| diagnostic_summary `max_badness` (726-727) | verdict badness ints | not purity |
| logical_fingerprint `metric_trend` (332-334), metric_drift_events (83-241), json_report 924 | authored `measurement/5` T-V pairs | not purity |
| maxent_report (122, 137, 215, 250-292), maxent_diagnostic (157-430, 398) | entropies / probabilities | not purity |
| context_profile_mining (573, 606, 690, 832) | fingerprint shifts / HAC distances | not purity |
| grothendieck (695) | H¹ values | not purity |
| json_report tallies (2009-2088) | zone/category ATOMS (tally-by-value) | `purity_zone(unknown)=unknown` becomes a countable band label — labeling handled by 0b (`n_scored`/`n_total`), not an ordering defect |
| all remaining `sort(` hits | constraint IDs / types / omegas / kernels | not purity |

## Conclusion

**No live path lets `unknown` reach an ordering predicate.** Purity values cross exactly two
boundaries into ordering-bearing machinery — the FPN precompute cache and the gc precompute
cache — and both (0a.2) collapse `unknown` to the `-1.0` sentinel that downstream `>= 0.0`
filters already exclude. All other ordering sites sort non-purity values.

## Policy (derived from R3, not per-site judgment)

- Descriptive top-N/band displays compute over scorable values and LABEL the exclusion
  (`n_scored/n_total` — encoded by Commit 0b).
- Any ordering/max/min feeding a dispositive verdict abstains at coverage < 1.0 (0b).
- New code must not let `unknown` cross into a sort: collapse to the sentinel at the cache
  boundary (the fpn/gc pattern) or guard with `number/1` and carry the exclusion count.

## Tests (injected unknowns — the corpus cannot produce the condition until C-FLOOR)

`prolog/tests/test_purity_absence.pl` tests 6-7:
- `fpn_ingest_collapses_unknown_to_sentinel`: injected `unknown` → `fpn_intrinsic = -1.0`.
- `gc_ingest_collapses_unknown_to_sentinel`: injected `unknown` → `gc_node_purity(-1.0, -1.0)`
  and the `>= 0.0` distribution filter excludes both.

7/7 green under the pipeline load chain + `giant_component_analysis.pl` (pasted in session).

## Session note (2026-07-23)

An operator `c-orchestrator.py` topic run was active during this audit (testsets grew
189→195 mid-session). Ordering audit is corpus-independent (code-path analysis + injected
tests). The testsets census will be re-run at C-FLOOR time against the then-current
snapshot; pipeline witness runs are serialized behind the orchestrator.
