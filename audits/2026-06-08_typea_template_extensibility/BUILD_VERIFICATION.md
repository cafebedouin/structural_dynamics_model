# Type-A snapshot floor — build verification (2026-06-08)

Strict Tier-2, schema-deferred build of the observer-temporal floor + residual detector. All
probes read-only except the engine edits under test. Run from `prolog/` unless noted. Witnesses
below were observed this session; re-run before citing as a settled premise.

**Changed (engine/wiring):** `constraint_indexing.pl` (`derive_directionality_at/4`,
`effective_time/3`, dynamic `time_indexed_directionality_source/4`), `drl_composition.pl`
(`classify_at_time/5` surfacing `snap(D,Backed,Eps,Supp,Theater)`), `transition_paths.pl`
(`snapshot_type` `:130` → time-aware, sync only), `temporal_residual.pl` (NEW, category-B),
`json_report.pl` (emit + writers), `stack.pl` (load). **No schema/template change.**

| V | Claim | Verdict | Witness |
|---|---|---|---|
| V1 | No regression | PASS | pre/post `pipeline_output.json` **byte-equal** after stripping `temporal_residual` + `manifest` (run pipeline, `git stash` the 5 files, run again, deep-compare) |
| V2 | `derive_directionality_at` ≡ `derive_directionality` + deterministic | PASS | equal AND single-solution over 500 (C,T) pairs (100 constraints × t∈{0,3,6,9,99}) |
| V3 | Classifier sync | **PARTIAL — named test passes; full sync OPEN** | `run_migration_tests` both tests green. But full `classify_at_time` ≡ `snapshot_type` is FALSE and always was: **3 unique mismatch points** at default ctx (the "7" was metric-duplicated — `measurement(_,C,_,T,_)` backtracks per metric); **sync-neutral to this edit** (3=3 / 7=7 pre vs post, witnessed on stashed code). The "two classifiers in sync" must-hold is **not satisfied** — it is unchangedly violated. See contamination join below. |
| V3b | Contamination: do counted flips touch a mismatch? | **2 flagged** | join {3 mismatch (C,T)} ∩ {52 counted default-ctx flips} = 2: `clinical_deskilling_automation` 0→2 (mismatch T=0), `milblogger_legitimacy_erosion` 12→18 (mismatch T=18). These are counted (both endpoints backed) yet a second classifier disagrees at an endpoint → flagged classifier-sensitive for the offline join. Likely cause **[UNVERIFIED]**: `snapshot_type` omits the `nb_setval` temporal theater/eps threading that `classify_at_time` sets and `classify_from_metrics` reads. |
| V4 | Cheap-given-revival | PASS | `temporal_residual.pl` has 0 `derive_directionality*` calls; d read off `classify_at_time/5` |
| V5 | Revival positive control | PASS | real flip: `ai_governance_accountability` @ analytical/civilizational/analytical/global, t3→t6 scaffold→tangled_rope, Δε=0.05, backed_times=4=times_examined |
| V6 | Phantom-flip guard | PASS | `with_retracted` t6 base_extractiveness → the V5 flip moves `flips 1→0, fab_adjacent 0→1` (t6 becomes `backed=false`); restore verified `flips→1` |
| V7 | Empty-is-readable | PASS | every measurement-bearing constraint emits `times_examined>0` (e.g. a 0-flip ctx: `{times_examined:3, backed_times:3, flips:0, fabrication_adjacent_transitions:0}`); no stderr swallow |
| V8 | Hub separation (category-B) | PASS | `grep cs_ temporal_residual.pl` → none; imported only by `stack.pl` + `json_report.pl`, no DR/CS computation module |
| V9 | Freshness + defer | PASS | `git diff --stat` touches only the 5 engine files; **no `schemas/`**; residual block regenerated in-run by `json_report` (single-writer, manifest-stamped) |

## Finding (re-witness before citing)

**The residual is NOT empty on the current corpus: 56/100 constraints show ≥1 backed flip; 155
counted flips** across the canonical contexts. Because d is frozen on the current corpus (no
time-indexed source), **every backed flip is observer-metric-driven (ε/suppression/theater), not
d-driven** (V2). This contradicts the pre-build "expected empty" prior. D-fork bearing: substantial
ε-driven flips at fixed role/d mean the cheap path produces signal → role-time-indexing (OQ-83 branch
b) is NOT forced by emptiness.

**Bounds (so 155 is not banked unqualified):** |Δε| median 0.07, 120/155 > 0.05, only 1/155 in the
≤0.02 jitter band → real ε movement, not boundary jitter; 150/155 flip-intervals on a fully-backed
series. **2 of the 52 default-context flips are classifier-sensitive (V3b).** The number the offline
join inherits is *155 flips, 2 flagged, median |Δε| 0.07* — not an unqualified 56. The
genuine-Type-A-residual vs committer-shadow *subtraction* is the offline join, gated on the
committer-time enrichment (ISSUES OQ-83 follow-on #1).

## Reproduce V1 (the load-bearing no-regression)

```bash
python3 python/run_pipeline.py && cp outputs/pipeline_output.json /tmp/post.json
git stash push prolog/constraint_indexing.pl prolog/drl_composition.pl prolog/transition_paths.pl prolog/stack.pl prolog/json_report.pl
python3 python/run_pipeline.py && cp outputs/pipeline_output.json /tmp/pre.json
git stash pop
# strip temporal_residual + manifest from both, deep-compare -> equal
```
