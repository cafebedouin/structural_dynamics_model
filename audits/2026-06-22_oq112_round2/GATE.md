# OQ-112 Round 2 — the verdict_join fail-closed completion gate (item 2, the consumer side)

**Date:** 2026-06-23  **Corpus:** live `testsets`, LIVE=92 (pinned, `WRITEUP.md`).
**Scope:** the Pattern-6-containing fix — markers + `verdict_join` fail-closed wire-in +
the widened absorbers + the AGENTS.md invariant, landed together (operator ruling
2026-06-23: the invariant becomes true of the engine at the commit the join enforces it).
Lands on top of the three now-trusted completion asserts (`:555`/`:734`/indexed). Item 7
and the ISSUES.md sync are NOT in this commit.

## The hazard (confirmed by read)

`diagnostic_summary/2` runs at `default_context`; a voided maxent makes `probe_maxent`
return `inconclusive`/`unavailable`, which are **dropped** — so the void contributes to
GREEN. `ds_subsystem_available(maxent)` only checks "any `maxent_run_info` exists," and
nothing in the verdict path reads the indexed stage at all, so an indexed void is **fully
silent**. That is the green-over-absence this gate closes.

## The fix (3 engine pieces + invariant)

1. **Attempt markers** (`json_report.pl`): `retractall` stale markers at run start, then
   `assertz(diagnostic_summary:maxent_attempted(classical|indexed))` BEFORE each absorbing
   catch — so the gate tells "not in this pipeline" (no marker) from "attempted but voided".
2. **Per-attempted-stage fail-closed** (`diagnostic_summary.pl`): `maxent_void_alerts/1`
   checks **each stage's OWN distinct completion fact at the consumed (default) context**
   (`maxent_run_info` for classical, `maxent_indexed_run_info` for indexed). Marker present
   + own fact absent → `alert(maxent_voided(Stage), moderate, maxent_completion)` injected
   into `verdict_join`, flooring the headline to **yellow** (operator ruling) and entering
   the serialized `Alerts` so the reason is machine-legible. ABSENCE is the test (catches
   throw AND plain clause-failure; `catch/3` is blind to the latter).
3. **Widened absorbers** (`json_report.pl`): `catch(G,_,true)` → `( catch(G,_,fail) -> true
   ; true )` so a stage FAILURE continues the run instead of crashing it mid-pipeline —
   without this the plain-failure path is a loud crash, not a gated void (and the W12a arm
   below would be unwitnessable).
4. **AGENTS.md** "completion-witness-or-fail-closed" Architecture Invariant (same commit).

## Four-case matrix + cross-term + latency — `gate_matrix.txt`

Real diagnostic path (`diagnostic_summary` → `verdict_join`) over a green-Base target
(`nicene_creed_authority__liturgical_habituation_reading`) under controlled stage states:

```
TARGET = nicene_creed_authority__liturgical_habituation_reading
COMPLETE         J=green  B=green   void[classical=no  indexed=no ]   <- gate no-op
  (indexed_run_info after throw: absent)
THROW-indexed    J=yellow B=green   void[classical=no  indexed=yes]   <- green->yellow, indexed void
  (no-priors indexed_run -> failed_plain ; indexed_run_info: absent)
FAIL-indexed     J=yellow B=green   void[classical=no  indexed=yes]   <- CATCH-BLIND plain failure floors
  (classical run_info after throw: absent)
THROW-classical  J=yellow B=green   void[classical=yes indexed=no ]
N0-legal         J=green  B=green   void[classical=no  indexed=no ]   <- legal-empty passes (fact present, N=0)
LATENCY/92: constraints with a maxent_voided alert under COMPLETE state = 0  (expect 0)
```

- **Each verdict is checked for the attempted stage SPECIFICALLY** (the `void[classical=·
  indexed=·]` columns), not globally. **Cross-term** (THROW-indexed row): classical
  PRESENT, indexed VOID → `void[classical=no indexed=yes]`. The gate reads the indexed
  stage's own absent fact at DefaultCtx and never the classical present fact — the same
  discrimination the distinct-fact decision bought at the assert layer, now at the join.
- **FAIL-indexed is the load-bearing arm:** the `:871–874` no-priors guard makes
  `maxent_indexed_run` **`failed_plain`** (a plain clause failure, catch-blind), the
  completion fact is absent, and the join floors to yellow — the channel the whole
  inverted-default design exists for, witnessed against the new indexed assert.
- **N0-legal:** `maxent_*_run_info(Ctx, 0, _)` present → no void alert, Joined == Base.
  N=0-with-witness is legal-empty and passes.
- **LATENCY/92:** under normal completion, 0 of 92 constraints get a void alert — the gate
  is a **no-op on the live corpus** (it fires only on a void, which 92 never produces).

## Supporting witnesses

- **Absorber-widening micro-witness:** `catch(fail,_,true)` → **FAILS** (would crash
  `run_json_report`); `( catch(fail,_,fail) -> true ; true )` → **SUCCEEDS** (absorbs plain
  failure); same on `throw(x)` → SUCCEEDS. Both channels absorbed.
- **End-to-end** `run_json_report` (exercises the markers + widened absorbers in the real
  code, not the probe's manual setup): **RJR_OK**; both maxent stages "done"; raw output
  written with **0** `maxent_voided` (gate latent on 92). No silent regression.
- **Regression:** `diagnostic_selftest` → **SELFTEST_PASS**.

## [EDGE] Recorded falsifier for the yellow ruling (operator, 2026-06-23)

The void's escalation-worthiness lives in the legible `maxent_voided(Stage)` Alert, not the
headline color. Falsifier: *if any consumer is found to branch on headline-color-only and
would mishandle a void-as-yellow, the void needs its own surfaced flag, not a color
promotion.* Grep of `python/` consumers of `verdict_join`: candidates that reference
`verdict_join` but not `alerts` are `audit_citation_status.py`, `run_pipeline.py`,
`twin_comparison.py` — but each is passthrough / verdict-compare / citation-status and
takes **no maxent-specific action a void-as-yellow would invalidate**, so **none is
witnessed to mishandle it**. The falsifier stands, candidates named; revisit if a
color-only maxent-dependent branch appears.

## Carried forward (NOT in this commit)

- **Item 7** (wasserstein `catch→0.0`, `json_report.pl:428–431`) — different consumer
  surface, schema-level output change; lands separately.
- **ISSUES.md OQ-112 sync** — supersede Round-1's single-falsifier wording **in the diff**
  (old + new both visible; the W2 claim-less→claim-bearing transition invisible to W3 is
  the load-bearing why), record item 2+ resolved / item 4 → Round 3, run `omega index` +
  `issues_status --check`, `[GATE]` green before that commit.
- Optional diagnostic `maxent_stage_failed(Ctx, E)` recording in the absorber recovery
  (the gate must not depend on it — failure path emits no E); deferred.
