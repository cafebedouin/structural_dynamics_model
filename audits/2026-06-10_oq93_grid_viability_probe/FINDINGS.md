# Findings — OQ-93 grid-viability probe (run 2026-06-10; pre-fix + post-fix)

**Verdict: Outcome 1 WITH A BUG RIDER.** The constructs compute their pre-registered values
exactly once a one-character bug — found BY the probe — is removed. The intent construct's top
verdict is range-dead as pre-registered. Raw runs: `runs/` (pre-fix, the bug witness),
`runs_postfix/` (the viability answer). Preregistration committed before the run: `e7e78a1b`.

## The bug the probe found (pre-fix run)

Pre-fix: **C4 κ exact on all five stories** (0.80/0.20/0.50/0.49/1.00 — machinery reads
authored data correctly) **while C1 G_sys = 0.0000 on all five**, including designed +0.588.
The C4/C1 split localized the break downstream of magnitude: `time_point_in_interval/2`
(`coercion_projection.pl:27`) ended in a `!` — comment "(Optimized)" — making it
FIRST-SOLUTION-ONLY (always T_start). Its sole caller, `coercion_gradient`'s future-points
`setof`, therefore never found a future point; every gradient failed; `system_gradient`'s
`[] → 0.0` fallback emitted a **success-shaped zero** for as long as the cut existed —
including the entire shim era. Two consequences:

1. **The stable-only basin was never data starvation alone.** Even the shim's injected
   t=[0,10] constant points could not have produced a nonzero gradient — but neither could
   AUTHORED data. The cut, not the diet, guaranteed `stable`.
2. **Build-discipline spine, twice over:** the 0.0 fallback is a fabricated default
   (Pattern 4) that made a failed computation indistinguishable from a flat result at every
   read site, and the cut itself is the bound-probe/clause-order family — an "optimization"
   that silently changed an enumerator into a semidet.

Fix: cut removed (`coercion_projection.pl`, comment with witness pointer at the site). Sole
caller verified by census (one call site). Post-fix corpus regression: full validation suite
GREEN (0 errors / 0 warnings), warning gate clean — corpus stories have no leveled grid, so
their gradients still fail → 0.0 → `stable`, unchanged behavior on the live corpus.

## Post-fix results vs preregistration (all five stories)

| story | G_sys expected | observed | pattern/intent expected | observed | κ expected | observed |
|---|---|---|---|---|---|---|
| rising | +0.588 | **+0.5880** | increasing_coercion | increasing_coercion | 0.80 | 0.80 |
| falling | −0.588 | **−0.5880** | decreasing_coercion | decreasing_coercion | 0.20 | 0.20 |
| flat_authored | 0.000 | **0.0000** | stable | stable | 0.50 | 0.50 |
| divergent | +0.156 | **+0.1560** | increasing_coercion | increasing_coercion | 0.49 | 0.49 |
| intent_max | +0.980 | **+0.9800** | increasing_coercion (top verdict UNREACHABLE) | increasing_coercion | 1.00 | 1.00 |

- **C1 PASS** (exact, ±0.0001 better than the pinned ±0.001). The authored-flat control's 0.0
  is now distinguishable from blindness: same run shows ±0.588 on the twins + `authored 32/32`
  provenance.
- **C2 PASS** — all three labels reached; the analytical finding stands as pre-registered: the
  construct is sign(G_sys)-thresholding, distinction capacity = 3 labels from one scalar.
- **C3 PASS + range-death WITNESSED** — basin exited on three stories (first non-`stable`
  verdicts in the construct's history); story 5 with FULL hand-authored Conditions-2–4 evidence
  and the maximal reachable gradient (0.98) still cannot produce `structural_coercive_intent`
  (threshold 1.00 strict > max reachable 0.98). The top verdict is dead on the metric domain
  by arithmetic, witnessed at the domain's edge.
- **C4 PASS** (exact). Calibration wart confirmed: per-level κ can exceed 1.0 (class weights
  sum 1.10; story 5 class κ(Tn)=1.10 feeds the 1.00 aggregate).
- **C5 PASS** — Confidence `high` on all five via real authored completeness; the corpus
  meanwhile reads honest `low` (0/8) with the shim off.

## What this licenses (under-claim per preregistration)

Success here is an existence proof: given best-case authored grids, the gradient/κ/pattern
pipeline computes EXACTLY what its design intends (post one-char fix), and intent's lower
verdicts work. It does NOT establish that LLM-generated stories would author useful grids, nor
that the outputs add analytical product beyond what `drift_trajectory`/temporal series already
yield (the duplicate test of *Unwired ≠ worthless* — not answerable by this probe), nor
anything about `stakes_inflation`'s semantic distinctness from existing scalars.

## Recommendation (recommends only — the Ω_C ruling is the operator's)

Per-construct, per the pre-registered mapping:
- **intent_engine's top verdict (`structural_coercive_intent`): retire or redesign regardless
  of the migrate decision.** Doubly dead, both witnessed: threshold unreachable on the metric
  domain (1.00 strict vs 0.98 max), and Conditions 2–4 read tables with no producer anywhere
  (this probe hand-authored their first-ever facts). The lower verdicts (increasing/decreasing/
  stable) ride the pattern construct.
- **gradient/κ/pattern: mechanically sound, trivially monotone.** The migrate-vs-retire
  question is now purely a VALUE question: (i) does sign(Δκ) + κ add product beyond the live
  drift/trajectory subsystems (duplicate check — a comparison read, cheap); (ii) is the
  producer-side cost (leveled-grid schema + prompt + `stakes_inflation` resurrection) worth it;
  (iii) the OQ-93 fork's producer-side caveat stands: authoring moves invention from category
  priors to LLM judgment rather than eliminating it.
- **Either ruling, the interim state is stable:** shim off, suite green, absence witnessed —
  nothing is blocked while the ruling waits.
