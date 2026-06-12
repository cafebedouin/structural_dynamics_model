# Unanimity-guard adjudication under the pinned criterion (operator, 2026-06-12)

**Site:** `signature_detection:only_mountain_classifications/1` — FCR's mountain-protection
guard (`appears_as_rope(low_extraction_profile)` exclusion). Census class A1.
**Substrate:** live corpus n=62; raw runs in `unanimity_adjudication_probe.{pl,out,stderr}`.

## Pinned criterion and outcomes

| test | Candidate A (pre-signature computed unanimity, `metric_based_type_indexed/3` over canonical seats) | Candidate B (`natural_law_signature(get_constraint_profile)`) |
|---|---|---|
| (1) extension preservation (62) | **FAIL** — true-set 0 vs old 6 | **FAIL** — true-set 0 vs old 6 |
| (2) seam positive control | **FAIL** — proper NL synthetic computes mountain at only 2/4 canonical seats (mid-power seats compute ROPE on ultra-low ε) | **FAIL** — and not contingently: see below |
| (3) reentrancy | safe by layering (no signature calls in classify_from_metrics subtree) — moot | safe (metric reads) — moot |

**Candidate B is dead by construction:** `has_viable_alternatives/2` returns only
`true`/`unknown`, never `false`, so `natural_law_signature`'s `HasAlternatives == false`
conjunct is unsatisfiable on any corpus that authors no `intent_viable_alternative` facts
(live corpus: zero). Second absence-gated conjunct stacks on OQ-43's
(`BeneficiaryCount == 0` vacuous). Corollary: `determine_pure_subtype`'s `pure_natural_law`
branch is UNREACHABLE on the live corpus. Filed as OQ-113.

**Why A fails is the load-bearing insight:** "all seats agree mountain" does not survive
translation from authored perception to computed metric types — the metric path legitimately
computes rope at mid-power seats for any ultra-low-ε constraint (seat detail in probe out).
Computed unanimity over canonical seats is the WRONG TEST for natural law; no tuning fixes it.

## Old guard's bite (corrected via `domain_priors:base_extractiveness/2`)

All 6 old-true stories are **load-bearing**: ε ∈ {0.02, 0.02, 0.08, 0.08, 0.12, 0.15}, all
far under `rope_epsilon_ceiling` 0.45 — without protection each enters FCR's rope-appearance
pool. (First bite-check pass queried `constraint_metric(C, base_extractiveness, _)` — wrong
table, all `none`; corrected same session. The wrong-table read is itself a reminder: ε lives
in `domain_priors:base_extractiveness/2`; `constraint_metric` uses key `extractiveness`.)

## Candidate C (evidence-derived, ran through the same gauntlet)

`constraint_claim(C, mountain) ∧ emerges_naturally(C) ∧ accessibility_collapse ≥
natural_law_collapse_min ∧ resistance ≤ natural_law_resistance_max` — the authored NL
certification chain. All inputs story-level, Phase-C-surviving, signature-layer-safe,
fail-closed on absence (unauthored metric ⇒ no number ⇒ guard false).

- Seam control: **PASS** (perspectives-free NL synthetic → true; old → false).
- Extension: covers all 6 old-true, **plus 3** — `demographic_skill_mismatch`,
  `institutional_trust_erosion`, `organization_floor` (mountain-claimed, NL-certified,
  authored cells NOT unanimous). Strictly criterion (1) fails by a 3-story superset, in the
  protective direction.
- Output impact if installed: `institutional_trust_erosion` currently fires **FCR** — would
  un-fire; the other 2 fire coupling_invariant_rope and would leave the rope-appearance pool.
  All 3 declare beneficiaries ⇒ they remain **FSM-examinable** (the mountain-scrutiny path is
  untouched; so does old-protected `scale_ceiling`, which already coexists with FSM).

## Disposition

Both NAMED candidates fail the gauntlet — the delegation's settled/prefer clauses don't
reach; the 3-story disagreement set is a genuine semantics question (should FCR examine
mountain-claimed NL-certified stories whose authored seats disagreed?) ⇒ **escalated to the
operator** with this package. Ruling recorded below.

**Operator ruling (2026-06-12):** _recorded in ISSUES.md OQ-109 after escalation._
