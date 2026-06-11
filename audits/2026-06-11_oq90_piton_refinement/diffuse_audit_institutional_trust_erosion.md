# Diffuse hand-audit — `institutional_trust_erosion` (OQ-90 Phase 0 gate extension, 2026-06-11)

## Why this audit exists

The OQ-92 Stage-D ruling (`audits/2026-06-10_oq92_step3_preregistration/`) gates gain_flow
driving classification behind a K=0 hand-audit of every `diffuse` claim. That audit
(`diffuse_audit_batch1.md`) ran on the corpus as it stood, which carried **3** corpus diffuse
claims. The live pipeline corpus now carries **4** — `institutional_trust_erosion` is a diffuse
claim that is now driving classification (it computes `false_ci_rope` and the OQ-90 refinement
will relabel it `piton`) and **has never passed the gate**. Operator ruling (2026-06-11): run the
same pre-existing criterion on it before re-registering the OQ-90 expected delta; identical engine
logic is exactly what the hand-audit exists to *not* trust, because a dishonestly-authored diffuse
claim would flip by identical logic too.

## Criterion (pre-existing — quoted, NOT rewritten)

From `audits/2026-06-10_oq92_step3_preregistration/diffuse_audit_batch1.md` (written before any
batch was read) and `PREREGISTRATION.md` Q1:

> a `diffuse` claim fails if the story artifact itself identifies a capturing seat;
> artifact-decidable only; outside-information cases pass (the priced detectability limit).

Findings reported as "0/N observed", N stated, never "clean" (Pattern-5/6 discipline).

## Subject

`prolog/testsets/institutional_trust_erosion.pl` (untracked working-tree file; see provenance
note). Authored facts:
- `narrative_ontology:stakeholder_gain_flow(institutional_trust_erosion, diffuse).` (:196)
- `narrative_ontology:fixing_cost_class(institutional_trust_erosion, prohibitive).` (:197)
- `domain_priors:base_extractiveness(institutional_trust_erosion, 0.15).` (:111)

## Artifact-decidable read

The story names beneficiary seats — `populist_entrepreneurs`, `alternative_media_platforms`,
`anti_establishment_movements` (`constraint_beneficiary/2` :132–134, :140) — and its
TRANSFER_FUNCTION (:203) reads:

> "Political opportunity and audience attention flow from mainstream institutions to populist
> entrepreneurs and alternative media platforms ... **The transfer is diffuse rather than
> concentrated — no single actor captures all the gains, but the pattern systematically
> advantages anti-establishment actors.**"

The question the criterion poses is narrow: does the artifact identify a **single capturing
seat**? It does not. The gains are authored as spreading across a *class* of anti-establishment
actors, and the artifact's own text affirmatively argues against single-seat capture. This is the
same structural shape as the three batch-1-adjacent corpus diffuse claims:
- `organization_floor`: gains to "organized blocs" generally (positional, not monetary).
- `reprogramming_safety_toxicity`: funding/patents to "pharma + academia + regulators",
  explicitly "not direct extraction".
- `regulatory_measurement_gap`: cost = "genuine scientific uncertainty, not rent-seeking".

In each, multiple beneficiary *classes* are named and the author argues no single concentrator.
`institutional_trust_erosion` is the **closest to the line** of the four (it names systematically-
advantaged actor classes most explicitly), but no single capturing seat is identified, so it does
not fail the criterion.

## Verdict

**0/1 observed.** One diffuse claim audited (`institutional_trust_erosion`); zero identified a
single capturing seat. Stated as observed, not "clean": the audit checked exactly this one claim
against the artifact-decidable criterion and found no obvious capturing seat. It is flagged as the
nearest-to-line case in the corpus, so if the obviousness threshold is ever tightened it is the
first to re-examine.

**Gate verdict: no Stage-D halt.** The OQ-90 expected delta may be re-registered to **2 rows**
(`regulatory_measurement_gap` + `institutional_trust_erosion`) with this audit as the attached
witness.

## Full-corpus diffuse census (the non-vacuous facts, 2026-06-11)

All four live-corpus diffuse claims, with the computed signature read from
`outputs/pipeline_output.json` (manifest `pipeline_run_at` 2026-06-11T16:44:05Z, n_constraints=52,
code_commit 411db0e7, code_dirty=true):

| constraint | fixing_cost | signature | OQ-90 effect |
|---|---|---|---|
| `regulatory_measurement_gap` | prohibitive | `false_ci_rope` | **flips → piton** |
| `institutional_trust_erosion` | prohibitive | `false_ci_rope` | **flips → piton** |
| `organization_floor` | prohibitive | `coupling_invariant_rope` | stays `rope` (leak control) |
| `reprogramming_safety_toxicity` | prohibitive | `coupling_invariant_rope` | stays `rope` (leak control) |

- **Diffuse prevalence moved 3 → 4** (feeds OQ-90 ruling-4 reporting).
- **`transient_neglect` cell remains corpus-EMPTY**: all four diffuse claims are `prohibitive`;
  no `cheap` diffuse story exists in the live corpus (verified by direct grep over
  `testsets/*.pl`, 2026-06-11). Its only witness stays prototype control 5
  (`audits/2026-06-10_gain_flow_prototype/`).
