# CS-Layer Shakeout — The Record

**Status: settled. This document is the analysis output and does not change.**
It records the adjudication of the seven CS-annotated constraints. Proposed
framework changes motivated by this analysis live in a separate document
(`cs_shakeout_proposals_open.md`) and must not be merged back into this one —
the separation is deliberate (see note at end).

Date of analysis: 2026-05-21
Method: all seven constraint stories read in full (`.pl` source, not just engine
reports). Adjudication is conceptual — does the declared `(kernel, authority)`
pair correctly describe the constraint's commitment structure — not a metric
audit. Confidence is per-constraint and stated.

---

## Per-constraint adjudication

All seven were read from full source. "Correct/defensible" is high confidence
at the single-constraint level. Taxonomic *treatment* of two of them is open and
lives in the proposals document; the descriptive adjudications below are settled.

| Constraint | Declared pair | Engine result | Adjudication | Confidence |
|---|---|---|---|---|
| `privilege_waiver_threshold` | (formalized, lineage, interp) | dead-end → no_pattern_match | **Correct description.** Common-law privilege doctrine = interpretive accretion. Dead-ends only because the taxonomy's one interpretive_accretion clause requires `fixed_text`. | High |
| `drift_denial_authority_structure` | (distributed, extraction) | dead-end → no_pattern_match | **Correct description.** Once-coherent kernel (superhuman control) now operationally abandoned while distributed authority denies the drift and extracts from claimed continuity. The captured pole of the diffuse family. | High (as a single-constraint description) |
| `capability_velocity_mismatch` | (distributed, distributed) | diffuse_reconstruction (clean) | **The error.** CS fields asserted on a referent with no kernel — a rate differential between capability and regulatory cycle time has nothing to codify. See diagnosis below. | High |
| `distributed_extraction_stakes` | (distributed, distributed) | diffuse_reconstruction (clean) | **Correct.** AI-alignment-governance legitimacy umbrella as benign pre-paradigmatic coordination: under-specified kernel, distributed authority, good-faith reconstruction, no declared victims. | High |
| `human_governance_residual` | (formalized, extraction, interp) | anchored_fixity_with_accretion + `false_anchored_fixity_accretion` verdict | **Defensible; verdict fired correctly.** Separation-of-duties / admin bypass discretion. `coordination_type=enforcement_mechanism` correctly triggered the verdict that the interpretive buffer is not functioning. Minor wrinkle: the "anchored fixity" framing is approximate — extraction here is from irreducible residual discretion, not from preventing kernel revision. | High (defensible); wrinkle noted |
| `beautiful_reports_feedback_loop` | (formalized, extraction, no interp) | anchored_fixity_brittle (clean) | **Defensible.** Russian military upward-distortion reporting loop; theater 0.82; the unrevisable demand-for-good-news is a real anchored kernel with no internal buffer (the milblogger network is suppressed external opposition, not a buffer). Verdict correctly did NOT fire — there is no hidden accretion layer to flag. | High |
| `gendered_retention_asymmetry` | (implicit, practice, no interp) | implicit_practice + `false_implicit_practice` verdict | **Defensible; verdict fired correctly.** Physician career-length asymmetry via caregiving externalization. Uncodified and practice-maintained (correct CS description); verdict correctly flagged the "practice" as non-innocent extraction. Identity-lock is the binding mechanism (a directionality concern, orthogonal to the CS pair). | High |

---

## The tally

- **Visible failures:** 2 of 7 (the two dead-ends).
- **True misclassifications:** **1 of 7** (`capability_velocity_mismatch`).
- The two dead-ends are **correct descriptions the five-pattern taxonomy could
  not express**, not generation errors. They are the most valuable outputs of
  the run: each forced a real gap in an a-priori taxonomy to the surface at n=7.

The corrected reading is that the visible failure rate (2/7) overstates the
problem and the clean-classification rate (5/7) understates it — one of the five
"successes" is the actual error, and two of the "failures" are discoveries.

---

## Verdict-layer record — clean across the set

This is the run's strongest single result. The "honor the assertion, flag the
inconsistency" architecture behaved correctly on every constraint that reached
the verdict layer:

- **Fired when it should:** `human_governance_residual`
  (`false_anchored_fixity_accretion`, via `enforcement_mechanism`),
  `gendered_retention_asymmetry` (`false_implicit_practice`). Both are extractive
  patterns wearing benign-mechanism clothing; both got flagged.
- **Stayed silent when it should:** `distributed_extraction_stakes` (genuinely
  benign diffuse_reconstruction), `beautiful_reports_feedback_loop` (genuinely
  brittle — no hidden buffer, so nothing to flag).

**Scope boundary worth recording.** The verdict layer catches *assertion-vs-metric
inconsistency*. It cannot catch *referent-applicability* errors — i.e. CS fields
asserted on something that isn't a commitment system. That is exactly why
`capability_velocity_mismatch` classified clean: there is no metric inconsistency,
the pair is internally fine, the problem is that the pair should not exist. This
class of error belongs to an upstream generation guardrail, not the verdict layer.
Do not expect the verdict layer to be extended to cover it; that would conflate
two different checks.

---

## The real error: `capability_velocity_mismatch`

The constraint's referent is a *velocity differential* — AI capability doubling
time vs. regulatory amendment cycle time. A rate gap has no kernel: there is no
core commitment being codified and maintained by an authority structure. The
generator attached `(distributed, distributed)` anyway, almost certainly by
pattern-matching "AI governance + distributed authority" off its neighbor
`distributed_extraction_stakes` (which *is* a genuine commitment system).

The tell is internal to the constraint: its own `cs_framing_underdetermination`
omega asks whether the kernel is "the foundational alignment texts" or "the
broader governance discourse." That hedge is the generator noticing it cannot
locate a kernel — because there isn't one — and asserting a definite pair regardless.

**Correct disposition:** omit the `cs_structure` block for this constraint (no CS
fields), OR — if the intended referent is the governance regime rather than the
rate gap — recognize it is the same referent as `distributed_extraction_stakes`
and should not be a separate CS instance. Either way, the fix is upstream in
generation, not in the classifier. The proposed guardrail (block definite CS-pair
assertion when the CS-framing omega is unresolved) would have caught this; it is
the one upstream fix clearly warranted by this run.
