# OQ-122 — Re-type test is confirmatory, not discriminating; the cap is claim-driven (two mechanisms), not extraction-driven

**Date:** 2026-06-13. **Subject:** `radiative_levitation_stratification` (Przybylski's-star stress run, 57-constraint corpus). **Evidence:** `probe.pl`, `probe_output.txt` (in-session probe, snapshot/restore via `probe_harness:with_retracted`/`with_overlay`, caches cleared each phase; post-restore sanity confirms baseline returns → no cache leak).

## Question
Web C proposed the re-type test (re-author `claim=mountain` → `rope`/`tangled_rope`, hold metrics fixed, re-run) as "the sharpest single next step … tells you in one run whether the verdict layer is measuring concealment (defensible) or just taxing every hypothesis that has authors (artifact)." Does it?

## Answer: No — it is confirmatory/tautological, not discriminating.
`type_1_false_summit` is gated by construction on the claim (`drl_core.pl:614`: `constraint_claim(C, mountain)` is the rule's first goal). Removing the mountain claim removes the rule's precondition, so "drops off RED" is guaranteed *a priori* and is equally consistent with BOTH readings (concealment SHOULD be claim-gated; the artifact IS claim-gated). No discriminating power.

**Witnessed (Intervention B, re-type mountain→tangled_rope, beneficiaries kept):** `dr_type` UNCHANGED across all 4 seats (TR/scaffold/scaffold/TR), `constraint_signature` STILL `false_summit_mountain` — only `type_1` stopped firing because its claim precondition vanished. Nothing structural moved.

## The discriminating test (beneficiary toggle) and what it found
**Intervention A — hold `claim=mountain`, retract `constraint_beneficiary`:**
- Baseline: `base_extractiveness = 0.03`; 3 agent beneficiaries; signature `false_summit_mountain`; `dr_type` = TR/scaffold/scaffold/TR (departs mountain at all 4 seats); `type_1` fires ×4 → RED.
- No beneficiaries: signature → `ambiguous`; `dr_type` = **mountain**/rope/rope/**mountain**; `type_1` STILL fires ×2.

**Finding 1 — RED is beneficiary-driven, not extraction-driven (confirms the artifact reading at metric level).** The `false_summit_mountain` gate REQUIRES `ε ≤ mountain_extractiveness_max` (0.25); measured ε = 0.03. High extraction would FAIL the gate. So near-zero extraction is a *precondition* for the flag; named **agent** beneficiaries (institutions/programs — filtered from vindicated-proposition beneficiaries by the agency gate, `narrative_ontology:agent_beneficiary/2`) are what trip it. A metrically-pristine mountain claim is RED-capped for naming its institutional stakeholders.

**Finding 2 — the RED is OVERDETERMINED; removing beneficiaries does NOT clear it.** Even beneficiary-free, moderate & institutional seats classify `rope` → 2 `type_1` firings persist → still capped RED. This is the claim-independent power-scaling residue (χ = ε·f(d)·σ(S) shifts mid-power seats off the mountain band for ALL mountain-claimers — documented in the OQ-50 comment, `drl_core.pl:605–612`). For *genuine* mountains the signature layer restores mountain at those seats; here the beneficiary-free constraint went to `ambiguous` (it does not pass `natural_law_signature`), so the residue stands. **Only not-claiming-mountain clears the cap; neither the physics nor the beneficiaries alone does.**

## Settled vs. operator's ruling
- **Settled (witnessed):** the cap is claim-driven via two mechanisms — agent-beneficiary presence on a pristine claim, AND a claim-independent mid-power-seat power-scaling residue — neither of which is extraction magnitude.
- **Ω_C design ruling (operator, escalated):** engine doctrine is "genuine natural laws have zero agent beneficiaries" (`signature_detection.pl:1520`). Applied to real science (always funded), that flags all of it. Is RED-capping a pristine-metric mountain-claim-with-institutional-beneficiaries the intended concealment semantics, or an over-broad author-tax? Design judgment, not settleable by probe.
