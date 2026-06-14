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

## Cell sweep (2026-06-13) — the falsifier that converts "false positive found" into a ruling
`cell_sweep.pl` / `cell_sweep_output.txt`. Enumerated the cell `{claim=mountain, base_extractiveness ≤ 0.25, agent_beneficiary ≠ ∅}` over the 57-corpus and hand-labelled each. Positive control: 5 mountain-claimers fall OUTSIDE the cell (all ε > 0.25) — membership is non-vacuous.

| constraint | ε | supp | thtr | FSM fires? | payer/victim? | label |
|---|---|---|---|---|---|---|
| actinide_replenishment_mechanism_flat_control | 0.03 | 0.02 | 0.08 | **YES** | none | **false positive** (physics, external benefit) |
| radiative_levitation_stratification | 0.03 | 0.02 | 0.05 | **YES** | none | **false positive** (physics, external benefit) |
| neutron_star_bombardment_reading | 0.12 | 0.08 | 0.15 | no (supp>0.05) | none | physics, external benefit (not flagged) |
| demographic_resource_allocation | 0.18 | 0.12 | 0.08 | no (supp>0.05) | payers + excluded | concealment candidate (NOT flagged) |
| demographic_skill_mismatch_c0 | 0.18 | 0.22 | 0.12 | no (supp>0.05) | payers + excluded | concealment candidate (NOT flagged) |
| scale_ceiling_c0 | 0.18 | 0.12 | 0.08 | no (supp>0.05) | payers (workers) + benef capital_holders/arbitrage | concealment candidate (NOT flagged) |

**Falsifier result:** the FSM RED-capping footprint over the whole corpus is exactly 2 constraints (type_1 needs claim=mountain; FSM needs ε≤0.25 ∧ supp≤0.05 ∧ emerges_naturally ∧ benef≠∅), and **both are false positives.** No true positive in the firing set ⇒ the gate has no discriminating power in the cell it actually fires on ⇒ author-tax confirmed.

**[SUPERSEDED 2026-06-13 by fix-witness run — see REVISION below] ~~Stronger — discriminating power is INVERTED.~~** ~~The `suppression ≤ 0.05` gate makes FSM fire on the 2 most-pristine cases and abstain on the 3 social cases…the detector anti-correlates with concealment.~~ **This claim was WRONG:** the social cases are already RED-capped by `coupling_invariant_rope` + base `type_1`, not lost. FSM's abstention is correct scoping. Retained for provenance; do not cite.

## RULING (2026-06-13) — core diagnosis (stands)
For `radiative_levitation` / `actinide_flat_control`, the FSM RED does **NOT** accurately report "shaped by coordination, not physics." It fires on benefit-*presence* where no agent victim exists, so by definition there is no extraction to conceal. The separating discriminator is **already authored**: agent-victim presence (`constraint_victim`/`payer`). The two FSM cases have `vic=0`; the star has no victim-set. ⇒ false positive.

## REVISION (2026-06-13) — fix-witness run (`fix_witness.pl` / `fix_witness_output.txt`) corrects the fix claim and the inversion claim

The cell sweep witnessed the DIAGNOSIS but neither FIX. The fix-witness run enumerated the proposed gate's cell, the overdetermination residue, and whether anything already flags the social cases. Three corrections:

1. **"Refine beats remove" — UNSUPPORTED on this corpus.** Proposed gate cell `{mountain, ε≤0.25, supp≤0.05, victim≠∅}` = **EMPTY**. Victim-gated FSM fires on nothing here; the 2 current FSM cases are both `vic=0` ⇒ exempted ⇒ **swap ≡ remove, observationally**. The social candidates are excluded by `supp>0.05`, not by the beneficiary term, so swapping beneficiary→victim leaves them excluded either way. The distinguishing case (low-supp ∧ victim) does not exist in this corpus. Victim-gate is the *correctly-scoped* fix (no victim ⇒ no concealment possible) but is **indistinguishable from removal** on available evidence.

2. **"Inversion" RETRACTED.** Section (4): the 3 social cases are already `type1=4`, `dr_type=rope` at all seats, signature `coupling_invariant_rope`, `fsm='.'` — caught by CIR + base `type_1`, NOT FSM. FSM abstaining on them (supp>0.05) loses no signal. FSM is correctly scoped as a pristine-false-summit detector (the operator's reading (b), now witnessed). The suppression gate is doing legitimate "presents as undisturbed natural law" work.

3. **Overdetermination witnessed — and the clean-control problem is ≥3 cases, not 2.** With all beneficiaries retracted (FSM off): `radiative` and `actinide` drop `type1 4→2`, survivors at moderate+institutional (`rope`) = the OQ-50 power-scaling residue — so the 2 FSM cases ARE the 2 power-scaling survivors (doubly-hit; both fixes needed per-constraint). But `neutron_star_bombardment_reading` (physics, `vic=0`) stays `type1=4` `[TR,TR,TR,TR]` UNCHANGED by beneficiary removal, `fsm='.'` — a **third** physics false-positive via NEITHER FSM nor power-scaling (distinct mechanism, signature TBD). Turning the clean controls GREEN needs **≥3 dispositions**, not 2.

**Net (revised):** core diagnosis stands (FSM RED on no-victim physics = false positive). The victim-gate is the right-shaped fix but untestable-vs-removal here. The "GREEN needs both fixes" framing is incomplete — there are ≥3 independent RED mechanisms across the 3 physics false-positives (FSM, OQ-50 power-scaling, and neutron_star's unidentified path). The concealment-detection job the inversion claim worried about is in fact handled (CIR + base type_1).
