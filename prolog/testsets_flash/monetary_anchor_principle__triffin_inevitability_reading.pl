% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma Inevitability (Monetary Anchor Principle Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint represents the 'Triffin inevitability' reading of the
 *   monetary anchor principle, asserting that the collapse of the Bretton
 *   Woods system was a structural inevitability due to the inherent
 *   contradiction of a reserve currency issuer (US) under a gold standard. To
 *   provide sufficient global liquidity, the US had to run deficits, which
 *   eventually depleted its gold reserves, making the gold-dollar
 *   convertibility unsustainable. This reading frames the transition as a
 *   physical/logical impossibility, not a policy choice. The constraint is
 *   claimed as a Mountain because it describes an irreducible economic limit,
 *   not a human-made arrangement that could be sustained by enforcement. The
 *   Bretton Woods institutional framework is identified as a 'victim' because
 *   it was the system that ultimately failed due to this structural
 *   contradiction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.95).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Inevitability (Monetary Anchor Principle Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'c7e2a482-50c8-4eb6-bfaa-d1c1490adb50').
narrative_ontology:cs_kernel_codification('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', implicit).
narrative_ontology:cs_authority_grounding('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', foundational, reserve_currency_dilemma_is_structural_contradiction).
narrative_ontology:cs_axiom_status(reserve_currency_dilemma_is_structural_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', reserve_currency_dilemma_is_structural_contradiction, empirically_contingent).
narrative_ontology:cs_axiom('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', foundational, gold_standard_inherently_unstable_for_global_reserve).
narrative_ontology:cs_axiom_status(gold_standard_inherently_unstable_for_global_reserve, holdable).
narrative_ontology:cs_axiom_grounding('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', gold_standard_inherently_unstable_for_global_reserve, empirically_contingent).
narrative_ontology:cs_reference_frame('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', gold_standard_structural_contradiction).
narrative_ontology:cs_drift_state('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', contemporary_economic_theory, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c7e2a482-50c8-4eb6-bfaa-d1c1490adb50', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, global_financial_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The system itself, which was designed around the gold-dollar peg, was the ultimate 'victim' of the inherent contradiction. It could not escape its own design flaws.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% As the issuer of the reserve currency, the US Treasury was forced to manage the dilemma, running deficits to provide global liquidity while simultaneously seeing its gold reserves depleted. Its choices were limited by the structural contradiction.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_treasury, agenda_setter,
    institutional, biographical, constrained, global).

% The IMF observed and analyzed the growing instability of the Bretton Woods system, recognizing the Triffin dilemma as a fundamental structural flaw. It could propose solutions but could not alter the underlying economic laws.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Benefited from the liquidity provided by US deficits, but also contributed to the pressure on gold reserves by converting dollars to gold when confidence wavered. Their actions accelerated the dilemma's resolution.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_financial_markets, beneficiary,
    organized, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable international monetary system with fixed exchange rates and a reliable source of global liquidity, facilitating international trade and investment.
% TRANSFER_FUNCTION: The system implicitly transferred the cost of maintaining global liquidity to the reserve currency issuer (US), which bore the burden of managing the gold-dollar convertibility.
% ABSENT_VOICES: Developing nations, whose economic stability was often collateral damage in the larger international monetary system, had limited voice in the design or reform of the Bretton Woods system. They would have argued for a more equitable and less volatile system.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma (as a structural inevitability) had not existed, the Bretton Woods system might have persisted longer or evolved differently, fundamentally altering the trajectory of international finance and global economic power dynamics. The world would have rearranged around a different monetary anchor principle.
% FOUNDING_PROBLEM: The post-WWII need for a stable international monetary system to prevent competitive devaluations and facilitate reconstruction and trade.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate the founding problem. However, the Triffin dilemma itself demonstrated that the solution contained the seeds of its own demise, rendering the original problem 'dead' in its Bretton Woods form, even if the need for monetary stability remains 'live' in new forms. Academic consensus and historical records from outside the US Treasury or IMF support this.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the dilemma itself was a structural necessity, not a mechanism for rent extraction by any single party. Suppression is very high (0.95) because the underlying economic laws were unyielding; no amount of policy intervention could 'suppress' the dilemma's eventual resolution. Theater ratio is negligible (0.01) as there was no performative maintenance of a non-functional aspect; the system was genuinely functional until the structural limits were reached. Accessibility collapse is high (0.98) because there was no viable alternative within the gold-standard framework once the dilemma matured. Resistance is low (0.02) because the dilemma was a systemic problem, not one that could be 'resisted' by individual actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bretton Woods framework, the dilemma was an existential threat that forced a system-level collapse. From the perspective of the US Treasury, it was an intractable policy challenge. Both seats experienced the constraint as an unyielding force, but with different operational implications. The engine's computation of per-seat classification should reflect this shared experience of inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bretton Woods framework is the primary 'payer' as it bore the ultimate cost of the dilemma's resolution. The US Treasury, as the reserve currency issuer, was an 'agenda_setter' forced to manage the contradiction, but ultimately a 'payer' of the structural cost. Global financial markets were incidental 'beneficiaries' of the liquidity but also contributed to the pressure. The dilemma itself was a systemic force, not a mechanism for concentrated benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The Triffin dilemma was not a case of mandatrophy; its mandate (providing global liquidity under a gold standard) was structurally impossible to sustain. The constraint did not atrophy; it reached its logical conclusion. The classification as a Mountain prevents mislabeling this structural inevitability as a human-made constraint that simply outlived its usefulness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_natural_law_vs_policy_choice,
    'Was the Triffin dilemma a genuine natural law of international finance, or were there policy choices that could have averted or mitigated its ''inevitable'' outcome?',
    'Counterfactual historical analysis exploring alternative policy paths (e.g., earlier SDR adoption, different gold-dollar convertibility rules) and their projected outcomes, or comparative analysis with other reserve currency systems.',
    'If policy choices could have averted it, the constraint''s ''emerges_naturally'' claim would be weakened, potentially reclassifying it from a Mountain to a Snare (if the ''inevitability'' was used to justify extraction) or a Tangled Rope (if it was a poorly designed coordination mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_natural_law_vs_policy_choice, conceptual, 'Ambiguity between structural inevitability and policy-driven outcome.').

omega_variable(
    systemic_victim_vs_beneficiary_of_transition,
    'While the Bretton Woods framework was a ''victim'' of the dilemma, did specific actors or nations benefit from the transition to a floating exchange rate regime, and does this imply a hidden extractive function?',
    'Economic analysis of wealth transfers and power shifts post-1971, identifying specific actors who gained disproportionately from the new monetary order.',
    'If identifiable beneficiaries captured significant gains from the transition, the ''extractiveness'' of the underlying ''inevitability'' might be re-evaluated as a Snare, where the ''natural law'' served as cover for a power shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_victim_vs_beneficiary_of_transition, empirical, 'Whether the systemic ''victim'' masks specific beneficiaries of the system''s collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.01).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.01).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.01).
narrative_ontology:measurement(mone_tr_t1970, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.01).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.01).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.01).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.02).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.03).
narrative_ontology:measurement(mone_be_t1970, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1970, 0.04).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1944, 0.85).
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1950, 0.88).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1958, 0.91).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1965, 0.93).
narrative_ontology:measurement(mone_su_t1970, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1970, 0.94).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1971, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, focusing on the structural inevitability of the Triffin dilemma. It is linked to sibling readings that emphasize policy choice or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
