% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity: Veto Trap Reading
 *   domain: institutional/political_economy
 *
 * SUMMARY:
 *   This constraint describes the EU Council's unanimity rule as a 'veto
 *   trap,' a structural vulnerability that enables minoritarian extraction.
 *   While ostensibly designed to protect national sovereignty, this reading
 *   argues that the rule is systematically exploited by individual member
 *   states to block collective action and extract concessions from the
 *   majority, leading to policy paralysis and diluted outcomes. The
 *   constraint is claimed as a Snare because its coordination function
 *   (ensuring full consent) is overshadowed by its extractive use.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.85).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.75).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity: Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'a9180c92-31e8-40b5-b1ba-b06bebc123b0').
narrative_ontology:cs_kernel_codification('a9180c92-31e8-40b5-b1ba-b06bebc123b0', formalized).
narrative_ontology:cs_authority_grounding('a9180c92-31e8-40b5-b1ba-b06bebc123b0', lineage).
narrative_ontology:cs_interpretation_layer_present('a9180c92-31e8-40b5-b1ba-b06bebc123b0').
narrative_ontology:cs_reading_relation('a9180c92-31e8-40b5-b1ba-b06bebc123b0', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9180c92-31e8-40b5-b1ba-b06bebc123b0', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('a9180c92-31e8-40b5-b1ba-b06bebc123b0', foundational, unanimity_as_extractive_leverage).
narrative_ontology:cs_axiom_status(unanimity_as_extractive_leverage, holdable).
narrative_ontology:cs_axiom_grounding('a9180c92-31e8-40b5-b1ba-b06bebc123b0', unanimity_as_extractive_leverage, empirically_contingent).
narrative_ontology:cs_axiom('a9180c92-31e8-40b5-b1ba-b06bebc123b0', secondary, minority_blocking_undermines_collective_good).
narrative_ontology:cs_axiom_status(minority_blocking_undermines_collective_good, holdable).
narrative_ontology:cs_axiom_grounding('a9180c92-31e8-40b5-b1ba-b06bebc123b0', minority_blocking_undermines_collective_good, instrumental).
narrative_ontology:cs_reference_frame('a9180c92-31e8-40b5-b1ba-b06bebc123b0', eu_integration_as_collective_action).
narrative_ontology:cs_drift_state('a9180c92-31e8-40b5-b1ba-b06bebc123b0', post_cold_war_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9180c92-31e8-40b5-b1ba-b06bebc123b0', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, proposing_coalition_majority).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_citizens_affected_by_delay).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Member states that leverage the unanimity rule to block proposals, extracting concessions, opt-outs, or side payments from the majority coalition. Their power is derived from the structural veto, not their size or economic weight.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_states, agenda_setter,
    powerful, immediate, arbitrage, national).

% The group of member states that support a policy proposal but are forced to make concessions to a blocking minority to achieve any progress. They bear the costs of delay and diluted policy outcomes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, proposing_coalition_majority, payer,
    institutional, biographical, constrained, continental).

% The executive body of the EU, responsible for proposing legislation. It experiences the unanimity rule as a constant constraint on its legislative agenda, often having to pre-emptively water down proposals to avoid vetoes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission, observer,
    institutional, generational, constrained, continental).

% Citizens across the EU who would benefit from proposed policies (e.g., environmental regulations, common defense initiatives) but whose interests are delayed or undermined by minoritarian blocking and the resulting policy paralysis or dilution.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens_affected_by_delay, payer,
    powerless, biographical, trapped, continental).

% The directly elected legislative body of the EU, which often supports proposals that are subsequently blocked or diluted in the Council due to unanimity requirements. Its democratic mandate is often frustrated by the veto trap.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_parliament, excluded,
    organized, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The unanimity rule is intended to ensure that all member states are genuinely committed to collective decisions, fostering deeper integration by requiring full consent.
% TRANSFER_FUNCTION: Transfers policy influence and material concessions from the majority coalition to individual blocking member states, in exchange for their consent to collective action.
% ABSENT_VOICES: The collective voice of the EU citizenry, particularly those who would benefit from blocked or diluted policies, is absent from the Council's decision-making process, which prioritizes national veto power over broader European interests.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, the EU Council would shift to qualified majority voting for all decisions, significantly accelerating policy adoption, reducing the leverage of individual states, and likely leading to a more integrated and effective EU, albeit with less national control.
% FOUNDING_PROBLEM: The founding problem was to ensure that no sovereign state could be coerced into collective action against its fundamental national interests, thereby protecting national sovereignty within a supranational framework.
% FOUNDING_PROBLEM_CORROBORATION: Member states consistently attest that protecting national sovereignty remains a live and critical problem, especially for smaller states. However, the EU Commission and many academics (outside the benefiting parties) argue that the current application of unanimity has moved beyond sovereignty protection to become an instrument of rent-seeking, suggesting the problem is 'live' but its solution has become extractive.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the veto power allows a single state to systematically transfer value (policy outcomes, concessions) from the majority to its own narrow interests. Suppression (0.75) is also high, as the rule actively suppresses the policy preferences of the majority coalition and prevents alternative decision-making paths. Theater ratio is low (0.20) because the blocking threats are real and effective, not merely performative; the 'sovereignty protection' narrative is a cover for genuine extraction. The increasing trend in extractiveness and suppression over time reflects the growing frequency and effectiveness of veto threats as a bargaining tool.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of blocking states, the unanimity rule is a legitimate defense of national interest (sovereignty_guarantor_reading). From the perspective of the majority coalition, it is an extractive mechanism (veto_trap_reading). The engine's classification will highlight this divergence, showing a Snare for the majority and a Rope/Mountain for the blocking states, based on their respective directionalities and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Blocking member states are clear beneficiaries (d=0.0-0.2) as they gain concessions and policy influence. The proposing coalition majority and EU citizens are victims (d=0.8-1.0) as they bear the costs of delay and diluted policy. The EU Commission and Parliament are observers/excluded, experiencing the constraint's effects without directly benefiting or being the primary target of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_extraction_boundary,
    'At what point does the exercise of a national veto transition from legitimate protection of sovereignty to minoritarian extraction?',
    'Empirical analysis of vetoes: distinguish between vetoes on core sovereignty issues (e.g., national security, fiscal policy) versus those on broader policy areas (e.g., environmental standards, common foreign policy) where national interest is less directly implicated.',
    'If a clear boundary can be drawn, it would refine the extractiveness metric, potentially reclassifying some ''extractive'' vetoes as legitimate ''sovereignty protection'' (reducing ε). If no clear boundary, the ''veto trap'' reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_extraction_boundary, conceptual, 'Distinguishing legitimate sovereignty protection from rent-seeking via veto.').

omega_variable(
    alternative_decision_making_impact,
    'What would be the full impact (positive and negative) of moving to qualified majority voting (QMV) for all decisions currently requiring unanimity?',
    'Counterfactual modeling and comparative analysis with other international bodies using QMV. This would involve assessing policy efficiency gains versus potential loss of national buy-in and increased risk of ''exit'' by disaffected states.',
    'If QMV leads to significantly better policy outcomes with manageable downsides, it strengthens the ''veto trap'' argument by showing a viable, less extractive alternative. If QMV leads to significant instability, it would lend credence to the ''sovereignty guarantor'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_decision_making_impact, empirical, 'Assessing the systemic effects of removing the unanimity rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__veto_trap_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(eu_c_tr_t2000, eu_council_unanimity__veto_trap_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(eu_c_tr_t2007, eu_council_unanimity__veto_trap_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(eu_c_tr_t2014, eu_council_unanimity__veto_trap_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(eu_c_tr_t2020, eu_council_unanimity__veto_trap_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__veto_trap_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(eu_c_be_t2007, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2007, 0.75).
narrative_ontology:measurement(eu_c_be_t2014, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2014, 0.8).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(eu_c_su_t2007, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(eu_c_su_t2014, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement(eu_c_su_t2020, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_common_foreign_security_policy).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_enlargement_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'EU Council Unanimity' kernel. This 'veto trap' reading focuses on the extractive use of the unanimity rule, distinct from its role in protecting sovereignty or fostering consensus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
