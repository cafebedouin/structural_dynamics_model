% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Statehood Criteria: Hybrid Reading (Objective + Normative Legitimacy)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of the Montevideo
 *   Convention's statehood criteria, which posits that in addition to
 *   objective criteria (territory, population, government, capacity to enter
 *   relations), a state must also possess normative legitimacy (e.g.,
 *   democratic governance, respect for human rights, non-aggression) to be
 *   recognized and fully integrated into the international community. This
 *   reading emerged strongly post-Cold War, particularly in the context of
 *   secessionist movements and humanitarian interventions. It is a contested
 *   interpretation, often advanced by liberal democratic states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.65).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.75).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Statehood Criteria: Hybrid Reading (Objective + Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'c0f608ec-a171-4ec6-afe2-df587c48508f').
narrative_ontology:cs_kernel_codification('c0f608ec-a171-4ec6-afe2-df587c48508f', formalized).
narrative_ontology:cs_authority_grounding('c0f608ec-a171-4ec6-afe2-df587c48508f', lineage).
narrative_ontology:cs_interpretation_layer_present('c0f608ec-a171-4ec6-afe2-df587c48508f').
narrative_ontology:cs_reading_relation('c0f608ec-a171-4ec6-afe2-df587c48508f', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('c0f608ec-a171-4ec6-afe2-df587c48508f', montevideo_statehood_criteria__constitutive_reading, influences).
narrative_ontology:cs_axiom('c0f608ec-a171-4ec6-afe2-df587c48508f', foundational, statehood_requires_normative_legitimacy).
narrative_ontology:cs_axiom_status(statehood_requires_normative_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c0f608ec-a171-4ec6-afe2-df587c48508f', statehood_requires_normative_legitimacy, deontological).
narrative_ontology:cs_axiom('c0f608ec-a171-4ec6-afe2-df587c48508f', secondary, sovereignty_is_conditional_on_human_rights).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional_on_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('c0f608ec-a171-4ec6-afe2-df587c48508f', sovereignty_is_conditional_on_human_rights, deontological).
narrative_ontology:cs_reference_frame('c0f608ec-a171-4ec6-afe2-df587c48508f', post_cold_war_liberal_order).
narrative_ontology:cs_drift_state('c0f608ec-a171-4ec6-afe2-df587c48508f', contemporary_multipolar_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('c0f608ec-a171-4ec6-afe2-df587c48508f', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_regime).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, states_accused_of_human_rights_violations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states actively promote and enforce the hybrid reading, using it to justify recognition or non-recognition, and to legitimize interventions. They benefit from a framework that aligns statehood with their values.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These movements may meet the objective Montevideo criteria (territory, population, government, capacity to enter relations) but are denied recognition due to lacking democratic governance or human rights records, effectively trapping them in a state of non-statehood.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, generational, trapped, regional).

% The hybrid reading strengthens the normative force of international human rights law by linking statehood itself to compliance, providing a powerful tool for advocacy and enforcement.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_regime, beneficiary,
    institutional, civilizational, constrained, universal).

% These states face challenges to their sovereignty and legitimacy, including potential humanitarian intervention, if their internal governance fails to meet the normative criteria embedded in the hybrid reading of statehood.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, states_accused_of_human_rights_violations, payer,
    powerful, biographical, constrained, national).

% Advocates of the purely declaratory reading argue that statehood is an objective fact, not subject to normative judgment, and that the hybrid reading introduces political bias and instability into international law.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, declaratory_theorists, excluded,
    analytical, generational, analytical, global).

% Proponents of the constitutive reading emphasize the role of recognition by existing states, which may or may not align with the normative criteria of the hybrid reading. They see the hybrid reading as an attempt to impose a specific political agenda.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, constitutive_theorists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the international community to coordinate on the legitimate boundaries of state sovereignty, linking it to shared normative values like democracy and human rights, thereby reducing the recognition of illiberal or aggressive entities.
% TRANSFER_FUNCTION: Transfers legitimacy and sovereign rights from entities that fail to meet normative criteria (e.g., non-democratic secessionist movements, human rights violators) to the international community and liberal democratic states, which gain justification for their actions.
% ABSENT_VOICES: Non-liberal secessionist movements and states prioritizing absolute sovereignty over human rights would object, arguing for a purely objective or recognition-based standard of statehood. Their voices are often marginalized in international forums dominated by liberal states.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, the criteria for statehood would revert to either purely objective (declaratory) or purely recognition-based (constitutive) standards. This would significantly alter the landscape of international relations, potentially legitimizing many currently unrecognized entities and removing a key justification for humanitarian intervention.
% FOUNDING_PROBLEM: The problem of how to define legitimate statehood in a world with diverse political systems, and how to prevent the recognition of entities that violate fundamental international norms or threaten global stability.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as evidenced by ongoing debates over recognition of contested territories, secessionist movements, and interventions in states accused of human rights abuses. International legal scholars, human rights organizations, and UN bodies corroborate the ongoing relevance of these challenges, distinct from the self-serving claims of liberal states.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading allows powerful states to deny recognition and intervene based on normative judgments, effectively extracting sovereign rights from entities that fail to conform. Suppression (0.75) is high as it actively suppresses alternative interpretations of statehood and limits the self-determination of non-liberal entities. Theater ratio (0.20) is low because the normative criteria are genuinely applied, though often selectively, and are not merely performative. The metrics show a clear increase in both extractiveness and suppression over time, reflecting the growing influence of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberal democratic states, this is a legitimate and necessary evolution of international law, a 'rope' coordinating global values. From the perspective of non-liberal secessionist movements, it is a 'snare' designed to deny their self-determination and impose external political models. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and the international human rights regime are primary beneficiaries, as this reading provides a legal and moral justification for their foreign policy and normative agenda. Non-liberal secessionist movements and states accused of human rights violations are the primary targets, facing denial of recognition or intervention. Analytical observers (declaratory and constitutive theorists) are excluded from the practical application of this reading, though they continue to contest its theoretical validity.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mislabeling what might appear as pure coordination (e.g., 'promoting democracy') as benign, by highlighting the asymmetric extraction from non-conforming entities. It also prevents mislabeling it as a pure 'snare' by acknowledging the genuine coordination function of establishing shared normative standards for international order. The rising extractiveness and suppression over time suggest a drift towards a more extractive form of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_criteria_objectivity,
    'Are the normative criteria (democratic governance, human rights, non-aggression) objective and universally applicable, or are they culturally specific and selectively applied by powerful states?',
    'Analysis of recognition patterns and intervention justifications across diverse geopolitical contexts, particularly cases where powerful states apply criteria inconsistently.',
    'If subjective and selectively applied, the constraint''s extractiveness and suppression are higher than measured, as the ''normative'' justification serves as cover for political interests. If objective, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_objectivity, conceptual, 'Objectivity vs. selectivity of normative statehood criteria.').

omega_variable(
    legitimacy_vs_sovereignty_tradeoff,
    'Does the emphasis on normative legitimacy for statehood fundamentally undermine the principle of sovereign equality and non-intervention, or does it represent a necessary evolution of international law?',
    'Long-term historical analysis of international legal development and the impact of interventions justified by normative criteria on global stability and state autonomy.',
    'If it fundamentally undermines sovereignty, the constraint is more extractive and coercive, eroding the ''rope'' aspect of international law. If it''s a necessary evolution, the coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_sovereignty_tradeoff, preference, 'Trade-off between normative legitimacy and traditional sovereignty.').

omega_variable(
    hybrid_reading_naturalness,
    'Is the hybrid reading a natural evolution of international law reflecting changing global values, or a constructed constraint imposed by a dominant ideological bloc?',
    'Comparative analysis of state practice and legal scholarship from diverse legal traditions (e.g., Western, post-colonial, non-Western) to identify convergence or divergence on normative criteria for statehood.',
    'If a natural evolution, the constraint''s legitimacy is higher, reducing perceived extractiveness. If constructed, it is more clearly a ''tangled rope'' or ''snare'' serving specific interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_naturalness, empirical, 'Natural evolution vs. constructed imposition of hybrid statehood criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(mont_tr_t1998, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(mont_tr_t2006, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(mont_tr_t2014, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(mont_be_t1998, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(mont_be_t2006, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(mont_be_t2014, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(mont_su_t1998, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(mont_su_t2006, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement(mont_su_t2014, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Montevideo Statehood Criteria kernel. The 'declaratory_reading' and 'constitutive_reading' are sibling constraints, each with distinct structural properties and classifications. This hybrid reading influences both by introducing normative conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
