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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid Statehood Criteria (Objective + Normative Legitimacy)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of the Montevideo
 *   Statehood Criteria, which asserts that statehood requires not only the
 *   four objective criteria (permanent population, defined territory,
 *   government, capacity to enter into relations with other states) but also
 *   normative legitimacy, typically understood as democratic governance,
 *   respect for human rights, and non-aggression. This reading emerged
 *   strongly post-Cold War, influencing recognition practices and providing
 *   justification for interventions. It is a 'Tangled Rope' because it
 *   coordinates international recognition while extracting from entities that
 *   fail its normative tests, requiring active enforcement.
 *
 * KEY AGENTS:
 *   - liberal_democratic_states: Agenda setter/Beneficiary (institutional/mobile)
 *   - non_liberal_secessionist_movements: Payer/Victim (powerless/trapped)
 *   - authoritarian_regimes_seeking_recognition: Payer/Victim (powerful/constrained)
 *   - international_human_rights_advocates: Beneficiary (organized/mobile)
 *   - international_law_scholars: Observer (analytical/analytical)
 *   - existing_community_of_states: Agenda setter/Beneficiary (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.75).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.8).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid Statehood Criteria (Objective + Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '6275437d-fb8b-4a8b-9bd6-2b5332814cdf').
narrative_ontology:cs_kernel_codification('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', formalized).
narrative_ontology:cs_authority_grounding('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', lineage).
narrative_ontology:cs_interpretation_layer_present('6275437d-fb8b-4a8b-9bd6-2b5332814cdf').
narrative_ontology:cs_reading_relation('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', foundational, state_sovereignty_is_conditional_on_legitimacy).
narrative_ontology:cs_axiom_status(state_sovereignty_is_conditional_on_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', state_sovereignty_is_conditional_on_legitimacy, deontological).
narrative_ontology:cs_axiom('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', foundational, human_rights_are_universal_and_enforceable).
narrative_ontology:cs_axiom_status(human_rights_are_universal_and_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', human_rights_are_universal_and_enforceable, deontological).
narrative_ontology:cs_reference_frame('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', post_cold_war_normative_shift).
narrative_ontology:cs_drift_state('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', contemporary_geopolitical_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6275437d-fb8b-4a8b-9bd6-2b5332814cdf', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_regimes_seeking_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, existing_community_of_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states largely define and enforce the normative criteria for statehood and recognition. They benefit from the enhanced legitimacy this reading provides for their foreign policy, including denying recognition or justifying intervention against non-compliant entities.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, mobile, global).

% Entities that may meet the objective Montevideo criteria (population, territory, government, capacity to enter relations) but fail to meet the normative standards (e.g., democratic governance, human rights) are denied recognition, facing severe obstacles to achieving statehood and international standing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, biographical, trapped, regional).

% Existing states or aspiring states with authoritarian governance or poor human rights records face delegitimization, sanctions, or even intervention, despite potentially meeting objective statehood criteria. They bear the cost of non-compliance with normative standards.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_regimes_seeking_recognition, payer,
    powerful, biographical, constrained, national).

% Organizations and individuals promoting human rights and democratic values benefit from this reading, as it provides a legal and normative basis for their advocacy and for holding states accountable to international standards.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Academics and legal experts who analyze, debate, and interpret the criteria for statehood and recognition. They contribute to the discourse but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% The collective body of recognized states that, through their diplomatic practice and international organizations, collectively interpret and enforce these criteria. They benefit from a more 'ordered' and normatively aligned international system, but also bear the costs of maintaining consensus and enforcement.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, existing_community_of_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, existing_community_of_states, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative framework for the international community to coordinate on the recognition of new states and the legitimacy of existing ones, integrating shared values like democracy and human rights into the legal definition of statehood.
% TRANSFER_FUNCTION: Transfers the right to full, unconditional recognition and non-intervention from entities failing normative criteria to the international community (especially liberal democratic states), which gains justification for denying recognition, imposing sanctions, or intervening.
% ABSENT_VOICES: Non-liberal states, secessionist movements, and proponents of strict state sovereignty would object to the imposition of normative criteria, arguing it infringes on self-determination and creates a hierarchical international order. They are often marginalized in the discourse of recognition by powerful states.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the normative basis for denying recognition to objectively-qualified but normatively-deficient entities would disappear. This would likely lead to a more permissive environment for state creation, less justification for humanitarian intervention or regime change, and a significant shift in the international legal and political landscape towards a more positivist, less value-laden approach to statehood.
% FOUNDING_PROBLEM: The perceived inadequacy of purely objective (declaratory) or purely political (constitutive) criteria to address human rights abuses, democratic deficits, and aggression by state-like entities, leading to a desire for a more 'just' and value-aligned international order post-Cold War.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and numerous liberal democratic states consistently advocate for these normative criteria, citing ongoing challenges to peace, human rights, and democratic governance as evidence that the problem remains live. This is corroborated by UN resolutions and state practice, not just the benefiting parties.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the normative criteria impose significant costs on entities that do not conform, potentially denying them statehood or subjecting them to sanctions/intervention. Suppression is also high as the international community actively enforces these criteria through diplomatic pressure, non-recognition, and sometimes military action. Theater ratio is moderate, reflecting that while the normative claims are genuinely held by many, their application can sometimes be selective or politically motivated. Accessibility collapse is high for non-compliant entities, as the path to legitimate statehood is severely restricted. Resistance is high from those targeted by these criteria.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberal democratic states and human rights advocates, this constraint is a legitimate and necessary evolution of international law, ensuring a more just world order. From the perspective of non-liberal secessionist movements or authoritarian regimes, it is an extractive imposition of values that infringes on sovereignty and self-determination, serving the interests of powerful states.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and human rights advocates are beneficiaries, as the constraint aligns with their values and provides tools for their foreign policy and advocacy. Non-liberal secessionist movements and authoritarian regimes are victims, bearing the costs of non-compliance. The existing community of states acts as an agenda setter, collectively enforcing the constraint, with some members benefiting more than others.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (promoting democracy and human rights) is still live, but its application is contested. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring its genuine coordination function for a value-aligned international order). The ongoing contestation over its legitimacy and application is key to understanding its dynamic nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_criteria_consistency,
    'Is the application of normative criteria for statehood and recognition consistent across all cases, or is it selectively applied based on geopolitical interests?',
    'Empirical analysis of recognition patterns and intervention decisions over time, comparing cases with similar objective and normative profiles but different geopolitical significance.',
    'If selectively applied, the effective extraction and suppression are higher for geopolitically disfavored entities, and the theater_ratio for the constraint increases, suggesting a stronger Snare component. If consistently applied, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_consistency, empirical, 'Consistency of normative criteria application.').

omega_variable(
    objective_vs_normative_primacy,
    'Which aspect of the hybrid reading holds ultimate primacy in practice: the objective Montevideo criteria or the normative legitimacy criteria?',
    'Analysis of cases where objective criteria are met but normative criteria are severely lacking (or vice versa), and observing the international community''s response (e.g., recognition, sanctions, intervention).',
    'If normative criteria consistently override objective facts, the constraint leans more towards a Snare for non-compliant entities. If objective criteria still provide a strong baseline, it reinforces the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_vs_normative_primacy, conceptual, 'Primacy of objective vs. normative criteria.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the normative legitimacy derived from universal principles (deontological) or from the consensus of powerful states (conventional/power-based)?',
    'Philosophical and legal analysis of the grounding arguments for the normative criteria, alongside empirical observation of which states'' interpretations prevail in practice.',
    'If primarily power-based, the constraint''s extraction is more clearly tied to the interests of powerful states, potentially increasing its Snare-like qualities. If genuinely universal, it strengthens the coordination function''s moral grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Source of normative legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(mont_tr_t1995, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(mont_tr_t2000, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(mont_tr_t2005, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(mont_tr_t2015, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(mont_tr_t2020, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(mont_be_t1995, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(mont_be_t2000, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(mont_be_t2005, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(mont_be_t2015, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement(mont_be_t2020, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(mont_su_t1995, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(mont_su_t2000, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(mont_su_t2005, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(mont_su_t2015, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement(mont_su_t2020, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, international_humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, international_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Montevideo Statehood Criteria' kernel. The other readings are 'declaratory_reading' and 'constitutive_reading', each representing a distinct structural claim about statehood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
