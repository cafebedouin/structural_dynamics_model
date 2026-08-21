% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause: Antisubordination Reading
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   This constraint represents the antisubordination reading of the Equal
 *   Protection Clause, which holds that the clause's primary purpose is to
 *   prevent the state from creating or perpetuating systems of caste-like
 *   subordination. It permits race-conscious state action aimed at
 *   dismantling hierarchy and rejects claims of 'reverse discrimination' from
 *   dominant groups. This reading is one of several competing interpretations
 *   of the Equal Protection Clause, each with distinct implications for civil
 *   rights and education policy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.4).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.3).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '001a25ae-af55-4c9b-b9b2-fdb12d9776e4').
narrative_ontology:cs_kernel_codification('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', fixed_text).
narrative_ontology:cs_authority_grounding('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', lineage).
narrative_ontology:cs_interpretation_layer_present('001a25ae-af55-4c9b-b9b2-fdb12d9776e4').
narrative_ontology:cs_reading_relation('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_axiom('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', foundational, equal_protection_forbids_caste_like_subordination).
narrative_ontology:cs_axiom_status(equal_protection_forbids_caste_like_subordination, holdable).
narrative_ontology:cs_axiom_grounding('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', equal_protection_forbids_caste_like_subordination, deontological).
narrative_ontology:cs_axiom('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', foundational, state_may_use_race_conscious_measures_to_dismantle_hierarchy).
narrative_ontology:cs_axiom_status(state_may_use_race_conscious_measures_to_dismantle_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', state_may_use_race_conscious_measures_to_dismantle_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', post_civil_war_reconstruction_amendments).
narrative_ontology:cs_drift_state('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('001a25ae-af55-4c9b-b9b2-fdb12d9776e4', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_social_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from state actions designed to dismantle caste-like subordination, such as affirmative action or targeted educational programs. Their ability to exit subordination is enhanced by these measures, but they remain constrained by systemic inequalities.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% These actors (e.g., state legislatures, university admissions offices) implement race-conscious policies aimed at dismantling subordination. They face legal challenges from those who oppose such measures but are empowered by this reading to pursue equity goals.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, biographical, constrained, national).

% While not 'victims' in the traditional sense under this reading, members of dominant groups may perceive themselves as bearing a cost when race-conscious remedial measures are implemented, as these measures may alter traditional access to opportunities. Their claims of 'reverse discrimination' are generally rejected by this reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_social_groups, payer,
    powerful, biographical, mobile, national).

% The federal judiciary interprets and applies the Equal Protection Clause. Under this reading, judges would uphold state actions designed to dismantle subordination and reject challenges based on a 'colorblind' interpretation. They are the ultimate arbiters of the clause's meaning.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% Advocates for a colorblind interpretation of the Equal Protection Clause are structurally excluded from the core logic of this reading, which prioritizes dismantling hierarchy over formal equality. Their arguments are considered misinterpretations of the clause's fundamental purpose.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to actively dismantle historical and ongoing caste-like subordination, ensuring that government power is used to promote substantive equality rather than entrench hierarchy.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and recognition from systems that perpetuate racial hierarchy towards historically oppressed groups, aiming to rebalance societal power structures.
% ABSENT_VOICES: Advocates for a strictly 'colorblind' interpretation of the Equal Protection Clause are largely absent from the core interpretive framework of this reading, as their premise of formal equality is seen as insufficient to address substantive inequality. They would argue that any race-conscious measure is inherently discriminatory.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, state actors would lose a crucial legal justification for race-conscious remedial measures. This would likely lead to a re-entrenchment of existing hierarchies, as the legal framework would no longer actively permit or encourage dismantling subordination, causing significant societal rearrangement.
% FOUNDING_PROBLEM: The Equal Protection Clause was designed to address the legacy of slavery and racial discrimination, preventing states from perpetuating systems of caste and subordination.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, legal scholars focused on critical race theory, and historical analyses of systemic inequality corroborate that the problem of caste-like subordination remains live. This is attested from outside the immediate beneficiaries by academic research and advocacy groups.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.4) because while it aims to dismantle hierarchy, the process itself can involve reallocating resources or opportunities, which some groups perceive as a cost. Suppression is low (0.3) as this reading actively encourages resistance to existing hierarchies rather than suppressing it. Theater ratio is low (0.1) because the reading's intent is direct and functional: to achieve substantive equality. The metrics reflect the ongoing struggle and contestation around implementing this reading, with some perceived 'extraction' from dominant groups and a need for active enforcement against resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this reading is a vital tool for justice and equity, reducing extraction. From the perspective of dominant social groups, it may be seen as imposing costs or even 'reverse discrimination,' leading to a perception of extraction. The engine's classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are the primary beneficiaries (d near 0.0) as the reading aims to improve their structural position. State actors implementing remedies are agenda-setters, aligning with the beneficiaries. Dominant social groups are payers (d near 1.0) as they bear the perceived costs of remedial measures. The federal judiciary acts as an observer, interpreting the clause, while colorblind advocates are excluded, their arguments not fitting the core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_definition_ambiguity,
    'How is ''caste-like subordination'' precisely defined and measured in practice, and does this definition evolve with societal changes?',
    'Judicial precedent establishing clear, measurable criteria for subordination, or legislative action providing specific definitions and metrics for identifying subordinated groups.',
    'A clear, stable definition would strengthen the reading''s enforceability and reduce contestation over who qualifies as a beneficiary. An ambiguous or shifting definition could lead to inconsistent application and increased resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_definition_ambiguity, conceptual, 'Ambiguity in defining the core concept of ''subordination''.').

omega_variable(
    remedial_measures_efficacy,
    'Are the race-conscious measures permitted by this reading actually effective at dismantling subordination, or do they create unintended consequences?',
    'Longitudinal empirical studies tracking the outcomes of specific remedial policies on various indicators of subordination (e.g., wealth gaps, educational attainment, political representation).',
    'If measures are shown to be ineffective or counterproductive, the legitimacy of this reading could be challenged, potentially shifting support towards alternative interpretations or requiring new policy approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_measures_efficacy, empirical, 'Empirical effectiveness of antisubordination remedies.').

omega_variable(
    scope_of_state_action,
    'What is the precise boundary between permissible state action to dismantle subordination and impermissible state action that itself creates new forms of classification or ''reverse discrimination''?',
    'Further judicial clarification through case law, establishing clear tests or frameworks for evaluating the constitutionality of specific race-conscious policies under this reading.',
    'A clearer boundary would reduce legal uncertainty for state actors and potentially reduce litigation. An unclear boundary could lead to a chilling effect on remedial policies or continued legal challenges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_state_action, conceptual, 'Defining the limits of permissible race-conscious state action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__antisubordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t10, equal_protection_kernel__antisubordination_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__antisubordination_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__antisubordination_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(equa_tr_t40, equal_protection_kernel__antisubordination_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(equa_tr_t50, equal_protection_kernel__antisubordination_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__antisubordination_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(equa_be_t10, equal_protection_kernel__antisubordination_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__antisubordination_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__antisubordination_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(equa_be_t40, equal_protection_kernel__antisubordination_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(equa_be_t50, equal_protection_kernel__antisubordination_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__antisubordination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(equa_su_t10, equal_protection_kernel__antisubordination_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__antisubordination_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__antisubordination_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(equa_su_t40, equal_protection_kernel__antisubordination_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(equa_su_t50, equal_protection_kernel__antisubordination_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the Equal Protection Clause kernel. It focuses on dismantling caste-like subordination, distinct from colorblindness or narrow remedial approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
