% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection: Anti-Caste Reading (Active Dismantling of Hierarchy)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.85).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.78).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, scaffold).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection: Anti-Caste Reading (Active Dismantling of Hierarchy)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).
narrative_ontology:has_sunset_clause(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '58716d9e-d041-4b92-bec4-eb8460e08b7d').
narrative_ontology:cs_kernel_codification('58716d9e-d041-4b92-bec4-eb8460e08b7d', fixed_text).
narrative_ontology:cs_authority_grounding('58716d9e-d041-4b92-bec4-eb8460e08b7d', lineage).
narrative_ontology:cs_interpretation_layer_present('58716d9e-d041-4b92-bec4-eb8460e08b7d').
narrative_ontology:cs_reading_relation('58716d9e-d041-4b92-bec4-eb8460e08b7d', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('58716d9e-d041-4b92-bec4-eb8460e08b7d', foundational, equality_requires_substantive_outcomes).
narrative_ontology:cs_axiom_status(equality_requires_substantive_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('58716d9e-d041-4b92-bec4-eb8460e08b7d', equality_requires_substantive_outcomes, deontological).
narrative_ontology:cs_axiom('58716d9e-d041-4b92-bec4-eb8460e08b7d', foundational, state_has_affirmative_duty_to_dismantle_hierarchy).
narrative_ontology:cs_axiom_status(state_has_affirmative_duty_to_dismantle_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('58716d9e-d041-4b92-bec4-eb8460e08b7d', state_has_affirmative_duty_to_dismantle_hierarchy, deontological).
narrative_ontology:cs_reference_frame('58716d9e-d041-4b92-bec4-eb8460e08b7d', post_reconstruction_era_promise).
narrative_ontology:cs_drift_state('58716d9e-d041-4b92-bec4-eb8460e08b7d', contemporary_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('58716d9e-d041-4b92-bec4-eb8460e08b7d', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, beneficiaries_of_hierarchy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, status_quo_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically and currently bear the brunt of racial, gender, and status hierarchies. They are the primary beneficiaries of state corrective action aimed at dismantling these structures, experiencing improved access to opportunities and resources.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups, beneficiary,
    powerless, generational, trapped, national).

% Champion the anti-caste interpretation of Equal Protection, working through legal, political, and social channels to advocate for and implement corrective action. They benefit from the legitimacy and mandate this reading provides for their efforts.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Individuals and institutions who have historically and currently benefit from existing racial, gender, and status hierarchies. They bear the costs of corrective action, which may include loss of unearned advantage, redistribution of resources, or changes to established practices.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, beneficiaries_of_hierarchy, payer,
    powerful, generational, constrained, national).

% Actively resist the anti-caste interpretation and state corrective action, often through legal challenges, political lobbying, and public discourse. They seek to maintain existing social and economic arrangements that implicitly or explicitly favor certain groups.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, status_quo_defenders, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, status_quo_defenders, agenda_setter).

% Responsible for interpreting the 14th Amendment and adjudicating challenges to state corrective action. Their rulings shape the scope and enforceability of the anti-caste reading, acting as a critical gatekeeper for its implementation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Enacts laws and policies for corrective action (e.g., affirmative action, anti-discrimination statutes) in response to the anti-caste mandate. Their actions are subject to judicial review and political contestation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, legislature, agenda_setter,
    institutional, biographical, analytical, national).

% Advocate for an interpretation of Equal Protection that prohibits explicit state classification based on race or status, even for remedial purposes. While part of the broader constitutional discourse, their core premise is structurally excluded from the anti-caste reading's framework.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_proponents, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and societal efforts to identify, measure, and actively dismantle systemic racial, gender, and status hierarchies, ensuring a more equitable distribution of opportunities and outcomes.
% TRANSFER_FUNCTION: Transfers resources, opportunities, and social status from historically dominant groups and institutions (who benefit from existing hierarchies) to historically subordinated groups, and shifts the burden of addressing systemic inequality to the state.
% ABSENT_VOICES: Historically marginalized communities whose experiences of systemic hierarchy were not initially centered in constitutional interpretation; those who benefit from the status quo and resist any re-distribution of power or resources, often framing their resistance as defense of 'merit' or 'individual liberty'.
% DISAPPEARANCE_RATIONALE: If this anti-caste interpretation vanished, the legal and political mandate for addressing systemic inequality through corrective action would disappear. Existing hierarchies would persist and deepen, state action would likely revert to formal equality without substantive impact, and the mobile software economy would reorganize around open payment routing.
% FOUNDING_PROBLEM: The persistence of substantive racial, gender, and status-based hierarchies and inequalities in society, despite formal legal equality, undermining the promise of the 14th Amendment.
% FOUNDING_PROBLEM_CORROBORATION: Extensive sociological research, economic data on wealth and income disparities, historical analyses of systemic discrimination, and ongoing testimony from civil rights organizations and affected communities, all from sources outside the direct beneficiaries of the anti-caste reading.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_ambiguity,
    'Is this constraint truly a distinct ''anti-caste'' reading, or is it a policy application of a broader ''substantive equality'' principle that could be instantiated differently?',
    'Analysis of judicial opinions and legal scholarship to determine if ''anti-caste'' is a coherent, distinct interpretive framework or merely a specific policy outcome of a more general principle.',
    'If a distinct reading, its structural properties are unique. If a policy application, the underlying principle might be a different constraint, and this would be a ''tangled_rope'' or ''scaffold'' that implements it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Distinguishing the ''anti-caste'' reading as a unique interpretive framework.').

omega_variable(
    formal_equality_structural_delta,
    'How would the structural properties of Equal Protection change if the ''formal_equality_reading'' were universally adopted as the sole interpretation?',
    'Counterfactual analysis of legal outcomes, resource distribution, and social mobility under a purely formal equality regime, compared to the current mixed interpretive landscape.',
    'The ''formal_equality_reading'' would likely result in lower measured extractiveness from beneficiaries of hierarchy (as state intervention would be limited) but higher effective extraction on subordinated groups (as systemic inequalities would persist unchallenged).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_equality_structural_delta, conceptual, 'Impact of the sibling ''formal_equality_reading'' on constraint structure.').

omega_variable(
    sunset_clause_feasibility,
    'Is the ''sunset clause'' (achievement of substantive equality) a realistic and measurable goal, or does the constraint risk becoming a permanent, self-perpetuating mechanism?',
    'Empirical tracking of key equality metrics (e.g., wealth gaps, educational attainment, representation) over multiple generations, coupled with a clear definition of ''substantive equality'' agreed upon by diverse stakeholders.',
    'If the sunset is deemed unachievable or perpetually deferred, the constraint''s classification would drift from ''scaffold'' towards ''tangled_rope'' or even ''snare'', indicating a permanent extractive mechanism rather than a transitional support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_feasibility, empirical, 'Feasibility and measurability of the scaffold''s sunset condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(four_tr_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(four_tr_t1985, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(four_tr_t2000, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.7).
narrative_ontology:measurement(four_be_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1970, 0.78).
narrative_ontology:measurement(four_be_t1985, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1985, 0.82).
narrative_ontology:measurement(four_be_t2000, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.65).
narrative_ontology:measurement(four_su_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(four_su_t1985, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(four_su_t2000, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
