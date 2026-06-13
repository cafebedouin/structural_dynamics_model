% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Boundary: Birth Threshold Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines personhood as beginning at birth, granting full
 *   moral and legal standing to all born humans. It is presented as a
 *   foundational principle of universal human rights, ensuring protection
 *   regardless of capabilities or potential. The constraint is structurally a
 *   Mountain, as its core premise is treated as an unchangeable moral law,
 *   with negligible extraction and suppression, and high accessibility
 *   collapse for alternatives. It benefits all born humans by guaranteeing
 *   their status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.05).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.1).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'aeeecd2c-6a9c-4864-8ba2-220c30100031').
narrative_ontology:cs_kernel_codification('aeeecd2c-6a9c-4864-8ba2-220c30100031', formalized).
narrative_ontology:cs_authority_grounding('aeeecd2c-6a9c-4864-8ba2-220c30100031', lineage).
narrative_ontology:cs_interpretation_layer_present('aeeecd2c-6a9c-4864-8ba2-220c30100031').
narrative_ontology:cs_reading_relation('aeeecd2c-6a9c-4864-8ba2-220c30100031', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('aeeecd2c-6a9c-4864-8ba2-220c30100031', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('aeeecd2c-6a9c-4864-8ba2-220c30100031', foundational, birth_as_sufficient_condition_for_personhood).
narrative_ontology:cs_axiom_status(birth_as_sufficient_condition_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('aeeecd2c-6a9c-4864-8ba2-220c30100031', birth_as_sufficient_condition_for_personhood, deontological).
narrative_ontology:cs_axiom('aeeecd2c-6a9c-4864-8ba2-220c30100031', foundational, universal_moral_equality_of_born_humans).
narrative_ontology:cs_axiom_status(universal_moral_equality_of_born_humans, holdable).
narrative_ontology:cs_axiom_grounding('aeeecd2c-6a9c-4864-8ba2-220c30100031', universal_moral_equality_of_born_humans, deontological).
narrative_ontology:cs_reference_frame('aeeecd2c-6a9c-4864-8ba2-220c30100031', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('aeeecd2c-6a9c-4864-8ba2-220c30100031', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aeeecd2c-6a9c-4864-8ba2-220c30100031', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_humans).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, ExtMetricName, E),
    domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the constraint primarily grants status and protection, rather than extracting resources. Suppression is low (0.1) as it requires minimal active enforcement against widespread acceptance, though some historical and fringe views might be 'suppressed'. Theater ratio is zero, as its function is direct and universally recognized. Accessibility collapse is high (0.95) because, within this framework, there are virtually no legitimate alternatives to recognizing born humans as persons. Resistance is low (0.05) due to broad societal and legal consensus.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap for this reading, as its core tenet is widely accepted. The 'victim' status of severely disabled infants is not one of extraction, but rather of being historically vulnerable to exclusion by other readings, which this constraint actively prevents. All seats largely agree on the beneficial nature of this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are the primary beneficiaries (d=0.0) as they receive inherent moral and legal standing. Legal systems act as agenda-setters (d=0.15) by codifying and enforcing this principle, benefiting from the clarity and stability it provides. Moral philosophers are observers (d=0.5) who analyze the constraint without direct benefit or cost. Severely disabled infants are explicitly included as beneficiaries/victims (d=0.0) in the sense that they are protected from being victimized by alternative readings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the principle that personhood begins at birth a genuine natural moral law, or a deeply entrenched social construct that benefits born humans?',
    'Cross-cultural and historical analysis of societies that have genuinely operated on different personhood criteria without external coercion, or philosophical arguments for its a priori necessity.',
    'If a social construct, its ''mountain'' classification would be a false summit, reclassifying it as a Tangled Rope or Snare, depending on the degree of extraction from those excluded by alternative readings. If a natural law, its Mountain status is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between natural moral law and deeply entrenched social construct.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''birth_threshold_reading'' of the ''personhood_boundary'' kernel. What structural elements would change if a ''fitness_contingent_reading'' were adopted?',
    'Analysis of legal and ethical frameworks derived from the ''fitness_contingent_reading'' (e.g., historical eugenics laws, certain philosophical arguments).',
    'A ''fitness_contingent_reading'' would shift the victim set to include pre-fitness entities (e.g., severely disabled infants), increase extraction from them, and likely increase suppression against those who advocate for their full personhood. The ''birth_threshold_reading'' forecloses this outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of adopting a ''fitness_contingent_reading'' on victim set and extraction.').

omega_variable(
    potential_based_reading_delta,
    'What structural elements would change if a ''potential_based_reading'' of the ''personhood_boundary'' kernel were adopted?',
    'Examination of ethical arguments and proposed legal frameworks that define personhood based on the potential for rational agency.',
    'A ''potential_based_reading'' would create ambiguity for individuals with severely limited or absent potential for rational agency (e.g., anencephalic infants), potentially shifting them into a victim category and increasing extraction from them by denying full moral standing. The ''birth_threshold_reading'' forecloses this exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_based_reading_delta, conceptual, 'Impact of adopting a ''potential_based_reading'' on moral standing for certain individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 1600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1600, personhood_boundary__birth_threshold_reading, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(pers_tr_t1700, personhood_boundary__birth_threshold_reading, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(pers_tr_t1800, personhood_boundary__birth_threshold_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(pers_tr_t1900, personhood_boundary__birth_threshold_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(pers_tr_t2000, personhood_boundary__birth_threshold_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(pers_tr_t2024, personhood_boundary__birth_threshold_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(pers_be_t1600, personhood_boundary__birth_threshold_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(pers_be_t1700, personhood_boundary__birth_threshold_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(pers_be_t1800, personhood_boundary__birth_threshold_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(pers_be_t1900, personhood_boundary__birth_threshold_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__birth_threshold_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__birth_threshold_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1600, personhood_boundary__birth_threshold_reading, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(pers_su_t1700, personhood_boundary__birth_threshold_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(pers_su_t1800, personhood_boundary__birth_threshold_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(pers_su_t1900, personhood_boundary__birth_threshold_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__birth_threshold_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__birth_threshold_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
