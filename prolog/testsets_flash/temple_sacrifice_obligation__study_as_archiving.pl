% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation: Study as Archiving
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the Halakhic (Jewish law) understanding that
 *   the obligation for Temple sacrifices remains binding, but in the absence
 *   of the Temple, study of these laws serves to preserve the knowledge for
 *   future restoration, rather than fulfilling the actual sacrificial
 *   command. The entire post-Temple period (since 70 CE) is thus a state of
 *   non-compliance with a divine command, creating a continuous, unresolvable
 *   extraction from the perspective of the divine will and the Jewish
 *   people's collective obligation. The authority structure (rabbinic
 *   tradition) maintains the binding status of this unperformable law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.65).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.9).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, snare).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation: Study as Archiving").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious_studies/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, 'd2bb0e6e-712d-4f06-a6a2-8cf653958782').
narrative_ontology:cs_kernel_codification('d2bb0e6e-712d-4f06-a6a2-8cf653958782', fixed_text).
narrative_ontology:cs_authority_grounding('d2bb0e6e-712d-4f06-a6a2-8cf653958782', lineage).
narrative_ontology:cs_interpretation_layer_present('d2bb0e6e-712d-4f06-a6a2-8cf653958782').
narrative_ontology:cs_reading_relation('d2bb0e6e-712d-4f06-a6a2-8cf653958782', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('d2bb0e6e-712d-4f06-a6a2-8cf653958782', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('d2bb0e6e-712d-4f06-a6a2-8cf653958782', foundational, divine_command_remains_binding_and_unfulfilled).
narrative_ontology:cs_axiom_status(divine_command_remains_binding_and_unfulfilled, holdable).
narrative_ontology:cs_axiom_grounding('d2bb0e6e-712d-4f06-a6a2-8cf653958782', divine_command_remains_binding_and_unfulfilled, deontological).
narrative_ontology:cs_axiom('d2bb0e6e-712d-4f06-a6a2-8cf653958782', foundational, study_is_preparation_not_substitution).
narrative_ontology:cs_axiom_status(study_is_preparation_not_substitution, holdable).
narrative_ontology:cs_axiom_grounding('d2bb0e6e-712d-4f06-a6a2-8cf653958782', study_is_preparation_not_substitution, conventional).
narrative_ontology:cs_reference_frame('d2bb0e6e-712d-4f06-a6a2-8cf653958782', post_temple_halakhic_continuity).
narrative_ontology:cs_drift_state('d2bb0e6e-712d-4f06-a6a2-8cf653958782', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d2bb0e6e-712d-4f06-a6a2-8cf653958782', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_tradition).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_people).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the core obligation is unfulfilled, representing a continuous 'debt' or 'lack' in the religious system. Suppression is very high (0.9) because the physical destruction of the Temple makes the command literally impossible to perform, leaving no alternative for direct compliance. Theater ratio is low (0.1) because the study is genuinely aimed at preservation and future performance, not merely a performative substitute for the actual sacrifice. Accessibility collapse is near total (0.95) as the physical means to perform the sacrifices are absent. Resistance is low (0.05) as the impossibility is widely accepted within the tradition, and study is the prescribed response.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the divine command, there is a continuous, unresolvable extraction due to non-compliance. From the perspective of rabbinic scholars, the constraint provides a framework for intellectual and spiritual engagement, ensuring the tradition's continuity. The Jewish people experience a collective longing and a sense of unfulfilled religious duty.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'divine_command' is the primary victim, as its will is unfulfilled (d=1.0). The 'jewish_people' are also victims, bearing the collective burden of non-compliance (d=0.9). 'Rabbinic_scholars' and the 'halakhic_tradition' are beneficiaries (d=0.1-0.2), as the study and interpretation of these laws become central to their authority and the continuity of the tradition in the Temple's absence. This reading emphasizes the ongoing, unfulfilled nature of the obligation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the study of Temple sacrifice laws a fulfillment, a suspension, or merely an archiving of the obligation?',
    'Theological consensus shift or a messianic event enabling Temple reconstruction and resumption of sacrifices.',
    'If study is a fulfillment (study_as_occupation), the constraint''s extractiveness would be low, and it would classify as a Rope. If the obligation is suspended (messianic_suspension), the constraint would be inert, classifying as a Piton. This reading (study_as_archiving) maintains high extractiveness due to non-compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the status of Temple sacrifice obligation in absence of the Temple.').

omega_variable(
    divine_command_agency,
    'To what extent can a ''divine command'' be considered a ''victim'' in the same sense as human agents?',
    'Philosophical and theological clarification on the nature of divine agency and the implications of non-compliance with divine will.',
    'If the divine command cannot be a victim, the constraint''s victim set is reduced to the Jewish people, potentially altering the perceived severity of extraction, though the structural non-compliance remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_agency, conceptual, 'Theological status of a divine command as a ''victim''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 100, 0.1).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 200, 0.1).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 300, 0.1).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 400, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 200, 0.63).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 300, 0.64).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 400, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 100, 0.9).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 200, 0.9).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 300, 0.9).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 400, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel, each with distinct structural properties and classifications. This reading emphasizes the unfulfilled nature of the obligation, with study serving as archiving.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
