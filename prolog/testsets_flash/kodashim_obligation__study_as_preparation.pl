% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Preparation for Messianic Restoration
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the obligation within Jewish law to study the
 *   Kodashim (sacrificial) order, despite the destruction of the Temple
 *   rendering actual performance impossible. This specific reading frames
 *   study not as a substitute for sacrifice, but as an instrumental
 *   preparation for its eventual messianic restoration. The constraint is
 *   understood as divinely ordained, immutable, and essential for the
 *   continuity of the covenant, with the primary beneficiary being a future,
 *   restored state of cosmic order and the generations who will live within
 *   it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.25).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.1).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.25).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'e0d8d90a-77f8-4984-9022-1cb71d90fc40').
narrative_ontology:cs_kernel_codification('e0d8d90a-77f8-4984-9022-1cb71d90fc40', fixed_text).
narrative_ontology:cs_authority_grounding('e0d8d90a-77f8-4984-9022-1cb71d90fc40', lineage).
narrative_ontology:cs_interpretation_layer_present('e0d8d90a-77f8-4984-9022-1cb71d90fc40').
narrative_ontology:cs_reading_relation('e0d8d90a-77f8-4984-9022-1cb71d90fc40', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('e0d8d90a-77f8-4984-9022-1cb71d90fc40', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('e0d8d90a-77f8-4984-9022-1cb71d90fc40', foundational, study_is_instrumental_preparation).
narrative_ontology:cs_axiom_status(study_is_instrumental_preparation, holdable).
narrative_ontology:cs_axiom_grounding('e0d8d90a-77f8-4984-9022-1cb71d90fc40', study_is_instrumental_preparation, theological).
narrative_ontology:cs_axiom('e0d8d90a-77f8-4984-9022-1cb71d90fc40', foundational, messianic_restoration_is_future_event).
narrative_ontology:cs_axiom_status(messianic_restoration_is_future_event, holdable).
narrative_ontology:cs_axiom_grounding('e0d8d90a-77f8-4984-9022-1cb71d90fc40', messianic_restoration_is_future_event, theological).
narrative_ontology:cs_reference_frame('e0d8d90a-77f8-4984-9022-1cb71d90fc40', divine_command_and_future_restoration).
narrative_ontology:cs_drift_state('e0d8d90a-77f8-4984-9022-1cb71d90fc40', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e0d8d90a-77f8-4984-9022-1cb71d90fc40', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_generation).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_preparation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_preparation),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because the 'cost' of study is primarily intellectual and spiritual effort, not material or coercive extraction. Suppression is low (0.1) as the obligation is largely internalized and self-enforced within the religious community, not through external coercion. Theater ratio is very low (0.05) because the study is considered genuinely functional for its stated purpose (preservation of knowledge for future performance), not merely performative. Accessibility collapse is high (0.9) as the divine origin and messianic imperative leave little room for alternative interpretations of the obligation's necessity. Resistance is low (0.05) due to the deeply ingrained theological acceptance of the obligation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'current_generation_of_scholars', the constraint is a demanding but spiritually rewarding obligation. From the perspective of the 'messianic_future_generation', it is a foundational act of preservation that ensures their ability to fulfill the commandments. The divine perspective (as understood by adherents) sees it as an eternal, unchanging command.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'messianic_future_generation' is the primary beneficiary (d=0.0), as they will directly benefit from the preserved knowledge enabling the restoration of sacrificial service. The 'current_generation_of_scholars' are both payers (bear the cost of study) and indirect beneficiaries (spiritual merit, communal role), placing their d closer to symmetric. The 'divine_authority' is the ultimate source and enforcer, but not a beneficiary in the extractive sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the obligation to study Kodashim a genuine natural law (divinely ordained, immutable) or a constructed obligation that benefits identifiable agents (the future messianic generation)?',
    'Theological consensus on the nature of divine commandment and the role of human agency in its perpetuation.',
    'If purely natural law, its classification as Mountain is robust. If significantly constructed, the beneficiary structure (messianic_future_generation) would push it towards a False Summit Mountain (Tangled Rope) classification, highlighting the deferred extraction from the present generation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, conceptual, 'Ambiguity between divine command and human-maintained obligation.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is the ''study_as_preparation'' reading of the ''kodashim_obligation'' kernel. How would the classification change under sibling readings?',
    'Analyzing the ''study_as_performance'' and ''study_as_archive'' readings as separate constraints, each with its own metrics and stakeholder analysis.',
    'The ''study_as_performance'' reading would likely show higher extractiveness (present generation bears full cosmic burden) and potentially a different claimed type (e.g., Tangled Rope). The ''study_as_archive'' reading would likely show lower extractiveness and be closer to a Rope or Mountain, with different beneficiaries (e.g., historical scholars).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Impact of alternative readings of the Kodashim obligation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_preparation, theater_ratio, 100, 0.05).
narrative_ontology:measurement(koda_tr_t200, kodashim_obligation__study_as_preparation, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_preparation, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(koda_be_t200, kodashim_obligation__study_as_preparation, base_extractiveness, 200, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_preparation, suppression_requirement, 100, 0.1).
narrative_ontology:measurement(koda_su_t200, kodashim_obligation__study_as_preparation, suppression_requirement, 200, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_obligation' kernel, each representing a distinct structural claim about the nature and purpose of studying sacrificial law in the absence of the Temple.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
