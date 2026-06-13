% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment: Performance Only Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the Temple
 *   Sacrifice Commitment kernel. It asserts that the divine command for
 *   sacrifice requires material instantiation, and therefore, in the absence
 *   of the Temple, the commitment is dormant. Study of sacrifice law is
 *   considered archival preservation or preparation, not an active
 *   fulfillment of the command. This reading is structurally a Mountain due
 *   to its grounding in an unchangeable physical reality (the absence of the
 *   Temple) and a literal interpretation of divine law, with negligible
 *   extraction or suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.1).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment: Performance Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '803dc36c-d885-41aa-912a-579efe41078a').
narrative_ontology:cs_kernel_codification('803dc36c-d885-41aa-912a-579efe41078a', fixed_text).
narrative_ontology:cs_authority_grounding('803dc36c-d885-41aa-912a-579efe41078a', lineage).
narrative_ontology:cs_interpretation_layer_present('803dc36c-d885-41aa-912a-579efe41078a').
narrative_ontology:cs_reading_relation('803dc36c-d885-41aa-912a-579efe41078a', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('803dc36c-d885-41aa-912a-579efe41078a', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('803dc36c-d885-41aa-912a-579efe41078a', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('803dc36c-d885-41aa-912a-579efe41078a', foundational, material_performance_is_essence).
narrative_ontology:cs_axiom_status(material_performance_is_essence, holdable).
narrative_ontology:cs_axiom_grounding('803dc36c-d885-41aa-912a-579efe41078a', material_performance_is_essence, deontological).
narrative_ontology:cs_axiom('803dc36c-d885-41aa-912a-579efe41078a', foundational, study_is_not_performance).
narrative_ontology:cs_axiom_status(study_is_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('803dc36c-d885-41aa-912a-579efe41078a', study_is_not_performance, conventional).
narrative_ontology:cs_reference_frame('803dc36c-d885-41aa-912a-579efe41078a', pre_destruction_literal_performance).
narrative_ontology:cs_drift_state('803dc36c-d885-41aa-912a-579efe41078a', post_destruction_diaspora, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('803dc36c-d885-41aa-912a-579efe41078a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading imposes no active burden or cost on adherents beyond the deferred obligation of a future performance. Suppression is low (0.1) as it primarily reflects the structural impossibility of performance, not active coercion. Theater ratio is zero (0.0) because there is no performative maintenance of a defunct practice; the practice is genuinely defunct. Accessibility collapse is high (0.95) because the physical conditions for performance are entirely absent, making alternatives (like symbolic performance) structurally impossible within this reading. Resistance is low (0.05) because the physical reality is largely accepted, even by those who advocate for other readings.
 *
 * PERSPECTIVAL GAP:
 *   There is little perspectival gap in the current state, as the physical absence of the Temple is universally acknowledged. The divergence arises in how different readings interpret the *meaning* of that absence for the commitment, not in the fact of the absence itself. This reading emphasizes the 'mountain' aspect of physical reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities who uphold this reading act as agenda-setters, defining the terms of the commitment. Adherents are payers in the sense that they bear the 'cost' of non-performance as a deferred obligation, but no active extraction occurs. Messianic restorationists are beneficiaries, as this reading reinforces the necessity of a future Temple. Historical scholars are observers, analyzing the constraint from an academic distance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_interpretive_choice,
    'Is the ''performance_only'' requirement a natural law derived from the intrinsic nature of sacrifice, or an interpretive choice within Halakha?',
    'Analysis of early rabbinic disputes and philosophical arguments regarding the essence of sacrifice, particularly those predating the Temple''s destruction, to determine if the material requirement was always considered absolute or if alternative interpretations were suppressed.',
    'If an interpretive choice, the constraint''s ''emerges_naturally'' claim would be weakened, potentially reclassifying it from Mountain to a more constructed type (e.g., Rope or Tangled Rope) if identifiable beneficiaries of this interpretation are found.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_interpretive_choice, conceptual, 'Ambiguity between physical necessity and interpretive tradition.').

omega_variable(
    future_victim_set_if_restored,
    'If the Temple were restored and sacrifices reinstituted under this reading, would a new victim set emerge due to ethical evolution?',
    'Hypothetical ethical analysis of animal welfare, environmental impact, and social equity in a modern context, compared to the historical context of sacrifice. This would involve engaging with contemporary ethical frameworks.',
    'If a new victim set (e.g., animals, marginalized groups bearing the burden of ritual) would emerge, the constraint, upon re-instantiation, would likely shift from Mountain to Snare or Tangled Rope, as its ''naturalness'' would be challenged by its extractive consequences in a changed ethical landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_victim_set_if_restored, preference, 'Potential for future extraction if the dormant practice is revived without ethical re-evaluation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (physical absence of Temple) or internalized (adherents believe they cannot perform)?',
    'Analysis of adherence patterns in communities where alternative, non-material forms of ''sacrifice'' are proposed. If adherents reject these alternatives due to a deep-seated belief in the sole validity of material performance, it suggests internalized suppression. If they would adopt them but for external barriers, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit (or in this case, after the removal of the physical barrier).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.0).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__performance_only, theater_ratio, 500, 0.0).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__performance_only, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__performance_only, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__performance_only, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__performance_only, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__performance_only, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__performance_only, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__performance_only, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__performance_only, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__performance_only, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__performance_only, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__performance_only, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
