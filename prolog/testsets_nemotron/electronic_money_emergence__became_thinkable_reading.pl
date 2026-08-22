% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Digital Money Emergence: Became Thinkable Reading
 *   domain: economic/monetary/technology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'became_thinkable_reading' of the
 *   electronic_money_emergence kernel. It asserts that digital money emerged
 *   as a social-technical possibility when the conceptual framework for
 *   dematerialized value became widely thinkable — a gradual diffusion
 *   process spanning decades — long before statistical agencies (M4/M5) or
 *   institutional bearers recognized it as a distinct category. The
 *   constraint is the cognitive-sociological barrier: the moment 'electronic
 *   money' becomes a coherent concept that practitioners, engineers, and
 *   early adopters can reason about and build toward. No single threshold
 *   event marks this; measurement by institutions lags conceptual innovation
 *   by decades. The reading's ε is low (0.12) because the constraint
 *   describes a natural epistemic barrier, not an enforced extraction.
 *
 * KEY AGENTS:
 *   - conceptual_innovators: Primary targets (powerless/moderate, identity_locked) — bear the cognitive cost of making the unthinkable thinkable
 *   - institutional_measurement_agencies: Secondary beneficiaries (institutional, analytical) — gain authority from eventually measuring what already diffused
 *   - financial_infrastructure_firms: Tertiary beneficiaries (powerful, organized) — capture value from the built systems, not the concept itself
 *   - analytical_observer: Observer (analytical, analytical) — sees the full diffusion structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.12).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.08).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Digital Money Emergence: Became Thinkable Reading").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic/monetary/technology").

domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'e1b707e7-06f6-470b-a74d-56a90e8f9d6c').
narrative_ontology:cs_kernel_codification('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', distributed).
narrative_ontology:cs_authority_grounding('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', practice).
narrative_ontology:cs_reading_relation('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', foundational, emergence_is_conceptual_diffusion_not_threshold).
narrative_ontology:cs_axiom_status(emergence_is_conceptual_diffusion_not_threshold, holdable).
narrative_ontology:cs_axiom_grounding('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', emergence_is_conceptual_diffusion_not_threshold, empirically_contingent).
narrative_ontology:cs_axiom('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', secondary, measurement_lags_conceptual_innovation_by_decades).
narrative_ontology:cs_axiom_status(measurement_lags_conceptual_innovation_by_decades, holdable).
narrative_ontology:cs_axiom_grounding('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', measurement_lags_conceptual_innovation_by_decades, empirically_contingent).
narrative_ontology:cs_reference_frame('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', distributed_conceptual_diffusion).
narrative_ontology:cs_drift_state('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', contemporary_statistical_catchup, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e1b707e7-06f6-470b-a74d-56a90e8f9d6c', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint is an epistemic barrier — the cognitive work of making a new monetary form thinkable — not an enforced transfer. Suppression is minimal (0.08) because no actor actively prevents the concept from spreading; the lag is structural (conceptual diffusion takes time). Theater ratio is near-zero (0.05) — no performative maintenance of a false narrative. Accessibility collapse is moderate (0.25): once the concept is thinkable, alternatives (thinking only in physical-cash terms) partially collapse but do not vanish; physical cash persists. Resistance is low (0.15): the constraint meets little active resistance because it is not imposed. The mountain claim (emerges_naturally: true) asserts this is a structural feature of how conceptual innovation works in monetary systems, not a human choice.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer sees a natural diffusion barrier (mountain). Institutional measurement agencies experience it as a lag they must eventually close — they are beneficiaries of the eventual measurement but not of the lag itself. Financial infrastructure firms benefit from the systems built after the concept diffuses, not from the constraint itself. Conceptual innovators bear the cognitive cost but do not experience it as extraction; it is the work of innovation.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared because the constraint is an epistemic barrier, not a transfer mechanism. The conceptual innovators (powerless/moderate, identity_locked) invest cognitive effort — this is not extraction but the cost of innovation. Institutional agencies (institutional, analytical) eventually measure the diffused concept — they benefit from the measurement but the lag is not extracted from them. Financial firms (powerful, organized) capture downstream value — the constraint does not transfer value to them.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — this constraint has no mandate. It describes a structural epistemic process, not an institutional arrangement with a founding problem that could outlive its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement,
    'Is this reading of electronic money emergence structurally distinct from the sibling readings (first_held_reading, m4_m5_collapse_reading), or does it describe the same constraint under a different label?',
    'Compare the structural relationship arrays (beneficiaries, victims, enforcement requirements) across the three constraint stories. If all three share the same ε and the same structural parties, they are one constraint; if the parties or ε differ, they are distinct constraints in a family.',
    'If the readings are structurally distinct, each carries its own ε and classification. If they are the same constraint viewed differently, the kernel_id masks a single constraint with observer-dependent classification — a violation of ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement, conceptual, 'Whether the three kernel readings instantiate one constraint or three distinct constraints').

omega_variable(
    naturalness_vs_construction_ambiguity,
    'Is the ''conceptual possibility becoming thinkable'' a natural law of innovation diffusion (mountain) or a constructed narrative that benefits certain institutional actors (tangled_rope/snare)?',
    'Historical analysis of whether the ''thinkability'' threshold correlates with identifiable beneficiary groups (e.g., central banks, statistical agencies, financial infrastructure firms) who gained authority or resources from the delayed measurement.',
    'If constructed, the mountain claim is a false summit and FSM should reclassify. If natural, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction_ambiguity, conceptual, 'Natural law vs. constructed narrative ambiguity for the emergence claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1970, 0.02).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1980, 0.03).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.045).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1980, 0.06).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.08).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2010, 0.11).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2020, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__became_thinkable_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.02).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint family (electronic_money_emergence kernel) decomposes the colloquial label 'when digital money emerged' into three structurally distinct claims with different ε values, different structural parties, and different classifications. This reading (became_thinkable) has low ε (~0.12), no enforcement, mountain classification. first_held_reading likely has higher ε (institutional threshold event with bearer/payer structure). m4_m5_collapse_reading likely has different ε (measurement artifact as constraint). Linked via affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
