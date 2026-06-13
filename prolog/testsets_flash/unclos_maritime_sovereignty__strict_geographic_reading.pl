% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Reading of Island Status
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict geographic reading' of UNCLOS
 *   Article 121, which dictates that only naturally formed land features
 *   above water at high tide qualify as islands capable of generating a
 *   territorial sea and Exclusive Economic Zone (EEZ). Artificial
 *   constructions, regardless of their size or permanence, are considered
 *   installations and do not alter the legal status of maritime zones. This
 *   reading is crucial in international law for preventing states from
 *   unilaterally expanding their maritime claims through artificial means,
 *   particularly in contested areas.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.2).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.4).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, mountain).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Reading of Island Status").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'a0ce8300-3bc4-4ed6-984b-f2e2098a292a').
narrative_ontology:cs_kernel_codification('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', fixed_text).
narrative_ontology:cs_authority_grounding('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', lineage).
narrative_ontology:cs_interpretation_layer_present('a0ce8300-3bc4-4ed6-984b-f2e2098a292a').
narrative_ontology:cs_reading_relation('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', foundational, natural_formation_is_prerequisite_for_sovereignty).
narrative_ontology:cs_axiom_status(natural_formation_is_prerequisite_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', natural_formation_is_prerequisite_for_sovereignty, deontological).
narrative_ontology:cs_axiom('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', foundational, artificial_structures_are_installations_not_territory).
narrative_ontology:cs_axiom_status(artificial_structures_are_installations_not_territory, holdable).
narrative_ontology:cs_axiom_grounding('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', artificial_structures_are_installations_not_territory, conventional).
narrative_ontology:cs_reference_frame('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', unclos_original_intent_1982).
narrative_ontology:cs_drift_state('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', contemporary_artificial_island_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0ce8300-3bc4-4ed6-984b-f2e2098a292a', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, ExtMetricName, E),
    domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because it asserts a fundamental, 'natural' distinction between land features based on their formation and permanence, which is then codified into law. Its extractiveness is low (0.2) as it primarily prevents extraction by others rather than performing it directly. Suppression (0.4) reflects the diplomatic and legal pressure required to uphold this interpretation against states attempting to create artificial islands. Theater ratio is low (0.1) as the distinction is largely functional. Accessibility collapse is high (0.8) because, from this reading's perspective, there are no legitimate alternatives to natural formation for island status. Resistance (0.3) comes from states that challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Naval powers and non-claimant states experience this as a Mountain, as it preserves freedom of navigation and prevents unilateral enclosure of the high seas. Expansionist coastal states, however, experience it as a Snare, as it actively suppresses their ability to extend sovereignty through artificial means. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are beneficiaries (d near 0.0) as this reading maintains a stable, predictable maritime order and preserves access to international waters. Expansionist coastal states are victims (d near 1.0) as their efforts to create new maritime zones are directly thwarted. The UNCLOS framework itself, as an international legal regime, acts as an agenda-setter, codifying and enforcing this distinction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy by ensuring that the legal definition of an island remains tied to a stable, natural criterion, rather than allowing it to drift into a justification for artificial expansion. If the 'natural feature' criterion were to atrophy, the constraint would cease to be a Mountain and would likely become a Snare for the international community, as states would engage in a race to construct features to claim territory, leading to increased extraction from shared resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_island_status,
    'Is the ''natural feature'' criterion for island status a genuine natural law, or a constructed legal interpretation that benefits identifiable agents?',
    'Analysis of historical negotiations and state practice: if the criterion was primarily a negotiated compromise rather than an attempt to codify a pre-existing physical reality, it leans towards constructed.',
    'If constructed, the constraint''s classification shifts from Mountain to a form of Rope or Tangled Rope, reflecting its negotiated rather than inherent nature, and the beneficiaries'' position becomes more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_island_status, conceptual, 'Ambiguity between natural law and legal construct for island status.').

omega_variable(
    reading_impact_on_maritime_claims,
    'How would the adoption of the ''expansive_construction_reading'' or ''hybrid_effective_control_reading'' alter the global distribution of maritime claims and resource access?',
    'Geospatial modeling of potential new maritime zones under alternative readings, combined with economic analysis of affected resource access (e.g., fishing grounds, mineral deposits).',
    'Alternative readings would significantly expand the maritime claims of states engaged in artificial island construction, leading to increased extraction from shared ocean resources and potentially escalating geopolitical tensions. This reading prevents such expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_impact_on_maritime_claims, empirical, 'Impact of alternative island status readings on maritime claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 20, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_doctrine).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, exclusive_economic_zone_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNCLOS maritime sovereignty kernel, specifically concerning island status. Its strict interpretation directly influences the viability and legitimacy of claims made under alternative, more expansive readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
