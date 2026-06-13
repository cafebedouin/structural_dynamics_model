% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope (Narrow Originalist Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'narrow originalist' reading of the U.S.
 *   Constitution's Commerce Clause, which holds that 'commerce among the
 *   several states' refers strictly to trade crossing state lines, and
 *   'regulate' means to make regular or facilitate, not to restrict or
 *   prohibit. Federal power is thus limited to removing state-imposed
 *   barriers to interstate trade and ensuring uniform commercial rules,
 *   leaving broad regulatory authority over intrastate economic activity to
 *   the states. This reading is one of several competing interpretations of
 *   the Commerce Clause, each generating a distinct constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.2).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.3).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.2).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope (Narrow Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6').
narrative_ontology:cs_kernel_codification('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', fixed_text).
narrative_ontology:cs_authority_grounding('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', lineage).
narrative_ontology:cs_interpretation_layer_present('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6').
narrative_ontology:cs_reading_relation('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', foundational, commerce_is_trade_crossing_state_lines).
narrative_ontology:cs_axiom_status(commerce_is_trade_crossing_state_lines, holdable).
narrative_ontology:cs_axiom_grounding('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', commerce_is_trade_crossing_state_lines, conventional).
narrative_ontology:cs_axiom('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', foundational, regulate_means_facilitate_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', regulate_means_facilitate_not_restrict, conventional).
narrative_ontology:cs_reference_frame('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', original_constitutional_compact).
narrative_ontology:cs_drift_state('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('23f857b7-3e44-43c6-8fd6-3cbfecb1d7d6', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.2) is low because this reading primarily aims to prevent state-level extraction (trade barriers) rather than enabling federal extraction. Suppression (0.3) is also low, as it mainly involves judicial invalidation of state laws that overstep the narrow federal boundary, rather than active federal coercion over broad economic activity. Theater ratio (0.1) is minimal; the judicial function is genuine. Accessibility collapse (0.7) is moderate, as it limits federal alternatives but preserves state-level ones. Resistance (0.4) is moderate, reflecting ongoing legal and political challenges from those advocating for broader federal power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this reading is a protective rope, preserving their sovereignty. From the perspective of federal legislators seeking to address national problems, it is a constraining snare, limiting their ability to act. The federal judiciary, as the arbiter, views it as a foundational mountain of constitutional principle.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are beneficiaries (d near 0.0) as they gain regulatory autonomy. The federal judiciary acts as the agenda-setter, defining and enforcing the limits. National regulatory uniformity and civil rights enforcement are victims (d near 1.0) as their scope is curtailed. The federal legislature is a payer (d near 1.0) as its power to address national problems is constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ambiguity,
    'Is the ''original public meaning'' of ''commerce'' and ''regulate'' truly as narrow as this reading asserts, or did the framers intend a more dynamic or expansive understanding?',
    'Further historical and linguistic analysis of 18th-century texts, including non-legal sources, to establish the semantic range of the terms at the time of ratification.',
    'If a broader original meaning is established, the conceptual grounding of this reading weakens, potentially shifting its classification towards a ''tangled_rope'' or ''snare'' if it is seen as an imposed interpretation rather than a discovered truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ambiguity, conceptual, 'Ambiguity in the original meaning of constitutional terms.').

omega_variable(
    economic_interconnectedness_drift,
    'Does the 18th-century understanding of ''commerce'' remain functionally relevant in a 21st-century economy characterized by deep national and global interconnectedness?',
    'Empirical studies demonstrating the extent to which seemingly local economic activities have direct and substantial effects on interstate and international commerce, even under a narrow definition.',
    'If economic interconnectedness renders the narrow definition functionally obsolete, the constraint''s persistence would rely more heavily on judicial enforcement against economic realities, increasing its ''suppression'' and ''extractiveness'' metrics and potentially reclassifying it as a ''snare'' from the perspective of national economic actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_interconnectedness_drift, empirical, 'Functional relevance of historical definitions in a modern economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_scope__narrow_originalist, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(comm_tr_t1850, commerce_clause_scope__narrow_originalist, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(comm_tr_t1900, commerce_clause_scope__narrow_originalist, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_scope__narrow_originalist, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__narrow_originalist, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__narrow_originalist, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_scope__narrow_originalist, base_extractiveness, 1789, 0.1).
narrative_ontology:measurement(comm_be_t1850, commerce_clause_scope__narrow_originalist, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(comm_be_t1900, commerce_clause_scope__narrow_originalist, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(comm_be_t1950, commerce_clause_scope__narrow_originalist, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__narrow_originalist, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__narrow_originalist, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_scope__narrow_originalist, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(comm_su_t1850, commerce_clause_scope__narrow_originalist, suppression_requirement, 1850, 0.25).
narrative_ontology:measurement(comm_su_t1900, commerce_clause_scope__narrow_originalist, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(comm_su_t1950, commerce_clause_scope__narrow_originalist, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__narrow_originalist, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__narrow_originalist, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_labor_standards).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_civil_rights_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause scope kernel. Each reading generates a different constraint with unique structural properties and classifications, linked by their shared constitutional origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
