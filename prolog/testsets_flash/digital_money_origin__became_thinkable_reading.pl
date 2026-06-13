% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin: Concept Became Thinkable
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint defines the origin of digital money as the period when
 *   its concept became technically and institutionally conceivable, preceding
 *   widespread implementation. This reading emphasizes the intellectual and
 *   infrastructural preconditions, including the development of cryptography,
 *   distributed networks, and the theoretical understanding of digital
 *   scarcity. It highlights the role of early institutional architects and
 *   technological visionaries in shaping the conceptual landscape, while
 *   acknowledging that this early framing could implicitly exclude
 *   alternative conceptualizations.
 *
 * KEY AGENTS:
 *   - early_institutional_architects: Primary beneficiary (institutional/arbitrage) — shaped the conceptual framework.
 *   - technological_visionaries: Primary beneficiary (powerful/mobile) — developed underlying technologies.
 *   - excluded_from_conceptual_framing: Primary victim (powerless/trapped) — those whose alternative ideas were not considered 'thinkable'.
 *   - monetary_authorities: Secondary actor (institutional/analytical) — later recognized or regulated digital money.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.2).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.3).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin: Concept Became Thinkable").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '576c6024-d484-45d8-b29e-b905afe57cdf').
narrative_ontology:cs_kernel_codification('576c6024-d484-45d8-b29e-b905afe57cdf', implicit).
narrative_ontology:cs_authority_grounding('576c6024-d484-45d8-b29e-b905afe57cdf', expertise).
narrative_ontology:cs_interpretation_layer_present('576c6024-d484-45d8-b29e-b905afe57cdf').
narrative_ontology:cs_reading_relation('576c6024-d484-45d8-b29e-b905afe57cdf', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('576c6024-d484-45d8-b29e-b905afe57cdf', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('576c6024-d484-45d8-b29e-b905afe57cdf', foundational, conceptual_precedence_over_implementation).
narrative_ontology:cs_axiom_status(conceptual_precedence_over_implementation, holdable).
narrative_ontology:cs_axiom_grounding('576c6024-d484-45d8-b29e-b905afe57cdf', conceptual_precedence_over_implementation, conventional).
narrative_ontology:cs_axiom('576c6024-d484-45d8-b29e-b905afe57cdf', foundational, technical_feasibility_as_origin_marker).
narrative_ontology:cs_axiom_status(technical_feasibility_as_origin_marker, holdable).
narrative_ontology:cs_axiom_grounding('576c6024-d484-45d8-b29e-b905afe57cdf', technical_feasibility_as_origin_marker, empirically_contingent).
narrative_ontology:cs_reference_frame('576c6024-d484-45d8-b29e-b905afe57cdf', conceptual_innovation_paradigm).
narrative_ontology:cs_drift_state('576c6024-d484-45d8-b29e-b905afe57cdf', contemporary_implementation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('576c6024-d484-45d8-b29e-b905afe57cdf', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, technological_visionaries).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_from_conceptual_framing).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).
:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it primarily describes a coordination of ideas and technological advancements that enabled a new possibility. Extraction is low (0.2) as the 'cost' is primarily the intellectual effort and the implicit exclusion of other conceptual paths, rather than active financial transfer. Suppression is moderate (0.3) reflecting the inherent difficulty and institutional inertia in adopting new paradigms, rather than active coercion. Theater ratio is low (0.1) as the conceptual work was genuinely foundational. The metrics reflect the early, pre-implementation phase where the constraint was more about possibility-space definition than rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   Early institutional architects and technological visionaries would experience this as a period of innovation and opportunity (beneficiary seat). Those with alternative conceptualizations, or those who were simply not part of the dominant intellectual circles, would experience it as a subtle form of exclusion (victim seat). The engine's per-seat classification would reflect this divergence, with beneficiaries seeing a pure Rope and victims experiencing a more constrained, potentially extractive, environment.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects and technological visionaries are beneficiaries (d=0.0-0.2) as they defined the conceptual space and gained influence/reputation. Those excluded from the conceptual framing are victims (d=0.8-1.0) as their ideas were not considered viable within the emerging paradigm. Monetary authorities are observers/later actors, not directly impacted by this specific 'conceivability' constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling early conceptual coordination as later-stage extraction. By focusing on the 'thinkable' phase, it highlights the genuine coordination of intellectual and technical resources required to make digital money a coherent concept, before the more extractive dynamics of implementation and regulation emerged. It acknowledges that even conceptual framing can have exclusionary effects, but these are distinct from the active rent-seeking of later phases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_date_ambiguity,
    'Is the origin of digital money best defined by its conceptual conceivability, its first practical use, or its regulatory recognition?',
    'Historical consensus among monetary historians and technologists, or a formal definition by an international body.',
    'If ''first_held_reading'' or ''regulatory_recognition_reading'' is adopted, the constraint''s origin date shifts later, and the set of beneficiaries/victims changes to reflect those involved in implementation or regulation, potentially altering its classification from Rope to Tangled Rope or Snare due to later-stage extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_date_ambiguity, conceptual, 'Ambiguity in defining the origin point of digital money.').

omega_variable(
    conceptual_barrier_extraction,
    'To what extent did the conceptual and institutional barriers preceding implementation (as per this reading) actively extract from or suppress alternative monetary innovations?',
    'Detailed historical analysis of suppressed alternative digital currency proposals and their proponents during the ''thinkable'' phase.',
    'If significant active suppression or extraction is found, the ''became_thinkable_reading'' might shift from Rope towards Tangled Rope, indicating that even conceptual framing can be extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_barrier_extraction, empirical, 'Assessing extraction from conceptual barriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1970, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__became_thinkable_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__became_thinkable_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__became_thinkable_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__became_thinkable_reading, base_extractiveness, 20, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__became_thinkable_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__became_thinkable_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel, focusing on the conceptual and institutional conceivability. It structurally influences the 'first_held_reading' and 'regulatory_recognition_reading' by setting the initial conceptual boundaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
