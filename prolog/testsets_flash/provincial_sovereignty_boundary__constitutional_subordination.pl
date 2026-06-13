% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Constitutional Subordination of Provinces
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint describes the constitutional principle that provinces are
 *   creations of the federal constitution, possessing no inherent
 *   sovereignty, and that any provincial exit from the federation requires
 *   federal consent. This reading asserts federal supremacy in key areas like
 *   national unity, equalization, and climate policy, and views separatist
 *   movements as constitutionally invalid. It is one reading of the broader
 *   'provincial_sovereignty_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.3).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.6).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.3).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'eefd0b0c-7675-4448-ac8e-6501f2d9bc05').
narrative_ontology:cs_kernel_codification('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', fixed_text).
narrative_ontology:cs_authority_grounding('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', lineage).
narrative_ontology:cs_interpretation_layer_present('eefd0b0c-7675-4448-ac8e-6501f2d9bc05').
narrative_ontology:cs_reading_relation('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', foundational, federal_constitution_supreme).
narrative_ontology:cs_axiom_status(federal_constitution_supreme, holdable).
narrative_ontology:cs_axiom_grounding('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', federal_constitution_supreme, conventional).
narrative_ontology:cs_axiom('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', foundational, provinces_derive_power_from_federal_act).
narrative_ontology:cs_axiom_status(provinces_derive_power_from_federal_act, holdable).
narrative_ontology:cs_axiom_grounding('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', provinces_derive_power_from_federal_act, conventional).
narrative_ontology:cs_reference_frame('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', original_confederation_act_intent).
narrative_ontology:cs_drift_state('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eefd0b0c-7675-4448-ac8e-6501f2d9bc05', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments_seeking_autonomy).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).
:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it provides a genuine coordination function (national unity, consistent federal policy) but also involves asymmetric extraction from provinces seeking greater autonomy. Extractiveness is moderate (0.3) as provinces do retain significant powers, but suppression is higher (0.6) due to the federal government's ultimate constitutional authority and the Supreme Court's role in enforcing this interpretation. The theater ratio is low (0.1), indicating that the constitutional framework is actively and genuinely enforced, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this is a necessary Rope for national governance. From the perspective of autonomy-seeking provinces, it is a Snare that limits their self-determination. The engine's classification will reflect this divergence based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and national unity advocates are beneficiaries, as the constraint empowers federal authority and maintains national cohesion. Provincial governments seeking autonomy and separatist movements are victims, as their aspirations for greater sovereignty or independence are directly curtailed by this constitutional reading. The Supreme Court acts as an agenda-setter, interpreting and enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_stability,
    'To what extent is the ''constitutional subordination'' reading of provincial sovereignty stable, or is it subject to ongoing reinterpretation by the Supreme Court or political pressure?',
    'Analysis of future Supreme Court rulings on federal-provincial jurisdiction and the outcomes of major federal-provincial disputes over policy areas like climate or resources.',
    'If the reading proves unstable and shifts towards greater provincial autonomy, the constraint''s extractiveness and suppression would decrease, potentially reclassifying it towards a Rope or even a Scaffold (if temporary). If it hardens, it moves closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretation_stability, empirical, 'Stability of constitutional interpretation regarding provincial sovereignty.').

omega_variable(
    federal_consent_legitimacy,
    'Is federal consent for provincial exit a legitimate constitutional requirement, or is it a political assertion that could be challenged by a sufficiently strong provincial mandate?',
    'A hypothetical provincial referendum on secession followed by a federal refusal to negotiate, leading to a constitutional crisis and potential international legal challenges.',
    'If federal consent is deemed a political assertion rather than an absolute constitutional bar, the ''trapped'' exit option for separatist movements becomes ''constrained'', reducing the constraint''s effective suppression and extractiveness from that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_consent_legitimacy, conceptual, 'Legitimacy of federal consent as a barrier to provincial exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1867, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1867, 0.05).
narrative_ontology:measurement(prov_tr_t1920, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1867, 0.2).
narrative_ontology:measurement(prov_be_t1920, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1920, 0.25).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1867, 0.4).
narrative_ontology:measurement(prov_su_t1920, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.7).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'provincial_sovereignty_boundary' kernel. It emphasizes federal supremacy and constitutional subordination of provinces, contrasting with 'compact_federalism' and 'resource_sovereignty_primacy' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
