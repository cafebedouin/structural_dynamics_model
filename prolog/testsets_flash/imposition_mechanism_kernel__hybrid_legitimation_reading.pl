% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation of New Norms (Imperial Era)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process by which new norms, often
 *   originating from an imperial center, achieved legitimacy and widespread
 *   adoption in a historical context. It posits a 'hybrid' mechanism, where
 *   the emperor's symbolic endorsement (authority transfer) combined with
 *   tangible institutional incentives (e.g., access to resources, social
 *   status) to drive adoption. This contrasts with purely bottom-up
 *   (endogenous climb) or purely top-down (exogenous override) models of norm
 *   imposition. The constraint is a 'hybrid_legitimation_reading' of the
 *   'imposition_mechanism_kernel'.
 *
 * KEY AGENTS:
 *   - imperial_court: Agenda-setter (institutional/arbitrage) — initiates and symbolically endorses new norms.
 *   - state_bureaucracy: Agenda-setter/Beneficiary (institutional/constrained) — implements incentives and enforcement, benefits from expanded authority.
 *   - social_elites: Beneficiary/Payer (powerful/constrained) — adopt norms for status/privilege, bear some initial adoption costs.
 *   - local_traditions: Victim (powerless/identity_locked) — displaced or suppressed by new norms.
 *   - peasantry: Payer (powerless/trapped) — adopts norms due to incentives or elite pressure, bears costs of compliance.
 *   - historical_sociologists: Observer (analytical/analytical) — analyze the mechanisms of norm imposition and legitimation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.4).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.5).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation of New Norms (Imperial Era)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '408ad838-354a-4ab5-8801-d7e4289317c3').
narrative_ontology:cs_kernel_codification('408ad838-354a-4ab5-8801-d7e4289317c3', formalized).
narrative_ontology:cs_authority_grounding('408ad838-354a-4ab5-8801-d7e4289317c3', lineage).
narrative_ontology:cs_interpretation_layer_present('408ad838-354a-4ab5-8801-d7e4289317c3').
narrative_ontology:cs_reading_relation('408ad838-354a-4ab5-8801-d7e4289317c3', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('408ad838-354a-4ab5-8801-d7e4289317c3', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('408ad838-354a-4ab5-8801-d7e4289317c3', foundational, legitimacy_is_hybrid_derived).
narrative_ontology:cs_axiom_status(legitimacy_is_hybrid_derived, holdable).
narrative_ontology:cs_axiom_grounding('408ad838-354a-4ab5-8801-d7e4289317c3', legitimacy_is_hybrid_derived, conventional).
narrative_ontology:cs_axiom('408ad838-354a-4ab5-8801-d7e4289317c3', secondary, imperial_charisma_is_normative_force).
narrative_ontology:cs_axiom_status(imperial_charisma_is_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('408ad838-354a-4ab5-8801-d7e4289317c3', imperial_charisma_is_normative_force, conventional).
narrative_ontology:cs_reference_frame('408ad838-354a-4ab5-8801-d7e4289317c3', imperial_normative_synthesis).
narrative_ontology:cs_drift_state('408ad838-354a-4ab5-8801-d7e4289317c3', post_imperial_collapse, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('408ad838-354a-4ab5-8801-d7e4289317c3', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, social_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_traditions).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peasantry).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the blend of voluntary adoption (due to symbolic appeal and incentives) and coercive pressure. Suppression (0.5) is also moderate, as alternatives (local traditions) are not entirely crushed but are actively disincentivized. Theater ratio (0.2) is low, indicating that the mechanisms of legitimation and enforcement are largely functional, not merely performative. The claimed type is Tangled Rope because it genuinely coordinates (aligning diverse populations with new imperial norms) but does so with asymmetric extraction (benefiting the imperial center and elites while displacing local practices and imposing costs on the populace).
 *
 * PERSPECTIVAL GAP:
 *   The imperial court and state bureaucracy would perceive this as a successful coordination mechanism, bringing order and unity. Social elites would see it as a pathway to status and privilege. Local traditions and the peasantry, however, would experience it as a form of cultural imposition and extraction, requiring them to abandon established practices for new, externally-derived ones. The stratified adoption (elites first, masses later) highlights this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court and state bureaucracy are clear beneficiaries (d near 0.0) as the norms extend their authority and control. Social elites are also beneficiaries (d near 0.2-0.3) due to the incentives and status gains, though they bear some adoption costs. Local traditions and the peasantry are victims (d near 0.7-0.9) as their existing norms are devalued or suppressed, and they face direct costs of compliance or loss of autonomy. The 'emperor's example' provides a symbolic benefit that dampens pure coercion for some, but the institutional incentives ensure compliance where charisma alone might fail.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the coercive and extractive elements) or a pure Snare (ignoring the genuine coordination function and symbolic appeal). The hybrid nature means the mandate is not fully atrophied, but its justification shifts over time from initial 'unification' to ongoing 'maintenance of order' which can mask accumulating extraction. The moderate enforcement costs and stratified adoption are key indicators of this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the primary source of legitimacy for these new norms symbolic authority transfer, institutional incentives, or a combination, and in what proportion?',
    'Detailed historical analysis of adoption patterns, elite vs. mass compliance rates, and the specific mechanisms of incentive distribution versus charismatic appeal.',
    'If primarily symbolic, the constraint is more ''rope-like'' (voluntary coordination); if primarily institutional, more ''snare-like'' (coercive extraction). This reading asserts a hybrid, which affects the balance of coordination vs. extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Ambiguity in the dominant mechanism of norm legitimation.').

omega_variable(
    hybrid_vs_pure_imposition_framing,
    'Is this constraint best understood as a hybrid legitimation process, or is it fundamentally an endogenous climb or exogenous override with secondary elements of the other?',
    'Comparative historical analysis across different imperial contexts and norm types, seeking cases that more cleanly fit the ''pure'' models to highlight the distinctiveness of the hybrid.',
    'If reclassified as a pure endogenous climb, the constraint would be closer to a Rope; if a pure exogenous override, closer to a Snare. The hybrid reading maintains a Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_vs_pure_imposition_framing, conceptual, 'This constraint is a ''hybrid_legitimation_reading'' of the ''imposition_mechanism_kernel''. Sibling readings include ''endogenous_climb_reading'' (bottom-up adoption) and ''exogenous_override_reading'' (state coercion). This reading differs by asserting a combined mechanism of symbolic authority and institutional incentives, rather than a single dominant force. A shift to a pure reading would alter the balance of coordination and extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'imposition_mechanism_kernel', exploring different mechanisms of norm legitimation. The 'endogenous_climb_reading' focuses on bottom-up adoption, and the 'exogenous_override_reading' on state coercion. This 'hybrid_legitimation_reading' integrates symbolic authority transfer and institutional incentives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
