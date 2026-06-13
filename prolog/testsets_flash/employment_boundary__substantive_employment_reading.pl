% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Definition for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint defines employment by economic dependence and algorithmic
 *   control, asserting that platform workers are employees regardless of
 *   their contractual form. It is one reading of the 'employment_boundary'
 *   kernel, which is contested by formalist and hybrid definitions. This
 *   reading aims to extend traditional labor protections to the gig economy,
 *   shifting significant costs and obligations onto platform companies. The
 *   metrics reflect the ongoing struggle to enforce this reclassification
 *   against strong resistance from platforms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.65).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.7).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Definition for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '3673bc5e-8225-4d11-a971-39efe764f6f6').
narrative_ontology:cs_kernel_codification('3673bc5e-8225-4d11-a971-39efe764f6f6', distributed).
narrative_ontology:cs_authority_grounding('3673bc5e-8225-4d11-a971-39efe764f6f6', distributed).
narrative_ontology:cs_reading_relation('3673bc5e-8225-4d11-a971-39efe764f6f6', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('3673bc5e-8225-4d11-a971-39efe764f6f6', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('3673bc5e-8225-4d11-a971-39efe764f6f6', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('3673bc5e-8225-4d11-a971-39efe764f6f6', economic_dependence_defines_employment, empirically_contingent).
narrative_ontology:cs_axiom('3673bc5e-8225-4d11-a971-39efe764f6f6', foundational, algorithmic_control_is_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_is_supervision, holdable).
narrative_ontology:cs_axiom_grounding('3673bc5e-8225-4d11-a971-39efe764f6f6', algorithmic_control_is_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('3673bc5e-8225-4d11-a971-39efe764f6f6', economic_dependence_control_as_employment).
narrative_ontology:cs_drift_state('3673bc5e-8225-4d11-a971-39efe764f6f6', contemporary_legal_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3673bc5e-8225-4d11-a971-39efe764f6f6', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_safety_net_funds).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_companies).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, gig_economy_business_model).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates social protections for workers (beneficiaries) while simultaneously extracting significant costs from platform companies (victims) through active enforcement. Extractiveness is high (0.65) due to the substantial financial burden placed on platforms. Suppression (0.70) is also high, reflecting the legal and regulatory battles required to overcome platform resistance and formalist interpretations. Theater ratio is low (0.10) as the reclassification efforts are direct and functional, not performative. Accessibility collapse is moderate (0.40) because while this reading aims to close off the 'independent contractor' alternative for platforms, they still have options like lobbying for new hybrid categories or exiting certain markets. Resistance is high (0.75) due to the intense opposition from platform companies and their allies.
 *
 * PERSPECTIVAL GAP:
 *   Platform workers and labor unions experience this as a beneficial coordination mechanism, providing essential protections. Platform companies, however, experience it as a highly extractive and suppressive force that fundamentally alters their business model. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers and social safety net funds are beneficiaries (low d) as they gain protections and contributions. Platform companies and the gig economy business model are targets (high d) as they bear the costs of reclassification. Labor unions act as agenda-setters, pushing for the constraint's enforcement. Formalist legal scholars are excluded, as their definitional framework is rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively being fought for and enforced, so mandatrophy is not a concern. The founding problem (platform worker precarity) is very much 'live,' and the constraint is a direct response to it. The classification as Tangled Rope accurately captures its dual function of coordination (for workers) and extraction (from platforms) through active enforcement, preventing mislabeling as a pure Snare or a simple Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formalist_definition,
    'Is employment fundamentally defined by economic reality (dependence, control) or by legal form (contract, supervision)?',
    'Judicial precedent and legislative action consistently prioritizing one set of criteria over the other in employment classification cases.',
    'If the formalist definition prevails, this constraint would be reclassified as a Snare (pure extraction from platforms without genuine coordination for workers) or a Piton (if the enforcement efforts become purely theatrical). If the substantive definition is universally adopted, it would solidify as a Rope (genuine coordination with accepted costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_formalist_definition, conceptual, 'The core conceptual ambiguity in defining employment for platform workers.').

omega_variable(
    platform_business_model_viability,
    'Can platform companies maintain economic viability and innovation if all their workers are classified as employees under this substantive reading?',
    'Empirical studies of platform companies'' financial performance and market adjustments in jurisdictions where this classification has been implemented.',
    'If platforms prove unable to adapt, the constraint''s extractiveness might be deemed unsustainable, leading to policy adjustments (e.g., a shift towards a hybrid model) or market contraction. If they adapt successfully, it strengthens the case for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_business_model_viability, empirical, 'The economic impact of reclassification on platform companies.').

omega_variable(
    worker_preference_for_flexibility,
    'To what extent do platform workers genuinely prefer the flexibility of independent contractor status over the security of employment, even with the associated precarity?',
    'Large-scale, independent surveys of platform workers that offer clear trade-offs between flexibility, benefits, and security, controlling for selection bias.',
    'If a significant portion of workers genuinely prioritize flexibility over employment benefits, it could weaken the coordination justification for this constraint, potentially pushing it towards a more extractive classification from the worker''s perspective (as it imposes unwanted obligations).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(worker_preference_for_flexibility, empirical, 'The role of worker preference for flexibility in employment status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__substantive_employment_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(empl_tr_t2018, employment_boundary__substantive_employment_reading, theater_ratio, 2018, 0.07).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__substantive_employment_reading, theater_ratio, 2021, 0.09).
narrative_ontology:measurement(empl_tr_t2023, employment_boundary__substantive_employment_reading, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(empl_tr_t2025, employment_boundary__substantive_employment_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(empl_be_t2015, employment_boundary__substantive_employment_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__substantive_employment_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__substantive_employment_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement(empl_be_t2023, employment_boundary__substantive_employment_reading, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement(empl_be_t2025, employment_boundary__substantive_employment_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2015, employment_boundary__substantive_employment_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__substantive_employment_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__substantive_employment_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(empl_su_t2023, employment_boundary__substantive_employment_reading, suppression_requirement, 2023, 0.68).
narrative_ontology:measurement(empl_su_t2025, employment_boundary__substantive_employment_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
