% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Definition of Employment Boundary (Platform Economy)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the formalist reading of the
 *   'employment_boundary' kernel, which defines employment primarily by
 *   explicit contractual terms and direct supervisory control. Under this
 *   reading, platform workers are classified as independent contractors,
 *   excluding them from traditional employment protections. The constraint is
 *   claimed as a 'rope' by its proponents, emphasizing its role in enabling
 *   flexible labor markets. However, the authored metrics reflect a high
 *   degree of extraction and suppression, indicating that its actual
 *   operation is substantially more extractive than its claimed function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.78).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.85).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Definition of Employment Boundary (Platform Economy)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'b8e91e18-80e3-41c8-9278-8143f5d689cf').
narrative_ontology:cs_kernel_codification('b8e91e18-80e3-41c8-9278-8143f5d689cf', formalized).
narrative_ontology:cs_authority_grounding('b8e91e18-80e3-41c8-9278-8143f5d689cf', extraction).
narrative_ontology:cs_interpretation_layer_present('b8e91e18-80e3-41c8-9278-8143f5d689cf').
narrative_ontology:cs_reading_relation('b8e91e18-80e3-41c8-9278-8143f5d689cf', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8e91e18-80e3-41c8-9278-8143f5d689cf', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('b8e91e18-80e3-41c8-9278-8143f5d689cf', foundational, freedom_of_contract_supremacy).
narrative_ontology:cs_axiom_status(freedom_of_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b8e91e18-80e3-41c8-9278-8143f5d689cf', freedom_of_contract_supremacy, conventional).
narrative_ontology:cs_axiom('b8e91e18-80e3-41c8-9278-8143f5d689cf', foundational, individual_autonomy_in_work_arrangements).
narrative_ontology:cs_axiom_status(individual_autonomy_in_work_arrangements, holdable).
narrative_ontology:cs_axiom_grounding('b8e91e18-80e3-41c8-9278-8143f5d689cf', individual_autonomy_in_work_arrangements, conventional).
narrative_ontology:cs_reference_frame('b8e91e18-80e3-41c8-9278-8143f5d689cf', traditional_contract_law_framework).
narrative_ontology:cs_drift_state('b8e91e18-80e3-41c8-9278-8143f5d689cf', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8e91e18-80e3-41c8-9278-8143f5d689cf', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, gig_economy_investors).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_employers).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, market_efficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the terms of engagement for platform workers, classifying them as independent contractors. They benefit from lower labor costs, reduced regulatory burden, and increased operational flexibility by externalizing employment-related costs and risks.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate under contracts that deny them employee benefits, minimum wage protections, and collective bargaining rights. They bear the costs of self-employment (e.g., health insurance, retirement savings, unemployment risk) and often face algorithmic management without formal recourse. Their exit options are limited by economic necessity and lack of comparable flexible alternatives.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, local).

% Profit from the business models enabled by the independent contractor classification, which allows for rapid scaling and high margins due to externalized labor costs. They exert influence through capital allocation and board representation.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, gig_economy_investors, beneficiary,
    powerful, biographical, mobile, global).

% Indirectly subsidize the platform economy by covering social costs (e.g., unemployment benefits, healthcare for uninsured workers) that would typically be borne by employers. They have no direct mechanism to opt out of this arrangement.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net, payer,
    powerless, generational, trapped, national).

% Operate under traditional employment laws, incurring higher labor costs for benefits, payroll taxes, and regulatory compliance. They face competitive pressure from platform companies that do not bear these costs, leading to calls for a 'level playing field'.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, payer,
    organized, biographical, constrained, national).

% Advocate for the reclassification of platform workers as employees to extend labor protections. They are largely excluded from the formal legal and contractual processes that define the employment boundary, operating through legislative and judicial challenges.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_unions_advocates, excluded,
    organized, generational, constrained, national).

% Interpret and defend the legal framework that prioritizes formal contractual terms and direct supervisory control in defining employment. They emphasize legal certainty and the importance of individual choice in contracting.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, legal_scholars_formalist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear legal framework for classifying work relationships, enabling flexible labor markets and fostering innovation in new business models by reducing regulatory overhead for platforms.
% TRANSFER_FUNCTION: Transfers the costs of social insurance, employment benefits, and regulatory compliance from platform companies to platform workers and the public social safety net, in exchange for perceived flexibility and market access.
% ABSENT_VOICES: Platform workers (as a collective), labor unions, and social policy advocates who argue that the formalist definition ignores economic realities and creates precarity. They are excluded from the primary contractual and legislative processes that uphold this definition.
% DISAPPEARANCE_RATIONALE: If the formalist definition of employment vanished overnight, platform companies would face immediate and widespread legal challenges, forcing a reclassification of workers and a fundamental restructuring of their business models, labor costs, and operational flexibility. The entire gig economy would reorganize.
% FOUNDING_PROBLEM: To provide legal clarity and flexibility for new forms of work that emerged with the digital economy, which did not fit neatly into traditional industrial employment models, thereby fostering innovation and entrepreneurship.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and formalist legal scholars attest that the problem of defining flexible work is still live, citing the need for continued innovation. Labor advocates, some economists, and substantive legal scholars attest that the founding problem is largely solved, and the arrangement now primarily serves to externalize costs; this is supported by empirical studies of worker conditions and the growth of platform market power.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) stems from the externalization of significant labor costs (benefits, taxes, insurance) from platform companies to workers and the state. Suppression (0.85) is high due to the legal and economic barriers preventing workers from challenging their classification or accessing alternative work with better protections. The low theater ratio (0.20) indicates that the formalist definition is genuinely applied in legal and contractual contexts, even if its stated justification (flexibility, innovation) increasingly serves as a cover for cost-shifting. Accessibility collapse is high (0.70) because legal precedent and economic structures severely limit alternatives for workers seeking traditional employment status within the platform economy.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies and investors perceive this constraint as a legitimate 'rope' that facilitates innovation and market efficiency. Platform workers, labor advocates, and traditional employers, however, experience it as a 'snare' that enables exploitation and unfair competition. The engine's computation will highlight this divergence between the claimed type and the actual operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and gig economy investors are clear beneficiaries, gaining from reduced labor costs and regulatory burdens. Platform workers, taxpayers (via the social safety net), and traditional employers are victims, bearing the costs of precarity, externalized social costs, and competitive disadvantage, respectively. The formalist legal scholars act as observers, interpreting and defending the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The formalist reading prevents mandatrophy resolution by continuously re-asserting the 'founding problem' of flexibility and innovation, even as the functional reality shifts towards systemic cost externalization and worker precarity. The legal framework's inertia, coupled with active enforcement by platform companies, maintains the constraint despite its original mandate being largely superseded by new economic realities. The 'contested' status of the founding problem highlights this ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_definition,
    'Is the formal contract and direct supervision the appropriate and sufficient determinant of employment status, or should economic dependence and algorithmic control be primary?',
    'Judicial rulings that prioritize substantive tests over formal contracts, or legislative action that redefines employment criteria for the platform economy.',
    'If substantive criteria gain legal precedence, platform workers would be reclassified as employees, leading to significant changes in labor protections and business models. This would shift the constraint''s classification towards a Snare for platforms and a Rope for workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_substantive_definition, conceptual, 'Ambiguity over the primary criteria for defining employment in the platform economy.').

omega_variable(
    flexibility_vs_precarity_outcome,
    'Does the independent contractor classification primarily deliver genuine flexibility and entrepreneurial opportunity for platform workers, or does it primarily create precarity and externalize costs?',
    'Longitudinal empirical studies tracking worker income stability, access to benefits, and self-reported autonomy versus economic pressure, especially comparing platform workers to traditional employees and genuinely independent contractors.',
    'If studies consistently show high precarity and low genuine autonomy, the ''flexibility'' justification for the formalist reading would be undermined, strengthening arguments for reclassification and increasing the perceived extractiveness of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_vs_precarity_outcome, empirical, 'Whether the primary outcome of the classification is worker flexibility or precarity.').

omega_variable(
    cost_externalization_legitimacy,
    'Is the externalization of social insurance and employment costs to workers and the state a legitimate outcome of independent contracting, or an illegitimate subsidy to platform companies?',
    'Public policy debates and legislative decisions regarding the social responsibility of platform companies, potentially leading to new tax structures or mandatory contributions to social safety nets.',
    'If deemed illegitimate, policy interventions would increase costs for platform companies, reducing their beneficiary status and potentially shifting the constraint''s effective extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_externalization_legitimacy, preference, 'Normative judgment on the legitimacy of cost externalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(empl_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(empl_tr_t9, employment_boundary__formalist_employment_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(empl_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(empl_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(empl_be_t9, employment_boundary__formalist_employment_reading, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(empl_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.77).
narrative_ontology:measurement(empl_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(empl_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.75).
narrative_ontology:measurement(empl_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(empl_su_t9, employment_boundary__formalist_employment_reading, suppression_requirement, 9, 0.82).
narrative_ontology:measurement(empl_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.84).
narrative_ontology:measurement(empl_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
