% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Public Health Mandate Proportionality Principle
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality_reading' of the
 *   'mandate_legitimacy_scope' kernel. It asserts that the legitimacy of
 *   public health mandates (e.g., vaccine mandates) is not absolute but
 *   depends on a careful balancing of disease severity, vaccine safety and
 *   efficacy, and the availability of less restrictive alternatives. A
 *   mandate for a highly severe disease with a safe, effective vaccine and no
 *   viable alternatives (e.g., measles) would be considered legitimate, while
 *   a mandate for a mild disease with moderate vaccine efficacy and many
 *   alternatives (e.g., seasonal flu) would not. The constraint is a Tangled
 *   Rope because it aims to coordinate public health while extracting from
 *   individual autonomy, requiring active enforcement and balancing competing
 *   values.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda setter (institutional/arbitrage) — sets and enforces mandates, balancing public good with individual rights.
 *   - individuals_subject_to_mandate: Payer (moderate/constrained) — bears the cost of compliance or faces penalties, with limited exit options.
 *   - vulnerable_populations: Beneficiary (powerless/immediate) — benefits from reduced disease transmission, but often lacks direct agency.
 *   - medical_autonomy_advocates: Excluded (organized/generational) — argues for individual rights as primary, often outside the mandate-setting process.
 *   - constitutional_courts: Observer (institutional/analytical) — adjudicates challenges to mandates based on constitutional principles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.45).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.6).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Public Health Mandate Proportionality Principle").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '9a349207-89bd-4b0c-8469-71d57db532f1').
narrative_ontology:cs_kernel_codification('9a349207-89bd-4b0c-8469-71d57db532f1', formalized).
narrative_ontology:cs_authority_grounding('9a349207-89bd-4b0c-8469-71d57db532f1', lineage).
narrative_ontology:cs_interpretation_layer_present('9a349207-89bd-4b0c-8469-71d57db532f1').
narrative_ontology:cs_reading_relation('9a349207-89bd-4b0c-8469-71d57db532f1', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('9a349207-89bd-4b0c-8469-71d57db532f1', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('9a349207-89bd-4b0c-8469-71d57db532f1', foundational, mandates_must_be_least_restrictive).
narrative_ontology:cs_axiom_status(mandates_must_be_least_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('9a349207-89bd-4b0c-8469-71d57db532f1', mandates_must_be_least_restrictive, deontological).
narrative_ontology:cs_axiom('9a349207-89bd-4b0c-8469-71d57db532f1', foundational, collective_benefit_must_outweigh_individual_burden).
narrative_ontology:cs_axiom_status(collective_benefit_must_outweigh_individual_burden, holdable).
narrative_ontology:cs_axiom_grounding('9a349207-89bd-4b0c-8469-71d57db532f1', collective_benefit_must_outweigh_individual_burden, instrumental).
narrative_ontology:cs_reference_frame('9a349207-89bd-4b0c-8469-71d57db532f1', liberal_democratic_rights_framework).
narrative_ontology:cs_drift_state('9a349207-89bd-4b0c-8469-71d57db532f1', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9a349207-89bd-4b0c-8469-71d57db532f1', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a principled framework for public health authorities to implement mandates that balance collective health protection with individual rights, ensuring legitimacy and public trust.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy (e.g., choice over medical procedures, freedom of movement) from individuals to the state, in exchange for collective health benefits and reduced disease burden.
% ABSENT_VOICES: Individuals who prioritize absolute bodily autonomy or who distrust public health institutions are often marginalized in the mandate-setting process, arguing that no collective benefit can justify coerced medical intervention.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health mandates would either become arbitrary (leading to overreach and public backlash) or impossible to implement (leading to uncontrolled disease spread). The legal and ethical landscape for public health interventions would be fundamentally altered, requiring new frameworks for justification.
% FOUNDING_PROBLEM: The historical tension between state power to protect public health and individual rights, particularly in the context of infectious disease outbreaks where individual actions have collective consequences.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and ethicists from diverse backgrounds corroborate the ongoing nature of this tension. Public health crises (e.g., pandemics) consistently reignite debates about the appropriate scope of state intervention, demonstrating that the problem is far from resolved. Legal challenges to mandates in various jurisdictions also attest to its live status.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because the proportionality principle attempts to limit the burden on individuals, but some extraction of autonomy is inherent in any mandate. Suppression (0.6) is present due to the coercive nature of mandates and the penalties for non-compliance. Theater ratio (0.1) is low, as the constraint's function is genuinely to guide policy, not merely to perform. Accessibility collapse (0.4) is moderate, as individuals still have some choices (e.g., compliance vs. penalty) but direct alternatives to the mandate itself are limited. Resistance (0.5) is moderate, reflecting ongoing legal and public challenges to mandates.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this constraint as a necessary framework for protecting collective well-being, while individuals subject to mandates often perceive it as an infringement on personal liberty. The proportionality principle attempts to bridge this gap by providing criteria for legitimate imposition, but the interpretation and application of these criteria remain contentious, leading to different experiences of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are beneficiaries (d=0.1) as the constraint legitimizes their actions and helps achieve public health goals. Vulnerable populations are also beneficiaries (d=0.2) as they are protected. Individuals subject to mandates are targets (d=0.8) as they bear the direct costs of compliance or penalties. Medical autonomy advocates are excluded (d=0.9) as their primary concern is often not fully integrated into the balancing act. Constitutional courts are analytical observers (d=0.5) as they evaluate the constraint's application impartially.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, by its very nature, is designed to prevent mandatrophy by requiring ongoing justification for mandates. If a mandate persists when disease severity decreases, vaccine efficacy wanes, or less restrictive alternatives become available, the proportionality principle would deem it illegitimate, forcing re-evaluation or removal. This prevents the constraint from becoming a Piton (inertial) or a Snare (pure extraction) by demanding a live, evidence-based justification for its continued existence. The challenge lies in the 'contested' status of the founding problem, where different parties disagree on whether the conditions for a mandate are still met.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine application of proportionality, or is it a cover for a public health primary approach?',
    'Analysis of mandate implementation: if mandates are applied broadly without granular assessment of severity/alternatives, it leans towards public_health_primary. If mandates are selectively applied and frequently adjusted based on new data, it supports proportionality_reading.',
    'If it''s a public_health_primary reading in disguise, the effective extractiveness and suppression are higher, as individual autonomy is systematically undervalued. If it''s a genuine proportionality reading, the constraint is a more legitimate form of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''proportionality_reading'' of the ''mandate_legitimacy_scope'' kernel. Sibling readings include ''public_health_primary'' and ''bodily_autonomy_primary''. The core disagreement is on the primary weighting of collective vs. individual rights.').

omega_variable(
    disease_severity_threshold,
    'What objective criteria define ''severe'' disease, and how are these applied consistently across different pathogens?',
    'Establishment of a transparent, evidence-based framework for disease severity assessment, including hospitalization rates, mortality, and long-term sequelae, applied by an independent body.',
    'Lack of clear criteria allows for arbitrary application of mandates, increasing perceived extractiveness and suppression. Clear criteria would reduce ambiguity and enhance legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disease_severity_threshold, empirical, 'Ambiguity in defining disease severity can shift the perceived legitimacy of mandates.').

omega_variable(
    less_restrictive_alternatives_scope,
    'What constitutes a ''less restrictive alternative'' (e.g., masking, testing, remote work), and how are their efficacy and feasibility evaluated against mandates?',
    'Systematic review and comparative effectiveness research on non-mandate interventions, alongside public health modeling of their impact on transmission and outcomes.',
    'If effective, less restrictive alternatives are systematically ignored, the mandate''s suppression is higher than justified. If genuinely ineffective, the mandate''s legitimacy is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(less_restrictive_alternatives_scope, empirical, 'The scope and effectiveness of alternatives are often contested, impacting mandate legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'mandate_legitimacy_scope' kernel, focusing on proportionality. It is linked to 'public_health_primary' and 'bodily_autonomy_primary' readings, which represent alternative framings of mandate legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
