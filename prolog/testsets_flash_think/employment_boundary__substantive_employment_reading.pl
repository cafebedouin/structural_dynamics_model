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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Definition for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'substantive employment reading'
 *   of the 'employment_boundary' kernel. It defines employment based on
 *   economic dependence and algorithmic control, asserting that platform
 *   workers are employees regardless of their contractual form. This reading
 *   aims to reclassify platform workers to grant them full employment rights
 *   and protections, imposing new obligations on platform companies. The
 *   constraint is claimed as a Tangled Rope because it seeks to coordinate
 *   labor relations while simultaneously extracting costs from platform
 *   companies to benefit workers and social welfare systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.65).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.75).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Definition for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '00c1c5b0-c6c1-4296-8caf-b55e2c99feaa').
narrative_ontology:cs_kernel_codification('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', formalized).
narrative_ontology:cs_authority_grounding('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', lineage).
narrative_ontology:cs_interpretation_layer_present('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa').
narrative_ontology:cs_reading_relation('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', foundational, economic_dependence_as_employment_marker).
narrative_ontology:cs_axiom_status(economic_dependence_as_employment_marker, holdable).
narrative_ontology:cs_axiom_grounding('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', economic_dependence_as_employment_marker, empirically_contingent).
narrative_ontology:cs_axiom('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', foundational, algorithmic_control_as_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_as_supervision, holdable).
narrative_ontology:cs_axiom_grounding('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', algorithmic_control_as_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', traditional_employment_protections).
narrative_ontology:cs_drift_state('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', contemporary_platform_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('00c1c5b0-c6c1-4296-8caf-b55e2c99feaa', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_welfare_systems).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_companies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, consumers).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, labor_rights_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, social_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently experience precarity due to lack of employment status. This reading would grant them legal protections, social insurance, and collective bargaining rights, improving their economic security. Their exit options from platform work are constrained by economic necessity and limited alternative employment.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    moderate, biographical, constrained, global).

% Currently benefit from classifying workers as independent contractors, avoiding costs associated with employment. This reading would impose significant new costs (social insurance, benefits, minimum wage, collective bargaining) and reduce operational flexibility. Their exit options are constrained by market presence and regulatory compliance.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_companies, payer,
    institutional, generational, constrained, global).

% Actively champion this reading to expand worker protections and union membership. They provide legal and political pressure for reclassification and would benefit from increased influence and resources if this reading is adopted.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_unions_advocates, agenda_setter,
    organized, generational, analytical, national).

% Are responsible for defining and enforcing labor laws. This reading provides a framework for them to extend existing employment protections to platform workers, potentially increasing tax revenue and reducing social welfare burdens, but also facing political and economic resistance from platforms.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, governments_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Currently bear the costs of platform worker precarity (e.g., unemployment benefits, healthcare gaps). This reading would integrate platform workers into formal social insurance schemes, increasing contributions and reducing strain on public services.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_welfare_systems, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(employment_boundary__substantive_employment_reading, social_welfare_systems).

% May face higher prices for platform services if companies pass on increased labor costs. Their choices are to absorb these costs or seek alternative services, but their direct influence on the constraint is limited.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers, payer,
    moderate, immediate, mobile, local).

% Adhere to a strict interpretation of employment based on formal contracts and direct supervision. They would argue against this substantive reading, but their perspective is excluded from the framework of this particular constraint.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, formalist_legal_scholars, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, social_welfare_systems).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear and consistent legal status for platform workers, coordinating labor relations, social protections, and corporate obligations within the platform economy.
% TRANSFER_FUNCTION: Transfers the costs of social insurance, benefits, and job security from individual platform workers and public welfare systems to platform companies, while also transferring regulatory oversight and enforcement responsibilities to governments.
% ABSENT_VOICES: Formalist legal scholars and platform lobbyists who advocate for a strict contractual definition of independent contracting, or those proposing a distinct 'third category' of worker. Their arguments are directly challenged or bypassed by this reading's framework.
% DISAPPEARANCE_RATIONALE: If this substantive definition of employment were universally adopted and then vanished overnight, millions of platform workers would revert to ambiguous or independent contractor status, leading to a massive shift in social welfare burdens, corporate liabilities, and labor rights, fundamentally reorganizing the platform economy.
% FOUNDING_PROBLEM: The emergence of the platform economy created a large and growing class of workers who, despite economic dependence and algorithmic control, lacked the legal status and protections of traditional employees, leading to widespread precarity and social dumping.
% FOUNDING_PROBLEM_CORROBORATION: Labor organizations, social policy researchers, and international labor bodies consistently attest to the ongoing problem of platform worker precarity and the inadequacy of existing legal frameworks. Platform companies and some economists contest this, emphasizing worker flexibility and entrepreneurial freedom.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is moderate because while platforms would bear significant new costs, the reclassification also formalizes a labor relationship, which has some coordination benefits. Suppression (0.75) is high due to the active resistance from platform companies against reclassification efforts, often involving lobbying, legal challenges, and the promotion of alternative 'third category' solutions. Theater ratio (0.40) reflects the ongoing efforts by platforms to maintain the fiction of independent contracting through various contractual and operational adjustments, even as the substantive reality of worker dependence becomes more apparent. The metrics show a gradual increase over time, reflecting the intensifying debate and pressure for reclassification.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies perceive this reading as a Snare, imposing unwarranted costs and stifling innovation, while platform workers and labor advocates see it as a Rope or Scaffold, providing essential protections and correcting a market imbalance. Governments and regulators are caught between these perspectives, weighing economic impact against social equity. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers and social welfare systems are the primary beneficiaries, gaining protections and contributions. Platform companies are the primary payers, bearing the costs of reclassification. Labor unions and governments act as agenda-setters, pushing for the adoption and enforcement of this reading. Consumers are indirect payers through potentially higher service costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine redefinition of employment, or a policy intervention framed as a definitional shift?',
    'Analysis of legal precedent and legislative intent in jurisdictions adopting this reading: does it fundamentally alter the concept of employment or merely extend existing protections?',
    'If a genuine redefinition, its impact on future labor relations is more profound; if a policy intervention, it may be more susceptible to political reversal or circumvention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies whether the reading is a definitional or policy shift.').

omega_variable(
    economic_impact_on_platforms_and_consumers,
    'What is the true economic impact of reclassification on platform companies'' viability, innovation, and consumer prices?',
    'Empirical studies from jurisdictions that have implemented similar reclassifications, analyzing changes in platform business models, investment, and consumer behavior.',
    'If the economic impact is severe, it could lead to job losses or reduced service availability, weakening the political will for enforcement. If manageable, it strengthens the case for reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_on_platforms_and_consumers, empirical, 'Assesses the economic consequences of implementing this reading.').

omega_variable(
    worker_preference_for_flexibility,
    'To what extent do platform workers genuinely prefer the flexibility of independent contracting over the benefits and security of employment?',
    'Large-scale, independent surveys of platform workers that offer clear trade-offs between flexibility, benefits, and security, controlling for selection bias.',
    'If a significant portion of workers genuinely prioritize flexibility, it could support hybrid models or weaken the moral case for universal reclassification under this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(worker_preference_for_flexibility, empirical, 'Examines worker preferences regarding employment status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__substantive_employment_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(empl_tr_t18, employment_boundary__substantive_employment_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__substantive_employment_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(empl_tr_t30, employment_boundary__substantive_employment_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(empl_be_t6, employment_boundary__substantive_employment_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(empl_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(empl_be_t18, employment_boundary__substantive_employment_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(empl_be_t24, employment_boundary__substantive_employment_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(empl_be_t30, employment_boundary__substantive_employment_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(empl_su_t6, employment_boundary__substantive_employment_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(empl_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(empl_su_t18, employment_boundary__substantive_employment_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(empl_su_t24, employment_boundary__substantive_employment_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(empl_su_t30, employment_boundary__substantive_employment_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, platform_worker_social_insurance).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, gig_economy_business_models).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, labor_law_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'employment_boundary' kernel, each representing a distinct structural claim about the definition of employment in the platform economy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
