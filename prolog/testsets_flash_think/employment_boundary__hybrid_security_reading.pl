% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Hybrid Worker Classification for Platform Economy
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid security' reading of the broader
 *   'employment_boundary' kernel. It posits that platform workers constitute
 *   a distinct third category, requiring tailored protections that differ
 *   from both traditional employment and independent contracting. This
 *   reading aims to formalize a middle ground, offering some benefits to
 *   workers while preserving flexibility for platform companies. The authored
 *   metrics reflect a 'tangled_rope' classification, indicating that while
 *   genuine coordination (basic protections) exists, it is coupled with
 *   significant extraction (institutionalized precarity) and requires active
 *   enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.55).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.65).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Worker Classification for Platform Economy").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '48d41caf-6301-447e-81e3-1805083cc665').
narrative_ontology:cs_kernel_codification('48d41caf-6301-447e-81e3-1805083cc665', formalized).
narrative_ontology:cs_authority_grounding('48d41caf-6301-447e-81e3-1805083cc665', lineage).
narrative_ontology:cs_interpretation_layer_present('48d41caf-6301-447e-81e3-1805083cc665').
narrative_ontology:cs_reading_relation('48d41caf-6301-447e-81e3-1805083cc665', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('48d41caf-6301-447e-81e3-1805083cc665', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('48d41caf-6301-447e-81e3-1805083cc665', foundational, platform_work_is_distinct).
narrative_ontology:cs_axiom_status(platform_work_is_distinct, holdable).
narrative_ontology:cs_axiom_grounding('48d41caf-6301-447e-81e3-1805083cc665', platform_work_is_distinct, empirically_contingent).
narrative_ontology:cs_axiom('48d41caf-6301-447e-81e3-1805083cc665', foundational, basic_protections_are_universal).
narrative_ontology:cs_axiom_status(basic_protections_are_universal, holdable).
narrative_ontology:cs_axiom_grounding('48d41caf-6301-447e-81e3-1805083cc665', basic_protections_are_universal, deontological).
narrative_ontology:cs_reference_frame('48d41caf-6301-447e-81e3-1805083cc665', traditional_binary_employment_contract).
narrative_ontology:cs_drift_state('48d41caf-6301-447e-81e3-1805083cc665', contemporary_platform_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('48d41caf-6301-447e-81e3-1805083cc665', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, traditional_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and benefits from a hybrid classification that avoids full employment obligations while maintaining a flexible workforce. Bears some costs for basic protections but gains significantly from reduced labor costs and regulatory certainty compared to full employment.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Receives basic protections (e.g., injury insurance, some medical benefits) that independent contractors typically lack, but remains excluded from full employment benefits like career development, retirement security, and collective bargaining rights. Bears the cost of precarity and limited social safety nets.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_workers, beneficiary).

% Faces potential competitive disadvantage due to higher labor costs and regulatory burdens compared to platform companies operating under the hybrid model. Advocates for a level playing field or clearer definitions.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, payer,
    organized, biographical, constrained, national).

% Analyzes the impact of the hybrid classification on worker rights and social equity. Advocates for stronger protections for platform workers, often pushing for full employment status or more robust hybrid models.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions_advocates, observer,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, labor_unions_advocates, agenda_setter).

% Develops and enforces the legal framework for the hybrid worker category, balancing economic innovation with social protection. Responds to lobbying from both platform companies and labor advocates.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, governments_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the flexibility and often lower costs of platform services, which are enabled by the hybrid labor model. Indirectly bears some costs if protections are passed on, but generally experiences a net benefit from convenience.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, consumers, beneficiary,
    moderate, immediate, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal and social framework for platform work that acknowledges its unique characteristics, offering some basic protections (e.g., injury insurance) while maintaining flexibility for platforms and workers, thereby regularizing a previously ambiguous labor category.
% TRANSFER_FUNCTION: Transfers some social security obligations (e.g., injury insurance, medical benefits) from the state or full employers to platform companies, while transferring flexibility and lower labor costs to platforms, and precarity (lack of full employment benefits) to workers.
% ABSENT_VOICES: Workers advocating for full employment status, traditional labor unions pushing for broader worker rights, and platforms seeking full independent contractor status are all present in the debate but their preferred outcomes are excluded by this specific hybrid framing.
% DISAPPEARANCE_RATIONALE: If the hybrid classification vanished overnight, platform workers would either default to full employment (massive cost for platforms, potentially collapsing many services) or full independent contractor status (loss of all protections, leading to social instability). The entire platform economy's legal and economic landscape would reorganize.
% FOUNDING_PROBLEM: The rapid growth of the platform economy created a large class of workers who did not fit traditional employment definitions, leading to legal ambiguity, lack of basic protections, and social instability for a significant portion of the workforce.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists, social policy researchers, and international labor organizations corroborate the existence and persistence of this problem, citing ongoing studies on worker precarity, the need for new frameworks, and the limitations of existing legal categories. Legislative hearings and policy debates also attest to its live status.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55 rising to 0.60) is moderate but significant, reflecting the institutionalization of precarity despite the provision of some benefits. Suppression (0.65 rising to 0.70) is high because the hybrid model requires active legal and regulatory enforcement to prevent workers from being reclassified as full employees or losing all protections. The theater ratio (0.30 rising to 0.35) indicates that while some protections are genuinely functional, a growing portion of the framework's maintenance is performative, defending the 'hybrid' label against calls for more comprehensive worker rights. The time series shows a slight increase in extractiveness and suppression as the hybrid model becomes more entrenched and its limitations clearer.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform companies, this hybrid classification is a necessary and beneficial coordination mechanism for a new economic reality. From the perspective of many platform workers and labor advocates, it is a form of extraction that institutionalizes precarity under the guise of 'tailored protections.' The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are clear beneficiaries (low d) as they gain regulatory certainty and avoid full employment costs. Platform workers are in a hybrid position, receiving some benefits but bearing significant costs of precarity (d near symmetric, but leaning towards target). Traditional employers are targets (high d) due to competitive disadvantage. Governments and regulators are agenda-setters, balancing competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'How does this ''hybrid_security_reading'' of the ''employment_boundary'' kernel interact with the ''formalist_employment_reading'' and ''substantive_employment_reading''?',
    'Analysis of legal precedents and policy outcomes: does this reading gain dominance, coexist in a contested space, or influence the terms of debate for the other readings?',
    'If this reading gains dominance, it could institutionalize a new labor paradigm. If it merely coexists, the contest over worker classification remains unresolved. If it influences, it shifts the debate''s parameters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'The relationship and impact of this specific reading within the broader kernel contest.').

omega_variable(
    extraction_vs_flexibility_tradeoff,
    'Is the measured extractiveness (precarity) an unavoidable cost of maintaining flexibility in the platform economy, or is it a mechanism to capture rents that could be mitigated without sacrificing flexibility?',
    'Comparative analysis of hybrid models in different jurisdictions: do models with lower extractiveness demonstrate similar levels of flexibility and innovation?',
    'If lower extractiveness is possible without losing flexibility, the ''tangled_rope'' classification is strengthened, indicating avoidable rent-seeking. If not, the extraction might be re-evaluated as a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_flexibility_tradeoff, empirical, 'Whether precarity is an inherent feature or an extractive outcome of the hybrid model.').

omega_variable(
    temporary_vs_permanent_status,
    'Is this hybrid worker classification intended as a temporary ''scaffold'' to bridge a transition to a more stable future, or is it becoming a permanent ''tangled_rope'' institutionalizing a new form of labor precarity?',
    'Policy review and legislative intent analysis: are there sunset clauses or explicit mechanisms for re-evaluation and evolution of the category? Longitudinal studies on worker outcomes.',
    'If intended as temporary, the current ''tangled_rope'' classification might shift towards ''scaffold'' if a clear transition path is identified. If permanent, the ''tangled_rope'' classification is reinforced, highlighting the long-term implications of institutionalized precarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporary_vs_permanent_status, conceptual, 'The intended and actual temporal status of the hybrid worker category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__hybrid_security_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__hybrid_security_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__hybrid_security_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(empl_be_t5, employment_boundary__hybrid_security_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(empl_be_t10, employment_boundary__hybrid_security_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(empl_be_t15, employment_boundary__hybrid_security_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(empl_su_t5, employment_boundary__hybrid_security_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(empl_su_t10, employment_boundary__hybrid_security_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(empl_su_t15, employment_boundary__hybrid_security_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, gig_economy_business_models).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, social_safety_net_eligibility).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, labor_union_organizing_strategies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'employment_boundary' kernel, which also includes 'formalist_employment_reading' and 'substantive_employment_reading'. Each reading represents a distinct structural claim about worker classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
