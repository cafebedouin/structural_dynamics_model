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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Definition for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint defines employment based on economic dependence and
 *   algorithmic control, asserting that platform workers are employees
 *   regardless of their contract. It is one reading of the
 *   'employment_boundary' kernel. This reading aims to extend labor
 *   protections to platform workers, shifting costs to platform companies.
 *   The metrics reflect the ongoing struggle for reclassification, with
 *   platforms actively resisting enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.65).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.7).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Definition for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '0335da38-076e-4162-8dcf-a2a05b1fe51c').
narrative_ontology:cs_kernel_codification('0335da38-076e-4162-8dcf-a2a05b1fe51c', distributed).
narrative_ontology:cs_authority_grounding('0335da38-076e-4162-8dcf-a2a05b1fe51c', practice).
narrative_ontology:cs_interpretation_layer_present('0335da38-076e-4162-8dcf-a2a05b1fe51c').
narrative_ontology:cs_reading_relation('0335da38-076e-4162-8dcf-a2a05b1fe51c', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('0335da38-076e-4162-8dcf-a2a05b1fe51c', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('0335da38-076e-4162-8dcf-a2a05b1fe51c', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('0335da38-076e-4162-8dcf-a2a05b1fe51c', economic_dependence_defines_employment, deontological).
narrative_ontology:cs_axiom('0335da38-076e-4162-8dcf-a2a05b1fe51c', foundational, algorithmic_control_is_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_is_supervision, holdable).
narrative_ontology:cs_axiom_grounding('0335da38-076e-4162-8dcf-a2a05b1fe51c', algorithmic_control_is_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('0335da38-076e-4162-8dcf-a2a05b1fe51c', substantive_labor_law_tradition).
narrative_ontology:cs_drift_state('0335da38-076e-4162-8dcf-a2a05b1fe51c', contemporary_platform_economy, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0335da38-076e-4162-8dcf-a2a05b1fe51c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_welfare_systems).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_companies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently bear the precarity of independent contractor status (no benefits, no job security). This reading would reclassify them as employees, granting them labor protections and social benefits, but also potentially reducing flexibility or access to work if platforms reduce supply.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    powerless, immediate, identity_locked, global).

% Currently benefit from classifying workers as independent contractors, avoiding payroll taxes, benefits, and labor law compliance. This reading would impose significant new costs and liabilities, requiring them to restructure their business models and potentially reduce worker supply or increase prices.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_companies, payer,
    institutional, biographical, constrained, global).

% Advocate for this substantive definition of employment to extend collective bargaining rights and protections to platform workers. They would gain membership and influence if this reading is adopted.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_unions, agenda_setter,
    organized, generational, mobile, national).

% Currently bear the costs of platform worker precarity (e.g., unemployment benefits, healthcare for uninsured). This reading would shift some of these costs to platform companies via payroll taxes and mandated benefits, stabilizing social safety nets.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_welfare_systems, beneficiary,
    institutional, generational, analytical, national).

% Benefit from low-cost, flexible services provided by platform workers. Reclassification could lead to increased service costs or reduced availability as platforms adjust to new labor costs.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers, payer,
    moderate, immediate, mobile, local).

% Adhere to a definition of employment based strictly on contract terms and direct control, arguing that economic dependence alone is insufficient for employee status. Their arguments are often sidelined in policy debates favoring substantive criteria.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, formalist_legal_scholars, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate labor protections and social benefits for platform workers, ensuring they receive the same rights and security as traditional employees, thereby reducing precarity and externalized social costs.
% TRANSFER_FUNCTION: Transfers costs (payroll taxes, benefits, compliance) from platform workers and social welfare systems to platform companies, and potentially from platform companies to consumers through higher prices.
% ABSENT_VOICES: Formalist legal scholars and some platform users who prioritize flexibility over traditional employment benefits would object, arguing that this definition stifles innovation or imposes unnecessary burdens. Their voices are often marginalized in policy debates driven by worker advocacy.
% DISAPPEARANCE_RATIONALE: If this definition vanished, platform workers would revert to independent contractor status, losing access to employment benefits. Platform companies would continue to externalize labor costs, and social welfare systems would bear the full burden of worker precarity. The labor market for platform work would remain highly precarious.
% FOUNDING_PROBLEM: The rise of the platform economy created a large class of workers who, despite economic dependence and algorithmic control, were denied traditional employment protections due to their contractual status as independent contractors.
% FOUNDING_PROBLEM_CORROBORATION: Labor organizations, social policy researchers, and many platform workers themselves attest that the problem of platform worker precarity is very much alive, citing ongoing struggles for fair wages, benefits, and job security. Platform companies contest this, arguing their model offers unparalleled flexibility.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) reflects the costs imposed on platform companies by reclassification, which they resist. Suppression (0.70) is high due to the active legal and lobbying efforts by platforms to maintain the independent contractor model. Theater ratio (0.20) is relatively low, as the debate is direct and functional, not performative. Accessibility collapse (0.40) is moderate, as alternative work arrangements exist, but the core issue of precarity remains. Resistance (0.75) is high, driven by labor unions and worker advocacy groups pushing for this redefinition.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies perceive this definition as an extractive imposition on their business model, while platform workers and labor advocates see it as a necessary correction to an extractive system. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers and social welfare systems are beneficiaries, gaining protections and cost relief. Platform companies and, indirectly, consumers are payers, bearing increased labor costs. Labor unions act as agenda-setters, actively pushing for this redefinition. Formalist legal scholars are excluded, as their arguments are often dismissed in favor of a substantive approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_dependence_threshold,
    'What specific threshold of economic dependence or algorithmic control constitutes ''employment'' under this reading, and how is it measured?',
    'Judicial precedent or legislative guidelines establishing clear, quantifiable criteria for economic dependence and algorithmic control in platform work.',
    'Clearer thresholds would reduce ambiguity, making enforcement more consistent and reducing legal challenges. Ambiguity allows platforms to continue resisting reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependence_threshold, empirical, 'Defines the operational boundary of ''economic dependence'' and ''algorithmic control''.').

omega_variable(
    platform_business_model_viability,
    'To what extent would universal reclassification of platform workers as employees fundamentally undermine the viability of current platform business models?',
    'Economic impact studies and pilot programs in jurisdictions that have implemented reclassification, analyzing changes in platform operations, worker supply, and consumer prices.',
    'If viability is severely undermined, it could lead to job losses or reduced service availability, potentially pushing policy towards a hybrid model. If platforms adapt, it strengthens the case for reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_business_model_viability, empirical, 'Assesses the economic impact of reclassification on platform companies.').

omega_variable(
    reading_legitimacy_source,
    'Is the legitimacy of this substantive reading primarily derived from a moral imperative to protect vulnerable workers, or from an economic imperative to internalize externalized social costs?',
    'Analysis of legislative debates, judicial reasoning, and advocacy group statements to identify the dominant normative grounding for this interpretation.',
    'If primarily moral, it strengthens the deontological grounding of the axioms. If primarily economic, it highlights the instrumental nature of the reclassification and its susceptibility to cost-benefit analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_source, conceptual, 'Examines the normative foundation of the substantive employment reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__substantive_employment_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__substantive_employment_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(empl_be_t5, employment_boundary__substantive_employment_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(empl_be_t10, employment_boundary__substantive_employment_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(empl_su_t5, employment_boundary__substantive_employment_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(empl_su_t10, employment_boundary__substantive_employment_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, gig_worker_benefits_access).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, social_security_funding).

% DUAL FORMULATION NOTE:
% This constraint is the 'substantive_employment_reading' of the 'employment_boundary' kernel, which also includes 'formalist_employment_reading' and 'hybrid_security_reading'. Each reading represents a distinct structural claim about the nature of employment for platform workers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
