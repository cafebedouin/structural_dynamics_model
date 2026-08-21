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
 *   regardless of their contractual status. It is one reading of the broader
 *   'employment_boundary' kernel, which is highly contested in labor law and
 *   policy. This reading aims to extend traditional employment protections to
 *   gig workers, shifting costs and responsibilities onto platform companies.
 *   The metrics reflect the ongoing struggle to enforce this definition
 *   against platform resistance.
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
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '48cd1693-c40f-40b1-99da-49ef03e78819').
narrative_ontology:cs_kernel_codification('48cd1693-c40f-40b1-99da-49ef03e78819', formalized).
narrative_ontology:cs_authority_grounding('48cd1693-c40f-40b1-99da-49ef03e78819', lineage).
narrative_ontology:cs_interpretation_layer_present('48cd1693-c40f-40b1-99da-49ef03e78819').
narrative_ontology:cs_reading_relation('48cd1693-c40f-40b1-99da-49ef03e78819', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('48cd1693-c40f-40b1-99da-49ef03e78819', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('48cd1693-c40f-40b1-99da-49ef03e78819', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('48cd1693-c40f-40b1-99da-49ef03e78819', economic_dependence_defines_employment, deontological).
narrative_ontology:cs_axiom('48cd1693-c40f-40b1-99da-49ef03e78819', foundational, algorithmic_control_is_managerial_control).
narrative_ontology:cs_axiom_status(algorithmic_control_is_managerial_control, holdable).
narrative_ontology:cs_axiom_grounding('48cd1693-c40f-40b1-99da-49ef03e78819', algorithmic_control_is_managerial_control, empirically_contingent).
narrative_ontology:cs_reference_frame('48cd1693-c40f-40b1-99da-49ef03e78819', traditional_employment_protections).
narrative_ontology:cs_drift_state('48cd1693-c40f-40b1-99da-49ef03e78819', contemporary_gig_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('48cd1693-c40f-40b1-99da-49ef03e78819', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_welfare_systems).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_companies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently lack traditional employment benefits and protections. Under this reading, they would gain access to social insurance, minimum wage, and job security, but might face reduced flexibility or fewer work opportunities if platforms reduce supply.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    powerless, immediate, constrained, global).

% Currently classify workers as independent contractors, avoiding employment-related costs. Under this reading, they would bear significant new costs for benefits, payroll taxes, and compliance, potentially leading to business model restructuring or withdrawal from certain markets.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_companies, payer,
    institutional, biographical, constrained, global).

% Currently face strain from a growing segment of the workforce lacking social protections. This reading would integrate platform workers into existing social safety nets, increasing revenue through payroll contributions and reducing reliance on other forms of public assistance.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_welfare_systems, beneficiary,
    institutional, generational, analytical, national).

% Advocate for the reclassification of platform workers as employees to extend collective bargaining rights and improve working conditions. They actively lobby for legislative and judicial adoption of this substantive definition.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_unions, agenda_setter,
    organized, generational, mobile, national).

% Currently bear the costs of traditional employment. This reading would level the playing field by imposing similar obligations on platform companies, reducing perceived unfair competition from the gig economy.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers, beneficiary,
    organized, biographical, mobile, national).

% Adhere to a strict interpretation of contract law and formal control as the basis for employment. They would argue that this substantive reading distorts established legal principles and creates uncertainty, but their arguments are often sidelined in policy debates driven by social outcomes.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, formalist_legal_scholars, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the provision of social protections and labor rights to a growing segment of the workforce, ensuring a baseline of security and fairness that would otherwise be fragmented or absent due to novel work arrangements.
% TRANSFER_FUNCTION: Transfers costs associated with social insurance, benefits, and labor law compliance from platform workers and public welfare systems to platform companies. It also transfers power from platforms to workers and unions.
% ABSENT_VOICES: Formalist legal scholars and some platform users (who might face higher costs or reduced service availability) are often excluded from the policy-making process that favors this substantive definition. They would argue for contractual freedom and consumer choice.
% DISAPPEARANCE_RATIONALE: If this substantive definition vanished, platform companies would revert to classifying workers as independent contractors, leading to a rapid erosion of worker protections, increased precarity, and greater strain on public welfare systems. Labor markets would re-segment, and the balance of power would shift back to platforms.
% FOUNDING_PROBLEM: The rise of the gig economy created a large class of workers performing labor under conditions of economic dependence and control, yet without the protections afforded to traditional employees, leading to precarity and social inequality.
% FOUNDING_PROBLEM_CORROBORATION: Labor organizations, social policy researchers, and many platform workers themselves attest that the problem of precarity and lack of protections is very much alive. Government reports and academic studies from outside the benefiting parties corroborate the ongoing challenges faced by gig workers.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the costs imposed on platform companies and the benefits gained by workers and social systems. Suppression (0.70) is high due to the active legal and political efforts required to reclassify workers against strong industry lobbying. Theater ratio (0.20) is relatively low, as the debate is direct and the stakes are clear, with less performative maintenance of a defunct function. The rising extractiveness and suppression over time reflect the increasing intensity of the legal and political battles to establish and enforce this definition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform workers and labor unions, this is a necessary re-alignment of legal definitions with economic reality, a 'rope' that provides essential coordination. From the perspective of platform companies, it is an 'snare' that imposes undue burdens and stifles innovation. The engine's classification will reflect the aggregate structural dynamics, which this reading aims to shift towards a more coordinated, albeit still extractive, outcome for platforms.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers and social welfare systems are beneficiaries, gaining protections and resources. Platform companies are victims, bearing new costs. Labor unions act as agenda-setters, actively pushing for this definition. Traditional employers are also beneficiaries, as it levels the playing field. Formalist legal scholars are excluded, as their arguments for contractual freedom are often overridden by the social policy goals of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_dependence_measurement,
    'How is ''economic dependence'' objectively measured in the context of platform work, given varying work patterns and income sources?',
    'Development of standardized, legally recognized metrics for economic dependence, potentially involving income thresholds, share of total income from one platform, or control over work schedule/pricing.',
    'Clearer measurement would reduce ambiguity in reclassification efforts, potentially increasing the effective suppression and extractiveness on platforms by making enforcement more consistent. Lack of clarity allows platforms to resist reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependence_measurement, empirical, 'Ambiguity in defining and measuring ''economic dependence'' for platform workers.').

omega_variable(
    algorithmic_control_scope,
    'To what extent does ''algorithmic control'' truly equate to managerial control, and how does it differ across various platform types (e.g., ride-sharing vs. freelance marketplaces)?',
    'Detailed ethnographic and computational studies of platform algorithms to map their influence on worker autonomy, pricing, and task allocation, distinguishing between coordination and control functions.',
    'If algorithmic control is found to be less pervasive or more coordinative than managerial, it could weaken the ''substantive employment'' argument, potentially shifting the classification towards a ''hybrid security'' model. If it''s found to be highly controlling, it strengthens this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_scope, conceptual, 'The conceptual equivalence and practical scope of ''algorithmic control'' as a proxy for traditional managerial control.').

omega_variable(
    platform_business_model_viability,
    'What is the actual economic impact of reclassifying all platform workers as employees on the viability of platform business models and the availability of flexible work?',
    'Longitudinal economic studies in jurisdictions that have implemented such reclassification, analyzing changes in platform profitability, employment levels, worker earnings, and consumer prices.',
    'If reclassification leads to widespread platform collapse or significant reduction in flexible work opportunities, it could generate political pressure to revert to a ''hybrid security'' model. If platforms adapt without major disruption, it strengthens the case for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_business_model_viability, empirical, 'Uncertainty about the economic consequences of full reclassification for platform companies and the labor market.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__substantive_employment_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__substantive_employment_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__substantive_employment_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(empl_be_t5, employment_boundary__substantive_employment_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(empl_be_t10, employment_boundary__substantive_employment_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(empl_be_t15, employment_boundary__substantive_employment_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(empl_su_t5, employment_boundary__substantive_employment_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(empl_su_t10, employment_boundary__substantive_employment_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(empl_su_t15, employment_boundary__substantive_employment_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, minimum_wage_enforcement).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, social_security_contribution_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
