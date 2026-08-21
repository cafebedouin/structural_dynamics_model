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
 *   human_readable: Formalist Definition of Employment (Platform Economy Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the formalist reading of the
 *   'employment_boundary' kernel, which defines employment strictly by formal
 *   contract and direct supervision. Under this reading, platform workers are
 *   classified as independent contractors, placing them outside the scope of
 *   traditional employment protections. This classification enables platform
 *   companies to externalize significant labor costs, shifting them onto
 *   workers and the public social safety net. Sibling readings include the
 *   'substantive_employment_reading' (focus on economic dependence) and the
 *   'hybrid_security_reading' (advocating for a third category of worker).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.85).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.75).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Definition of Employment (Platform Economy Reading)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '4216817d-f27e-4479-b726-7a0ccddff210').
narrative_ontology:cs_kernel_codification('4216817d-f27e-4479-b726-7a0ccddff210', formalized).
narrative_ontology:cs_authority_grounding('4216817d-f27e-4479-b726-7a0ccddff210', extraction).
narrative_ontology:cs_interpretation_layer_present('4216817d-f27e-4479-b726-7a0ccddff210').
narrative_ontology:cs_reading_relation('4216817d-f27e-4479-b726-7a0ccddff210', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('4216817d-f27e-4479-b726-7a0ccddff210', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('4216817d-f27e-4479-b726-7a0ccddff210', foundational, contractual_freedom_supremacy).
narrative_ontology:cs_axiom_status(contractual_freedom_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('4216817d-f27e-4479-b726-7a0ccddff210', contractual_freedom_supremacy, conventional).
narrative_ontology:cs_axiom('4216817d-f27e-4479-b726-7a0ccddff210', foundational, direct_control_as_employment_test).
narrative_ontology:cs_axiom_status(direct_control_as_employment_test, holdable).
narrative_ontology:cs_axiom_grounding('4216817d-f27e-4479-b726-7a0ccddff210', direct_control_as_employment_test, conventional).
narrative_ontology:cs_reference_frame('4216817d-f27e-4479-b726-7a0ccddff210', traditional_contract_law_framework).
narrative_ontology:cs_drift_state('4216817d-f27e-4479-b726-7a0ccddff210', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4216817d-f27e-4479-b726-7a0ccddff210', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, gig_economy_investors).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the terms of engagement for platform workers, benefiting from the classification of workers as independent contractors. They actively lobby for and defend this formalist interpretation in courts and legislatures, externalizing significant labor costs.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of lacking employment benefits (healthcare, unemployment insurance, paid leave), minimum wage protections, and collective bargaining rights. Their 'flexibility' comes at the cost of precarity and limited recourse against algorithmic management.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, local).

% Indirectly subsidize platform companies by covering costs that would otherwise be borne by employers, such as healthcare for uninsured workers or unemployment benefits for those without employer-provided insurance. This externalization strains public resources.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net, payer,
    organized, biographical, trapped, national).

% Are tasked with enforcing existing labor laws, but often find their authority challenged by the formalist classification. They investigate misclassification claims and propose new regulations, but face significant legal and political hurdles.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_regulators, observer,
    institutional, biographical, analytical, national).

% Benefit from the lower labor costs and reduced liabilities enabled by the independent contractor model, which contributes to higher valuations and profitability for platform companies. They have a strong interest in maintaining the formalist definition.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, gig_economy_investors, beneficiary,
    powerful, biographical, mobile, global).

% Operate under traditional employment laws, incurring higher labor costs for benefits, payroll taxes, and compliance. They face unfair competition from platform companies that externalize these costs, leading to pressure to adopt similar models or exit markets.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear (from this reading's perspective) legal and operational framework for platform companies to rapidly scale and deploy a flexible workforce, enabling new forms of service delivery and economic activity without the overhead of traditional employment.
% TRANSFER_FUNCTION: Transfers costs associated with employment benefits, payroll taxes, and social protections from platform companies to platform workers (who bear precarity) and the public social safety net (which absorbs externalized costs).
% ABSENT_VOICES: Labor unions and worker advocacy groups, who would argue for reclassification and collective bargaining rights; economists who quantify the social costs of misclassification; and workers who have experienced severe precarity or injury without recourse.
% DISAPPEARANCE_RATIONALE: If this formalist definition vanished overnight, platform companies would immediately face massive reclassification lawsuits, demands for benefits, and potentially unionization. Their business models, which rely heavily on cost externalization, would be fundamentally altered, leading to a significant reorganization of the gig economy.
% FOUNDING_PROBLEM: To enable new, flexible forms of work and service delivery that traditional employment law was not designed for, fostering innovation, entrepreneurship, and economic growth by reducing regulatory burdens on nascent digital platforms.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and their investors attest that the founding problem of fostering innovation and flexibility is still live. Labor advocates, academic researchers, and some government bodies (e.g., California's AB5, EU directives) contest this, citing evidence of widespread worker exploitation and misclassification, suggesting the problem is substantially solved or a pretext for rent-seeking. Legislative hearings and independent economic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.85) because platform companies avoid substantial costs (benefits, taxes, minimum wage) by leveraging this classification. Suppression is also high (0.75) due to the active legal and political efforts by platforms to maintain this definition, coupled with workers' limited bargaining power and exit options. The theater ratio (0.45) reflects the performative narrative of 'flexibility' and 'entrepreneurship' that often masks the underlying precarity and lack of control experienced by many platform workers. The increasing trend in all metrics over the interval reflects the growing entrenchment of this model and the escalating costs externalized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform companies, this constraint provides essential flexibility and innovation, enabling new business models. From the perspective of platform workers and labor advocates, it is a mechanism for exploitation and cost externalization, creating a precarious workforce. The engine's classification as a Snare reflects the latter, highlighting the coercive and extractive nature despite the 'coordination' narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and their investors are the primary beneficiaries, gaining from reduced labor costs and increased valuations (low directionality). Platform workers, taxpayers, and traditional employers are the targets, bearing the costs of precarity, social safety net strain, and unfair competition, respectively (high directionality). Labor regulators act as observers, attempting to mediate or challenge the constraint's effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling this arrangement as genuine coordination. While a 'coordination' story of flexibility exists, the high extractiveness, active suppression, and clear victim groups indicate that the primary function is extraction, with the coordination narrative serving as cover. The persistence of the constraint depends on active enforcement and the suppression of alternatives, rather than mutual benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is the formalist_employment_reading of the employment_boundary kernel. What would a shift to a sibling reading change structurally?',
    'Legal or legislative redefinition of employment, or judicial precedent establishing new criteria for worker classification.',
    'A shift to the substantive_employment_reading would reclassify many platform workers as employees, imposing significant costs on platforms. A shift to the hybrid_security_reading would create a new category with tailored protections, altering cost structures and worker rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of the employment_boundary kernel and outlines impacts of alternative readings.').

omega_variable(
    substantive_vs_formal_ambiguity,
    'Is employment fundamentally defined by formal contractual terms and direct supervision (formalist view) or by economic dependence and algorithmic control (substantive view)?',
    'Judicial rulings that prioritize economic reality over contractual form, or legislative action that codifies new tests for employment status.',
    'If the substantive view prevails, many platform workers would be reclassified as employees, leading to increased labor costs for platforms and enhanced protections for workers. If the formalist view is reinforced, the current extractive model persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_formal_ambiguity, conceptual, 'Ambiguity regarding the core definition of employment in the platform economy.').

omega_variable(
    hybrid_category_viability,
    'Is a ''third category'' of worker, distinct from both employee and independent contractor, a viable and equitable solution for platform workers, or does it merely create new ambiguities and potential for exploitation?',
    'Empirical evaluation of ''third category'' implementations in jurisdictions that have adopted them, assessing their impact on worker protections, platform costs, and market dynamics.',
    'If a hybrid category proves viable and equitable, it could offer a path to balancing flexibility with security. If it proves to be a ''false solution,'' it might perpetuate or reconfigure existing extractive dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_category_viability, empirical, 'Uncertainty about the efficacy and equity of a hybrid worker classification category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2005, employment_boundary__formalist_employment_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(empl_tr_t2009, employment_boundary__formalist_employment_reading, theater_ratio, 2009, 0.3).
narrative_ontology:measurement(empl_tr_t2013, employment_boundary__formalist_employment_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(empl_tr_t2017, employment_boundary__formalist_employment_reading, theater_ratio, 2017, 0.4).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__formalist_employment_reading, theater_ratio, 2021, 0.43).
narrative_ontology:measurement(empl_tr_t2025, employment_boundary__formalist_employment_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(empl_be_t2005, employment_boundary__formalist_employment_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(empl_be_t2009, employment_boundary__formalist_employment_reading, base_extractiveness, 2009, 0.7).
narrative_ontology:measurement(empl_be_t2013, employment_boundary__formalist_employment_reading, base_extractiveness, 2013, 0.75).
narrative_ontology:measurement(empl_be_t2017, employment_boundary__formalist_employment_reading, base_extractiveness, 2017, 0.8).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__formalist_employment_reading, base_extractiveness, 2021, 0.83).
narrative_ontology:measurement(empl_be_t2025, employment_boundary__formalist_employment_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2005, employment_boundary__formalist_employment_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(empl_su_t2009, employment_boundary__formalist_employment_reading, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(empl_su_t2013, employment_boundary__formalist_employment_reading, suppression_requirement, 2013, 0.65).
narrative_ontology:measurement(empl_su_t2017, employment_boundary__formalist_employment_reading, suppression_requirement, 2017, 0.7).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__formalist_employment_reading, suppression_requirement, 2021, 0.73).
narrative_ontology:measurement(empl_su_t2025, employment_boundary__formalist_employment_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
