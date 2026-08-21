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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary (Platform Economy Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint describes the 'formalist' reading of the employment
 *   boundary, where employment is strictly defined by formal contract and
 *   direct supervision, thereby classifying platform workers as independent
 *   contractors. This reading is a component of the broader
 *   'employment_boundary' kernel, which is highly contested in labor
 *   economics and social policy. This specific reading excludes platform
 *   workers from traditional employment protections, externalizing costs to
 *   workers and the state, and is actively enforced by platform companies and
 *   often upheld by legal frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.85).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.7).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Platform Economy Reading)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '62a29b63-b201-4a3a-b762-55b159d82c05').
narrative_ontology:cs_kernel_codification('62a29b63-b201-4a3a-b762-55b159d82c05', formalized).
narrative_ontology:cs_authority_grounding('62a29b63-b201-4a3a-b762-55b159d82c05', lineage).
narrative_ontology:cs_interpretation_layer_present('62a29b63-b201-4a3a-b762-55b159d82c05').
narrative_ontology:cs_reading_relation('62a29b63-b201-4a3a-b762-55b159d82c05', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('62a29b63-b201-4a3a-b762-55b159d82c05', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('62a29b63-b201-4a3a-b762-55b159d82c05', foundational, contractual_autonomy_priority).
narrative_ontology:cs_axiom_status(contractual_autonomy_priority, holdable).
narrative_ontology:cs_axiom_grounding('62a29b63-b201-4a3a-b762-55b159d82c05', contractual_autonomy_priority, conventional).
narrative_ontology:cs_axiom('62a29b63-b201-4a3a-b762-55b159d82c05', foundational, direct_supervision_as_control_test).
narrative_ontology:cs_axiom_status(direct_supervision_as_control_test, holdable).
narrative_ontology:cs_axiom_grounding('62a29b63-b201-4a3a-b762-55b159d82c05', direct_supervision_as_control_test, conventional).
narrative_ontology:cs_reference_frame('62a29b63-b201-4a3a-b762-55b159d82c05', traditional_employment_law_framework).
narrative_ontology:cs_drift_state('62a29b63-b201-4a3a-b762-55b159d82c05', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62a29b63-b201-4a3a-b762-55b159d82c05', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the terms of engagement for platform workers, classifying them as independent contractors. They benefit from avoiding payroll taxes, minimum wage laws, and social insurance contributions. They actively lobby for this classification to be maintained.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Are classified as independent contractors, bearing the full cost of self-employment taxes, lack of benefits (health insurance, paid leave), and no minimum wage protections. Their 'flexibility' often comes with precarity and algorithmic control. Exit means losing their primary income source.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, local).

% Subsidize platform companies by covering costs (e.g., unemployment benefits, healthcare for uninsured workers) that would otherwise be borne by employers. This externalization of costs falls on the general tax base.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net, payer,
    organized, generational, constrained, national).

% Benefit from a clear distinction between their employees and platform workers, maintaining their existing labor cost structures. However, they also face potential competitive pressure from platforms with lower labor costs.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, beneficiary,
    powerful, biographical, mobile, national).

% Are largely excluded from organizing platform workers due to their independent contractor status, which often falls outside traditional collective bargaining frameworks. They advocate for reclassification or new protections.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_unions, excluded,
    organized, generational, trapped, national).

% Are tasked with interpreting and enforcing existing labor laws, often struggling to apply traditional definitions of employment to the novel arrangements of the platform economy. Their rulings shape the persistence of this formalist reading.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, regulators_courts, agenda_setter,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, legally defined boundary between 'employee' and 'independent contractor', simplifying compliance for businesses that adhere to traditional employment models and enabling the platform model's flexibility claims.
% TRANSFER_FUNCTION: Transfers labor costs and social insurance obligations from platform companies to individual platform workers and the public social safety net, in exchange for 'flexibility' for workers and lower operating costs for platforms.
% ABSENT_VOICES: Platform workers, if empowered to collectively bargain, would object to the lack of benefits and protections. Labor unions, currently largely excluded, would advocate for reclassification and collective representation. The broader public, if fully aware of the externalized costs, might demand policy changes.
% DISAPPEARANCE_RATIONALE: If the formalist employment boundary vanished overnight, platform companies would face immediate pressure to reclassify workers, leading to significant changes in their business models, pricing, and labor practices. Workers would gain protections, and the burden on public safety nets would decrease. The entire platform economy would undergo a fundamental restructuring.
% FOUNDING_PROBLEM: The need for clear legal definitions to distinguish between different forms of labor engagement, particularly in the context of new economic models that emphasize flexibility and task-based work.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and some policymakers argue the problem is live, emphasizing the need for flexibility and innovation. Labor advocates, economists, and social policy experts (outside the benefiting parties) argue the problem has been co-opted, and the current classification primarily serves to externalize costs, making the 'founding problem' a cover for extraction.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is high (0.85) because platform companies avoid significant labor costs by externalizing them to workers and the public. Suppression (0.70) is substantial due to the legal and economic barriers preventing workers from challenging their classification or organizing. The theater ratio (0.20) is relatively low, as the formalist distinction is genuinely applied, though its justification as 'flexibility' often masks underlying precarity. The increasing extractiveness and suppression over time reflect the hardening of this classification as the platform economy matured.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies perceive this as a legitimate, efficient, and flexible arrangement, while platform workers and social safety nets experience it as a highly extractive and precarious structure. Regulators and courts are caught between these competing interpretations, with their decisions often reinforcing one perspective over the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are clear beneficiaries, gaining cost advantages. Platform workers and the social safety net are victims, bearing externalized costs. Traditional employers benefit from a clear boundary, while labor unions are excluded from organizing platform workers under this framework. Regulators and courts act as agenda-setters, interpreting and enforcing the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling pure extraction as coordination by highlighting the asymmetric cost externalization. The 'flexibility' narrative, while having some coordination function for platforms, primarily serves as cover for avoiding employer obligations. The rising extractiveness and suppression over time indicate a drift towards a Snare, where the coordination story is increasingly a pretext for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_worker_preference_for_flexibility,
    'To what extent do platform workers genuinely prefer the flexibility of independent contractor status over the benefits and protections of employment, given full information and no coercive pressure?',
    'Large-scale, independent surveys with robust controls for selection bias and economic necessity, or policy experiments offering genuine choice between employment and independent contractor models with equivalent compensation.',
    'If preference for flexibility is low, the ''choice'' narrative supporting this formalist reading collapses, strengthening arguments for reclassification. If high, it would lend some legitimacy to the formalist boundary, though not necessarily its extractive aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_worker_preference_for_flexibility, empirical, 'Assesses the validity of the ''flexibility'' justification for independent contractor status.').

omega_variable(
    externalized_cost_quantification,
    'What is the full economic cost externalized by platform companies to platform workers and the state (e.g., lost tax revenue, increased social safety net spending, worker out-of-pocket costs for benefits)?',
    'Comprehensive economic modeling and public accounting by independent government agencies or academic researchers.',
    'A high quantified cost would provide strong empirical evidence for the extractive nature of this reading, supporting policy interventions for reclassification or new worker protections. A low cost would weaken the extraction argument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externalized_cost_quantification, empirical, 'Quantifies the societal costs of the formalist employment boundary.').

omega_variable(
    substantive_vs_formal_definition_of_employment,
    'Is employment fundamentally defined by formal legal contracts and direct supervision (formalist view), or by economic dependence and control over work processes (substantive view)?',
    'Legal and philosophical debate, evolving case law, and legislative action that explicitly adopts one definitional framework over the other.',
    'Adoption of a substantive definition would fundamentally undermine this formalist reading, leading to widespread reclassification of platform workers. Continued adherence to the formalist view entrenches the current extractive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_formal_definition_of_employment, conceptual, 'The core conceptual disagreement between this reading and its substantive sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2010, employment_boundary__formalist_employment_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(empl_tr_t2014, employment_boundary__formalist_employment_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(empl_tr_t2018, employment_boundary__formalist_employment_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(empl_tr_t2024, employment_boundary__formalist_employment_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t2010, employment_boundary__formalist_employment_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(empl_be_t2014, employment_boundary__formalist_employment_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__formalist_employment_reading, base_extractiveness, 2018, 0.8).
narrative_ontology:measurement(empl_be_t2024, employment_boundary__formalist_employment_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2010, employment_boundary__formalist_employment_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(empl_su_t2014, employment_boundary__formalist_employment_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__formalist_employment_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(empl_su_t2024, employment_boundary__formalist_employment_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, gig_economy_worker_protections).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, social_safety_net_funding).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'employment_boundary' kernel. Its structural properties differ significantly from the 'substantive_employment_reading' and 'hybrid_security_reading' of the same kernel, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
