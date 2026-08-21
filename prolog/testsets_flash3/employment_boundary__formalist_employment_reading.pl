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
 *   This constraint represents the 'formalist' reading of the employment
 *   boundary, which defines employment strictly by formal contract and direct
 *   supervision, thereby classifying platform workers as independent
 *   contractors. This reading is a key mechanism for platform companies to
 *   externalize labor costs and avoid traditional employer obligations. The
 *   high extractiveness and suppression reflect the economic reality for
 *   platform workers under this classification, and the active lobbying and
 *   legal defense by platform companies to maintain it. This is one reading
 *   of the 'employment_boundary' kernel, contested by
 *   'substantive_employment_reading' and 'hybrid_security_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.85).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.78).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Platform Economy Reading)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'd7a49497-9193-4c0a-8100-dac8e359706c').
narrative_ontology:cs_kernel_codification('d7a49497-9193-4c0a-8100-dac8e359706c', formalized).
narrative_ontology:cs_authority_grounding('d7a49497-9193-4c0a-8100-dac8e359706c', lineage).
narrative_ontology:cs_interpretation_layer_present('d7a49497-9193-4c0a-8100-dac8e359706c').
narrative_ontology:cs_reading_relation('d7a49497-9193-4c0a-8100-dac8e359706c', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7a49497-9193-4c0a-8100-dac8e359706c', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('d7a49497-9193-4c0a-8100-dac8e359706c', foundational, control_requires_direct_supervision).
narrative_ontology:cs_axiom_status(control_requires_direct_supervision, holdable).
narrative_ontology:cs_axiom_grounding('d7a49497-9193-4c0a-8100-dac8e359706c', control_requires_direct_supervision, conventional).
narrative_ontology:cs_axiom('d7a49497-9193-4c0a-8100-dac8e359706c', foundational, contractual_form_determines_status).
narrative_ontology:cs_axiom_status(contractual_form_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('d7a49497-9193-4c0a-8100-dac8e359706c', contractual_form_determines_status, conventional).
narrative_ontology:cs_reference_frame('d7a49497-9193-4c0a-8100-dac8e359706c', traditional_master_servant_doctrine).
narrative_ontology:cs_drift_state('d7a49497-9193-4c0a-8100-dac8e359706c', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7a49497-9193-4c0a-8100-dac8e359706c', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the terms of engagement for platform workers, classifying them as independent contractors. They benefit from avoiding payroll taxes, minimum wage laws, benefits, and collective bargaining obligations. They actively lobby for legislation that codifies this classification.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of self-employment, including no minimum wage, no benefits, no unemployment insurance, and no collective bargaining rights. They often experience high precarity and economic dependence, despite the formal classification. Their exit options are limited by the need for income and lack of alternative employment.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, local).

% Subsidize the platform economy indirectly by covering costs that would otherwise be borne by employers, such as healthcare for uninsured workers, and increased demand on public assistance programs due to low and unstable platform wages. They have limited direct influence over platform classification.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net, payer,
    organized, generational, constrained, national).

% Operate under traditional employment laws, bearing the costs of payroll taxes, benefits, and labor protections. They are excluded from the 'flexibility' and cost savings enjoyed by platform companies, leading to competitive disadvantage and calls for a level playing field.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, excluded,
    powerful, biographical, constrained, national).

% Advocate for platform workers to be classified as employees to gain collective bargaining rights and protections. They are excluded from organizing platform workers under current formalist interpretations of labor law, which defines independent contractors as outside their purview.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Analyze the legal and economic implications of the formalist classification, often arguing that it fails to capture the substantive reality of platform work. They propose alternative legal frameworks and advocate for policy changes.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, legal_scholars_activists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the legal and economic expectations around labor, by providing a clear, if narrow, definition of 'employment' that allows for a distinct category of 'independent contractor' to exist, enabling new business models based on flexible labor supply.
% TRANSFER_FUNCTION: Transfers the costs of labor protections, benefits, and social safety net contributions from platform companies to individual platform workers and the public (taxpayers). It also transfers the risk of economic downturns and precarity to workers.
% ABSENT_VOICES: Platform workers, if empowered to collectively bargain, would object to their classification and demand employment rights. Traditional employers would object to the uneven playing field. Labor unions are actively trying to be present but are structurally excluded from organizing independent contractors.
% DISAPPEARANCE_RATIONALE: If the formalist employment boundary vanished overnight, platform companies would face immediate reclassification of their workforce, incurring massive new labor costs and obligations. This would fundamentally alter their business model, likely leading to significant restructuring, price increases, or withdrawal from markets. The labor market would shift, with many platform workers gaining employment protections.
% FOUNDING_PROBLEM: The legal system needed a clear distinction between employees and independent contractors to determine tax obligations, labor rights, and employer responsibilities, particularly as new forms of work emerged that didn't fit traditional factory models.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and some legal scholars argue the problem is live, as new forms of work continue to challenge traditional definitions, requiring clear boundaries for innovation. Labor advocates and many economists argue the problem is 'dead' in the sense that the current formalist boundary is being exploited to externalize costs, and the original intent of protecting workers is being subverted; they point to the economic dependence of platform workers as evidence that the boundary no longer serves its original purpose fairly. Independent legal analysis and economic studies corroborate the latter view.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) is high because platform companies derive significant cost savings by avoiding employment-related expenses, effectively transferring these costs to workers and the public. Suppression (0.78) is high due to the legal and economic barriers preventing workers from challenging their classification or organizing, reinforced by active enforcement by platforms and supportive legal frameworks. The theater ratio (0.20) is relatively low, as the formalist distinction, while contested, is actively applied and defended, not merely performed. The metrics show a rising trend in extractiveness and suppression as the platform economy matures and the formalist interpretation hardens.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform companies, this constraint is a 'rope' or 'scaffold' that enables innovation and flexibility. From the perspective of platform workers and labor advocates, it is a 'snare' that extracts value and suppresses rights. The engine's classification will reflect the latter due to the high extractiveness and suppression metrics, despite the claimed type of 'snare' (which aligns with the victim's perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are clear beneficiaries (d=0.0-0.1) as they capture the economic gains from this classification. Platform workers and taxpayers (via the social safety net) are the primary targets (d=0.9-1.0), bearing the costs of precarity and externalized social welfare. Traditional employers and labor unions are excluded, experiencing the constraint as a competitive disadvantage or a barrier to their mission.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to provide clear legal distinctions for different work arrangements. However, under this formalist reading, the function has drifted from its original purpose to primarily facilitate cost externalization for platform companies. The 'contested' status of the founding problem and 'world_rearranges' disappearance verdict indicate a potential mandatrophy where the constraint persists not for its original coordination function, but for the extraction it enables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formal_control,
    'To what extent does algorithmic management and economic dependence constitute ''control'' equivalent to direct supervision, thereby challenging the formalist distinction?',
    'Empirical studies on the actual working conditions and autonomy of platform workers, and legal rulings that prioritize substantive control over formal contractual terms.',
    'If substantive control is recognized as equivalent, the formalist reading''s justification collapses, leading to reclassification of platform workers as employees and a significant shift in labor law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_formal_control, empirical, 'Ambiguity regarding the nature of ''control'' in platform work.').

omega_variable(
    social_cost_externalization,
    'What is the full societal cost of externalizing labor protections and benefits for platform workers, and how does it compare to the economic benefits claimed by platform companies?',
    'Comprehensive economic modeling that quantifies the burden on public services, lost tax revenue, and increased social inequality, offset against innovation and consumer benefits.',
    'If externalized costs significantly outweigh benefits, it strengthens the argument for reclassification or new regulatory frameworks to internalize these costs, undermining the formalist reading''s policy justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_externalization, empirical, 'Uncertainty about the true economic impact of platform worker classification.').

omega_variable(
    worker_preference_authenticity,
    'To what extent is platform workers'' stated preference for ''flexibility'' a genuine choice, versus a constrained choice driven by lack of alternatives or economic necessity?',
    'Surveys and qualitative studies that control for economic precarity and alternative employment opportunities, examining how preferences shift under different conditions.',
    'If ''flexibility'' is largely a constrained choice, it weakens the argument that platform workers genuinely choose independent contractor status, supporting claims of exploitation under the formalist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_preference_authenticity, empirical, 'Authenticity of worker preference for independent contractor status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__formalist_employment_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__formalist_employment_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__formalist_employment_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(empl_be_t5, employment_boundary__formalist_employment_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(empl_be_t10, employment_boundary__formalist_employment_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(empl_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(empl_be_t20, employment_boundary__formalist_employment_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(empl_su_t5, employment_boundary__formalist_employment_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(empl_su_t10, employment_boundary__formalist_employment_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(empl_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(empl_su_t20, employment_boundary__formalist_employment_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, gig_economy_regulatory_frameworks).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, social_safety_net_funding).

% DUAL FORMULATION NOTE:
% This constraint is the 'formalist_employment_reading' of the 'employment_boundary' kernel. It is linked to 'substantive_employment_reading' and 'hybrid_security_reading' as alternative interpretations of the same underlying legal and economic reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
