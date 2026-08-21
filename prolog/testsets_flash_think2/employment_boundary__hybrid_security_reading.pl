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
 *   This constraint defines a 'hybrid' worker classification for the platform
 *   economy, distinct from traditional employment and independent
 *   contracting. It aims to provide tailored protections for platform workers
 *   while preserving the flexibility desired by platforms. This story
 *   instantiates the 'hybrid_security_reading' of the broader
 *   'employment_boundary' kernel, focusing on the perspective that such a
 *   category is necessary and beneficial, even if imperfect. The metrics
 *   reflect a system that provides some coordination (basic protections) but
 *   also institutionalizes a degree of precarity and extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.55).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.65).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Worker Classification for Platform Economy").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'ecbb83fe-0665-428e-b016-495643b18f88').
narrative_ontology:cs_kernel_codification('ecbb83fe-0665-428e-b016-495643b18f88', formalized).
narrative_ontology:cs_authority_grounding('ecbb83fe-0665-428e-b016-495643b18f88', lineage).
narrative_ontology:cs_interpretation_layer_present('ecbb83fe-0665-428e-b016-495643b18f88').
narrative_ontology:cs_reading_relation('ecbb83fe-0665-428e-b016-495643b18f88', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecbb83fe-0665-428e-b016-495643b18f88', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('ecbb83fe-0665-428e-b016-495643b18f88', foundational, platform_work_is_distinct).
narrative_ontology:cs_axiom_status(platform_work_is_distinct, holdable).
narrative_ontology:cs_axiom_grounding('ecbb83fe-0665-428e-b016-495643b18f88', platform_work_is_distinct, conventional).
narrative_ontology:cs_axiom('ecbb83fe-0665-428e-b016-495643b18f88', foundational, basic_protections_are_necessary).
narrative_ontology:cs_axiom_status(basic_protections_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ecbb83fe-0665-428e-b016-495643b18f88', basic_protections_are_necessary, deontological).
narrative_ontology:cs_reference_frame('ecbb83fe-0665-428e-b016-495643b18f88', balancing_flexibility_and_security).
narrative_ontology:cs_drift_state('ecbb83fe-0665-428e-b016-495643b18f88', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ecbb83fe-0665-428e-b016-495643b18f88', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, governments_regulators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, consumers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, traditional_employees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the flexibility of a non-employment classification, avoiding full employment costs (e.g., benefits, payroll taxes) while retaining significant control over workers through algorithmic management. They actively lobby for and shape hybrid classification models.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive some basic protections (e.g., injury insurance, minimum wage in some jurisdictions) but lack comprehensive employment benefits like career development, retirement security, and robust collective bargaining rights. Their precarity is institutionalized by the hybrid status.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    powerless, biographical, constrained, local).

% Face potential downward pressure on wages and benefits in sectors where platform work competes with traditional employment, as the hybrid category creates a lower-cost labor pool that can undermine established labor standards.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employees, payer,
    organized, biographical, constrained, national).

% Advocate for full employment status for platform workers, arguing that hybrid models institutionalize precarity. They are often excluded from direct negotiation or policy-making processes that establish these hybrid classifications, despite their significant stake.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Seek to balance economic innovation, worker protection, and fiscal stability. They define and enforce worker categories, aiming to provide a legal framework for platform work that addresses its unique characteristics without stifling growth or creating excessive liabilities.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, governments_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the flexibility and often lower costs of on-demand platform services, which are enabled by the labor model. They may indirectly bear some costs if protections are passed through, but generally experience convenience and choice.
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
% COORDINATION_FUNCTION: To provide a legal and social framework for platform work that acknowledges its unique characteristics, ensuring some basic protections for workers while maintaining operational flexibility for platform companies.
% TRANSFER_FUNCTION: Transfers some responsibility for worker welfare (e.g., injury insurance, minimum earnings) from individual workers to platform companies, while simultaneously transferring the cost of avoiding full employment benefits (e.g., retirement, career development) from platform companies to workers.
% ABSENT_VOICES: Labor unions and advocates for full employment status for all workers are often marginalized in the policy debates around hybrid classifications. They would argue that this 'third category' is a strategic move to avoid full labor responsibilities, rather than a genuine innovation.
% DISAPPEARANCE_RATIONALE: If this hybrid classification vanished overnight, platform companies would be forced to either classify all workers as employees (significantly increasing costs and potentially reducing service flexibility) or revert to a purely independent contractor model (removing all protections). This would lead to a major restructuring of the platform economy, labor markets, and consumer services.
% FOUNDING_PROBLEM: The rapid growth of the platform economy created a large class of workers who did not fit neatly into existing legal categories of 'employee' or 'independent contractor,' leading to legal ambiguity, lack of basic protections, and calls for regulatory clarity.
% FOUNDING_PROBLEM_CORROBORATION: Governments, labor organizations, and platform companies all acknowledge the initial problem of classification ambiguity. Academic research, policy papers, and legislative debates from independent bodies corroborate the need for a new framework, though they dispute the optimal solution and the extent to which the problem remains 'live' versus 'solved' by existing hybrid models.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.55) is moderate because while some protections are offered, the hybrid status often falls short of full employment benefits, leaving workers with significant precarity. Suppression (0.65) is substantial, as the legal framework actively prevents full employment classification and limits collective bargaining for platform workers. The theater ratio (0.40) indicates that while genuine protections exist, a portion of the 'protection' narrative serves to legitimize a lower-cost labor model. The measurement series shows a slight increase in extractiveness and suppression over time, suggesting that as the hybrid model matures, it may lean more towards extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform companies and some regulators, this hybrid classification is a necessary and innovative solution to a new economic reality, providing a 'rope' for coordination. From the perspective of platform workers and labor advocates, it functions more as a 'tangled_rope' or even a 'snare,' institutionalizing precarity and extracting value under the guise of flexibility and tailored protections. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and governments/regulators are beneficiaries and agenda-setters, shaping the rules to balance their interests (flexibility, stability). Consumers benefit from the services. Platform workers and traditional employees are victims, bearing the costs of precarity and downward pressure on labor standards. Labor unions are excluded, their preferred outcome (full employment) being actively suppressed by the hybrid framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_category_legitimacy,
    'Is the ''hybrid'' worker category a genuine innovation in labor law, or a strategic compromise that institutionalizes precarity and undermines existing labor protections?',
    'Longitudinal studies comparing the welfare outcomes of platform workers under hybrid models versus full employment or pure independent contractor models, alongside legal analysis of the ''distinctness'' claims.',
    'If primarily a strategic compromise, the constraint''s effective extractiveness and suppression would be higher, pushing its classification closer to a Snare. If a genuine innovation, the coordination function would be more prominent, supporting a Rope or Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_category_legitimacy, conceptual, 'Whether the hybrid classification is a legitimate third category or a means to avoid full labor responsibilities.').

omega_variable(
    effectiveness_of_tailored_protections,
    'Do the ''tailored protections'' (e.g., medical 91.5%, injury 86.2%) genuinely improve platform workers'' welfare to a degree comparable to full employment benefits, or are they insufficient?',
    'Empirical studies comparing the comprehensive welfare (health, retirement, career progression, income stability) of platform workers under hybrid models against traditional employees and independent contractors.',
    'If protections are found to be largely insufficient, the extractiveness metric would be higher, reflecting the uncompensated costs borne by workers. If highly effective, extractiveness would be lower, supporting the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_tailored_protections, empirical, 'Assessment of whether tailored protections adequately address platform worker welfare.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of full employment status for platform workers primarily structural (legal definitions, platform power) or internalized (workers'' preference for flexibility, lack of awareness of full benefits)?',
    'Post-exit suppression trajectory: if workers continue to prefer flexible, unprotected work even after structural barriers to full employment are removed, reclassify as partially internalized. Surveys on worker preferences and understanding of benefits.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — workers carry the suppression with them after exit. If purely structural, removing legal barriers would significantly alter worker choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for full employment status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__hybrid_security_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__hybrid_security_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__hybrid_security_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(empl_be_t5, employment_boundary__hybrid_security_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(empl_be_t10, employment_boundary__hybrid_security_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(empl_be_t15, employment_boundary__hybrid_security_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(empl_su_t5, employment_boundary__hybrid_security_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(empl_su_t10, employment_boundary__hybrid_security_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(empl_su_t15, employment_boundary__hybrid_security_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, minimum_wage_laws).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, social_security_eligibility).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, collective_bargaining_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
