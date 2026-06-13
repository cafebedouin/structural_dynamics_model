% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market Efficiency (Market Efficiency Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint models the 'market efficiency' reading of flexible
 *   employment, where it is viewed as a legitimate mechanism for matching
 *   labor supply and demand, maximizing worker autonomy, and delivering
 *   consumer convenience. This reading emphasizes the coordination function
 *   of platforms and downplays any extractive or precarious aspects. It is
 *   one reading of the 'flexible_employment_legitimacy' kernel, which also
 *   includes 'precarity_extraction_reading' and
 *   'developmental_state_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.35).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.2).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market Efficiency (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '56cba968-0794-4a33-8154-ee750ec2b8d6').
narrative_ontology:cs_kernel_codification('56cba968-0794-4a33-8154-ee750ec2b8d6', implicit).
narrative_ontology:cs_authority_grounding('56cba968-0794-4a33-8154-ee750ec2b8d6', practice).
narrative_ontology:cs_interpretation_layer_present('56cba968-0794-4a33-8154-ee750ec2b8d6').
narrative_ontology:cs_reading_relation('56cba968-0794-4a33-8154-ee750ec2b8d6', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('56cba968-0794-4a33-8154-ee750ec2b8d6', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('56cba968-0794-4a33-8154-ee750ec2b8d6', foundational, labor_market_clearing_is_efficient).
narrative_ontology:cs_axiom_status(labor_market_clearing_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('56cba968-0794-4a33-8154-ee750ec2b8d6', labor_market_clearing_is_efficient, empirically_contingent).
narrative_ontology:cs_axiom('56cba968-0794-4a33-8154-ee750ec2b8d6', foundational, worker_autonomy_is_maximized).
narrative_ontology:cs_axiom_status(worker_autonomy_is_maximized, holdable).
narrative_ontology:cs_axiom_grounding('56cba968-0794-4a33-8154-ee750ec2b8d6', worker_autonomy_is_maximized, deontological).
narrative_ontology:cs_reference_frame('56cba968-0794-4a33-8154-ee750ec2b8d6', perfect_competition_labor_market).
narrative_ontology:cs_drift_state('56cba968-0794-4a33-8154-ee750ec2b8d6', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('56cba968-0794-4a33-8154-ee750ec2b8d6', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate digital platforms that connect workers with tasks, framing flexible employment as an efficient matching service. They benefit from low overheads and a readily available labor pool, and assert their algorithms are neutral market facilitators.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Value the autonomy and flexibility offered by platform work, using it to supplement income, manage personal schedules, or bridge employment gaps. They perceive the arrangement as a choice that maximizes their personal utility and control over their labor.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, beneficiary,
    moderate, biographical, mobile, local).

% Benefit from convenient, on-demand services at competitive prices, enabled by the efficient allocation of labor through flexible employment models. They are largely unaware of the underlying labor conditions.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers, beneficiary,
    organized, immediate, arbitrage, local).

% Face competition from platform companies that operate with lower labor costs and fewer regulatory burdens. They are pressured to adapt their employment models or lose market share, perceiving the flexible model as a threat to established labor standards.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers, payer,
    powerful, biographical, constrained, national).

% Are largely excluded from organizing flexible workers due to their classification as independent contractors. They would advocate for reclassification and collective bargaining rights, but their voice is marginalized within this market-efficiency framework.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Efficiently matches available labor supply to fluctuating consumer demand for services, minimizing transaction costs and maximizing worker autonomy in choosing tasks.
% TRANSFER_FUNCTION: Facilitates direct payment from consumers to workers for services, with a platform fee deducted by the platform company for coordination and infrastructure.
% ABSENT_VOICES: Labor unions and advocates for traditional employment standards are largely absent from the framing of flexible employment as a purely efficient market mechanism; they would highlight the erosion of worker protections and the concentration of power in platform companies.
% DISAPPEARANCE_RATIONALE: If the legitimacy of flexible employment as a market-clearing mechanism vanished, platform companies would face immediate regulatory challenges, potentially leading to reclassification of workers, increased labor costs, and a restructuring of the on-demand service economy. Workers would lose perceived autonomy, and consumers would face higher prices or reduced service availability.
% FOUNDING_PROBLEM: Traditional labor markets were rigid, with high transaction costs for short-term tasks and limited flexibility for workers seeking non-standard hours or supplementary income.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and many flexible workers attest that the problem of rigid labor markets and the need for flexible income opportunities remains live. Critics (labor economists, social policy researchers) acknowledge the historical problem but argue the current 'solution' has created new, more severe problems, making the status contested.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the platform's take rate as a necessary cost for market coordination, rather than pure rent. Suppression (0.20) is low, as workers are framed as having high autonomy and exit options. Theater ratio (0.10) is low, as the market-matching function is considered genuine. The metrics reflect the internal logic of the market efficiency reading, not an external critique.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform companies and many flexible workers, this arrangement is a beneficial rope, offering efficiency and autonomy. From the perspective of traditional employers and labor unions (who are largely excluded from this framing), it would be seen as a more extractive or precarious arrangement. This reading prioritizes the 'choice' and 'efficiency' aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are agenda-setters and beneficiaries, as they design and profit from the system. Flexible workers are also beneficiaries, valuing the autonomy and supplemental income. Consumers benefit from convenience. Traditional employers are payers, facing competitive pressure. Labor unions are excluded, unable to influence the terms of engagement within this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as extraction by emphasizing the genuine market-clearing function and worker autonomy. It assumes the founding problem of rigid labor markets is still live and that flexible employment is an appropriate solution, thus not mandatrohpic from its own perspective. The low theater ratio reflects this belief in the constraint's functional integrity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_efficiency_vs_precarity,
    'Is flexible employment primarily a market-clearing mechanism that enhances efficiency and worker autonomy, or does it primarily create structural precarity that enables platform extraction?',
    'Longitudinal studies tracking worker income stability, access to benefits, and bargaining power, alongside analysis of platform profit margins and market concentration.',
    'If resolved towards precarity, the constraint would reclassify towards a Snare or Tangled Rope, with significantly higher extractiveness and suppression. If resolved towards market efficiency, the current Rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_vs_precarity, empirical, 'Ambiguity regarding the primary structural effect of flexible employment.').

omega_variable(
    platform_algorithm_neutrality,
    'Are platform algorithms truly neutral market facilitators, or do they embed biases that favor platform interests and disempower workers?',
    'Audits of platform algorithms by independent researchers, examining pricing, task allocation, and worker rating systems for evidence of bias or manipulation.',
    'Evidence of algorithmic bias would undermine the ''neutral coordination'' claim, increasing perceived extractiveness and suppression, potentially shifting classification towards Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_algorithm_neutrality, empirical, 'Whether platform algorithms are neutral or biased.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''market_efficiency_reading'' of the ''flexible_employment_legitimacy'' kernel. How would the classification change if the ''precarity_extraction_reading'' or ''developmental_state_reading'' were adopted?',
    'Adopting the ''precarity_extraction_reading'' would significantly increase extractiveness and suppression, likely leading to a Snare classification. The ''developmental_state_reading'' would likely classify it as a Scaffold, emphasizing its transitional nature and the need for state intervention.',
    'The classification would shift dramatically, reflecting the different structural interpretations of flexible employment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative kernel readings on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2010, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(flex_tr_t2014, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2014, 0.08).
narrative_ontology:measurement(flex_tr_t2018, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2018, 0.09).
narrative_ontology:measurement(flex_tr_t2024, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(flex_be_t2010, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(flex_be_t2014, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(flex_be_t2018, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2018, 0.3).
narrative_ontology:measurement(flex_be_t2024, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2010, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(flex_su_t2014, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2014, 0.15).
narrative_ontology:measurement(flex_su_t2018, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2018, 0.18).
narrative_ontology:measurement(flex_su_t2024, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
