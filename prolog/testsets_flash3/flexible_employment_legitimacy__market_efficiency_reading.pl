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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'market efficiency' reading of flexible
 *   employment, where it is viewed as a legitimate and beneficial mechanism
 *   for matching labor supply to demand. This reading emphasizes worker
 *   autonomy, consumer choice, and the efficiency gains from digital
 *   platforms. It is one reading of the 'flexible_employment_legitimacy'
 *   kernel, distinct from 'precarity_extraction_reading' and
 *   'developmental_state_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.25).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.3).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'a5266784-5bf3-4061-b45a-5f673754daa0').
narrative_ontology:cs_kernel_codification('a5266784-5bf3-4061-b45a-5f673754daa0', implicit).
narrative_ontology:cs_authority_grounding('a5266784-5bf3-4061-b45a-5f673754daa0', practice).
narrative_ontology:cs_interpretation_layer_present('a5266784-5bf3-4061-b45a-5f673754daa0').
narrative_ontology:cs_reading_relation('a5266784-5bf3-4061-b45a-5f673754daa0', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5266784-5bf3-4061-b45a-5f673754daa0', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('a5266784-5bf3-4061-b45a-5f673754daa0', foundational, labor_is_a_commodity).
narrative_ontology:cs_axiom_status(labor_is_a_commodity, holdable).
narrative_ontology:cs_axiom_grounding('a5266784-5bf3-4061-b45a-5f673754daa0', labor_is_a_commodity, conventional).
narrative_ontology:cs_axiom('a5266784-5bf3-4061-b45a-5f673754daa0', foundational, individual_autonomy_maximizes_welfare).
narrative_ontology:cs_axiom_status(individual_autonomy_maximizes_welfare, holdable).
narrative_ontology:cs_axiom_grounding('a5266784-5bf3-4061-b45a-5f673754daa0', individual_autonomy_maximizes_welfare, deontological).
narrative_ontology:cs_reference_frame('a5266784-5bf3-4061-b45a-5f673754daa0', perfectly_efficient_labor_market).
narrative_ontology:cs_drift_state('a5266784-5bf3-4061-b45a-5f673754daa0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a5266784-5bf3-4061-b45a-5f673754daa0', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the digital platforms that connect workers to tasks, setting the terms of engagement and algorithmically matching supply and demand. They benefit from low overhead and a flexible labor pool, claiming to provide efficient market clearing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Choose when and where to work, valuing the autonomy and supplemental income. They see platforms as providing access to work that fits their schedules and skills, and view wage fluctuations as normal market signals.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, beneficiary,
    moderate, biographical, mobile, local).

% Benefit from convenient, on-demand services at competitive prices. They perceive the system as efficient and responsive to their needs, with flexible labor enabling this responsiveness.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers, beneficiary,
    organized, immediate, mobile, local).

% Face competition from flexible labor models, which can offer lower prices due to reduced overhead from benefits and fixed wages. They must adapt their own labor practices or risk losing market share, viewing flexible employment as a disruptive but legitimate market force.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers, payer,
    powerful, biographical, constrained, national).

% Analyze flexible employment through the lens of supply and demand, viewing it as an efficient mechanism for allocating labor resources. They focus on aggregate welfare gains and the responsiveness of labor markets to technological change.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_economists_market_efficiency_school, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a highly granular and dynamic matching of labor supply (individual workers) to demand (tasks from consumers/businesses), optimizing for efficiency and responsiveness in a decentralized manner.
% TRANSFER_FUNCTION: Facilitates direct payment for tasks from consumers to workers, with a portion (platform fee) transferred to platform companies for coordination services. It also transfers flexibility and autonomy to workers, and cost savings to consumers.
% ABSENT_VOICES: Workers seeking traditional employment benefits and protections, and labor unions advocating for collective bargaining, are not central to this reading's framing of market efficiency. They would argue for reclassification of workers and stronger labor protections.
% DISAPPEARANCE_RATIONALE: If flexible employment platforms and the underlying market-clearing mechanisms vanished, a significant portion of on-demand services would disappear, leading to higher costs and reduced convenience for consumers, and a loss of flexible income opportunities for workers. The labor market would become less fluid and responsive.
% FOUNDING_PROBLEM: Traditional labor markets were rigid, inefficient in matching granular demand with supply, and offered limited flexibility for workers seeking non-standard work arrangements.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and many flexible workers attest that traditional labor markets remain rigid and that flexible employment continues to solve the problem of matching granular demand with supply. Consumers corroborate the value of on-demand services. Labor economists from the market efficiency school provide theoretical and empirical support for the ongoing problem and solution.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.25) is considered low, representing the necessary costs of platform operation and market coordination rather than surplus extraction. Suppression (0.30) is also low, as workers are seen as freely choosing flexible arrangements with ample exit options. Theater ratio (0.10) is minimal, reflecting a belief that the stated function (market clearing) genuinely aligns with actual operation. The metrics reflect the internal logic of the market efficiency reading.
 *
 * PERSPECTIVAL GAP:
 *   From this reading's perspective, the system is a net positive for all participants, with costs representing fair market prices for coordination and services. Other readings would highlight the asymmetric power dynamics and the costs borne by workers in terms of benefits and job security, leading to different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies, consumers, and flexible workers are all considered beneficiaries, as the system provides value to each. Traditional employers are payers, as they must adapt to the competitive pressure of flexible labor. Labor economists from the market efficiency school serve as analytical observers, validating the system's efficiency.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_worker_autonomy_vs_necessity,
    'To what extent is worker participation in flexible employment truly autonomous choice, versus a necessity driven by lack of traditional employment options or economic precarity?',
    'Longitudinal studies tracking worker motivations, alternative employment opportunities, and economic security before and after entering flexible work. Analysis of exit rates and reasons for leaving flexible platforms.',
    'If participation is largely driven by necessity, the ''flexible_workers'' seat''s exit_options would shift from ''mobile'' to ''constrained'' or ''identity_locked'' (due to economic dependence), increasing their effective extraction and potentially reclassifying the constraint from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_worker_autonomy_vs_necessity, empirical, 'Assesses the genuine voluntariness of flexible work participation.').

omega_variable(
    platform_algorithm_neutrality_vs_bias,
    'Are platform algorithms truly neutral market-clearing mechanisms, or do they embed biases that disproportionately benefit platforms or certain worker/consumer groups?',
    'Audits of platform algorithms by independent researchers, analysis of wage-setting mechanisms, and examination of task allocation patterns for evidence of systemic bias or rent-seeking beyond stated coordination functions.',
    'Evidence of algorithmic bias or non-neutrality would increase the ''platform_companies'' seat''s effective extraction and could shift the constraint''s classification towards a ''tangled_rope'' or ''snare'' from the perspective of disadvantaged workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_algorithm_neutrality_vs_bias, empirical, 'Examines the fairness and neutrality of platform matching algorithms.').

omega_variable(
    market_efficiency_vs_precarity_framing,
    'Is flexible employment primarily an efficient market-clearing mechanism, or does it fundamentally create and exploit worker precarity?',
    'This is a conceptual omega. Resolution depends on the normative framework adopted: prioritizing aggregate economic efficiency and individual flexibility (market_efficiency_reading) versus prioritizing worker security and social welfare (precarity_extraction_reading). No single empirical test resolves this framing conflict.',
    'Adopting the precarity framing would fundamentally reclassify the constraint as a ''snare'' or ''tangled_rope'' from the perspective of workers, with significantly higher extractiveness and suppression metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_efficiency_vs_precarity_framing, conceptual, 'The core conceptual conflict between market efficiency and worker precarity framings of flexible employment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'market_efficiency_reading' of the 'flexible_employment_legitimacy' kernel. It emphasizes efficiency and autonomy, contrasting with the 'precarity_extraction_reading' (focus on exploitation) and 'developmental_state_reading' (focus on state-led formalization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
