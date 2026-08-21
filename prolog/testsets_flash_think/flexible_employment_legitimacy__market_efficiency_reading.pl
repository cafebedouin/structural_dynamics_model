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
 *   This constraint story instantiates the 'market efficiency' reading of
 *   flexible employment, which views it as a legitimate and beneficial
 *   mechanism for matching labor supply and demand. From this perspective,
 *   flexible employment optimizes resource allocation, provides autonomy to
 *   workers, and delivers cost-effective services to consumers. The metrics
 *   reflect this framing, portraying the arrangement as a coordination
 *   mechanism with low extraction and suppression.
 *
 * KEY AGENTS:
 *   - platform_companies: Agenda-setter/Beneficiary (institutional/arbitrage) — facilitate market, benefit from efficiency
 *   - flexible_workers: Beneficiary (moderate/mobile) — choose flexible work for autonomy/income
 *   - consumers: Beneficiary (organized/mobile) — benefit from lower costs/availability
 *   - traditional_employers: Payer (powerful/constrained) — adapt to competitive labor market
 *   - labor_unions: Excluded (organized/constrained) — less relevant in flexible market
 *   - economists_market_efficiency_school: Observer (analytical/analytical) — analyze market outcomes, advocate deregulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.25).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.15).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'b70b2a5e-fbb7-41ed-9f7f-3d84baded09d').
narrative_ontology:cs_kernel_codification('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', implicit).
narrative_ontology:cs_authority_grounding('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', self_enforcing).
narrative_ontology:cs_reading_relation('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', foundational, labor_market_clears_efficiently).
narrative_ontology:cs_axiom_status(labor_market_clears_efficiently, holdable).
narrative_ontology:cs_axiom_grounding('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', labor_market_clears_efficiently, empirically_contingent).
narrative_ontology:cs_axiom('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', foundational, worker_autonomy_maximizes_utility).
narrative_ontology:cs_axiom_status(worker_autonomy_maximizes_utility, holdable).
narrative_ontology:cs_axiom_grounding('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', worker_autonomy_maximizes_utility, empirically_contingent).
narrative_ontology:cs_reference_frame('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', perfect_competition_labor_market).
narrative_ontology:cs_drift_state('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', contemporary_platform_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b70b2a5e-fbb7-41ed-9f7f-3d84baded09d', '').
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

% Develop and operate platforms that connect workers with demand, enabling efficient market clearing. They benefit from transaction fees and market growth, seen as a fair return for innovation and coordination.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Choose flexible work for autonomy, supplemental income, or as a bridge to other opportunities. They benefit from low barriers to entry and the ability to set their own hours, maximizing personal utility.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, beneficiary,
    moderate, biographical, mobile, local).

% Benefit from lower costs, increased availability, and convenience of services enabled by flexible labor markets. Their choices drive demand and signal market needs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers, beneficiary,
    organized, immediate, mobile, local).

% Face increased competition for labor, which may require adjusting wages or benefits to attract talent. They adapt to the evolving labor market dynamics.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers, payer,
    powerful, biographical, constrained, national).

% Represent traditional employment models and collective bargaining. From this reading, their concerns about precarity are seen as resistance to market efficiency and worker autonomy, making their voice less relevant to the core function.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Analyze labor market outcomes, advocating for policies that reduce friction and enable efficient matching of supply and demand. They view flexible employment as a positive development.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, economists_market_efficiency_school, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches diverse labor supply (seeking flexibility, supplemental income, or temporary work) with fluctuating demand for services, optimizing resource allocation in the labor market.
% TRANSFER_FUNCTION: Facilitates direct payment for services from consumers to flexible workers, with a small, efficiency-justified platform fee accruing to platform companies for coordination and infrastructure.
% ABSENT_VOICES: Labor unions and advocates for traditional employment benefits would object, arguing that flexible employment undermines worker protections and creates precarity. However, this reading prioritizes individual worker choice and market efficiency.
% DISAPPEARANCE_RATIONALE: If flexible employment vanished overnight, many services would become significantly more expensive or unavailable due to inefficient labor matching. Many workers would lose a preferred source of income and autonomy, and platforms would lose their core business model, leading to a major reorganization of the service economy.
% FOUNDING_PROBLEM: Inefficient and rigid traditional labor markets, leading to underemployment for those seeking flexible work and unmet demand for services requiring adaptable labor supply.
% FOUNDING_PROBLEM_CORROBORATION: Economists from the market efficiency school and platform companies consistently attest to the ongoing problem of labor market rigidities and the benefits of flexible solutions. Many flexible workers also corroborate the value of the autonomy and income opportunities provided.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.25) reflects the view that flexible employment represents a fair exchange of value, with platform fees justified by coordination and infrastructure costs. Suppression (0.15) is low because workers are seen as freely choosing flexible arrangements, with ample exit options. The theater ratio (0.08) is minimal, as the core function of market clearing is considered genuine and effective. Accessibility collapse is moderate (0.40) because flexible work is one option among others, not a forced choice. Resistance is low (0.10) because the arrangement is seen as mutually beneficial by its participants.
 *
 * PERSPECTIVAL GAP:
 *   This story explicitly adopts the market efficiency perspective. Other readings, such as the 'precarity extraction' reading, would assign significantly higher extractiveness and suppression, viewing flexible employment as a mechanism for surplus value extraction. The 'developmental state' reading would see it as a transitional form requiring state management. The engine's classification will highlight the divergence between this claimed 'rope' and the potentially more extractive classifications from other perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are clear beneficiaries, collecting fees for facilitating an efficient market. Flexible workers are also beneficiaries, as they gain autonomy and income opportunities that suit their needs. Consumers benefit from efficient, cost-effective services. Traditional employers are indirect payers as they must adapt to a more competitive labor market. Labor unions are excluded, as their model is less relevant to this market structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine market-clearing mechanism, or is it a form of structural precarity or a transitional state?',
    'Empirical studies on worker welfare, income stability, and long-term career progression for flexible workers, compared against traditional employment models, alongside analysis of platform power dynamics and algorithmic control.',
    'If reclassified as precarity, extractiveness and suppression would be significantly higher, shifting the type towards Snare or Tangled Rope. If reclassified as transitional, it would become a Scaffold, implying a sunset clause and state intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between market efficiency, precarity, and transitional state readings of flexible employment.').

omega_variable(
    worker_choice_vs_structural_constraint,
    'To what extent is ''flexible worker autonomy'' a genuine choice, versus a structural constraint imposed by limited alternative employment options or economic necessity?',
    'Surveys and qualitative studies on worker motivations, exit options, and the availability of traditional employment with comparable income and benefits. Analysis of labor market conditions and unemployment rates.',
    'If worker choice is found to be substantially constrained, the suppression metric would increase, and the directionality for flexible_workers would shift towards ''target'', increasing effective extraction from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_choice_vs_structural_constraint, empirical, 'The degree of genuine choice for flexible workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2000, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(flex_tr_t2005, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2005, 0.06).
narrative_ontology:measurement(flex_tr_t2010, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2010, 0.07).
narrative_ontology:measurement(flex_tr_t2015, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(flex_tr_t2020, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(flex_tr_t2025, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2025, 0.08).
narrative_ontology:measurement(flex_tr_t2030, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2030, 0.08).

% Extraction over time
narrative_ontology:measurement(flex_be_t2000, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(flex_be_t2005, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(flex_be_t2010, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(flex_be_t2015, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2015, 0.24).
narrative_ontology:measurement(flex_be_t2020, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(flex_be_t2025, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2025, 0.25).
narrative_ontology:measurement(flex_be_t2030, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2030, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2000, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(flex_su_t2005, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(flex_su_t2010, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2010, 0.13).
narrative_ontology:measurement(flex_su_t2015, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2015, 0.14).
narrative_ontology:measurement(flex_su_t2020, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(flex_su_t2025, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2025, 0.15).
narrative_ontology:measurement(flex_su_t2030, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2030, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, traditional_employment_regulation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, social_safety_nets).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'flexible_employment_legitimacy' kernel, focusing on market efficiency. It is linked to its sibling readings which offer alternative interpretations of the same phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
