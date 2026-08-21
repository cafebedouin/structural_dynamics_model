% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy: Market Libertarian Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint represents a market-libertarian reading of AI governance
 *   legitimacy, asserting that legitimacy derives from voluntary exchange and
 *   property rights. It claims that innovation flourishes when unencumbered
 *   by collective mandates and that dignity is protected through exit options
 *   and competitive markets, not centralized oversight. It interprets the
 *   encyclical's subsidiarity principle as supporting decentralization but
 *   rejects its solidarity demands as illegitimate coercion. The constraint
 *   is claimed as a 'mountain' because it frames property rights and
 *   voluntary exchange as pre-political, natural laws.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy: Market Libertarian Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '387e813a-9281-4c40-a8cd-5a09e41c3953').
narrative_ontology:cs_kernel_codification('387e813a-9281-4c40-a8cd-5a09e41c3953', fixed_text).
narrative_ontology:cs_authority_grounding('387e813a-9281-4c40-a8cd-5a09e41c3953', practice).
narrative_ontology:cs_reading_relation('387e813a-9281-4c40-a8cd-5a09e41c3953', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('387e813a-9281-4c40-a8cd-5a09e41c3953', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('387e813a-9281-4c40-a8cd-5a09e41c3953', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_axiom('387e813a-9281-4c40-a8cd-5a09e41c3953', foundational, private_property_pre_political_right).
narrative_ontology:cs_axiom_status(private_property_pre_political_right, holdable).
narrative_ontology:cs_axiom_grounding('387e813a-9281-4c40-a8cd-5a09e41c3953', private_property_pre_political_right, deontological).
narrative_ontology:cs_axiom('387e813a-9281-4c40-a8cd-5a09e41c3953', foundational, voluntary_exchange_maximizes_welfare).
narrative_ontology:cs_axiom_status(voluntary_exchange_maximizes_welfare, holdable).
narrative_ontology:cs_axiom_grounding('387e813a-9281-4c40-a8cd-5a09e41c3953', voluntary_exchange_maximizes_welfare, empirically_contingent).
narrative_ontology:cs_reference_frame('387e813a-9281-4c40-a8cd-5a09e41c3953', unfettered_market_order).
narrative_ontology:cs_drift_state('387e813a-9281-4c40-a8cd-5a09e41c3953', contemporary_regulatory_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('387e813a-9281-4c40-a8cd-5a09e41c3953', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, those_lacking_market_power).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, private_property_rights).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, free_market_principles).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, individual_liberty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from minimal regulatory burdens, allowing rapid innovation and market entry. Their success is seen as a natural outcome of voluntary exchange and property rights.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, global).

% Profit from unencumbered capital allocation and the growth of AI ventures. They view market mechanisms as the most efficient means of allocating resources and generating returns.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Value the freedom to choose, innovate, and transact without collective mandates or centralized oversight. Their dignity is seen as protected by their ability to exit undesirable arrangements.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Bear the costs of market failures, lack of collective action, and the absence of social safety nets. Their limited exit options mean they must accept market terms, even if unfavorable.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, those_lacking_market_power, payer,
    powerless, immediate, constrained, local).

% Struggle to address collective problems (e.g., environmental impacts, social inequality) that market mechanisms alone do not resolve. They lack the means for collective mandates that this reading rejects.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    powerless, generational, trapped, local).

% Experience reduced bargaining power and limited alternatives in labor markets dominated by a few employers. Their 'voluntary' exchange is constrained by structural dependencies.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets, payer,
    powerless, biographical, trapped, local).

% Actively promote the principles of voluntary exchange, property rights, and minimal state intervention as the foundation for legitimate AI governance. They frame these principles as pre-political and universally applicable.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, market_libertarian_advocates, agenda_setter,
    organized, generational, analytical, global).

% Analyze the structural implications of market-libertarian AI governance, assessing its claims against empirical outcomes and alternative ethical frameworks. They are not directly subject to its enforcement but can critique its legitimacy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates innovation, resource allocation, and individual action through decentralized market mechanisms, price signals, and voluntary contracts, minimizing the need for centralized planning.
% TRANSFER_FUNCTION: Transfers wealth, opportunity, and decision-making power from collective bodies and those lacking capital/market power to entrepreneurs, investors, and individuals with high autonomy and resources.
% ABSENT_VOICES: Those advocating for collective mandates, social safety nets, or non-market forms of dignity (e.g., universal basic income, public goods provision) are excluded from the core legitimacy framework, dismissed as advocating for 'illegitimate coercion'.
% DISAPPEARANCE_RATIONALE: If the principles of voluntary exchange and property rights vanished overnight, the entire market system, including AI innovation and investment, would collapse. Economic activity would cease, and society would have to fundamentally reorganize its production and distribution mechanisms.
% FOUNDING_PROBLEM: To foster innovation, protect individual liberty, and ensure efficient resource allocation by preventing state overreach and collective mandates that stifle progress and infringe on natural rights.
% FOUNDING_PROBLEM_CORROBORATION: Market economists, libertarian think tanks, and proponents of free-market policies attest that the problem of state overreach and inefficient collective mandates remains live, citing ongoing regulatory proposals and historical examples of market success versus government failure. Critics, however, argue the problem has shifted to market power concentration.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) and suppression (0.15) reflect the reading's claim that market outcomes are natural and voluntary, not coercive. The high accessibility_collapse (0.85) and low resistance (0.10) further support the 'mountain' claim, implying that alternatives to market-based governance are inherently limited or illegitimate. The low theater_ratio (0.10) suggests that the mechanisms (contract law, private arbitration) are seen as directly functional in upholding these 'natural' principles, rather than performative. The metrics are kept flat to reflect the reading's assertion of timeless, unchanging principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the constraint is a natural, efficient system that protects liberty and fosters innovation. From the perspective of victims, the same system can appear highly extractive and suppressive, as their 'voluntary' participation is often constrained by structural inequalities and limited alternatives. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-autonomy individuals are structural beneficiaries, as the constraint's operation directly facilitates their activities and protects their gains. Those lacking market power, communities facing coordination failures, and workers in monopsony labor markets are structural targets, as they bear the costs of market outcomes without the means to shape them. Market-libertarian advocates act as agenda-setters, actively promoting and defending this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling market outcomes as 'extraction' or 'coercion' by framing them as natural consequences of individual liberty and property rights. It asserts that any attempt to impose collective mandates or centralized oversight would itself be an illegitimate form of coercion, thus preventing mandatrophy by rejecting the premise of collective mandates as a legitimate 'mandate' in the first place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_market,
    'Is the market-libertarian framework for AI governance a genuine natural law (mountain) derived from pre-political rights, or a constructed constraint (snare/tangled_rope) that benefits identifiable agents?',
    'Comparative historical analysis of market evolution and the role of state-enforced property rights; philosophical inquiry into the foundations of ''natural'' rights versus social constructs.',
    'If found to be a constructed constraint, its classification would shift from mountain to a more extractive type (e.g., tangled_rope or snare), highlighting the active enforcement and asymmetric benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_market, conceptual, 'Ambiguity of market principles as natural law versus social construct.').

omega_variable(
    voluntary_exchange_in_unequal_power,
    'To what extent is ''voluntary exchange'' truly voluntary when there are significant power asymmetries and limited exit options for some parties?',
    'Empirical studies of labor markets, platform economies, and access to essential services, focusing on the real-world choices and constraints faced by those with less market power.',
    'If ''voluntary'' is found to be highly constrained for many, the effective extractiveness and suppression of the constraint would be higher than currently measured, potentially shifting its classification towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_exchange_in_unequal_power, empirical, 'The actual voluntariness of market exchanges under power imbalances.').

omega_variable(
    subsidiarity_solidarity_tension,
    'Is the encyclical''s solidarity principle an ''illegitimate coercion'' (as this reading claims), or a necessary complement to subsidiarity for ensuring human dignity and the common good?',
    'Theological and ethical analysis of Catholic Social Doctrine, examining the internal coherence and historical interpretation of subsidiarity and solidarity.',
    'If solidarity is deemed a legitimate and necessary principle, this reading''s rejection of it would be seen as a misinterpretation, weakening its claim to comprehensive ethical grounding and potentially opening space for more collective mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_tension, conceptual, 'The normative status of solidarity in relation to subsidiarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(ai_g_tr_t40, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(ai_g_tr_t50, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(ai_g_be_t40, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(ai_g_be_t50, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(ai_g_su_t40, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(ai_g_su_t50, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'AI governance legitimacy' kernel, each with different structural properties and classifications. They are linked to show their contested relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
