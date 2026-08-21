% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes the techno-nationalist reading of performance
 *   legitimacy, where a state's legitimacy is grounded in its ability to
 *   achieve technological self-sufficiency and global leadership in strategic
 *   industries. This involves massive directed investment, export controls,
 *   and supply chain resilience, often at the expense of market-driven
 *   allocation and consumer sectors. The constraint is claimed as a 'rope' by
 *   its proponents (coordinating national efforts for a collective good), but
 *   its operational metrics reflect substantial extraction and suppression,
 *   leading to an engine-computed 'tangled_rope' or 'snare' classification
 *   from other seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.8).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.85).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '95e6f169-9796-41ed-85d7-cd6c379a853a').
narrative_ontology:cs_kernel_codification('95e6f169-9796-41ed-85d7-cd6c379a853a', formalized).
narrative_ontology:cs_authority_grounding('95e6f169-9796-41ed-85d7-cd6c379a853a', extraction).
narrative_ontology:cs_interpretation_layer_present('95e6f169-9796-41ed-85d7-cd6c379a853a').
narrative_ontology:cs_reading_relation('95e6f169-9796-41ed-85d7-cd6c379a853a', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('95e6f169-9796-41ed-85d7-cd6c379a853a', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('95e6f169-9796-41ed-85d7-cd6c379a853a', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('95e6f169-9796-41ed-85d7-cd6c379a853a', foundational, technological_sovereignty_is_national_security).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_national_security, holdable).
narrative_ontology:cs_axiom_grounding('95e6f169-9796-41ed-85d7-cd6c379a853a', technological_sovereignty_is_national_security, instrumental).
narrative_ontology:cs_axiom('95e6f169-9796-41ed-85d7-cd6c379a853a', foundational, state_directed_investment_optimizes_strategic_outcomes).
narrative_ontology:cs_axiom_status(state_directed_investment_optimizes_strategic_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('95e6f169-9796-41ed-85d7-cd6c379a853a', state_directed_investment_optimizes_strategic_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('95e6f169-9796-41ed-85d7-cd6c379a853a', strategic_autonomy_framework).
narrative_ontology:cs_drift_state('95e6f169-9796-41ed-85d7-cd6c379a853a', contemporary_geopolitical_competition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('95e6f169-9796-41ed-85d7-cd6c379a853a', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planners_and_leadership).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs massive state investment and policy toward strategic industries, prioritizing technological self-sufficiency and global leadership for national security and great-power status. Benefits from enhanced national power and legitimacy derived from perceived success in these areas.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planners_and_leadership, agenda_setter,
    institutional, civilizational, constrained, national).

% Receives substantial state funding, subsidies, and protected market access. Benefits from guaranteed demand and insulation from global competition, allowing for long-term R&D and strategic growth aligned with national goals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    organized, generational, constrained, national).

% Designated as key players in strategic industries, receiving state support to achieve global competitiveness. Benefits from domestic market protection, export promotion, and diplomatic backing, but remains dependent on state policy for its privileged position.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    organized, generational, constrained, global).

% Faces resource diversion (capital, talent) away from market-driven growth, potentially leading to higher costs, reduced innovation, or slower development in non-strategic areas. Bears the indirect costs of state-directed industrial policy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    organized, biographical, constrained, national).

% Their preferred economic principles (free markets, efficiency, open competition) are suppressed in favor of state-directed industrial policy. They bear the cost of perceived economic inefficiencies and foregone market opportunities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation_advocates, payer,
    moderate, biographical, constrained, national).

% Are actively excluded from strategic domestic markets through protectionist policies, export controls, and subsidies for national champions. They would prefer open, competitive markets but are structurally barred from participating in key sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, global_competitors, excluded,
    institutional, generational, trapped, global).

% Analyze the economic efficiency, geopolitical implications, and long-term sustainability of techno-nationalist industrial policies. Their findings may challenge or corroborate the state's claims of success.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, state_planners_and_leadership).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national resources, industrial policy, and scientific research to achieve technological self-sufficiency and global leadership in strategic industries, thereby enhancing national security and great-power status.
% TRANSFER_FUNCTION: Transfers significant capital, skilled labor, and market opportunities from consumer-oriented and market-driven sectors to state-backed strategic industries, often at the expense of economic efficiency and consumer choice.
% ABSENT_VOICES: Advocates for pure market allocation, consumer welfare groups, and international trade organizations are largely excluded from the policy-making process. They would argue for greater economic openness, efficiency, and reduced state intervention, but their perspectives are suppressed by the national security framing.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the state would lose its primary mechanism for directing industrial policy. Resources would flow to market-driven sectors, strategic industries would face unmitigated global competition, and the nation's perceived path to technological self-sufficiency and great-power status would be fundamentally altered, leading to a significant reorganization of economic and geopolitical priorities.
% FOUNDING_PROBLEM: Perceived vulnerability in critical technologies, reliance on foreign supply chains, and a desire to secure national security and great-power status in an increasingly competitive and uncertain global environment.
% FOUNDING_PROBLEM_CORROBORATION: National security analysts, military strategists, and state-affiliated think tanks consistently corroborate the ongoing criticality of technological self-sufficiency and strategic industry leadership. While independent economists may contest the efficiency of the chosen methods, the strategic imperative is widely acknowledged by a broad range of non-benefiting parties.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.8) due to the significant diversion of resources from market-efficient uses to state-directed strategic industries, imposing costs on consumer sectors and distorting market signals. Suppression is also high (0.85) as market mechanisms, alternative investment paths, and foreign competition are actively curtailed through policy and enforcement. The theater ratio is moderate (0.4), reflecting genuine strategic investment alongside performative aspects for national prestige and internal political legitimation. The measurement series show a gradual increase in both extractiveness and suppression over time, indicating a hardening of the policy framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planners and leadership, this constraint is a necessary coordination mechanism for national survival and prosperity in a competitive world. From the perspective of consumer sectors and market advocates, it is an extractive mechanism that distorts the economy and imposes costs for an uncertain strategic benefit. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State planners and leadership, along with defense-adjacent tech sectors and national champions, are the primary beneficiaries, receiving directed resources and protection. Consumer sectors and market-driven allocation advocates are the victims, bearing the costs of resource diversion and suppressed market efficiency. Global competitors are structurally excluded, their market access suppressed to protect domestic strategic industries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_vs_security_tradeoff,
    'Is the economic inefficiency and resource misallocation inherent in this techno-nationalist strategy a necessary and proportionate cost for achieving national security and technological sovereignty, or is it excessive?',
    'Comparative analysis of long-term economic performance and security outcomes in states pursuing different industrial strategies, alongside independent cost-benefit analyses of specific strategic investments.',
    'If the costs are deemed excessive relative to the security gains, the constraint''s extractiveness would be re-evaluated as higher, and its coordination function questioned, potentially reclassifying it closer to a Snare. If deemed proportionate, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_security_tradeoff, empirical, 'Assesses the true trade-off between economic efficiency and national security goals.').

omega_variable(
    true_technological_independence,
    'Does this strategy genuinely lead to technological self-sufficiency and independence, or does it create new forms of dependency (e.g., on state subsidies, specific supply chains, or foreign intellectual property for foundational components)?',
    'Longitudinal studies tracking the origin and control of critical technologies, intellectual property, and supply chain vulnerabilities over several decades, independent of state-sponsored reporting.',
    'If new dependencies are identified, the claim of ''self-sufficiency'' would be undermined, increasing the theater_ratio and potentially reclassifying the constraint as more extractive, as the costs are borne for an unfulfilled promise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_technological_independence, empirical, 'Evaluates the actual achievement of technological independence versus the creation of new dependencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__techno_nationalist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__techno_nationalist_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(perf_tr_t50, performance_legitimacy__techno_nationalist_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(perf_be_t50, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 50, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(perf_su_t50, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, global_supply_chain_resilience).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, export_control_regimes).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, livelihood_security_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
