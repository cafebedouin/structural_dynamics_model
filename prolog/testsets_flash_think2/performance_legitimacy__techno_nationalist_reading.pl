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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes a techno-nationalist reading of performance
 *   legitimacy, where a state's right to rule is grounded in its ability to
 *   achieve technological self-sufficiency and global leadership in strategic
 *   industries. This involves massive directed investment, export controls,
 *   and supply chain resilience, often at the expense of market-driven
 *   allocation and consumer sectors. It is one reading of the broader
 *   'performance_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.78).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.85).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'ea53ba44-cd09-4a0b-bf98-1edd642672ff').
narrative_ontology:cs_kernel_codification('ea53ba44-cd09-4a0b-bf98-1edd642672ff', formalized).
narrative_ontology:cs_authority_grounding('ea53ba44-cd09-4a0b-bf98-1edd642672ff', extraction).
narrative_ontology:cs_interpretation_layer_present('ea53ba44-cd09-4a0b-bf98-1edd642672ff').
narrative_ontology:cs_reading_relation('ea53ba44-cd09-4a0b-bf98-1edd642672ff', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('ea53ba44-cd09-4a0b-bf98-1edd642672ff', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea53ba44-cd09-4a0b-bf98-1edd642672ff', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('ea53ba44-cd09-4a0b-bf98-1edd642672ff', foundational, national_security_through_tech_dominance).
narrative_ontology:cs_axiom_status(national_security_through_tech_dominance, holdable).
narrative_ontology:cs_axiom_grounding('ea53ba44-cd09-4a0b-bf98-1edd642672ff', national_security_through_tech_dominance, instrumental).
narrative_ontology:cs_axiom('ea53ba44-cd09-4a0b-bf98-1edd642672ff', foundational, strategic_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(strategic_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('ea53ba44-cd09-4a0b-bf98-1edd642672ff', strategic_autonomy_is_paramount, conventional).
narrative_ontology:cs_reference_frame('ea53ba44-cd09-4a0b-bf98-1edd642672ff', state_directed_industrial_policy_framework).
narrative_ontology:cs_drift_state('ea53ba44-cd09-4a0b-bf98-1edd642672ff', contemporary_geopolitical_competition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ea53ba44-cd09-4a0b-bf98-1edd642672ff', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, private_capital_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies formulate and implement industrial policies, direct massive state investments, and enforce export controls and supply chain resilience measures. Their legitimacy is tied to achieving the stated goals of technological self-sufficiency and global leadership.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_agencies, agenda_setter,
    institutional, generational, constrained, national).

% These sectors receive substantial state funding, preferential policies, and guaranteed market access, allowing them to grow and innovate regardless of immediate market signals. Their success is directly linked to the techno-nationalist agenda.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    powerful, biographical, constrained, national).

% Large, often state-backed, enterprises designated as key players in strategic industries. They benefit from directed investment, R&D subsidies, and protection from foreign competition, enabling them to pursue global leadership.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    powerful, biographical, constrained, global).

% These sectors (e.g., retail, services, non-strategic manufacturing) may face resource diversion, higher input costs due to strategic priorities, and reduced investment, leading to slower growth or stagnation. Consumers may experience higher prices or fewer choices.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    moderate, immediate, constrained, national).

% The principle of allocating resources based purely on market signals (supply, demand, price) is actively suppressed in favor of state-directed industrial policy. Its 'voice' is heard in economic critiques but not in policy formulation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation, excluded,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).

% Investors whose capital is not aligned with strategic industries may find their opportunities limited, face regulatory hurdles, or be implicitly pressured to redirect funds towards state-preferred sectors, reducing overall market efficiency.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, private_capital_investors, payer,
    powerful, biographical, constrained, national).

% Foreign governments and companies observe the techno-nationalist policies, often responding with their own industrial strategies, trade barriers, or diplomatic pressure, viewing it as a challenge to global free markets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_competitors, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate national resources, industrial policy, and scientific efforts towards achieving specific strategic technological goals, ensuring national security and global great-power status.
% TRANSFER_FUNCTION: Transfers significant capital, talent, and market access from consumer-oriented or purely market-driven sectors to state-backed strategic industries, often at the expense of overall economic efficiency or consumer welfare.
% ABSENT_VOICES: Advocates for pure market liberalization, consumer choice, and small/medium enterprises not aligned with strategic goals are structurally excluded from policy-making. They would argue for less state intervention and more open competition.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, national resources would likely flow based on market signals, potentially leading to greater consumer welfare and economic efficiency but a loss of strategic autonomy and a different trajectory for national security and global influence. The industrial landscape would fundamentally shift.
% FOUNDING_PROBLEM: A perceived vulnerability in critical technologies, over-reliance on foreign supply chains, and the ambition to achieve or maintain great-power status in a competitive geopolitical landscape.
% FOUNDING_PROBLEM_CORROBORATION: National security analysts, military strategists, and state media consistently attest to the ongoing nature of these vulnerabilities and the necessity of the techno-nationalist approach. Some independent geopolitical analysts might corroborate the *existence* of strategic vulnerabilities, even if they disagree with the *solution*.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the diversion of national resources and suppression of market signals to benefit strategic industries, leading to inefficiencies and costs borne by other sectors. Suppression (0.85) is very high due to active state enforcement, industrial policy, and control over capital flows, which actively collapse market alternatives. Theater ratio (0.40) is moderate, as there are genuine efforts towards technological advancement, but also significant performative aspects in declaring 'self-sufficiency' and 'leadership' to maintain public support and state authority. Accessibility collapse is high (0.75) because market-based alternatives for resource allocation are significantly curtailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planning agencies and strategic industries, this constraint is a necessary coordination mechanism for national survival and prosperity. From the perspective of consumer sectors or market-oriented investors, it is an extractive and suppressive force that distorts the economy for political ends. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies, defense-adjacent tech sectors, and national champion firms are clear beneficiaries, receiving directed resources and protection. Consumer sectors, private capital investors, and the principle of market-driven allocation are the primary targets, bearing the costs of resource diversion and suppressed alternatives. The directionality for beneficiaries is low (subsidized), while for victims it is high (extracted from).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    techno_nationalism_efficacy,
    'Does this techno-nationalist approach genuinely enhance national security and great-power status, or does it create new vulnerabilities (e.g., through economic inefficiency, reduced innovation in non-strategic sectors, or international isolation)?',
    'Long-term empirical studies comparing outcomes in techno-nationalist economies versus more market-oriented ones, considering both strategic and broader economic indicators.',
    'If found ineffective or counterproductive, the legitimacy claim would erode, potentially leading to a reclassification towards a Snare or Piton as the coordination story collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(techno_nationalism_efficacy, empirical, 'The actual effectiveness of techno-nationalist policies for stated goals.').

omega_variable(
    market_distortion_cost,
    'What is the true economic cost of market distortion and suppressed innovation in non-strategic sectors due to directed investment and state control?',
    'Independent economic modeling and counterfactual analysis, assessing foregone growth and innovation in sectors not prioritized by the state.',
    'A high, unacknowledged cost would significantly increase the effective extractiveness and suppression, pushing the classification further towards Snare, as the ''coordination'' benefit is outweighed by hidden costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_distortion_cost, empirical, 'The hidden economic costs of state-directed industrial policy.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy truly derived from achieving technological leadership, or is this a narrative used to justify and maintain state control and resource extraction for other political ends?',
    'Sociological and political science analysis of public opinion, elite discourse, and policy outcomes, particularly in times of economic hardship or strategic setbacks.',
    'If the technological leadership narrative is primarily a cover, the constraint''s classification would shift towards a Snare, as the coordination function is revealed as largely theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity of the ultimate source of state legitimacy in techno-nationalist contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(perf_tr_t2005, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement(perf_tr_t2030, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(perf_be_t2005, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2025, 0.77).
narrative_ontology:measurement(perf_be_t2030, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(perf_su_t2005, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2025, 0.84).
narrative_ontology:measurement(perf_su_t2030, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, global_supply_chain_resilience).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, international_trade_agreements).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
