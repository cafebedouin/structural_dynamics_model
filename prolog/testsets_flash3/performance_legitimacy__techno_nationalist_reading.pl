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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   legitimacy, where the state's authority is grounded in achieving
 *   technological self-sufficiency and global leadership in strategic
 *   industries. This involves massive directed investment, export controls,
 *   and prioritizing supply chain resilience, often at the expense of market
 *   efficiency and consumer welfare. It is one reading of the broader
 *   'performance_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6').
narrative_ontology:cs_kernel_codification('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', formalized).
narrative_ontology:cs_authority_grounding('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', extraction).
narrative_ontology:cs_interpretation_layer_present('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6').
narrative_ontology:cs_reading_relation('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', foundational, technological_sovereignty_is_national_security).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_national_security, holdable).
narrative_ontology:cs_axiom_grounding('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', technological_sovereignty_is_national_security, deontological).
narrative_ontology:cs_axiom('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', foundational, strategic_industry_dominance_ensures_great_power_status).
narrative_ontology:cs_axiom_status(strategic_industry_dominance_ensures_great_power_status, holdable).
narrative_ontology:cs_axiom_grounding('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', strategic_industry_dominance_ensures_great_power_status, empirically_contingent).
narrative_ontology:cs_reference_frame('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', national_technological_autonomy).
narrative_ontology:cs_drift_state('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', contemporary_global_competition, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('54d35e08-5a0b-4bbd-aa53-fd2f52e8b6c6', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, foreign_tech_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Direct massive state investment into strategic industries, prioritize technological breakthroughs, and implement export controls. Their legitimacy is tied to achieving national security and great-power status through technological dominance.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive substantial state subsidies, R&D grants, and preferential market access. They are expected to deliver cutting-edge technologies that enhance national security and global competitiveness, often with guaranteed procurement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    organized, biographical, mobile, national).

% Designated by the state to lead in strategic industries, receiving state backing, protection from foreign competition, and access to critical resources. They are expected to achieve global market leadership and technological breakthroughs.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    powerful, generational, constrained, global).

% Bear the costs of diverted investment, protectionist policies leading to higher prices for domestic goods, and reduced access to foreign technologies. Their needs are secondary to strategic industrial goals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    moderate, immediate, constrained, national).

% Represents the economic principle of allocating resources based on supply and demand. It is overridden by state directives, leading to inefficiencies and misallocations from a purely economic perspective.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).

% Are actively excluded or severely restricted from participating in strategic domestic markets through tariffs, non-tariff barriers, and direct prohibitions. They would offer competitive alternatives but are seen as a threat to national self-sufficiency.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_tech_firms, excluded,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national resources, industrial policy, and scientific research towards a unified goal of technological self-sufficiency and global leadership in strategic sectors, preventing fragmented efforts.
% TRANSFER_FUNCTION: Transfers significant financial resources, talent, and market access from consumer-oriented sectors and market-driven allocation to state-designated strategic industries and national champions.
% ABSENT_VOICES: Advocates for free markets, consumer choice, and international economic integration are marginalized; they would argue for efficiency gains and innovation through open competition, but their perspective is deemed secondary to national security imperatives.
% DISAPPEARANCE_RATIONALE: If this techno-nationalist framing of legitimacy vanished, state-directed investment would cease, strategic industries would face market competition, and resources would reallocate towards consumer demand or other development priorities. The national economy's structure and global positioning would fundamentally shift.
% FOUNDING_PROBLEM: The nation faced perceived vulnerabilities in critical technologies, dependence on foreign supply chains, and a desire to achieve great-power status through industrial and technological might.
% FOUNDING_PROBLEM_CORROBORATION: National security strategists and military leadership consistently corroborate the ongoing live status of technological vulnerability and the imperative for self-sufficiency. While economists may dispute the efficiency of the approach, the national security rationale is widely accepted by non-benefiting state actors.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because resources are forcibly reallocated from efficient market-driven sectors to state-prioritized strategic industries, creating economic distortions and costs for non-strategic sectors. Suppression is also high (0.75) due to active state intervention, protectionist policies, and control over resource allocation to enforce industrial policy. Theater ratio is low (0.20) because the commitment to technological self-sufficiency is genuine and actively pursued, not merely performative. The claimed type is Tangled Rope, reflecting a genuine coordination function (national strategic alignment) coupled with asymmetric extraction from market-driven sectors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planning agencies and strategic industries, this constraint is a necessary coordination mechanism for national survival and prosperity. From the perspective of consumer sectors and market advocates, it is an extractive mechanism that distorts the economy and imposes costs for a politically determined goal. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies, defense-adjacent tech sectors, and national champion firms are clear beneficiaries, receiving direct support and market protection. Consumer sectors and market-driven allocation are victims, bearing the costs of distorted markets and diverted resources. Foreign tech firms are excluded, as their presence would undermine the goal of self-sufficiency. The directionality for beneficiaries is low (subsidized), and for victims, it is high (targeted for extraction).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_vs_security_tradeoff,
    'What is the true economic cost of achieving technological self-sufficiency through state-directed investment, compared to the security benefits gained?',
    'Independent, long-term economic modeling that accounts for both direct and opportunity costs of state intervention versus the quantifiable reduction in strategic vulnerability.',
    'If costs significantly outweigh benefits, the constraint''s extractiveness might be re-evaluated as less justified by coordination, potentially shifting it towards a Snare. If benefits are demonstrably high, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_security_tradeoff, empirical, 'Assessing the economic efficiency of techno-nationalist policies against their security objectives.').

omega_variable(
    market_distortion_vs_industrial_capacity,
    'To what extent do state-directed investments genuinely build sustainable industrial capacity, versus merely creating rent-seeking opportunities and market distortions?',
    'Empirical studies tracking the long-term competitiveness and innovation output of subsidized industries post-intervention, compared to market-driven benchmarks.',
    'If state intervention primarily leads to rent-seeking, the ''coordination'' function is weakened, increasing the effective extractiveness and pushing the classification closer to a Snare. If it genuinely fosters innovation, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_distortion_vs_industrial_capacity, empirical, 'Distinguishing genuine industrial development from market distortions caused by state intervention.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state control over resources, market access) or internalized (ideological commitment to national goals overriding economic rationality)?',
    'Post-policy-shift analysis: if economic actors continue to prioritize national strategic goals even after direct state controls are relaxed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — economic actors carry the suppression with them after direct state pressure is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in state-directed economic policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__techno_nationalist_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__techno_nationalist_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its focus on technological self-sufficiency and global leadership influences, and is influenced by, other readings of national legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
