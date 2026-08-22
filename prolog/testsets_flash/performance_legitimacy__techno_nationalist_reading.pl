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
 *   This constraint represents the 'techno-nationalist' reading of
 *   performance legitimacy, where a state's right to rule is primarily
 *   justified by its ability to achieve technological self-sufficiency and
 *   global leadership in strategic industries. This involves massive
 *   state-directed investment, protectionist policies, and the prioritization
 *   of national champions, often at the expense of market efficiency and
 *   consumer welfare. It is one reading of the broader
 *   'performance_legitimacy' kernel.
 *
 * KEY AGENTS:
 *   - state_planning_agencies: Agenda setter (institutional/identity_locked)
 *   - defense_adjacent_tech_sectors: Beneficiary (organized/mobile)
 *   - national_champions: Beneficiary (powerful/constrained)
 *   - consumer_sectors: Payer (moderate/immediate)
 *   - market_driven_allocation: Payer (powerless/trapped)
 *   - private_enterprises_outside_strategic_sectors: Payer (moderate/constrained)
 *   - citizens_bearing_opportunity_costs: Payer (powerless/identity_locked)
 *   - international_competitors: Excluded (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.85).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.9).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, snare).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'a15cc708-8f25-4d8c-a584-3fae33760b16').
narrative_ontology:cs_kernel_codification('a15cc708-8f25-4d8c-a584-3fae33760b16', implicit).
narrative_ontology:cs_authority_grounding('a15cc708-8f25-4d8c-a584-3fae33760b16', extraction).
narrative_ontology:cs_interpretation_layer_present('a15cc708-8f25-4d8c-a584-3fae33760b16').
narrative_ontology:cs_reading_relation('a15cc708-8f25-4d8c-a584-3fae33760b16', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('a15cc708-8f25-4d8c-a584-3fae33760b16', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('a15cc708-8f25-4d8c-a584-3fae33760b16', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('a15cc708-8f25-4d8c-a584-3fae33760b16', foundational, technological_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a15cc708-8f25-4d8c-a584-3fae33760b16', technological_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('a15cc708-8f25-4d8c-a584-3fae33760b16', foundational, state_directed_industrial_policy_is_optimal).
narrative_ontology:cs_axiom_status(state_directed_industrial_policy_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('a15cc708-8f25-4d8c-a584-3fae33760b16', state_directed_industrial_policy_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('a15cc708-8f25-4d8c-a584-3fae33760b16', strategic_autonomy_framework).
narrative_ontology:cs_drift_state('a15cc708-8f25-4d8c-a584-3fae33760b16', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a15cc708-8f25-4d8c-a584-3fae33760b16', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, private_enterprises_outside_strategic_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, citizens_bearing_opportunity_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies define strategic industries, allocate massive state investment, and implement policies to achieve technological self-sufficiency. Their legitimacy and power are directly tied to the success of this techno-nationalist agenda.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive substantial state funding, subsidies, and preferential policies. They are shielded from market competition and benefit from guaranteed demand for their products, often with less pressure for immediate commercial viability.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    organized, biographical, mobile, national).

% Large state-backed or state-aligned enterprises in strategic sectors that are designated to lead global competition. They benefit from state support, market protection, and diplomatic backing, but are also subject to state directives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    powerful, biographical, constrained, global).

% Bear the opportunity costs of massive state investment in strategic industries, often through reduced public spending in other areas, higher prices for domestically produced strategic goods, or less innovation in non-strategic consumer-facing industries.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    moderate, immediate, constrained, national).

% The principle of allocating resources based on market signals and efficiency is systematically overridden by state directives and industrial policy. This leads to misallocation of capital and talent from a purely economic efficiency perspective.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).

% Face competition for talent and capital from state-backed strategic industries, often without the same subsidies or market protections. Their growth is secondary to the national strategic goals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, private_enterprises_outside_strategic_sectors, payer,
    moderate, biographical, constrained, national).

% Citizens are expected to support the national project of technological leadership, often through accepting lower living standards, reduced consumer choice, or delayed social welfare improvements in favor of strategic investments. Their identity is fused with the national project.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, citizens_bearing_opportunity_costs, payer,
    powerless, generational, identity_locked, national).

% Foreign companies and nations that would otherwise compete in strategic industries are excluded or disadvantaged by protectionist policies, subsidies, and export controls. They are the target of the techno-nationalist strategy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_competitors, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national resources (capital, talent, policy) towards a singular, long-term goal of technological self-sufficiency and global leadership in strategic sectors, overcoming market fragmentation and short-term profit motives.
% TRANSFER_FUNCTION: Directs massive state capital, talent, and policy support from the broader economy (consumer sectors, market-driven allocation) to specific strategic industries and national champions, in exchange for projected national security and great-power status.
% ABSENT_VOICES: Advocates for pure market-driven allocation, consumer welfare groups, and non-strategic private enterprises would argue for a more balanced economic development model. They are marginalized by the overarching national strategic imperative.
% DISAPPEARANCE_RATIONALE: If this techno-nationalist legitimacy constraint vanished, state-directed investment would cease, strategic industries would face market competition, and resources would reallocate towards consumer demand and efficiency. The national security and great-power status narrative would lose its central economic justification, leading to a fundamental reorganization of the political economy.
% FOUNDING_PROBLEM: The perceived vulnerability to external technological dependencies, the desire for national security autonomy, and the ambition to achieve great-power status through industrial and technological might.
% FOUNDING_PROBLEM_CORROBORATION: State media and official pronouncements consistently attest to the live status of external threats and the necessity of technological self-sufficiency. Independent geopolitical analysts and defense strategists from outside the direct beneficiaries also corroborate the existence of these vulnerabilities and strategic competition, though they may dispute the efficacy or costs of the techno-nationalist approach.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because resources are forcibly diverted from market-driven allocation and consumer sectors to strategic industries, creating significant opportunity costs and inefficiencies. Suppression is also very high (0.90) due to the state's extensive control over capital, talent, and market access, actively suppressing alternative economic models or resistance to strategic directives. Theater ratio is low (0.20) because the commitment to technological leadership is genuine and actively pursued, not merely performative, though some projects may be more symbolic than effective. Accessibility collapse is high (0.75) as market-based alternatives are systematically dismantled or marginalized. Resistance is moderate (0.40) as the national security narrative provides strong ideological cover, but economic inefficiencies and consumer dissatisfaction can generate diffuse, low-level resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planning agencies and strategic industries, this is a necessary 'rope' for national survival and prosperity. From the perspective of consumer sectors and market-driven allocation, it operates as a 'snare' that extracts resources and suppresses alternatives for the benefit of a select few, justified by a national narrative. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies, defense-adjacent tech sectors, and national champions are clear beneficiaries, receiving direct and indirect state support. Consumer sectors, market-driven allocation, non-strategic private enterprises, and citizens bearing opportunity costs are the primary targets, bearing the costs of resource diversion and suppressed alternatives. The 'identity_locked' exit option for citizens reflects the strong ideological fusion with the national project, making exit from the system's demands unthinkable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare, not a rope, because its coordination function (directing national resources) is inextricably linked with asymmetric extraction (from market-driven sectors to strategic ones) and relies heavily on suppression of alternatives. The national security mandate is live, preventing mandatrophy, but the mechanism of achieving it is highly extractive. The classification prevents mislabeling state-directed extraction as mere coordination for national interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_directed_investment,
    'Is massive state-directed investment in strategic industries actually the most effective path to technological self-sufficiency and global leadership, or would market-driven approaches yield better long-term results?',
    'Comparative economic studies of nations employing different industrial policies over multi-decade periods, assessing innovation rates, market competitiveness, and technological breakthroughs.',
    'If market-driven approaches prove more effective, the justification for the high extraction and suppression would weaken, potentially reclassifying the constraint towards a pure snare or piton if the ''performance'' aspect is not met.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_directed_investment, empirical, 'Whether the means (state direction) effectively achieve the stated ends (technological leadership).').

omega_variable(
    national_security_vs_economic_efficiency,
    'What is the optimal balance between national security imperatives (requiring strategic industry control) and economic efficiency (requiring market allocation), and is the current techno-nationalist reading over-prioritizing security at excessive economic cost?',
    'Policy debates incorporating diverse expert opinions (defense, economics, social welfare) and public discourse, leading to a re-evaluation of national priorities and resource allocation.',
    'A shift in the perceived optimal balance could lead to a reduction in state intervention, lowering extraction and suppression, potentially moving the constraint towards a tangled rope or even a rope if coordination benefits become more symmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_security_vs_economic_efficiency, preference, 'The trade-off between national security and economic efficiency.').

omega_variable(
    identity_lock_legitimacy,
    'To what extent is the ''identity_locked'' exit option for citizens a genuine expression of national unity and shared purpose, versus an internalized suppression mechanism resulting from state propaganda and lack of alternatives?',
    'Longitudinal studies of public opinion, independent media analysis, and the observed behavior of citizens when presented with genuine, low-cost alternatives or opportunities for dissent.',
    'If internalized suppression is dominant, the effective suppression of the constraint is higher than structural measures suggest, and the legitimacy claim is weaker, pushing the classification more firmly towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_legitimacy, empirical, 'Structural vs. internalized suppression mechanism for citizen identity lock.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''techno_nationalist_reading'' the most appropriate framing for this constraint, or would a ''quantitative_growth_reading'' or ''livelihood_security_reading'' better capture the underlying dynamics and stakeholder experiences?',
    'Analysis of policy documents, public discourse, and resource allocation patterns to identify which performance metric (technological leadership, GDP growth, or citizen welfare) consistently receives primary emphasis and resource commitment.',
    'If an alternative reading is adopted, the constraint''s classification, beneficiaries, and victims would shift significantly to reflect the priorities of that reading (e.g., a quantitative_growth_reading might show different beneficiaries and a lower extractiveness if growth is genuinely broad-based).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the performance_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(perf_tr_t2005, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(perf_be_t2005, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(perf_su_t2005, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'techno_nationalist_reading' of the 'performance_legitimacy' kernel. It focuses on technological self-sufficiency and strategic industry leadership as the primary justification for state authority, distinct from other readings that emphasize economic growth or citizen welfare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
