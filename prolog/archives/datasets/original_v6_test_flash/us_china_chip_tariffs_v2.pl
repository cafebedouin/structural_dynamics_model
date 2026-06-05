% ============================================================================
% CONSTRAINT STORY: us_china_chip_tariffs_v2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_chip_tariffs_v2, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_china_chip_tariffs_v2
 *   human_readable: US Tariffs on Chinese High-Tech Goods (2024)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The US tariffs on Chinese high-tech goods, particularly semiconductors,
 *   aim to bolster domestic manufacturing and reduce reliance on foreign
 *   suppliers. However, these tariffs also create significant costs for US
 *   consumers and disrupt global supply chains. The policy is a complex
 *   interplay of economic and political factors, with varying impacts on
 *   different stakeholders.
 *
 * KEY AGENTS:
 *   - US Chip Manufacturers: Primary beneficiary (institutional/arbitrage) - benefits from reduced competition and subsidies.
 *   - Chinese Chip Manufacturers: Primary victim (powerless/trapped) - faces restricted market access.
 *   - US Consumers: Secondary victim (moderate/constrained) - bears increased costs.
 *   - Global Supply Chain Efficiency:  Moderately negatively affected (powerful/constrained).
 *   - Governments Promoting Domestic Production: (institutional/constrained) benefits from US policy direction but constrained by direct cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_chip_tariffs_v2, 0.6).
domain_priors:suppression_score(us_china_chip_tariffs_v2, 0.7).
domain_priors:theater_ratio(us_china_chip_tariffs_v2, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_chip_tariffs_v2, tangled_rope).
narrative_ontology:human_readable(us_china_chip_tariffs_v2, "US Tariffs on Chinese High-Tech Goods (2024)").
narrative_ontology:topic_domain(us_china_chip_tariffs_v2, "economic/political").

domain_priors:requires_active_enforcement(us_china_chip_tariffs_v2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_chip_manufacturers).
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, governments_promoting_domestic_production).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, chinese_chip_manufacturers).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, us_consumers).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, global_supply_chain_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Chinese chip manufacturers face limited alternative markets in the short term and are significantly harmed by the tariffs, which restrict their access to the US market. They are largely trapped due to existing trade agreements and technology dependencies.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% US consumers bear the increased costs of high-tech goods due to the tariffs. While they have some limited mobility through purchasing alternatives, they are largely constrained by the availability and pricing of domestic products. The exit option is 'constrained' because they face switching costs and limited choices.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% US chip manufacturers benefit from reduced competition and increased government subsidies/support, creating arbitrage opportunities and incentivizing domestic production. They experience the constraint as a coordination mechanism to strengthen the domestic industry.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Global supply chain efficiency suffers from the fragmentation and increased costs resulting from the tariffs. It experiences both extraction (due to reduced efficiency) and coordination (attempting to realign to changing regulations).
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Governments in other countries who also seek to reduce dependency on specific nations' chip production benefit from the US's actions by having more global alignment in reducing dependency. However, they are constrained in that US tariffs also apply to other countries that send chips to China, potentially hurting their economies.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a global and civilizational perspective, the tariffs represent a tangled rope, with both coordination (re-shoring/friend-shoring, reducing single-country dependence) and extraction (increased costs, inefficiencies in supply chains). The policy requires active enforcement and has asymmetric effects.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_chip_tariffs_v2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_chip_tariffs_v2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_china_chip_tariffs_v2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-high. The tariffs extract value from Chinese manufacturers and US consumers through increased prices and reduced market access. Suppression (0.70): High. The tariffs significantly suppress alternative supply chains and market options for both producers and consumers. Theater Ratio (0.30): Relatively low. The tariffs have a real economic impact beyond mere performative signaling.
 *
 * PERSPECTIVAL GAP:
 *   The tariffs are viewed differently by various actors. US chip manufacturers see them as a rope, enhancing their competitiveness. Chinese manufacturers and US consumers experience them as a snare, limiting their choices and increasing costs. Analytical observers recognize the tangled rope nature, with both coordination (domestic production) and extraction (global inefficiency).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's position relative to the extraction flow. Beneficiaries (US chip manufacturers) have a low d value, while victims (Chinese manufacturers, US consumers) have a high d value. Global supply chain efficiency is negatively affected due to the disruptions and inefficiencies created by the tariffs. It is moderately negative as some global supply chain will also benefit with increased redundancy in systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The policy is classified as a Tangled Rope because it combines elements of both coordination and extraction. The coordination aspect is the attempt to strengthen the US domestic chip industry and reduce reliance on foreign sources. The extraction aspect is the increased costs and inefficiencies imposed on Chinese manufacturers, US consumers, and global supply chains. This is a Tangled Rope, not just a Snare, because there's a clear and intended coordination function, even if it is implemented with significant coercive costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_reshoring,
    'Will reshoring chip production to the US prove economically sustainable and secure the supply chain?',
    'Longitudinal tracking of domestic chip production costs, technological advancement, and vulnerability to geopolitical shocks.',
    'If effective: tariffs transition to scaffold/rope for US. If ineffective: tariffs become a piton, causing long-term economic harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_reshoring, empirical, 'Sustainability of domestic chip production').

omega_variable(
    china_retaliation_impact,
    'How will China retaliate to the tariffs, and what will be the global economic consequences?',
    'Monitoring Chinese policy responses and their impact on global trade flows, technology access, and geopolitical stability.',
    'If China retaliates strongly: tariffs become snare for global economy. If China adapts: tariffs have limited long-term impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_retaliation_impact, empirical, 'Impact of Chinese retaliation').

omega_variable(
    technology_leapfrogging,
    'Will tariffs incentivize or hinder innovation in semiconductor technology and related sectors?',
    'Tracking patent filings, investment trends, and technological breakthroughs in both the US and China.',
    'If incentivizes innovation: potential scaffold with sunset as global innovation becomes more balanced. If hinders: becomes a long-term snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_leapfrogging, empirical, 'Impact on technological innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_chip_tariffs_v2, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_china_chip_tariffs_v2, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_c_tr_t3, us_china_chip_tariffs_v2, theater_ratio, 3, 0.25).
narrative_ontology:measurement(us_c_tr_t5, us_china_chip_tariffs_v2, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_china_chip_tariffs_v2, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_c_be_t3, us_china_chip_tariffs_v2, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(us_c_be_t5, us_china_chip_tariffs_v2, base_extractiveness, 5, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_chip_tariffs_v2, resource_allocation).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, global_trade_relations).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, semiconductor_supply_chain_resilience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
