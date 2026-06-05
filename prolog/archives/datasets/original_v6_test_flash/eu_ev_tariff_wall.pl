% ============================================================================
% CONSTRAINT STORY: eu_ev_tariff_wall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_ev_tariff_wall, []).

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
 *   constraint_id: eu_ev_tariff_wall
 *   human_readable: EU Tariffs and Trade Barriers on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU's imposition of tariffs and trade barriers on Chinese electric
 *   vehicles is a multifaceted issue, driven by concerns over fair
 *   competition, protection of domestic industries, and strategic positioning
 *   in the global EV market. The measures aim to level the playing field and
 *   prevent market dominance by Chinese manufacturers, who benefit from state
 *   subsidies and advanced technology. However, these actions also raise
 *   concerns about increased costs for consumers, potential retaliation from
 *   China, and hindering the transition to electric vehicles.
 *
 * KEY AGENTS:
 *   - EU Auto Manufacturers: Primary beneficiary (institutional/arbitrage) - Protected market share and leverage for technology transfer.
 *   - EU Consumers: Primary victim (powerless/trapped) - Higher prices and limited choice in EV options.
 *   - Chinese EV Manufacturers: Secondary victim (moderate/constrained) - Barriers to entry and reduced market access.
 *   - EU Member States: Mixed beneficiary/victim (institutional/constrained) - Protection of domestic industries vs. risk of retaliation and reduced access to cheaper EVs.
 *   - Analytical Observer: Assesses long term global impacts (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_ev_tariff_wall, 0.65).
domain_priors:suppression_score(eu_ev_tariff_wall, 0.7).
domain_priors:theater_ratio(eu_ev_tariff_wall, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_ev_tariff_wall, extractiveness, 0.65).
narrative_ontology:constraint_metric(eu_ev_tariff_wall, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eu_ev_tariff_wall, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_ev_tariff_wall, tangled_rope).
narrative_ontology:human_readable(eu_ev_tariff_wall, "EU Tariffs and Trade Barriers on Chinese Electric Vehicles").
narrative_ontology:topic_domain(eu_ev_tariff_wall, "economic/political").

domain_priors:requires_active_enforcement(eu_ev_tariff_wall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, eu_auto_manufacturers).
narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, eu_member_states).
narrative_ontology:constraint_victim(eu_ev_tariff_wall, chinese_ev_manufacturers).
narrative_ontology:constraint_victim(eu_ev_tariff_wall, eu_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% EU Consumers face higher prices and limited choices due to tariffs, with little power to influence trade policy. Their exit options are limited as they are bound by national markets and regulations.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Chinese EV Manufacturers face significant barriers to entry in the EU market due to tariffs, but they can adapt by investing in local production or focusing on other markets. They benefit from the existing scale of production and technological advancement, but the tariffs constrain their market access.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% EU Auto Manufacturers benefit from protection against cheaper Chinese EVs, allowing them to maintain market share and profitability. They can also use tariffs as leverage to negotiate technology transfer agreements. They can arbitrage this situation by focusing on higher-margin vehicles and lobbying for continued protection.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% EU member states benefit from the protection of domestic industries and potential increases in tax revenue, but they also risk retaliation from China and reduced access to cheaper EVs. They are constrained by the need to balance economic interests with geopolitical considerations.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% From a global perspective, the EU tariffs represent a complex interplay of protectionism, strategic competition, and climate policy. The tariffs aim to protect domestic industries but may also hinder the transition to electric vehicles and increase costs for consumers. It's a tangled rope, where the benefits and drawbacks are interconnected.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_ev_tariff_wall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_ev_tariff_wall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_ev_tariff_wall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Significant extraction due to increased prices for EU consumers and reduced market access for Chinese manufacturers. The tariffs transfer wealth from consumers and Chinese companies to EU auto manufacturers and potentially EU member states (through increased tax revenue). Suppression (0.70): High suppression due to regulatory hurdles and import restrictions effectively limiting the availability of cheaper Chinese EVs, thus reducing consumer choice. Theater ratio (0.30): Relatively low theater as the main goal of the tariffs is not to project an image of taking action but to genuinely impact the economic flows of the market and protect EU manufacturing.
 *
 * PERSPECTIVAL GAP:
 *   EU consumers experience the tariffs as a snare, facing higher prices and limited choices. Chinese EV manufacturers see it as a tangled rope, allowing them to adapt but constraining their market entry. EU auto manufacturers experience it as a rope, benefiting from reduced competition. EU Member states experience it as a tangled rope due to the various factors they must consider, and the analytical observer is tasked with observing the issue holistically.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Beneficiaries like EU auto manufacturers benefit from tariff protection, leading to low d and negative chi. Victims like EU consumers and Chinese EV manufacturers face increased costs and reduced market access, resulting in high d and positive chi. EU member states have a more complex relationship with directionality influenced by economic and geopolitical considerations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retaliation_risk,
    'What is the likelihood and impact of Chinese retaliation against EU exports?',
    'Analysis of historical trade disputes and Chinese economic policy',
    'High retaliation risk would make the tariffs less beneficial for EU member states. Low risk would embolden the EU to impose further restrictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_risk, empirical, 'The risk of Chinese retaliation affects the overall benefits of the tariffs.').

omega_variable(
    innovation_impact,
    'How will the tariffs affect innovation in the EU auto industry?',
    'Analysis of R&D spending and patent filings by EU auto manufacturers.',
    'If tariffs stimulate innovation, they may strengthen the EU auto industry in the long run. If they stifle competition, they may lead to stagnation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_impact, empirical, 'The effect of tariffs on innovation in the EU.').

omega_variable(
    climate_goal_impact,
    'How will the tariffs affect the EU''s climate goals related to electric vehicle adoption?',
    'Modeling of EV sales and emissions under different tariff scenarios.',
    'If tariffs slow down EV adoption, they may hinder the EU''s climate goals. If they encourage local EV production, they may accelerate the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_goal_impact, empirical, 'The impact on climate goals due to tariff implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_ev_tariff_wall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_e_tr_t0, eu_ev_tariff_wall, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eu_e_tr_t5, eu_ev_tariff_wall, theater_ratio, 5, 0.25).
narrative_ontology:measurement(eu_e_tr_t10, eu_ev_tariff_wall, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(eu_e_be_t0, eu_ev_tariff_wall, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(eu_e_be_t5, eu_ev_tariff_wall, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(eu_e_be_t10, eu_ev_tariff_wall, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_ev_tariff_wall, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_ev_tariff_wall, global_trade_relations).
narrative_ontology:affects_constraint(eu_ev_tariff_wall, eu_climate_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
