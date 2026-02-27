% ============================================================================
% CONSTRAINT STORY: china_ev_export_oversupply
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_ev_export_oversupply, []).

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
 *   constraint_id: china_ev_export_oversupply
 *   human_readable: Chinese EV Export Oversupply and Market Distortion
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's extensive subsidies for its electric vehicle (EV) industry,
 *   coupled with aggressive export strategies, are creating a global
 *   oversupply of EVs. This oversupply distorts international markets,
 *   suppresses competition, and puts pressure on other countries' EV
 *   industries. The long-term effects of these policies on global trade and
 *   technological innovation are uncertain but potentially significant.
 *
 * KEY AGENTS:
 *   - Chinese EV Manufacturers: Primary beneficiaries (institutional/arbitrage)
 *   - Chinese Government: Primary beneficiary (institutional/arbitrage)
 *   - Foreign EV Manufacturers: Primary victim (powerless/trapped)
 *   - Consumer Choice: Victim (moderate/constrained)
 *   - Domestic EV Industries: Victim (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_ev_export_oversupply, 0.6).
domain_priors:suppression_score(china_ev_export_oversupply, 0.7).
domain_priors:theater_ratio(china_ev_export_oversupply, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_ev_export_oversupply, extractiveness, 0.6).
narrative_ontology:constraint_metric(china_ev_export_oversupply, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(china_ev_export_oversupply, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_ev_export_oversupply, tangled_rope).
narrative_ontology:human_readable(china_ev_export_oversupply, "Chinese EV Export Oversupply and Market Distortion").
narrative_ontology:topic_domain(china_ev_export_oversupply, "economic/political").

domain_priors:requires_active_enforcement(china_ev_export_oversupply).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_ev_export_oversupply, chinese_ev_manufacturers).
narrative_ontology:constraint_beneficiary(china_ev_export_oversupply, chinese_government).
narrative_ontology:constraint_victim(china_ev_export_oversupply, foreign_ev_manufacturers).
narrative_ontology:constraint_victim(china_ev_export_oversupply, consumer_choice).
narrative_ontology:constraint_victim(china_ev_export_oversupply, domestic_ev_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Foreign EV industries are trapped by the oversupply and face suppressed profits, potential bankruptcies, and reduced market share. They have limited exit options due to the scale and government backing of Chinese EV manufacturers.
constraint_indexing:constraint_classification(china_ev_export_oversupply, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Chinese EV manufacturers benefit from government subsidies and an oversupply, allowing them to gain market share and establish a dominant position. They can arbitrage their position due to government support and economies of scale.
constraint_indexing:constraint_classification(china_ev_export_oversupply, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer sees a tangled rope: Chinese EV manufacturers gain global market share and political capital, while foreign industries and consumer choice are suppressed. This dynamic has long-term implications for the global economy and geopolitical relations.
constraint_indexing:constraint_classification(china_ev_export_oversupply, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Domestic EV industries outside of China are constrained by the subsidized EVs and must compete in an uneven market. They may have some organizational capacity to lobby for protectionist measures but are ultimately constrained by the economic pressure.
constraint_indexing:constraint_classification(china_ev_export_oversupply, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_ev_export_oversupply_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_ev_export_oversupply, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_ev_export_oversupply, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_ev_export_oversupply, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_ev_export_oversupply_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The constraint extracts significant value from foreign EV industries and consumers by creating an artificial oversupply. Suppression (0.70): High. The scale of the subsidies and exports suppress competition from foreign manufacturers, limiting their ability to compete effectively. Theater ratio (0.30): Low. The Chinese government's support for the EV industry is largely substantive, with real effects on production and exports. There is limited performative aspect to the support.
 *
 * PERSPECTIVAL GAP:
 *   Chinese EV manufacturers benefit from government support and see the oversupply as a means of gaining market share. Foreign EV industries are trapped by the oversupply and face suppressed profits. An analytical observer sees a tangled rope, with China gaining economic and political power while other countries' industries are weakened.
 *
 * DIRECTIONALITY LOGIC:
 *   Chinese EV manufacturers and the Chinese government are beneficiaries with arbitrage options, experiencing low extraction. Foreign EV industries are victims with trapped exit options, experiencing high extraction. Domestic EV industries are victims with constrained exit options, experiencing moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a Tangled Rope due to the coordination function of the subsidies (supporting the Chinese EV industry) and the extractive nature of the oversupply (harming foreign industries). This prevents mislabeling it as pure coordination or pure extraction, as both elements are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_level_sustainability,
    'To what extent can the Chinese government continue to subsidize the EV industry at its current level?',
    'Analysis of China''s fiscal capacity and political priorities.',
    'If subsidies are unsustainable, the oversupply may correct itself. If sustainable, market distortion will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_level_sustainability, empirical, 'Sustainability of Chinese EV subsidies').

omega_variable(
    trade_protection_effectiveness,
    'How effective will trade protection measures be in mitigating the impact of the oversupply?',
    'Analysis of trade policies and their historical impact on similar industries.',
    'If effective, domestic industries may be shielded. If ineffective, they will continue to struggle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trade_protection_effectiveness, empirical, 'Effectiveness of trade protection measures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_ev_export_oversupply, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_ev_export_oversupply, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chin_tr_t5, china_ev_export_oversupply, theater_ratio, 5, 0.3).
narrative_ontology:measurement(chin_tr_t10, china_ev_export_oversupply, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_ev_export_oversupply, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(chin_be_t5, china_ev_export_oversupply, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(chin_be_t10, china_ev_export_oversupply, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_ev_export_oversupply, resource_allocation).
narrative_ontology:affects_constraint(china_ev_export_oversupply, global_trade_imbalances).
narrative_ontology:affects_constraint(china_ev_export_oversupply, industrial_policy_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
