% ============================================================================
% CONSTRAINT STORY: arg_ev_tariff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-11-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arg_ev_tariff, []).

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
 *   constraint_id: arg_ev_tariff
 *   human_readable: Argentine Tariff on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   Argentina's proposed tariff on Chinese electric vehicles (EVs) is a
 *   protectionist measure aimed at bolstering the domestic auto industry.
 *   However, it raises concerns about consumer welfare, trade relations with
 *   China, and the overall transition to sustainable transportation. The
 *   tariff is designed to reduce competition from cheaper Chinese EVs and
 *   encourage the purchase of domestically produced vehicles, which are often
 *   more expensive and technologically less advanced.
 *
 * KEY AGENTS:
 *   - Argentine Consumers: Primary victims (powerless/trapped) - face higher prices and reduced choices.
 *   - Chinese EV Exporters: Secondary victims (moderate/constrained) - limited access to the Argentine market.
 *   - Argentine Auto Manufacturers: Primary beneficiaries (institutional/arbitrage) - reduced competition and potential increased sales.
 *   - Argentine Government: Intermediary (institutional/constrained) - aims to protect domestic industry but faces trade and economic risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arg_ev_tariff, 0.6).
domain_priors:suppression_score(arg_ev_tariff, 0.7).
domain_priors:theater_ratio(arg_ev_tariff, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arg_ev_tariff, extractiveness, 0.6).
narrative_ontology:constraint_metric(arg_ev_tariff, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(arg_ev_tariff, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arg_ev_tariff, snare).
narrative_ontology:human_readable(arg_ev_tariff, "Argentine Tariff on Chinese Electric Vehicles").
narrative_ontology:topic_domain(arg_ev_tariff, "economic/political").

domain_priors:requires_active_enforcement(arg_ev_tariff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arg_ev_tariff, argentine_auto_manufacturers).
narrative_ontology:constraint_victim(arg_ev_tariff, argentine_consumers).
narrative_ontology:constraint_victim(arg_ev_tariff, chinese_ev_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Argentine consumers face higher EV prices and limited choices due to the tariff, effectively trapped in a market with less competition.
constraint_indexing:constraint_classification(arg_ev_tariff, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Chinese EV exporters are constrained by the tariff, limiting their access to the Argentine market and impacting their global market share.
constraint_indexing:constraint_classification(arg_ev_tariff, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Argentine auto manufacturers benefit from reduced competition, potentially leading to increased sales and market share, but may also face pressure to innovate and compete in the long run. They can arbitrage the situation by increasing prices without improving quality. Coordination function: protects the domestic auto industry.
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Argentine government aims to protect the domestic auto industry and promote economic growth, but also risks retaliatory measures from China and reduced consumer welfare. Constrained because they must balance different interests.
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the tariff represents a form of protectionism that may hinder the adoption of EVs and the transition to a more sustainable transportation system.
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arg_ev_tariff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arg_ev_tariff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arg_ev_tariff, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arg_ev_tariff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arg_ev_tariff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is set at 0.60, reflecting the significant cost to consumers in terms of higher prices and limited choices. Suppression is high at 0.70, indicating the reduction in competition and the constraint on alternative EV options. The theater ratio is moderate at 0.30, suggesting some genuine intent to protect the domestic industry but also potential performative aspects in signaling protectionist policies.
 *
 * PERSPECTIVAL GAP:
 *   Argentine consumers and Chinese EV exporters see the tariff as a snare, limiting their options and increasing costs. Argentine auto manufacturers see it as a rope, providing coordination and protection. The Argentine government sees it as a tangled rope, balancing the benefits of protecting domestic industry with the risks of trade retaliation and reduced consumer welfare. The analytical observer sees the policy as a mixed bag with long-term consequences for innovation and sustainability.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (Argentine auto manufacturers) experience a low d value due to their protected market position and potential for increased profits. Victims (Argentine consumers and Chinese EV exporters) experience high d values due to limited choices and reduced market access, respectively. The government's d value is moderate, reflecting their constrained position and the need to balance competing interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The tariff policy could be misconstrued as pure extraction, but it also has a coordination component by aiming to protect and develop the domestic auto industry. The tangled rope classification acknowledges both the extractive effects on consumers and exporters and the coordination function of supporting local manufacturing. Mandatrophy is resolved because the government's intent is not solely to extract but to foster domestic growth, albeit with potentially negative consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_retaliation,
    'Will China retaliate with tariffs on Argentine exports?',
    'Monitoring trade policy announcements and analyzing trade flows',
    'If yes, Argentina''s overall trade balance will worsen. If no, the tariff''s protectionist effect will be more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_retaliation, empirical, 'The likelihood of Chinese retaliation').

omega_variable(
    consumer_demand_elasticity,
    'How sensitive is Argentine consumer demand for EVs to price increases?',
    'Analyzing sales data before and after the tariff implementation',
    'If highly elastic, the tariff will significantly reduce EV sales. If inelastic, the tariff will primarily transfer wealth from consumers to manufacturers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_demand_elasticity, empirical, 'Consumer demand elasticity for EVs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arg_ev_tariff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arg__tr_t0, arg_ev_tariff, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arg__tr_t5, arg_ev_tariff, theater_ratio, 5, 0.3).
narrative_ontology:measurement(arg__tr_t10, arg_ev_tariff, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(arg__be_t0, arg_ev_tariff, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(arg__be_t5, arg_ev_tariff, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(arg__be_t10, arg_ev_tariff, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arg_ev_tariff, resource_allocation).
narrative_ontology:affects_constraint(arg_ev_tariff, global_ev_market).
narrative_ontology:affects_constraint(arg_ev_tariff, china_argentina_trade_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
