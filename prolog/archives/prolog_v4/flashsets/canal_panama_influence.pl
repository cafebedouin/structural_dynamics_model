% ============================================================================
% CONSTRAINT STORY: canal_panama_influence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canal_panama_influence, []).

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
 *   constraint_id: canal_panama_influence
 *   human_readable: Geopolitical Influence over Panama Canal
 *   domain: political
 *
 * SUMMARY:
 *   The Panama Canal, a crucial global trade route, is subject to a
 *   geopolitical struggle for influence between the US and China, with Swiss
 *   companies playing a key role. This complex interplay affects Panama's
 *   sovereignty and economic stability. The situation can be viewed as a
 *   tangled rope, with Panama caught in the middle of competing interests.
 *
 * KEY AGENTS:
 *   - United States: Historically dominant power, seeking to maintain influence (powerful/constrained)
 *   - China: Rising power, seeking to expand influence (powerful/constrained)
 *   - Panama: Nation seeking to maintain sovereignty and benefit economically (powerless/trapped)
 *   - Swiss Companies: Neutral actors, providing expertise and seeking commercial gain (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canal_panama_influence, 0.55).
domain_priors:suppression_score(canal_panama_influence, 0.6).
domain_priors:theater_ratio(canal_panama_influence, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canal_panama_influence, extractiveness, 0.55).
narrative_ontology:constraint_metric(canal_panama_influence, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(canal_panama_influence, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canal_panama_influence, tangled_rope).
narrative_ontology:human_readable(canal_panama_influence, "Geopolitical Influence over Panama Canal").
narrative_ontology:topic_domain(canal_panama_influence, "political").

domain_priors:requires_active_enforcement(canal_panama_influence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canal_panama_influence, china).
narrative_ontology:constraint_beneficiary(canal_panama_influence, swiss_companies).
narrative_ontology:constraint_victim(canal_panama_influence, united_states).
narrative_ontology:constraint_victim(canal_panama_influence, panama).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Panama experiences the constraint as a snare due to its limited ability to resist external influence and its dependence on the canal for economic stability. It's trapped in a geopolitical power play with limited options.
constraint_indexing:constraint_classification(canal_panama_influence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Swiss companies benefit from the canal's operations and seek to maintain a neutral position, leveraging their expertise for commercial gain, giving them arbitrage opportunities.
constraint_indexing:constraint_classification(canal_panama_influence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% China experiences the canal as a tangled rope. It benefits from the canal for trade but is also constrained by the US's historical influence. It's attempting to increase influence, but constrained by existing agreements.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The US views the situation as a tangled rope. While historically dominant, its influence is being challenged. It benefits from the canal's stability but is constrained by the rising influence of China and other actors.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer sees the Panama Canal situation as a tangled rope: multiple actors vying for influence, with the canal's operation and Panama's sovereignty caught in the middle.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canal_panama_influence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canal_panama_influence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canal_panama_influence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canal_panama_influence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(canal_panama_influence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the pressure on Panama to concede to the demands of larger powers. Suppression (0.60) indicates the limitations on Panama's ability to resist these pressures and the restrictions on alternative routes. The theater ratio (0.40) indicates that while there is some public discourse and diplomatic activity surrounding the canal, much of the real influence is exerted behind the scenes.
 *
 * PERSPECTIVAL GAP:
 *   The US and China see the situation as a tangled rope, constrained by existing agreements and each other's influence. Panama, however, experiences it as a snare, with limited options and a high degree of dependence. Swiss companies see the situation as a rope, from which they derive benefits through arbitrage.
 *
 * DIRECTIONALITY LOGIC:
 *   China and Swiss companies are identified as beneficiaries as they gain from the canal. The US is identified as a victim as its historical dominance is challenged. Panama is a victim as it experiences pressure from external powers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_canal_alternatives,
    'Will alternative shipping routes or technologies reduce the canal''s strategic importance?',
    'Assess feasibility and cost of other routes (Arctic, land bridges) and evaluate adoption of alternative shipping tech.',
    'Reduced strategic value shifts the balance of power and lessens the geopolitical tensions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_canal_alternatives, empirical, 'Impact of alternative shipping solutions on canal''s geopolitical significance.').

omega_variable(
    panama_sovereignty_assertion,
    'Can Panama successfully assert its sovereignty and resist undue influence from external powers?',
    'Monitor Panama''s political and economic decisions, its relations with US/China, and its efforts to diversify its economy.',
    'Stronger Panamanian sovereignty could mitigate the extractiveness of the geopolitical rivalry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panama_sovereignty_assertion, conceptual, 'Panama''s ability to assert its sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canal_panama_influence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cana_tr_t0, canal_panama_influence, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cana_tr_t10, canal_panama_influence, theater_ratio, 10, 0.4).
narrative_ontology:measurement(cana_tr_t20, canal_panama_influence, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(cana_be_t0, canal_panama_influence, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cana_be_t10, canal_panama_influence, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cana_be_t20, canal_panama_influence, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canal_panama_influence, global_infrastructure).
narrative_ontology:affects_constraint(canal_panama_influence, global_trade_routes).
narrative_ontology:affects_constraint(canal_panama_influence, us_china_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
