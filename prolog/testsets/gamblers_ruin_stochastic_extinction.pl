% ============================================================================
% CONSTRAINT STORY: gamblers_ruin_stochastic_extinction
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_gamblers_ruin_stochastic_extinction, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: gamblers_ruin_stochastic_extinction
 * human_readable: Gambler's Ruin
 * domain: mathematical/economic
 * * SUMMARY:
 * Gambler's Ruin is a statistical theorem stating that a gambler with finite
 * wealth, playing a fair or negative-expectation game against an opponent with
 * effectively infinite wealth (the "House"), will eventually go bankrupt with
 * a probability approaching 1. It represents the inescapable "suction" of a
 * random walk toward an absorbing boundary (zero capital).
 * * KEY AGENTS:
 * - The Finite Gambler: Subject (Powerless), whose capital is a limited buffer
 *   against the variance of the random walk.
 * - The House/Casino: Beneficiary (Institutional), who uses the Law of Large
 *   Numbers to guarantee profits from an aggregate of finite-capital players.
 * - The Analytical Observer: Auditor (Analytical), who sees the ruin as a
 *   pure extraction mechanism disguised as a game of chance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: 0.90. This is a highly extractive constraint. It mathematically
% guarantees the total transfer of wealth from the powerless agent to the
% institutional agent given enough time.
domain_priors:base_extractiveness(gamblers_ruin_stochastic_extinction, 0.90).
% Rationale: 0.50. The constraint suppresses alternative strategies (like
% Martingale) by imposing implicit limits (table caps, finite player wealth)
% that make them non-viable.
domain_priors:suppression_score(gamblers_ruin_stochastic_extinction, 0.50).
% Rationale: 0.05. The constraint is a mathematical reality; there is no
% performative or theatrical component.
domain_priors:theater_ratio(gamblers_ruin_stochastic_extinction, 0.05).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, extractiveness, 0.90).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as an immutable law of mathematics/nature.
narrative_ontology:constraint_claim(gamblers_ruin_stochastic_extinction, snare).
narrative_ontology:human_readable(gamblers_ruin_stochastic_extinction, "Gambler's Ruin").

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(gamblers_ruin_stochastic_extinction, casino_operators).
narrative_ontology:constraint_beneficiary(gamblers_ruin_stochastic_extinction, deep_pocket_investors).
narrative_ontology:constraint_victim(gamblers_ruin_stochastic_extinction, finite_capital_players).
narrative_ontology:constraint_victim(gamblers_ruin_stochastic_extinction, leveraged_small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE FINITE GAMBLER (SNARE)
% Feels like an immutable Mountain, but its high extraction (ε=0.9) makes it a
% Snare. The inevitability of the random walk extracts all capital.
% χ = 0.90 * π(powerless) * σ(local) = 0.90 * 1.5 * 0.8 = 1.08.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CASINO OWNER (ROPE)
% For the House, this is a pure coordination mechanism for profit. Their
% institutional power inverts the perceived extraction.
% χ = 0.90 * π(institutional) * σ(global) = 0.90 * -0.2 * 1.2 = -0.216.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The observer sees the raw metrics. High base extraction and suppression,
% with no sunset clause or real coordination function, is a Snare.
% χ = 0.90 * π(analytical) * σ(universal) = 0.90 * 1.15 * 1.0 = 1.035.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gamblers_ruin_stochastic_extinction_tests).

test(perspectival_gap) :-
    % Verify the gap between the player (Snare) and the house (Rope).
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(threshold_validation_snare) :-
    % Verify that the base extractiveness meets the Snare threshold.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, ExtMetricName, E),
    assertion(E >= 0.46).

:- end_tests(gamblers_ruin_stochastic_extinction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core challenge here is classifying a mathematical law. While it feels
 * like a Mountain (immutable, unchangeable), its functional effect is pure,
 * asymmetric extraction (ε=0.90), which violates the Mountain's low-extraction
 * requirement (ε <= 0.15). The system correctly identifies this as a Snare for
 * any agent with finite resources. The constraint's claim to be a `natural_law`
 * is true, but this truth is weaponized to create a Snare.
 *
 * The Perspectival Gap is stark:
 * - For the powerless gambler, it's a Snare that guarantees eventual ruin.
 * - For the institutional casino, their "infinite" capital and negative power
 *   modifier (π=-0.2) transform the Snare into a Rope—a reliable tool for
 *   coordinating revenue. This is a classic example of how power can invert
 *   the perception and function of a constraint.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is a rare case where a high-extraction constraint (ε=0.90) is not a
 * Tangled Rope because it lacks a genuine, constructed coordination function
 * and does not require active enforcement. It's a "natural" Snare. The system
 * avoids misclassifying it as a Mountain by strictly adhering to the extraction
 * metric, revealing the exploitative application of a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gamblers_ruin,
    'Is the "infinite bank" assumption valid in a world of finite capital, or does it just mean "vastly larger"?',
    'Empirical analysis of casino failure rates or systemic financial crises where "the house" also goes bankrupt.',
    'If the house is also finite, the constraint becomes symmetric, potentially shifting from a Snare to a complex game theory problem.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gamblers_ruin_stochastic_extinction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a mathematical theorem, its properties are constant. The temporal data
% reflects this stability, showing no drift over the interval.
%
% Theater ratio over time (constant):
narrative_ontology:measurement(grse_tr_t0, gamblers_ruin_stochastic_extinction, theater_ratio, 0, 0.05).
narrative_ontology:measurement(grse_tr_t5, gamblers_ruin_stochastic_extinction, theater_ratio, 5, 0.05).
narrative_ontology:measurement(grse_tr_t10, gamblers_ruin_stochastic_extinction, theater_ratio, 10, 0.05).

% Extraction over time (constant):
narrative_ontology:measurement(grse_ex_t0, gamblers_ruin_stochastic_extinction, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(grse_ex_t5, gamblers_ruin_stochastic_extinction, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(grse_ex_t10, gamblers_ruin_stochastic_extinction, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% For the casino, the law of large numbers is a mechanism to allocate the
% resources (bets) of many players toward the house.
narrative_ontology:coordination_type(gamblers_ruin_stochastic_extinction, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */