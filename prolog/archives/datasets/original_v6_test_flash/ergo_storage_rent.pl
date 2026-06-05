% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_storage_rent, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_storage_rent
 *   human_readable: Ergo Storage Rent (UTXO Demurrage)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Ergo's storage rent is a mechanism designed to incentivize activity on
 *   the network by charging a demurrage fee on UTXOs that remain unspent for
 *   four years. This mechanism aims to reduce chain bloat and ensure a more
 *   active circulation of coins, but it also creates a risk for long-term
 *   inactive holders who may lose their funds. The implementation and effects
 *   of storage rent create a complex dynamic with varying perspectives.
 *
 * KEY AGENTS:
 *   - Long-term Inactive UTXO Holders: Primary target (powerless/trapped) - Risk losing funds due to storage rent.
 *   - Active Ergo Users: Primary beneficiary (institutional/arbitrage) - Benefit from a more active and efficient network.
 *   - Ergo Miners: Secondary beneficiary (powerful/arbitrage) - Receive storage rent fees as compensation for maintaining the network.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_storage_rent, 0.55).
domain_priors:suppression_score(ergo_storage_rent, 0.4).
domain_priors:theater_ratio(ergo_storage_rent, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_storage_rent, extractiveness, 0.55).
narrative_ontology:constraint_metric(ergo_storage_rent, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ergo_storage_rent, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_storage_rent, tangled_rope).
narrative_ontology:human_readable(ergo_storage_rent, "Ergo Storage Rent (UTXO Demurrage)").
narrative_ontology:topic_domain(ergo_storage_rent, "technological/economic").

domain_priors:requires_active_enforcement(ergo_storage_rent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_storage_rent, active_ergo_users).
narrative_ontology:constraint_beneficiary(ergo_storage_rent, ergo_miners).
narrative_ontology:constraint_victim(ergo_storage_rent, long_term_inactive_utxo_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Inactive UTXO holders may be unaware of the storage rent mechanism and lose funds without active participation. They are essentially trapped if they lose their keys or remain inactive for extended periods.
constraint_indexing:constraint_classification(ergo_storage_rent, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Active users benefit from the increased availability of circulating coins and the network security provided by miners who are incentivized by storage rent revenue. They can arbitrage the system by moving their coins to avoid the rent.
constraint_indexing:constraint_classification(ergo_storage_rent, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, storage rent is a complex mechanism that aims to balance the economic incentives of active participation with the potential for loss of funds for inactive users. It can be seen as both a coordination tool and an extractive measure.
constraint_indexing:constraint_classification(ergo_storage_rent, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Miners benefit from the storage rent as it supplements block rewards, incentivizing them to maintain network security. They can arbitrage by choosing which transactions to include in blocks.
constraint_indexing:constraint_classification(ergo_storage_rent, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_storage_rent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_storage_rent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ergo_storage_rent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is set to 0.55 because while the rent does extract value from inactive users, the intention is not purely extractive but also to promote network health. Suppression is 0.40, as users have the option to move their UTXOs to avoid the rent, but there is a barrier for those unaware or unable to do so. The theater ratio is low, as the mechanism is directly tied to economic incentives rather than performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Inactive UTXO holders view the storage rent as a snare because they risk losing their funds, especially if they are unaware of the mechanism or have lost access to their wallets. Active users and miners, on the other hand, view it as a rope because it promotes network activity and provides compensation for miners. The analytical observer sees the tangled rope nature of the mechanism, balancing extraction and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is based on the beneficiary/victim status and exit options. Inactive holders have limited exit options (trapped), leading to a high directionality value and classification as a snare. Active users and miners have arbitrage options, resulting in lower directionality values and classifications as ropes. The analytical observer takes a more neutral stance, leading to a tangled rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utxo_activity_threshold,
    'What is the optimal inactivity period before storage rent applies to balance encouraging activity with preserving long-term holdings?',
    'Empirical analysis of UTXO activity patterns and user behavior on the Ergo network.',
    'Determines the degree of extraction experienced by inactive users and the effectiveness of the mechanism in promoting coin circulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utxo_activity_threshold, empirical, 'Optimal inactivity period for storage rent').

omega_variable(
    alternative_demurrage_mechanisms,
    'Are there alternative demurrage mechanisms that could achieve similar economic benefits with reduced impact on long-term inactive holders?',
    'Comparative analysis of different demurrage models and their potential effects on user behavior and network economics.',
    'Determines whether the current storage rent mechanism is the most efficient and equitable solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_demurrage_mechanisms, conceptual, 'Alternative demurrage mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_tr_t0, ergo_storage_rent, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ergo_tr_t5, ergo_storage_rent, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ergo_tr_t10, ergo_storage_rent, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ergo_be_t0, ergo_storage_rent, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ergo_be_t5, ergo_storage_rent, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ergo_be_t10, ergo_storage_rent, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_storage_rent, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
