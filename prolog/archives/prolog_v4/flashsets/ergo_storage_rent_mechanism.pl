% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_storage_rent_mechanism, []).

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
 *   constraint_id: ergo_storage_rent_mechanism
 *   human_readable: Ergo Storage Rent (Demurrage)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Ergo introduces a "Storage Rent" or demurrage fee for inactive data
 *   stored on the blockchain. This mechanism aims to prevent blockchain bloat
 *   by incentivizing users to either move their funds or pay a small fee for
 *   keeping their data on-chain indefinitely. The storage rent can be viewed
 *   as a tangled rope, providing benefits to active network participants and
 *   the Ergo Foundation while potentially extracting value from long-term
 *   inactive users.
 *
 * KEY AGENTS:
 *   - Long-Term Inactive Users: Primary target (powerless/trapped) - Subject to the demurrage fee if their UTXOs remain inactive for a prolonged period.
 *   - Active Network Participants: Primary beneficiary (powerful/mobile) - Benefit from a more efficient and scalable blockchain.
 *   - Ergo Foundation: Secondary beneficiary (institutional/arbitrage) - Receives a portion of the storage rent fees to fund network development and maintenance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_storage_rent_mechanism, 0.35).
domain_priors:suppression_score(ergo_storage_rent_mechanism, 0.2).
domain_priors:theater_ratio(ergo_storage_rent_mechanism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, extractiveness, 0.35).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_storage_rent_mechanism, tangled_rope).
narrative_ontology:human_readable(ergo_storage_rent_mechanism, "Ergo Storage Rent (Demurrage)").
narrative_ontology:topic_domain(ergo_storage_rent_mechanism, "economic/technological").

domain_priors:requires_active_enforcement(ergo_storage_rent_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, active_network_participants).
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, ergo_foundation).
narrative_ontology:constraint_victim(ergo_storage_rent_mechanism, long_term_inactive_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Inactive user who has lost their keys and cannot move their funds. They are trapped and subject to storage rent indefinitely.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% User who holds Ergo long-term and actively participates in the network, albeit infrequently, they are subject to the rent but benefit from the network's overall health.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The Ergo Foundation benefits from the storage rent mechanism as it ensures network viability and prevents bloat. It's a rope because it facilitates coordination.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Active participants benefit from a cleaner, more efficient blockchain due to the storage rent. This promotes network health and decentralization.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ergo_storage_rent_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The storage rent extracts a small fee from inactive users, contributing to network maintenance. Suppression: Suppression arises from the fact that users need to actively move their funds to avoid the rent, potentially creating a barrier to entry for some. Theater ratio: The theater ratio is low because the mechanism is primarily functional, aimed at preventing bloat.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ based on the agent's activity and ability to exit. Inactive users see the mechanism as a snare, as they are potentially trapped and subject to the fee. Active participants and the Ergo Foundation see it as a rope, ensuring network viability and efficiency. The long-term hodler sees it as a tangled rope, subject to the rent, but aware of its benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by whether the agent benefits from or bears the cost of the storage rent. Inactive users bear the cost, while active users and the foundation benefit. Active participants have mobile exit, as they can move their funds and are benefited by the network viability. Ergo Foundation benefits from network health, contributing to its growth.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rent_threshold_fairness,
    'Is the rent threshold calibrated fairly to balance network viability and user burden?',
    'Analyze the distribution of UTXOs and their activity to determine an optimal threshold for storage rent.',
    'Determines whether the storage rent disproportionately affects small or infrequent users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_threshold_fairness, empirical, 'Fairness of storage rent threshold').

omega_variable(
    key_loss_mitigation,
    'How can key loss be mitigated to prevent permanent extraction from trapped users?',
    'Explore social recovery mechanisms or other methods for key recovery.',
    'Reduces the negative impact of storage rent on users who have lost their private keys.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(key_loss_mitigation, conceptual, 'Mitigation of extraction due to key loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_tr_t0, ergo_storage_rent_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ergo_tr_t5, ergo_storage_rent_mechanism, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ergo_tr_t10, ergo_storage_rent_mechanism, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(ergo_be_t0, ergo_storage_rent_mechanism, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ergo_be_t5, ergo_storage_rent_mechanism, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(ergo_be_t10, ergo_storage_rent_mechanism, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_storage_rent_mechanism, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
