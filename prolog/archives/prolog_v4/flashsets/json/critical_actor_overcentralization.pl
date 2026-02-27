% ============================================================================
% CONSTRAINT STORY: critical_actor_overcentralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_actor_overcentralization, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: critical_actor_overcentralization
 *   human_readable: The Single Point of Failure
 *   domain: logistical/technological/economic
 *
 * SUMMARY:
 *   Critical actor overcentralization describes a scenario where a network’s
 *   functional survival depends entirely on a single node—be it a
 *   clearinghouse bank, a cloud provider, or a charismatic leader. This
 *   creates a single point of failure that can be exploited or can fail,
 *   harming all dependent actors.
 *
 * KEY AGENTS:
 *   - Critical Actor: Primary beneficiary (institutional/arbitrage) - Benefits from increased power, influence, and revenue streams.
 *   - Network Participants: Primary victims (powerless/trapped) - Vulnerable due to dependence on the central actor.
 *   - Regulators: Constrained actors (moderate/constrained) - Can influence but not fully control the central actor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_actor_overcentralization, 0.65).
domain_priors:suppression_score(critical_actor_overcentralization, 0.75).
domain_priors:theater_ratio(critical_actor_overcentralization, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_actor_overcentralization, extractiveness, 0.65).
narrative_ontology:constraint_metric(critical_actor_overcentralization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(critical_actor_overcentralization, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_actor_overcentralization, tangled_rope).
narrative_ontology:human_readable(critical_actor_overcentralization, "The Single Point of Failure").
narrative_ontology:topic_domain(critical_actor_overcentralization, "logistical/technological/economic").

domain_priors:requires_active_enforcement(critical_actor_overcentralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_actor_overcentralization, critical_actor).
narrative_ontology:constraint_victim(critical_actor_overcentralization, network_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The critical actor benefits from their central role through increased power, influence, and revenue streams. They can arbitrage their position to further solidify control.
constraint_indexing:constraint_classification(critical_actor_overcentralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Network participants are trapped due to high switching costs or lack of alternatives, making them vulnerable to exploitation or failure of the critical actor. They bear the brunt of the system's collapse if the single point of failure occurs.
constraint_indexing:constraint_classification(critical_actor_overcentralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Regulators are constrained by existing regulations and lobbying efforts of the critical actor but also benefit from the stability the critical actor provides. The system extracts from them by increasing systemic risk, while also allowing them to maintain a degree of control. They can not fully exit, but are more than trapped.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The analytical observer can see both sides. High centralization coordinates resources and increases efficiency up to a point. That point is reached when the rest of the network becomes unable to act on its own.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_actor_overcentralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_actor_overcentralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(critical_actor_overcentralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The system is classified as a tangled rope, because a single point of failure is a severe and total problem for its victim, the participants of the network, but the critical actor also provides a coordination function. The critical actor gains power and benefits from extraction, and requires active enforcement to maintain its position.
 *
 * PERSPECTIVAL GAP:
 *   The gap stems from the differential distribution of benefits and costs. The critical actor experiences the system as beneficial and enabling (rope), while network participants experience it as constraining and risky (snare). Regulators have a mixed view (tangled_rope), acknowledging both the benefits and the risks.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent. The critical actor, as beneficiary with arbitrage options, has a low d. Network participants, trapped and bearing the costs, have a high d. Regulators, being between, have a moderate d.
 *
 * MANDATROPHY ANALYSIS:
 *   The original classification as a snare was incorrect because it did not account for the coordination function provided by the critical actor. By classifying it as a tangled rope, we acknowledge both the benefits and the risks associated with overcentralization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actor_substitutability,
    'How easily can the critical actor be replaced or substituted with alternative actors or decentralized systems?',
    'Assess availability of alternative providers, transition costs, and regulatory hurdles for switching to different models.',
    'High substitutability reduces the ''snare'' effect; low substitutability increases the risk of systemic failure and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actor_substitutability, empirical, 'Assesses the difficulty of substituting the critical actor with alternatives.').

omega_variable(
    failure_probability,
    'What is the probability of failure for the critical actor, considering technological, economic, and political risks?',
    'Conduct a risk assessment that considers historical data, stress tests, and expert opinions on potential failure modes.',
    'Higher failure probability significantly amplifies the snare characteristic of the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(failure_probability, empirical, 'Evaluates the likelihood of the critical actor''s failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_actor_overcentralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crit_tr_t0, critical_actor_overcentralization, theater_ratio, 0, 0.1).
narrative_ontology:measurement(crit_tr_t5, critical_actor_overcentralization, theater_ratio, 5, 0.2).
narrative_ontology:measurement(crit_tr_t10, critical_actor_overcentralization, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(crit_be_t0, critical_actor_overcentralization, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(crit_be_t5, critical_actor_overcentralization, base_extractiveness, 5, 0.525).
narrative_ontology:measurement(crit_be_t10, critical_actor_overcentralization, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(critical_actor_overcentralization, market_concentration).
narrative_ontology:affects_constraint(critical_actor_overcentralization, interbank_lending_contagion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
