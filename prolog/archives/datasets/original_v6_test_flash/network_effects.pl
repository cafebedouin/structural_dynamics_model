% ============================================================================
% CONSTRAINT STORY: network_effects
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effects, []).

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
 *   constraint_id: network_effects
 *   human_readable: Network Effects (Demand-Side Economies of Scale)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Network effects, where a product or service becomes more valuable as more
 *   people use it, create a complex interplay of coordination and extraction.
 *   While users benefit from increased utility and standardization, they also
 *   face potential lock-in, data privacy risks, and the possibility of the
 *   network declining in value. Platform owners are the primary
 *   beneficiaries, while late adopters and users of competing networks may
 *   experience extraction.
 *
 * KEY AGENTS:
 *   - Platform Owners: Primary beneficiary (institutional/arbitrage) - benefit from increasing network value.
 *   - Early Adopters: Secondary beneficiary (powerful/mobile) - gain early access and influence.
 *   - Late Adopters: Primary victim (powerless/trapped) - potentially trapped in declining or less valuable networks.
 *   - Users of Competing Networks: Secondary victim (moderate/constrained) - face potential lock-in to a single network.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effects, 0.35).
domain_priors:suppression_score(network_effects, 0.3).
domain_priors:theater_ratio(network_effects, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effects, extractiveness, 0.35).
narrative_ontology:constraint_metric(network_effects, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(network_effects, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effects, tangled_rope).
narrative_ontology:human_readable(network_effects, "Network Effects (Demand-Side Economies of Scale)").
narrative_ontology:topic_domain(network_effects, "economic/technological").

domain_priors:requires_active_enforcement(network_effects).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effects, platform_owners).
narrative_ontology:constraint_beneficiary(network_effects, early_adopters).
narrative_ontology:constraint_victim(network_effects, late_adopters).
narrative_ontology:constraint_victim(network_effects, users_of_competing_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Users trapped in a declining network (Snare). Users who are late adopters or unable to switch due to high switching costs or lack of alternatives may find themselves trapped in a declining network, extracting value from their continued usage despite the declining utility. High extractiveness, low power, trapped exit.
constraint_indexing:constraint_classification(network_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Platform Owners (Rope). Platform owners benefit from the increasing value of their network as more users join, which can translate into higher revenue, market share, and competitive advantage. Low extractiveness, high power, arbitrage exit.
constraint_indexing:constraint_classification(network_effects, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: Typical User (Tangled Rope). Users benefit from increased utility as more people join the network, but also bear costs related to potential lock-in, data privacy concerns, and the risk of the network declining in value. Medium extractiveness, medium power, mobile exit.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 4: Analytical Observer (Tangled Rope). From a broad, long-term perspective, network effects create both coordination benefits (increased utility, standardization) and extraction risks (monopolies, lock-in, data exploitation). The overall impact on society is a mix of rope and snare, hence Tangled Rope.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effects_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effects, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(network_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. While network effects provide significant benefits, they can also lead to lock-in, data exploitation, and the suppression of alternatives. The extractiveness score reflects this tension. Suppression (0.30): Moderate. Network effects create barriers to entry for competing networks and can limit user choice. The suppression score reflects the reduced ability of users to switch to alternative platforms. Theater Ratio (0.10): Low. The coordination aspect is generally not theatrical, but there can be theater related to claiming network effects before the network is big enough to provide real value.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions of users and platform owners. Platform owners see network effects as a rope, enabling coordination and growth. Users see a more complex picture, with benefits and risks, leading to a tangled rope classification. Users trapped in declining networks perceive a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic follows the structural positions of the agents. Platform owners (institutional/arbitrage) have a low 'd' value, experiencing network effects as a coordination mechanism. Trapped users (powerless/trapped) have a high 'd' value, experiencing network effects as a snare. Typical users (moderate/mobile) have a medium 'd' value, experiencing both benefits and risks.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as Tangled Rope at the analytical level to reflect the combination of coordination and extraction. Classifying it solely as Rope would ignore the potential for lock-in and data exploitation. Classifying it solely as Snare would ignore the significant benefits that network effects can provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_stability,
    'How stable is the tipping point in a network effect? Can a seemingly dominant network be rapidly displaced by a new entrant?',
    'Historical analysis of network dominance and displacement across various industries and technologies.',
    'If tipping points are easily reversed, the extraction is less severe. If tipping points are highly stable, the extraction becomes more entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_stability, empirical, 'Stability of tipping points in network effects.').

omega_variable(
    switching_cost_impact,
    'How significantly do switching costs affect users'' ability to leave a network?',
    'Surveys and economic modeling of switching behavior in different network contexts.',
    'High switching costs exacerbate the snare effect. Low switching costs reduce the snare effect and increase user power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_impact, empirical, 'The impact of switching costs on user behavior.').

omega_variable(
    data_privacy_valuation,
    'How do users value their data privacy in relation to the benefits of network participation?',
    'User surveys and experiments to elicit willingness-to-pay for data privacy.',
    'Higher valuation of privacy increases the perceived extraction from networks that exploit user data. Lower valuation reduces the perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_privacy_valuation, preference, 'User valuation of data privacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effects, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(netw_tr_t0, network_effects, theater_ratio, 0, 0.05).
narrative_ontology:measurement(netw_tr_t5, network_effects, theater_ratio, 5, 0.08).
narrative_ontology:measurement(netw_tr_t10, network_effects, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(netw_be_t0, network_effects, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(netw_be_t5, network_effects, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(netw_be_t10, network_effects, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effects, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
