% ============================================================================
% CONSTRAINT STORY: matching_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_matching_markets, []).

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
 *   constraint_id: matching_markets
 *   human_readable: Matching Market Congestion Externality
 *   domain: economic
 *
 * SUMMARY:
 *   In matching markets, increased participation can lead to congestion,
 *   reducing the matching probability for individual participants. This
 *   congestion externality creates a tension between the benefits of a larger
 *   market and the costs of reduced matching efficiency. Market platforms
 *   benefit from increased participation, while late adopters and
 *   low-priority participants suffer from congestion.
 *
 * KEY AGENTS:
 *   - Market Platforms: Beneficiary (institutional/arbitrage)
 *   - Late Adopters: Victim (powerless/trapped)
 *   - Early Adopters: Beneficiary (powerful/mobile)
 *   - Low Priority Participants: Victim (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(matching_markets, 0.5).
domain_priors:suppression_score(matching_markets, 0.3).
domain_priors:theater_ratio(matching_markets, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(matching_markets, extractiveness, 0.5).
narrative_ontology:constraint_metric(matching_markets, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(matching_markets, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(matching_markets, tangled_rope).
narrative_ontology:human_readable(matching_markets, "Matching Market Congestion Externality").
narrative_ontology:topic_domain(matching_markets, "economic").

domain_priors:requires_active_enforcement(matching_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(matching_markets, market_platforms).
narrative_ontology:constraint_beneficiary(matching_markets, early_adopters).
narrative_ontology:constraint_victim(matching_markets, late_adopters).
narrative_ontology:constraint_victim(matching_markets, low_priority_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Late adopters experience reduced matching probabilities due to congestion caused by earlier participants. They are often trapped as they need the market to find matches but suffer from its congestion.
constraint_indexing:constraint_classification(matching_markets, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Market platforms benefit from increased participation, even with congestion, as a larger market attracts more users and generates more revenue. They can arbitrage the situation.
constraint_indexing:constraint_classification(matching_markets, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the congestion externality as a tangled rope, with coordination (platform benefits) and extraction (late adopter costs) intertwined.
constraint_indexing:constraint_classification(matching_markets, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Early adopters often experience better matching probabilities before congestion becomes severe. They can benefit from the network effects without significant congestion costs. Mobile since congestion hasn't driven them off the platform
constraint_indexing:constraint_classification(matching_markets, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(matching_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(matching_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(matching_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(matching_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.5): Moderate. Late adopters and low-priority participants experience reduced matching probabilities due to congestion, representing a cost extracted from them. Suppression (0.3): Low. Participants are not entirely suppressed as they can still find matches, but the probability is reduced. Platforms benefit but also need to balance congestion to maintain market quality. Theater Ratio (0.1): Low. There is little theatrical activity associated with this externality.
 *
 * PERSPECTIVAL GAP:
 *   Late adopters experience the externality as a snare because they are trapped in a congested market with reduced matching probabilities. Market platforms, however, benefit from increased participation and see the externality as a form of coordination. An analytical observer sees both the coordination and extraction aspects, classifying it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by who benefits and who bears the costs of the congestion externality. Market platforms benefit from increased participation, resulting in a low d value. Late adopters and low-priority participants bear the costs of reduced matching probabilities, resulting in a high d value. This leads to different classifications from different perspectives. Early Adopters are mobile at t=0, making them distinct from late adopters who are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that different actors experience the same phenomenon differently. What appears as coordination from the platform's perspective is extraction from the late adopter's perspective. The tangled rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    congestion_measurement,
    'How accurately can congestion be measured and predicted in matching markets?',
    'Development of better congestion metrics and predictive models using market data.',
    'Improved measurement could lead to better congestion management strategies and fairer market outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_measurement, empirical, 'Accuracy of congestion measurement and prediction').

omega_variable(
    platform_intervention,
    'What interventions can platforms use to mitigate congestion without reducing overall market participation?',
    'Experimentation with pricing strategies, matching algorithms, and market segmentation.',
    'Effective interventions can improve market efficiency and reduce the negative impact on late adopters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_intervention, empirical, 'Effectiveness of platform interventions for congestion mitigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(matching_markets, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(matc_tr_t0, matching_markets, theater_ratio, 0, 0.05).
narrative_ontology:measurement(matc_tr_t5, matching_markets, theater_ratio, 5, 0.1).
narrative_ontology:measurement(matc_tr_t10, matching_markets, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(matc_be_t0, matching_markets, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(matc_be_t5, matching_markets, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(matc_be_t10, matching_markets, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(matching_markets, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
