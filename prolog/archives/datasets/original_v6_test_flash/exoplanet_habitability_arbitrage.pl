% ============================================================================
% CONSTRAINT STORY: exoplanet_habitability_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exoplanet_habitability_arbitrage, []).

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
 *   constraint_id: exoplanet_habitability_arbitrage
 *   human_readable: Exoplanetary Habitability Arbitrage Strategy
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint represents the strategic pivot in the search for
 *   extraterrestrial life, moving from seeking "Earth twins" to identifying
 *   anomalies based on our solar system's observed statistical "weirdness".
 *   It's a tangled rope because it provides funding and opportunity for some,
 *   while potentially hindering others and leading to neglect of potentially
 *   habitable but less anomalous worlds. The extraction arises from
 *   researchers finding past methods of earth-twin hunting less valuable. The
 *   arbitrage comes from identifying potentially overlooked, and more easily
 *   discovered, signatures of life on non-earth-like planets.
 *
 * KEY AGENTS:
 *   - Earth-Twin Focused Research: Primary target (powerless/trapped) - research teams whose expertise is now less valued.
 *   - Individual Researchers: Secondary actors (moderate/constrained) - researchers who have some flexibility but also face costs in adapting.
 *   - Research Groups with Flexible Funding: Primary beneficiary (institutional/arbitrage) - groups who can quickly shift focus and gain funding.
 *   - Telescope Time Allocators: Beneficiary (institutional/arbitrage) - can allocate time based on perceived promise.
 *   - Public Understanding of Science: Victim (powerless/trapped) - shift in narrative may lead to public misunderstanding if nuances are lost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_habitability_arbitrage, 0.55).
domain_priors:suppression_score(exoplanet_habitability_arbitrage, 0.45).
domain_priors:theater_ratio(exoplanet_habitability_arbitrage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, extractiveness, 0.55).
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_habitability_arbitrage, tangled_rope).
narrative_ontology:human_readable(exoplanet_habitability_arbitrage, "Exoplanetary Habitability Arbitrage Strategy").
narrative_ontology:topic_domain(exoplanet_habitability_arbitrage, "technological/scientific").

domain_priors:requires_active_enforcement(exoplanet_habitability_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_habitability_arbitrage, research_groups_flexible_funding).
narrative_ontology:constraint_beneficiary(exoplanet_habitability_arbitrage, telescope_time_allocators).
narrative_ontology:constraint_victim(exoplanet_habitability_arbitrage, earth_twin_focused_research).
narrative_ontology:constraint_victim(exoplanet_habitability_arbitrage, public_understanding_of_science).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Researchers stuck in the 'Earth-twin' paradigm find their research less valued and funding opportunities dwindling. They are trapped by previous specialization and potentially find it difficult to pivot, making them targets of the paradigm shift.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Researchers with some flexibility face a mixed situation. They benefit from new areas opening up, but are also constrained by the need to learn new skills and potentially abandon prior work, so extraction is mitigated by a coordination element.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Research groups that can quickly shift focus towards habitability anomalies stand to gain significant funding and telescope time, experiencing this constraint as pure coordination and an opportunity for arbitrage.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Those deciding telescope time can allocate it based on perceived promise, seeing the arbitrage as a tool towards maximizing discovery; more likely to take bets.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A long-term view reveals a coordination/extraction mix: Focusing on anomalies may accelerate discovery, but carries the risk of overlooking more subtle, Earth-like environments, or propagating false claims more easily, creating an epistemic cost.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanet_habitability_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exoplanet_habitability_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exoplanet_habitability_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate - substantial resources are diverted away from previous research strategies. Suppression (0.45): Moderate - some researchers find it difficult to shift, limiting their options. Theater ratio (0.30): Relatively low; focuses on genuine discovery rather than performative research.
 *
 * PERSPECTIVAL GAP:
 *   The Earth-Twin Focused Research see a snare, being trapped by a funding shift. Meanwhile, Research Groups with Flexible Funding see opportunity (rope). Individual researchers see a mixed benefit and extraction (tangled rope). Telescope time allocators see a more effective resource allocation strategy (rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality depends on the actor's flexibility. Those stuck in the 'Earth-twin' paradigm are targeted by the shift, those able to adapt benefit. The power levels and exit options reflect these differences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    earth_twin_rarity,
    'How rare are true ''Earth twins'' in the galaxy?',
    'Increased exoplanet surveys and atmospheric characterization.',
    'If Earth twins are very rare: the arbitrage strategy is justified. If they are common: the arbitrage strategy may be premature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(earth_twin_rarity, empirical, 'Rarity of Earth twins affects viability of arbitrage').

omega_variable(
    anomaly_detectability,
    'How easily can habitable anomalies be detected and interpreted?',
    'Development of better atmospheric models and detection technologies.',
    'If anomalies are easily detected and interpreted: the arbitrage strategy is efficient. If they are difficult: the strategy may lead to many false positives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anomaly_detectability, empirical, 'Ease of detecting anomalies affects strategy efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_habitability_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exop_tr_t0, exoplanet_habitability_arbitrage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(exop_tr_t5, exoplanet_habitability_arbitrage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(exop_tr_t10, exoplanet_habitability_arbitrage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(exop_be_t0, exoplanet_habitability_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exop_be_t5, exoplanet_habitability_arbitrage, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(exop_be_t10, exoplanet_habitability_arbitrage, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_habitability_arbitrage, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
