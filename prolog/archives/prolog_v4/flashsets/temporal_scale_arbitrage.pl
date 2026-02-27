% ============================================================================
% CONSTRAINT STORY: temporal_scale_arbitrage
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_scale_arbitrage, []).

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
 *   constraint_id: temporal_scale_arbitrage
 *   human_readable: Temporal Scale Arbitrage in Astronomy
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint describes the strategic exploitation of cosmic events
 *   occurring across vast ranges of time scales—from nanoseconds to human
 *   lifetimes. While potentially beneficial for rapidly advancing our
 *   understanding of the universe, this "temporal scale arbitrage" can also
 *   lead to the neglect of long-term observational studies, which are crucial
 *   for understanding slowly evolving phenomena. This creates a tension
 *   between short-term gains and the long-term accumulation of knowledge,
 *   impacting the allocation of resources and the overall direction of
 *   astronomical research.
 *
 * KEY AGENTS:
 *   - Early Adopter Astronomers: Moderate/Constrained - Exploit short timescale phenomena
 *   - Private Observatories: Institutional/Arbitrage - Benefit from fast science publicity
 *   - Public Astronomy Funding: Institutional/Constrained - Supports both short and long term projects
 *   - Long Term Observational Studies: Powerless/Trapped - Crucial for understanding slow evolution
 *   - Analytical Observer: Analytical/Analytical - Sees entire temporal scale system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_scale_arbitrage, 0.6).
domain_priors:suppression_score(temporal_scale_arbitrage, 0.4).
domain_priors:theater_ratio(temporal_scale_arbitrage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_scale_arbitrage, extractiveness, 0.6).
narrative_ontology:constraint_metric(temporal_scale_arbitrage, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(temporal_scale_arbitrage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_scale_arbitrage, tangled_rope).
narrative_ontology:human_readable(temporal_scale_arbitrage, "Temporal Scale Arbitrage in Astronomy").
narrative_ontology:topic_domain(temporal_scale_arbitrage, "technological").

domain_priors:requires_active_enforcement(temporal_scale_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_scale_arbitrage, early_adopter_astronomers).
narrative_ontology:constraint_beneficiary(temporal_scale_arbitrage, private_observatories).
narrative_ontology:constraint_victim(temporal_scale_arbitrage, public_astronomy_funding).
narrative_ontology:constraint_victim(temporal_scale_arbitrage, long_term_observational_studies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Long-term observational studies are trapped because they cannot rapidly adapt to new discoveries. They are powerless against the short-term gains pursued by other actors, ultimately leading to a snare where crucial long-term data is sacrificed for immediate results.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% Perspective 2: Early adopter astronomers are constrained because they need to invest in new technology to take advantage of discoveries, but also mobile as they can choose which events to focus on. This results in a tangled rope scenario where they benefit from being first to a discovery, but can also lose if they invest in a fruitless area. They extract from public funding by diverting it into short term opportunistic studies.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Private observatories can arbitrage because they can freely choose where to invest their resources. They benefit because they are unencumbered by the same institutional oversight and public accountability as public institutions. This results in a rope scenario where they effectively coordinate resources to maximize scientific output - or financial gain related to publicity of 'fast' science.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Public astronomy funding can be considered a piton, a previously beneficial institution now degraded. While initially designed for long-term gains, the increasing incentive for short-term opportunistic studies has resulted in a system more theatrical than functional, where resources are diverted into studies that generate short-term news cycles, rather than valuable long-term data.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Analytical observer sees a tangled rope as there is a mix of coordination and extraction. The exploitation of cosmic events is coordinated via technology, while long term studies are being extracted from, generating a complex system of benefits and losses. The overall impact on scientific knowledge can be positive, but with a cost of neglecting crucial long-term data.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_scale_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_scale_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_scale_arbitrage, TR),
    TR >= 0.70.

:- end_tests(temporal_scale_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): is relatively high because funding being allocated to short term opportunistic projects is at the cost of longer duration programs. Suppression (0.4): moderately difficult to start longer term studies due to funding pressures. Theater Ratio (0.3): Low, because the scientific output of the longer duration studies has valuable data that is not able to generate much media. Claimed Type: Tangled Rope as there is coordination due to the technologies required to make short-term studies, but at the extractive cost of long term.
 *
 * PERSPECTIVAL GAP:
 *   The main perspectives of disagreement here is the long term observational studies that are trapped. This is in contrast to the early adopter astronomers, and private observatories, which are able to adapt quicker. The perspective on Public funding is also degraded, as a system which was created to help all parties, is instead benefiting short term projects more so.
 *
 * DIRECTIONALITY LOGIC:
 *   Early Adopter Astronomers benefit from first-mover advantages in exploiting short-term phenomena. Private Observatories benefit from increased attention and potentially lucrative discoveries from rapid responses. Public Astronomy Funding is constrained by needing to support both short-term and long-term projects but is influenced by the incentives for fast results. Long-Term Observational Studies bear the cost of resources being diverted to short-term projects and are trapped by the slow pace of their research. Analytical Observer evaluates the overall impact and structural incentives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate is resolved by balancing short term gain against the need to keep long-term observational studies going. There is a risk in solely focusing on short-term due to the amount of knowledge gained over time that comes from these studies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_vs_short_term_value,
    'What is the relative value of long-term observational studies compared to short-term opportunistic studies?',
    'Perform a cost-benefit analysis weighing the benefits of short term studies, versus the lost data of not investing in long term. Look at the total scientific output that comes from each type of study.',
    'If long-term more valuable: Snare classification reinforced, shift resources away from short-term. If short-term more valuable: Rope classification reinforced, resource allocation justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_vs_short_term_value, empirical, 'Compares the relative value of long term to short term astronomical studies.').

omega_variable(
    impact_on_public_funding,
    'How much does temporal scale arbitrage affect the allocation of public funding in astronomy?',
    'Study allocation of funds, comparing which types of studies are funded and looking for evidence of resource diversion towards short-term opportunistic studies.',
    'If significant effect: Justify more oversight and less of a piton. If not significant effect: Justify the current allocations',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_public_funding, empirical, 'Identifies if public funding is being affected by temporal scale arbitrage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_scale_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temporal_scale_arbitrage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t5, temporal_scale_arbitrage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(temp_tr_t10, temporal_scale_arbitrage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temporal_scale_arbitrage, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(temp_be_t5, temporal_scale_arbitrage, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(temp_be_t10, temporal_scale_arbitrage, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_scale_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(temporal_scale_arbitrage, telescope_time_allocation).
narrative_ontology:affects_constraint(temporal_scale_arbitrage, astronomical_data_ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
