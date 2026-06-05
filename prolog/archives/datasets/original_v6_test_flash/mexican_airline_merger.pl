% ============================================================================
% CONSTRAINT STORY: mexican_airline_merger
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mexican_airline_merger, []).

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
 *   constraint_id: mexican_airline_merger
 *   human_readable: Mexican Airline Merger Antitrust Exemption
 *   domain: economic
 *
 * SUMMARY:
 *   The Mexican government's encouragement of an airline merger, ostensibly
 *   to bolster Mexicana de Aviación, creates an antitrust exemption that
 *   allows for potential price fixing and reduced service quality for
 *   passengers. This can be viewed as a tangled rope, where the government
 *   attempts to coordinate the airline industry, but extracts value from
 *   passengers and competing airlines in the process. The success of this
 *   strategy is contingent on the government's motives and the price
 *   sensitivity of passengers.
 *
 * KEY AGENTS:
 *   - Mexican Airline Passengers: Primary target (powerless/trapped) - Subject to potentially higher prices and reduced service quality.
 *   - Merged Airline: Primary beneficiary (institutional/constrained) - Benefits from reduced competition and potential price fixing.
 *   - Mexican Government: Secondary beneficiary (institutional/arbitrage) - Aims to strengthen the national airline for national pride and economic stability.
 *   - Competing Airlines: Victims (moderate/constrained) - Negatively affected by the increased market power of the merged entity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mexican_airline_merger, 0.6).
domain_priors:suppression_score(mexican_airline_merger, 0.7).
domain_priors:theater_ratio(mexican_airline_merger, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mexican_airline_merger, extractiveness, 0.6).
narrative_ontology:constraint_metric(mexican_airline_merger, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(mexican_airline_merger, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mexican_airline_merger, tangled_rope).
narrative_ontology:human_readable(mexican_airline_merger, "Mexican Airline Merger Antitrust Exemption").
narrative_ontology:topic_domain(mexican_airline_merger, "economic").

domain_priors:requires_active_enforcement(mexican_airline_merger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mexican_airline_merger, merged_airline).
narrative_ontology:constraint_beneficiary(mexican_airline_merger, mexican_government).
narrative_ontology:constraint_victim(mexican_airline_merger, mexican_airline_passengers).
narrative_ontology:constraint_victim(mexican_airline_merger, competing_airlines).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Passengers have limited alternatives due to the merger reducing competition. They are subject to potentially higher prices and reduced service quality with little recourse.
constraint_indexing:constraint_classification(mexican_airline_merger, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The merged airline benefits from reduced competition and potential price fixing, but is also constrained by the need to maintain some level of service and regulatory oversight. Faces potential scrutiny if services degrade drastically. Coordination: securing increased market share. Extraction: extracting rents from passengers.
constraint_indexing:constraint_classification(mexican_airline_merger, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% The Mexican government benefits from having a stronger national airline (or so it intends), which is seen as a matter of national pride and economic stability. Coordination: the government is attempting to stabilize the airline industry. Note that this assumes the intervention is well-intentioned; regulatory capture would require a directionality override.
constraint_indexing:constraint_classification(mexican_airline_merger, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Smaller competing airlines are constrained because of the merger. They are negatively affected by the increased market power of the merged entity and suppressed by the antitrust exemption. A move to organize may change this perspective.
constraint_indexing:constraint_classification(mexican_airline_merger, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the merger creates a tangled rope: a coordination attempt (strengthening Mexicana de Aviación) with asymmetric extraction (passengers and competing airlines bear the costs). The government sacrifices free market principles to attempt a more regulated airline industry.
constraint_indexing:constraint_classification(mexican_airline_merger, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mexican_airline_merger_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mexican_airline_merger, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mexican_airline_merger, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mexican_airline_merger, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mexican_airline_merger_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The merger enables the merged airline to potentially increase prices and reduce service quality, extracting value from passengers. The government's suppression of competition reinforces this extraction. Suppression (0.70): High. The antitrust exemption suppresses competition, limiting passenger options and protecting the merged airline from market forces. Theater ratio (0.30): Low. The focus is ostensibly on stabilizing the airline industry, but there is genuine coordination intended.
 *
 * PERSPECTIVAL GAP:
 *   The merged airline sees a rope (coordination), as it gains market power and stability. The government sees a rope (coordination), as it believes it's strengthening a national asset. Passengers see a snare, as they face limited options and potentially higher prices. Competing airlines also see a snare, as they are suppressed by the merged entity's increased market power. The analytical observer sees a tangled rope, recognizing the mixed coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Passengers are victims with few exit options, so they experience a high extraction. The merged airline benefits from reduced competition and market power, so they experience a lower extraction. The government benefits from having a stronger national airline, so it experiences a negative extraction. Competing airlines face constrained exit options so they experience a snare.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    government_motives,
    'Are the government''s motives genuinely about strengthening the airline industry, or is there regulatory capture?',
    'Analyzing lobbying efforts and post-merger regulatory decisions.',
    'If regulatory capture: extractiveness increases, and the government perspective shifts towards snare or piton. If genuine: tangled rope classification remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_motives, empirical, 'Government motives for encouraging the merger').

omega_variable(
    consumer_price_sensitivity,
    'How price-sensitive are Mexican airline passengers?',
    'Analyzing demand elasticity data and passenger behavior after the merger.',
    'If highly price-sensitive: passengers switch to alternatives or reduce travel, limiting extractiveness. If insensitive: merged airline can raise prices significantly, increasing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_price_sensitivity, empirical, 'Price sensitivity of Mexican airline passengers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mexican_airline_merger, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mexi_tr_t0, mexican_airline_merger, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mexi_tr_t5, mexican_airline_merger, theater_ratio, 5, 0.35).
narrative_ontology:measurement(mexi_tr_t10, mexican_airline_merger, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(mexi_be_t0, mexican_airline_merger, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mexi_be_t5, mexican_airline_merger, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(mexi_be_t10, mexican_airline_merger, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mexican_airline_merger, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
