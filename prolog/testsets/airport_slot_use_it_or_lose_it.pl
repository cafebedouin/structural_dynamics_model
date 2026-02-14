% ============================================================================
% CONSTRAINT STORY: airport_slot_use_it_or_lose_it
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_airport_slot_use_it_or_lose_it, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: airport_slot_use_it_or_lose_it
 *   human_readable: "Use-it-or-lose-it" rule for airport landing slots
 *   domain: economic
 *
 * SUMMARY:
 *   A regulatory rule requires airlines to operate a high percentage (e.g.,
 *   80%) of their allocated landing/take-off slots at congested airports or
 *   forfeit them. During periods of suppressed demand (like the COVID-19
 *   pandemic), this forces airlines to fly near-empty "ghost flights,"
 *   burning fuel and incurring massive costs for no economic output, purely
 *   to retain their valuable slot assets.
 *
 * KEY AGENTS (by structural relationship):
 *   - Airlines: Primary target (powerful/constrained) — bear the direct cost of ghost flights to avoid losing multi-million dollar assets.
 *   - Airport Slot Coordinators/Regulators: Primary beneficiary (institutional/arbitrage) — maintain a stable, predictable system for managing a scarce resource.
 *   - The General Public: Secondary victim (powerless/trapped) — bears the environmental cost of unnecessary carbon emissions.
 *   - Analytical Observer: Sees the full structure, including the valid coordination function and the perverse extractive outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is the value destroyed (fuel, labor, maintenance) by flying an
% empty plane, relative to the value of the asset being protected. High.
domain_priors:base_extractiveness(airport_slot_use_it_or_lose_it, 0.75).

% Suppression is extremely high. The alternative is to lose a slot worth
% millions, a catastrophic loss for an airline at a hub airport.
domain_priors:suppression_score(airport_slot_use_it_or_lose_it, 0.85).

% The flights are real, the rule is real. This is not performative theater.
domain_priors:theater_ratio(airport_slot_use_it_or_lose_it, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, extractiveness, 0.75).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(airport_slot_use_it_or_lose_it, tangled_rope).

% --- Binary flags ---
% The rule is enforced by slot coordination bodies.
domain_priors:requires_active_enforcement(airport_slot_use_it_or_lose_it).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
% The regulators who manage the system and the incumbent airlines who are
% protected from new entrants by the high barrier to entry.
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, airport_slot_coordinators).
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, incumbent_airlines_in_normal_conditions).

% Who bears disproportionate cost?
% The airlines forced to fly empty planes, and the public bearing the environmental cost.
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, airlines_in_low_demand_conditions).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, the_general_public).

% Gate requirements for Tangled Rope are met:
% - constraint_beneficiary/2 (coordination function)
% - constraint_victim/2 (asymmetric extraction)
% - requires_active_enforcement/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE AIRLINES (PRIMARY TARGET)
% They are powerful actors, but their exit from this constraint is highly
% constrained. Losing a slot is not a viable business option. As victims,
% the engine derives a high d, leading to high χ. The rule is a Snare.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE REGULATORS (PRIMARY BENEFICIARY)
% They see the rule as a necessary tool for coordination at congested airports.
% From their institutional, arbitrage position, the rule's extractive side-effects
% are secondary to its function. As beneficiaries, engine derives low d, so low χ.
% The rule is a Rope.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% This perspective sees both the coordination function and the asymmetric
% extraction. It recognizes that a system designed for coordination has,
% under changed conditions, become primarily extractive. This is the
% definition of a Tangled Rope.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GENERAL PUBLIC (SECONDARY VICTIM)
% The public experiences the negative externality (carbon emissions) without
% any direct benefit. They are powerless and trapped within the consequences
% of the system. For them, it is an unalloyed Snare.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(airport_slot_use_it_or_lose_it_tests).

test(perspectival_gap_airline_vs_regulator, [nondet]) :-
    % Verify the core perspectival gap.
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypeAirline, context(agent_power(powerful), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypeRegulator, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeAirline == snare),
    assertion(TypeRegulator == rope),
    TypeAirline \= TypeRegulator.

test(analytical_view_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_requirements) :-
    % Verify that the necessary structural facts are declared for a Tangled Rope.
    narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, _),
    narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, _),
    domain_priors:requires_active_enforcement(airport_slot_use_it_or_lose_it).

:- end_tests(airport_slot_use_it_or_lose_it_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): This score reflects the immense waste of
 *     flying a large aircraft with no passengers or cargo. The value being
 *     extracted is the full operational cost (fuel, crew, maintenance) for
 *     zero economic benefit, measured against the asset's value.
 *   - Suppression (S=0.85): An airline's slots at a hub like Heathrow or JFK
 *     are among its most valuable, illiquid assets. The threat of losing them
 *     creates a powerful coercive force, suppressing any alternative action.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the regulator (institutional), the rule is a
 *   Rope—a coordination mechanism that ensures scarce resources aren't hoarded,
 *   maintaining systemic stability. For the airline (powerful/constrained),
 *   it is a Snare—a coercive trap that forces value destruction under threat
 *   of a greater loss. This divergence, where a constraint is simultaneously
 *   a valid coordination tool and a mechanism for asymmetric extraction, is
 *   the hallmark of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `airport_slot_coordinators` benefit from the stability and
 *     control the rule provides. `incumbent_airlines` also benefit in normal
 *     times, as the rule acts as a barrier to entry for new competitors.
 *   - Victims: `airlines_in_low_demand_conditions` are the direct victims,
 *     bearing the financial cost of the ghost flights. `the_general_public`
 *     is an indirect victim, bearing the environmental cost.
 *   - This structure directly informs the directionality `d`. The regulators,
 *     as institutional beneficiaries, receive a low `d`, resulting in a low
 *     effective extraction (χ). The airlines, as victims with constrained
 *     exit, receive a high `d`, resulting in a very high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   This classification correctly identifies the dual nature of the constraint,
 *   preventing a simplistic mislabeling. Calling it a pure Snare would ignore
 *   its legitimate (and necessary) coordination function in normal times.
 *   Calling it a pure Rope would ignore the catastrophic waste and coercion
 *   it generates under specific, but recurring, market conditions. The
 *   Tangled Rope classification captures this essential duality, showing how a
 *   system designed for coordination can become pathologically extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_airport_slot_use_it_or_lose_it,
    'Is the rule''s primary latent function coordination (preventing hoarding) or protectionism (barring new entrants)?',
    'Analysis of regulatory correspondence, lobbying records from incumbent airlines vs. low-cost carriers, and economic modeling of slot allocation without the 80% rule.',
    'If primarily for coordination, its Rope-like properties are dominant. If primarily for protectionism, its Snare-like properties are dominant, and the coordination claim is more theatrical.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(airport_slot_use_it_or_lose_it, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint's extractiveness is highly sensitive to external conditions
% (passenger demand). The data below models the shift from a low-extraction
% state (pre-pandemic) to a high-extraction state (current).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(asuioli_tr_t0, airport_slot_use_it_or_lose_it, theater_ratio, 0, 0.10).
narrative_ontology:measurement(asuioli_tr_t5, airport_slot_use_it_or_lose_it, theater_ratio, 5, 0.10).
narrative_ontology:measurement(asuioli_tr_t10, airport_slot_use_it_or_lose_it, theater_ratio, 10, 0.10).

% Extraction over time (dramatic increase due to demand shock):
% T=0: Pre-pandemic, high demand. Extraction is low, representing minor inefficiencies.
narrative_ontology:measurement(asuioli_ex_t0, airport_slot_use_it_or_lose_it, base_extractiveness, 0, 0.20).
% T=5: Mid-pandemic, rules suspended. Extraction is zero.
narrative_ontology:measurement(asuioli_ex_t5, airport_slot_use_it_or_lose_it, base_extractiveness, 5, 0.0).
% T=10: Post-pandemic, rules partially reinstated with low demand. Extraction is extremely high.
narrative_ontology:measurement(asuioli_ex_t10, airport_slot_use_it_or_lose_it, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The rule is a mechanism for allocating a scarce resource.
narrative_ontology:coordination_type(airport_slot_use_it_or_lose_it, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options (constrained vs. arbitrage)
% accurately models the power dynamics between the airlines and the regulators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */