% ============================================================================
% CONSTRAINT STORY: hilbert_hotel_infinite_capacity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-02
% ============================================================================

:- module(constraint_hilbert_hotel_infinite_capacity, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hilbert_hotel_infinite_capacity
 *   human_readable: Hilbert's Hotel Infinite Capacity Accommodation
 *   domain: technological
 *
 * SUMMARY:
 *   Hilbert's Hotel demonstrates the seemingly paradoxical properties of infinity,
 *   showing that a fully occupied hotel with an infinite number of rooms can still
 *   accommodate additional guests. This is a conceptual constraint showing the limit
 *   of finite intuition when dealing with infinite sets.
 *
 * KEY AGENTS (by structural relationship):
 *   - Guests: Occupants of the hotel (powerless/trapped) — benefit from accommodation, potentially displaced by new arrivals.
 *   - Manager: Organizer of the hotel (institutional/arbitrage) — Facilitates accommodation by re-allocating rooms.
 *   - Mathematicians: Analytical observers (analytical/analytical) — Understand the underlying set theory principles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hilbert_hotel_infinite_capacity, 0.01). % Very low extraction - more about coordination.
domain_priors:suppression_score(hilbert_hotel_infinite_capacity, 0.01).   % Very low suppression - no real alternatives are blocked.
domain_priors:theater_ratio(hilbert_hotel_infinite_capacity, 0.01).       % No performative aspect

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilbert_hotel_infinite_capacity, extractiveness, 0.01).
narrative_ontology:constraint_metric(hilbert_hotel_infinite_capacity, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(hilbert_hotel_infinite_capacity, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(hilbert_hotel_infinite_capacity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hilbert_hotel_infinite_capacity, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hilbert_hotel_infinite_capacity, mountain).
narrative_ontology:human_readable(hilbert_hotel_infinite_capacity, "Hilbert's Hotel Infinite Capacity Accommodation").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(hilbert_hotel_infinite_capacity).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(hilbert_hotel_infinite_capacity). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(hilbert_hotel_infinite_capacity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hilbert_hotel_infinite_capacity, hotel_management).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hilbert_hotel_infinite_capacity, hotel_guests).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(hilbert_hotel_infinite_capacity, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(hilbert_hotel_infinite_capacity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(hilbert_hotel_infinite_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilbert_hotel_infinite_capacity_tests).

test(perspectival_agreement) :-
    % Verify perspectival agreement: All agents see it as a Mountain.
    constraint_indexing:constraint_classification(hilbert_hotel_infinite_capacity, mountain, context(_, _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hilbert_hotel_infinite_capacity, ExtMetricName, E),
    E =< 0.25. % Mountain: extractiveness <= 0.25

:- end_tests(hilbert_hotel_infinite_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This is a Mountain constraint because it reflects a mathematical property
 *   of infinite sets, specifically that an infinite set can have new elements
 *   added without increasing its cardinality.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap, as all agents, regardless of their
 *   role, recognize the inherent mathematical truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Hotel Management benefits from understanding and utilizing the principle to
 *   accommodate guests. Guests may perceive a minor extraction in the form of
 *   being moved rooms, but this is negligible compared to the benefit of
 *   accommodation. The mathematician benefits from the consistency of logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling as pure extraction because the
 *   extraction is minimal. The primary function is the coordination of
 *   accommodation, which is facilitated by the properties of infinity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hilbert_hotel,
    'Does the idealized nature of Hilbert's Hotel accurately reflect real-world resource allocation challenges involving finite constraints?',
    'Empirical studies comparing idealized models with real-world resource management practices.',
    'If True: Hilbert's Hotel offers valuable insights. If False: It serves as a purely theoretical construct.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hilbert_hotel_infinite_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No Temporal Data required as extractiveness is very low.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(hilbert_hotel_infinite_capacity, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(hilbert_hotel_infinite_capacity, 0.01).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(hilbert_hotel_infinite_capacity, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not needed for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */