% ============================================================================
% CONSTRAINT STORY: roman_road_network
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_roman_road_network, []).

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
 *   constraint_id: roman_road_network
 *   human_readable: "The Roman Road Network as a Mechanism of Imperial Control and Economic Integration"
 *   domain: technological/political
 *
 * SUMMARY:
 *   The network of roads built by the Roman Empire was a monumental feat of
 *   engineering that enabled military logistics, trade, and communication
 *   across vast territories. While providing a genuine coordination function,
 *   its construction and maintenance relied heavily on coercive extraction
 *   (conscripted labor, land appropriation) and it served as a tool for
 *   military suppression and centralized control.
 *
 * KEY AGENTS (by structural relationship):
 *   - Roman State & Elites: Primary beneficiary (institutional/arbitrage) — profits from increased trade, tax revenue, and military speed.
 *   - Provincial Laborers & Conquered Peoples: Primary target (powerless/trapped) — provides the uncompensated labor and appropriated land, and is subject to the control the roads enable.
 *   - Long-distance Merchants: Secondary beneficiary (moderate/mobile) — uses the roads for commerce but is subject to Roman taxes and authority.
 *   - Analytical Observer: Historian/systems analyst — sees both the coordination benefits and the extractive costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_road_network, 0.55). % High extraction from forced labor and land appropriation.
domain_priors:suppression_score(roman_road_network, 0.70).   % Structural property (raw, unscaled). The network suppressed local autonomy and alternative routes.
domain_priors:theater_ratio(roman_road_network, 0.15).       % Highly functional; not theatrical during its prime.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_road_network, extractiveness, 0.55).
narrative_ontology:constraint_metric(roman_road_network, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(roman_road_network, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(roman_road_network, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(roman_road_network). % Required for Tangled Rope. Roads were patrolled, and labor was coerced.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-constructed system.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(roman_road_network, roman_state_and_elites).
narrative_ontology:constraint_beneficiary(roman_road_network, long_distance_merchants).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(roman_road_network, provincial_laborers).
narrative_ontology:constraint_victim(roman_road_network, conquered_peoples).

% Gate requirements fulfilled for Tangled Rope:
%   - constraint_beneficiary/2 declared (coordination function)
%   - constraint_victim/2 declared (asymmetric extraction)
%   - requires_active_enforcement/1 declared

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The provincial laborer forced to build the road. They experience pure coercion.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ ≈ 0.55 * 1.42 * 1.0 (national scope) = 0.78. This is a clear Snare.
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
constraint_indexing:constraint_classification(roman_road_network, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Roman State or a well-connected senator. They see a pure coordination good.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% χ ≈ 0.55 * -0.12 * 1.0 = -0.066. This is a clear Rope.
constraint_indexing:constraint_classification(roman_road_network, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The historian who sees both the coordination function and the extractive cost.
% The metrics (ε=0.55, S=0.70) and the presence of both beneficiary and victim groups
% lead to the Tangled Rope classification.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.55 * 1.15 * 1.2 (global scope) = 0.76. Meets Tangled Rope threshold.
constraint_indexing:constraint_classification(roman_road_network, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SECONDARY BENEFICIARY (ROPE)
% A merchant using the roads for commerce. They benefit greatly from coordination,
% but are still subject to the state's power (taxes, tolls).
% Engine derives d from: beneficiary membership + mobile exit → d ≈ 0.15 → f(d) ≈ -0.01 → low χ
% χ ≈ 0.55 * -0.01 * 1.0 = -0.0055. Still a Rope, but less subsidized than the state's view.
constraint_indexing:constraint_classification(roman_road_network, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_road_network_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(roman_road_network, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_road_network, rope, context(agent_power(institutional), _, _, _)),
    snare \= rope.

test(analytical_classification) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(roman_road_network, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_fulfilled) :-
    % Check that all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(roman_road_network, _),
    narrative_ontology:constraint_victim(roman_road_network, _),
    domain_priors:requires_active_enforcement(roman_road_network).

:- end_tests(roman_road_network_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This score reflects the significant, non-consensual
 *     extraction of value through forced labor (from soldiers, slaves, and conquered
 *     peoples) and the appropriation of land without compensation. This was the
 *     primary "cost" of the network's construction.
 *   - Suppression Score (S=0.70): The roads were instruments of power. They enabled
 *     rapid legion deployment to crush rebellions, superseding and often erasing
 *     local pathways. This created a centralized, state-controlled logistics network
 *     that suppressed local autonomy and alternative power structures.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Roman state (institutional), the network is a pure
 *   coordination good (Rope) that enables the existence of the empire itself. The
 *   extractive costs are externalized and considered a necessary price of civilization.
 *   For the provincial laborer (powerless), the road is a symbol of their subjugation
 *   and a product of their forced labor (Snare). They experience its coercive,
 *   extractive nature directly, with few of the coordination benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the clear structural asymmetry.
 *   - Beneficiaries (`roman_state_and_elites`): They benefit from the roads' function
 *     (military speed, trade revenue, administrative control) without bearing the
 *     direct labor cost. Their `arbitrage` exit option reflects their ability to
 *     direct resources and control the network for maximum gain. This results in a
 *     low derived `d` value (≈0.05).
 *   - Victims (`provincial_laborers`): They provide the primary input (labor, land)
 *     under duress and are the targets of the military control the roads facilitate.
 *     Their `trapped` status within the imperial system results in a very high
 *     derived `d` value (≈0.95).
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of what Mandatrophy analysis is designed to prevent.
 *   A simplistic "public goods" analysis would label the road network a Rope, focusing
 *   only on its coordination function ("All roads lead to Rome," "They brought order").
 *   This framework forces a reckoning with the extractive foundation of that coordination.
 *   By declaring victims and quantifying the extractive cost (ε), the system correctly
 *   identifies the structure as a Tangled Rope from an analytical view, acknowledging
 *   both its function and its coercive underpinnings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_roman_road_network,
    'What was the precise ratio of coerced (slave, provincial) to non-coerced (legionary duty) labor in road construction across different eras and provinces?',
    'Detailed archaeological and historical analysis of labor records, which are largely unavailable. Isotope analysis of remains near roads could provide clues on worker origins.',
    'If labor was mostly willing legionaries, ε would decrease (e.g., to 0.40). If almost entirely slave/conscript, ε would increase (e.g., to 0.70). This would solidify the Snare/Tangled Rope classifications.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(roman_road_network, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As a high-extraction constraint (ε=0.55 > 0.46),
% this is required. The model shows initial high extraction during conquest, a slight
% dip as trade becomes a co-equal function, and rising theater in the late empire
% as maintenance falters but the idea of the roads remains.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rrn_tr_t0, roman_road_network, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rrn_tr_t5, roman_road_network, theater_ratio, 5, 0.15).
narrative_ontology:measurement(rrn_tr_t10, roman_road_network, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rrn_ex_t0, roman_road_network, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(rrn_ex_t5, roman_road_network, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(rrn_ex_t10, roman_road_network, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The road network is a quintessential example of large-scale infrastructure.
narrative_ontology:coordination_type(roman_road_network, global_infrastructure).

% Network relationships (structural influence edges)
% The road network was fundamental to the stability and function of the empire.
narrative_ontology:affects_constraint(roman_road_network, roman_imperial_stability).
narrative_ontology:affects_constraint(roman_road_network, roman_grain_supply).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately captures the
% structural relationships between the Roman state, provincial subjects, and
% merchants.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */