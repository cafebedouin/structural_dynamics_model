% ============================================================================
% CONSTRAINT STORY: trajans_bridge_lifecycle
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_trajans_bridge_lifecycle, []).

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
 *   constraint_id: trajans_bridge_lifecycle
 *   human_readable: Trajan's Bridge over the Danube (Lifecycle)
 *   domain: technological/military
 *
 * SUMMARY:
 *   Trajan's Bridge, built circa 105 AD, was a monumental Roman engineering
 *   feat to facilitate the conquest of Dacia. A marvel of coordination for
 *   the Roman military, it enabled the extraction of Dacian resources. After
 *   the Roman withdrawal, the bridge was dismantled, but its piers remained in
 *   the Danube for over 1,500 years, becoming a navigational hazard. This
 *   story models the constraint in its final, atrophied state, where its
 *   original function has vanished but its physical remnants persist due to
 *   institutional inertia and later, for theatrical/heritage purposes.
 *
 * KEY AGENTS (by structural relationship):
 *   - Danube River Navigators: Primary target (powerless/trapped) — bore the cost of the navigational hazard for centuries.
 *   - Roman Empire (historical): Primary beneficiary (institutional/arbitrage) — used the bridge for conquest and resource extraction.
 *   - Modern Heritage Authorities: Theatrical beneficiary (institutional/mobile) — maintain the memory and remains for symbolic/touristic value.
 *   - Analytical Observer: Modern historian/archaeologist — sees the full lifecycle from functional infrastructure to inert Piton.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics (representing the *final, atrophied state*) ---
domain_priors:base_extractiveness(trajans_bridge_lifecycle, 0.18). % Cost of the navigational hazard.
domain_priors:suppression_score(trajans_bridge_lifecycle, 0.20).   % Structural property (raw, unscaled). The piers don't suppress alternatives, they are just an obstacle.
domain_priors:theater_ratio(trajans_bridge_lifecycle, 0.85).       % Piton detection (>= 0.70). High ratio of symbolic/heritage value to actual function.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, extractiveness, 0.18).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(trajans_bridge_lifecycle, piton).

% --- Binary flags ---
% N/A

% --- Emergence flag (required for mountain constraints) ---
% N/A

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain. They reflect the *current* state.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, modern_heritage_authorities).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, danube_river_navigators).

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

% PERSPECTIVE 1: THE RIVER NAVIGATOR (MOUNTAIN)
% For centuries, the piers were an unchangeable, costly feature of the
% river. From this perspective, they are a natural hazard, a fixed obstacle
% with no recourse. Engine derives d ≈ 0.95 (victim + trapped).
% χ ≈ 0.18 * 1.42 * 0.9 = 0.23. Low extraction, but feels like an immutable fact.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE MODERN HERITAGE AUTHORITY (ROPE)
% This agent sees the remains as a tool for coordinating tourism and
% national identity. It is a source of value, not cost.
% Engine derives d from beneficiary status. With mobile exit, d ≈ 0.15 → f(d) ≈ -0.01.
% Effective extraction χ is near zero or negative.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The historian or archaeologist sees the full picture: a once-functional
% object now maintained primarily for theatrical reasons. The high theater
% ratio (0.85) is the dominant signal.
% Engine derives analytical d ≈ 0.72 → f(d) ≈ 1.15.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trajans_bridge_lifecycle_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (navigator) and beneficiary (heritage).
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(analytical_piton_detection) :-
    % Verify the analytical perspective correctly identifies the Piton.
    domain_priors:theater_ratio(trajans_bridge_lifecycle, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, piton, context(agent_power(analytical), _, _, _)).

test(piton_threshold_validation) :-
    narrative_ontology:constraint_metric(trajans_bridge_lifecycle, extractiveness, E),
    domain_priors:theater_ratio(trajans_bridge_lifecycle, TR),
    E > 0.10, E =< 0.25,
    TR >= 0.70.

:- end_tests(trajans_bridge_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics model the bridge's *final state* as inert remnants.
 *   - Base Extractiveness (ε=0.18): Low, representing the diffuse, long-term cost imposed on river navigation by the hazardous piers. It's not organized extraction, but a negative externality.
 *   - Suppression (0.20): Low. The piers did not prevent other river crossings; they were simply an obstacle to navigation along the river's course.
 *   - Theater Ratio (0.85): Very high. This is the key metric. The modern value of the ruins is almost entirely symbolic, historical, and touristic. Its original function (military logistics) is zero. This high ratio is the classic signature of a Piton.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound, spanning three distinct classifications based on structural position:
 *   - The River Navigator (Mountain): Experiences the piers as an unchangeable, costly part of the physical world, like a natural reef. No agency, no negotiation.
 *   - The Heritage Authority (Rope): Sees the ruins as a cultural asset, a tool for coordinating tourism and building national identity. For them, it is a pure coordination good.
 *   - The Historian (Piton): Recognizes both prior perspectives but contextualizes them within the object's lifecycle. The historian sees the high theater ratio and the complete loss of original function, leading to the Piton classification.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `modern_heritage_authorities`. They derive symbolic and economic (tourism) value from the *story* of the bridge, not its function.
 *   - Victim: `danube_river_navigators`. For over a millennium, they bore the physical risk and cost imposed by the underwater piers, a legacy cost of a long-dead imperial project.
 *   This clear beneficiary/victim structure, centered on the theatrical vs. practical nature of the remains, drives the perspectival differences.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of mandatrophy. A massive, functional piece of infrastructure (a Tangled Rope in its heyday) decays into a Piton. The system correctly avoids misclassifying the modern heritage-based "coordination" (Rope perspective) as the object's primary nature. By flagging the high theater ratio, it identifies the constraint as inertial and performative, preventing its symbolic value from being mistaken for a genuine, active function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trajans_bridge_lifecycle,
    'What was the quantifiable economic cost of the piers as a navigational hazard over the centuries?',
    'Analysis of historical shipping logs, archaeological evidence of wrecks near the site, and hydrographic modeling.',
    'If the cost was very high, the base extractiveness (ε) would be higher, potentially pushing it towards a low-grade Snare from the navigator''s view. If negligible, the Mountain classification holds more firmly.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trajans_bridge_lifecycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This data models the bridge's decay from a highly extractive military tool
% to an inert, theatrical Piton. This lifecycle drift is central to the story.

% Theater ratio over time (metric_substitution detection):
% Starts low (purely functional), ends high (purely theatrical).
narrative_ontology:measurement(tbl_tr_t0, trajans_bridge_lifecycle, theater_ratio, 0, 0.10).
narrative_ontology:measurement(tbl_tr_t5, trajans_bridge_lifecycle, theater_ratio, 5, 0.50). % Post-dismantling, value starts becoming historical.
narrative_ontology:measurement(tbl_tr_t10, trajans_bridge_lifecycle, theater_ratio, 10, 0.85). % Modern heritage era.

% Extraction over time (extraction_accumulation in reverse):
% Starts high (enabling conquest), ends low (residual hazard).
narrative_ontology:measurement(tbl_ex_t0, trajans_bridge_lifecycle, base_extractiveness, 0, 0.55). % Represents the value extracted from Dacia.
narrative_ontology:measurement(tbl_ex_t5, trajans_bridge_lifecycle, base_extractiveness, 5, 0.25). % Roman withdrawal, value plummets.
narrative_ontology:measurement(tbl_ex_t10, trajans_bridge_lifecycle, base_extractiveness, 10, 0.18). % Modern era, only hazard cost remains.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (reflects its original function)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(trajans_bridge_lifecycle, global_infrastructure).

% Network relationships (structural influence edges)
% The bridge was a necessary precondition for the successful conquest of Dacia.
narrative_ontology:affects_constraint(trajans_bridge_lifecycle, trajans_conquest_of_dacia).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships of the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */