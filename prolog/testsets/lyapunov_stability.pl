% ============================================================================
% CONSTRAINT STORY: lyapunov_stability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_lyapunov_stability, []).

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
 *   constraint_id: lyapunov_stability
 *   human_readable: Lyapunov Stability Criteria
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   Lyapunov stability is a mathematical concept from dynamical systems theory
 *   that describes whether a system will return to an equilibrium state after
 *   a small perturbation. This constraint represents the fundamental,
 *   unchangeable limits on a system's resilience. If a perturbation is too
 *   large, the system will not return to its original state, regardless of
 *   any agent's desires. This applies to physical systems, ecological models,
 *   economic models, and control systems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Dynamical Systems (e.g., ecosystems, economies): The entities whose behavior is governed by the stability limits. (powerless/trapped)
 *   - External Perturbations (e.g., shocks, policy changes): The forces that test the stability of the system.
 *   - Scientists and Engineers: Analytical observers who use the concept to model and predict system behavior. (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lyapunov_stability, 0.15).
domain_priors:suppression_score(lyapunov_stability, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(lyapunov_stability, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lyapunov_stability, extractiveness, 0.15).
narrative_ontology:constraint_metric(lyapunov_stability, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(lyapunov_stability, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(lyapunov_stability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(lyapunov_stability, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lyapunov_stability, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(lyapunov_stability).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(lyapunov_stability). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(lyapunov_stability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), this constraint does not
% have socially constructed beneficiaries or victims. It is a universal limit
% on system dynamics. The concepts of benefit and cost are observer-dependent
% interpretations of its effects, not structural properties of the constraint itself.

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

% PERSPECTIVE 1: A SYSTEM SUBJECT TO THE CONSTRAINT
% For any system whose dynamics are described by these principles (e.g., an
% ecosystem, a planetary orbit), the stability criteria are an unchangeable
% fact of its existence.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: A POLICYMAKER ATTEMPTING TO STABILIZE A SYSTEM
% An agent attempting to manage a system (e.g., an economy) is still bound
% by its intrinsic stability limits. They can introduce inputs, but cannot
% violate the underlying mathematical constraints.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% A scientist or mathematician analyzing the system sees it as a fundamental
% law of dynamics.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lyapunov_stability_tests).

test(uniform_classification_mountain) :-
    % Verify that all indexed perspectives agree on the Mountain classification,
    % as expected for a natural law.
    constraint_indexing:constraint_classification(lyapunov_stability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lyapunov_stability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(lyapunov_stability, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == mountain,
    TypeAnalytical == mountain.

test(threshold_validation_mountain) :-
    % Verify metrics are within the canonical range for a Mountain.
    narrative_ontology:constraint_metric(lyapunov_stability, extractiveness, E),
    narrative_ontology:constraint_metric(lyapunov_stability, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the constraint has the required profile for a natural law signature.
    domain_priors:emerges_naturally(lyapunov_stability),
    narrative_ontology:constraint_metric(lyapunov_stability, accessibility_collapse, ACC),
    narrative_ontology:constraint_metric(lyapunov_stability, resistance, RES),
    ACC >= 0.85,
    RES =< 0.15.

:- end_tests(lyapunov_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Lyapunov stability is a mathematical property of dynamical systems, making
 *   it a textbook example of a Mountain constraint.
 *   - Base Extractiveness (ε=0.15): Low. The constraint doesn't "extract" value;
 *     it describes inherent system behavior. The small value represents the
 *     "cost" imposed by reality, i.e., the fact that not all states are stable.
 *   - Suppression (S=0.05): Very low. The constraint doesn't suppress
 *     alternatives; it defines the boundaries of what is possible. One cannot
 *     "choose" an alternative to mathematical law.
 *   - Theater (T=0.01): Negligible. There is no performative aspect to a
 *     mathematical principle.
 *   - NL Profile: The high accessibility collapse (0.95) and low resistance (0.05)
 *     reflect its status as an inescapable, non-negotiable feature of reality.
 *     It emerges naturally from the mathematics of differential equations.
 *
 * PERSPECTIVAL AGREEMENT:
 *   All perspectives classify this as a Mountain because its truth is invariant
 *   to the observer's position, power, or goals. Whether you are a species in
 *   an ecosystem, a central banker managing an economy, or a physicist modeling
 *   an orbit, the stability criteria are a fixed boundary condition.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, directionality is not a meaningful concept. The constraint
 *   is not directed "at" anyone. It is a symmetric, universal principle.
 *   Therefore, beneficiary and victim declarations are omitted, as they imply
 *   an asymmetric social or political structure that does not exist here.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain is crucial. It prevents misinterpreting
 *   the resilience of a natural or physical system as a form of designed
 *   coordination (Rope) or a system of control (Snare). It correctly identifies
 *   stability as an emergent property of underlying laws, not an imposed order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_lyapunov_stability,
    'To what extent is the mathematical model of Lyapunov stability a complete description of real-world system resilience, versus an idealized approximation?',
    'Empirical testing of complex systems (e.g., ecosystems, economies) against model predictions, especially near tipping points.',
    'If the model is a near-perfect description, the Mountain classification is absolute. If real-world systems show significant deviation (e.g., due to quantum or stochastic effects not in the model), it could imply the existence of other, less-understood constraints affecting the system.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lyapunov_stability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a Mountain constraint is expected to be flat, as natural
% laws do not drift over human time scales. These are included for formal
% completeness. Extraction is low, so this is not strictly required by the linter.
%
% Theater ratio over time:
narrative_ontology:measurement(lyapunov_stability_tr_t0, lyapunov_stability, theater_ratio, 0, 0.01).
narrative_ontology:measurement(lyapunov_stability_tr_t5, lyapunov_stability, theater_ratio, 5, 0.01).
narrative_ontology:measurement(lyapunov_stability_tr_t10, lyapunov_stability, theater_ratio, 10, 0.01).

% Extraction over time:
narrative_ontology:measurement(lyapunov_stability_ex_t0, lyapunov_stability, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(lyapunov_stability_ex_t5, lyapunov_stability, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(lyapunov_stability_ex_t10, lyapunov_stability, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% As a Mountain, this constraint does not have a coordination function.
% narrative_ontology:coordination_type(lyapunov_stability, [type]).

% Network relationships: This is a foundational concept.
narrative_ontology:affects_constraint(lyapunov_stability, ecological_tipping_points).
narrative_ontology:affects_constraint(lyapunov_stability, economic_cycle_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The constraint is a Mountain, and directionality
% is not applicable.
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(lyapunov_stability, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */