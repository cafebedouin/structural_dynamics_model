% ============================================================================
% CONSTRAINT STORY: logistic_map_dynamics
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_logistic_map_dynamics, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: logistic_map_dynamics
 *   human_readable: The Logistic Map (Bifurcation and Chaos)
 *   domain: mathematical/biological
 *
 * SUMMARY:
 *   The Logistic Map (x_{n+1} = r * x_n * (1 - x_n)) is a simple polynomial
 *   mapping that demonstrates how complex, chaotic behavior can arise from
 *   simple non-linear dynamical equations. It serves as a fundamental
 *   constraint on the predictability of systems with feedback and limited
 *   resources, revealing a deep structure that both enables and limits control.
 *
 * KEY AGENTS (by structural relationship):
 *   - Long-term Planners: Primary target (powerless/constrained) — bears the
 *     extraction of predictive certainty in the chaotic regime.
 *   - Nonlinear Analysts & Cryptographers: Primary beneficiary (organized/mobile) —
 *     gain powerful tools for understanding complexity and generating randomness.
 *   - Ecosystem Managers: Institutional beneficiary (institutional/mobile) — use
 *     the map's stable regime as a coordination tool for sustainable yields.
 *   - Analytical Observer: Sees the full dual nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate. In its chaotic regime (high r), the map "extracts"
% information (precision) from any simulation and returns entropy. Maintaining
% predictive control requires massive re-investment in computational precision
% for only a few additional steps of foresight.
domain_priors:base_extractiveness(logistic_map_dynamics, 0.35).

% Rationale: It suppresses the viability of simple linear growth models
% (e.g., Malthusian), rendering them functionally inadequate in
% resource-limited environments by demonstrating the necessity of non-linear feedback.
domain_priors:suppression_score(logistic_map_dynamics, 0.40).

% Rationale: The map is a formal mathematical truth. It has zero performative
% or theatrical component.
domain_priors:theater_ratio(logistic_map_dynamics, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, extractiveness, 0.35).
narrative_ontology:constraint_metric(logistic_map_dynamics, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(logistic_map_dynamics, theater_ratio, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
% The map is a Tangled Rope: it provides a genuine coordination function
% (predictability in stable regimes) while also containing an asymmetric
% extractive component (information destruction in chaotic regimes).
narrative_ontology:constraint_claim(logistic_map_dynamics, tangled_rope).
narrative_ontology:human_readable(logistic_map_dynamics, "The Logistic Map (Bifurcation and Chaos)").

% --- Binary flags ---
% Required for Tangled Rope. The "enforcement" is the mathematical rule itself,
% which is inescapable within its domain.
domain_priors:requires_active_enforcement(logistic_map_dynamics).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(logistic_map_dynamics, nonlinear_analysts).
narrative_ontology:constraint_beneficiary(logistic_map_dynamics, cryptography_designers).
narrative_ontology:constraint_beneficiary(logistic_map_dynamics, ecosystem_managers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(logistic_map_dynamics, long_term_planners).
narrative_ontology:constraint_victim(logistic_map_dynamics, proponents_of_linear_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE POPULATION ITERATE (THE SYSTEM STATE)
% For the state variable x_n itself, the iterative law is an absolute,
% unyielding Mountain. It cannot "choose" its next state; it is determined.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ECOSYSTEM MANAGER (INSTITUTIONAL BENEFICIARY)
% For a manager who can control the parameter 'r' (e.g., via harvest rates),
% the map is a Rope. By keeping the system in a stable, non-chaotic regime
% (r < 3.57), they use it as a tool to coordinate for a sustainable yield.
constraint_indexing:constraint_classification(logistic_map_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE LONG-TERM PLANNER (IN CHAOTIC REGIME)
% For a planner attempting to predict outcomes in the chaotic regime (high r),
% the map is a Snare. It extracts predictive certainty due to sensitive
% dependence on initial conditions (the "Butterfly Effect"), trapping the
% planner in a state of irreducible uncertainty.
constraint_indexing:constraint_classification(logistic_map_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees the full structure: a system that provides coordination
% (stable regime) but also has an inherent, asymmetric extractive property
% (chaotic regime). This dual nature is the definition of a Tangled Rope.
% χ = 0.35 * f(0.73) * 1.2 ≈ 0.35 * 1.15 * 1.2 ≈ 0.483
constraint_indexing:constraint_classification(logistic_map_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(logistic_map_dynamics_tests).

test(perspectival_gap) :-
    % Verify the manager (beneficiary) and planner (victim) disagree.
    constraint_indexing:constraint_classification(logistic_map_dynamics, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(logistic_map_dynamics, snare, context(agent_power(powerless), time_horizon(biographical), _, _)),
    constraint_indexing:constraint_classification(logistic_map_dynamics, mountain, context(agent_power(powerless), time_horizon(immediate), _, _)).

test(analytical_classification_is_tangled_rope) :-
    % The claim must match the analytical view.
    narrative_ontology:constraint_claim(logistic_map_dynamics, tangled_rope),
    constraint_indexing:constraint_classification(logistic_map_dynamics, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(logistic_map_dynamics),
    narrative_ontology:constraint_beneficiary(logistic_map_dynamics, _),
    narrative_ontology:constraint_victim(logistic_map_dynamics, _).

:- end_tests(logistic_map_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.35) and suppression (0.40) were chosen to
 *   reflect the map's dual nature. It is not a pure, inert Mountain of
 *   mathematics because its chaotic regime actively destroys information
 *   (extracts precision) and suppresses simpler, inadequate models. These
 *   values place it firmly in the Tangled Rope category from an analytical
 *   view, which correctly captures its character as both a tool for
 *   coordination (in stable regimes) and a source of irreducible complexity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and depends entirely on the parameter 'r' and the
 *   agent's ability to control it.
 *   - The Ecosystem Manager (institutional) can set 'r' to a low value,
 *     experiencing the map as a predictable, useful Rope for coordination.
 *   - The Long-Term Planner (powerless) facing a system with high 'r'
 *     experiences it as a Snare that destroys foresight.
 *   - The system state variable itself (powerless, trapped) experiences the
 *     update rule as an immutable Mountain of physics at every instant.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Nonlinear analysts and cryptographers benefit from the
 *     very complexity that victimizes planners. They gain a formal model for
 *     chaos and a source of pseudo-randomness. Ecosystem managers benefit from
 *     the map's predictive power in stable regimes.
 *   - Victims: Long-term planners and proponents of simple linear models are
 *     the primary victims. The former lose predictive certainty, and the
 *     latter have their models invalidated by the map's demonstrated reality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents two errors. First, it avoids
 *   mislabeling it as a pure Mountain, which would ignore the extractive cost
 *   of chaos and its role in suppressing other models. Second, it avoids
 *   mislabeling it as a pure Snare, which would ignore its genuine and widely
 *   used coordination function in stable, predictable regimes for fields like
 *   ecology and resource management. The Tangled Rope classification correctly
 *   holds both truths in tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_logistic_map_dynamics,
    'Is the chaos in the logistic map a fundamental feature of reality (Mountain) or an artifact of a simplified model (Tangled Rope)?',
    'Discovery of a higher-dimensional, more fundamental model that explains apparent chaos as a projection of a simpler, predictable dynamic.',
    'If a simpler underlying model exists, this constraint is a Tangled Rope of modeling choice. If not, it is a true Mountain of computational irreducibility.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(logistic_map_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is below the 0.46 threshold for mandatory temporal data.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The logistic map serves as a foundational standard for modeling and
% understanding non-linear dynamics and the onset of chaos.
narrative_ontology:coordination_type(logistic_map_dynamics, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% groups and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */