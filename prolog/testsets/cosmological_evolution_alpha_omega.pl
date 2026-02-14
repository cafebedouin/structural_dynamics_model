% ============================================================================
% CONSTRAINT STORY: cosmological_evolution_alpha_omega
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_cosmological_evolution_alpha_omega, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 *   constraint_id: cosmological_evolution_alpha_omega
 *   human_readable: The Physical Laws Governing the Universe's Lifecycle
 *   domain: physics/cosmology
 *
 * SUMMARY:
 *   Models the fundamental physical laws as a constraint governing the universe's
 *   evolution. The laws themselves are an immutable Mountain with near-zero
 *   extractiveness. However, agents embedded within different cosmological eras
 *   experience the *manifestation* of these laws differently. The story uses
 *   temporal measurements to track the changing state of the universe (e.g.,
 *   rising theater as star formation ceases) under these constant laws.
 *
 * KEY AGENTS (by structural relationship):
 *   - Baryonic Matter (powerless/trapped): The "victim" of entropy and gravitational collapse, but also the beneficiary of structure formation.
 *   - Kardashev Type III Civilization (institutional/arbitrage): An agent capable of coordinating matter on a galactic scale, using the physical laws as a pure coordination mechanism.
 *   - Gravity: The primary coordinating agent, metaphorically the "beneficiary".
 *   - Cosmological Theorist (analytical/analytical): The observer who perceives the underlying, unchanging laws.
 *
 * [RESOLVED MANDATROPHY]
 *   The original formulation conflated the physical process of entropy (where
 *   free energy is "extracted" from the system) with the structural property of
 *   base_extractiveness (ε). This regeneration clarifies that the constraint is
 *   the *laws of physics*, not the *state of the universe*. The laws themselves
 *   are not extractive (ε ≈ 0); they are a set of rules. This correctly classifies
 *   the constraint as a Mountain from the analytical perspective, avoiding the
 *   false classification of physical law as a Snare. The "extractive" feel of
 *   black holes or heat death is a narrative interpretation of a physical
 *   process, not structural extraction between agents.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS) - Represents the unchanging physical laws
   ========================================================================== */

% --- Numerical metrics ---
% The laws of physics are non-extractive. ε represents the cost of verification,
% which is near-zero for fundamental constants.
domain_priors:base_extractiveness(cosmological_evolution_alpha_omega, 0.01).
domain_priors:suppression_score(cosmological_evolution_alpha_omega, 0.01).   % Alternatives are incoherent, not suppressed.
domain_priors:theater_ratio(cosmological_evolution_alpha_omega, 0.0).       % The final state has no function, but the laws are not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cosmological_evolution_alpha_omega, extractiveness, 0.01).
narrative_ontology:constraint_metric(cosmological_evolution_alpha_omega, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(cosmological_evolution_alpha_omega, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cosmological_evolution_alpha_omega, accessibility_collapse, 1.0). % No alternatives are conceivable.
narrative_ontology:constraint_metric(cosmological_evolution_alpha_omega, resistance, 0.0). % Cannot resist physical law.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cosmological_evolution_alpha_omega, mountain).

% --- Emergence flag (required for mountain constraints) ---
domain_priors:emerges_naturally(cosmological_evolution_alpha_omega).

% --- Structural relationships ---
% In this cosmological context, these are metaphorical. "Beneficiary" is the
% agent of structure, "Victim" is that which is structured or consumed.
narrative_ontology:constraint_beneficiary(cosmological_evolution_alpha_omega, gravity).
narrative_ontology:constraint_victim(cosmological_evolution_alpha_omega, baryonic_matter).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BARYONIC MATTER (A STAR) IN THE STELLIFEROUS ERA (ROPE)
% During the star-forming era, physical laws are a Rope that allows matter to
% coordinate into complex structures (stars, galaxies).
constraint_indexing:constraint_classification(cosmological_evolution_alpha_omega, rope,
    context(agent_power(powerless),
            time_horizon(biographical), % The 'life' of a star
            exit_options(trapped),      % Trapped within its gravity well
            spatial_scope(local))).     % A single solar system

% PERSPECTIVE 2: A KARDASHEV TYPE III CIVILIZATION (ROPE)
% An institutional-scale actor sees the laws of physics as a pure coordination
% mechanism (Rope) for large-scale engineering projects like Dyson spheres.
constraint_indexing:constraint_classification(cosmological_evolution_alpha_omega, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can manipulate matter and energy within the laws
            spatial_scope(regional))). % A galaxy

% PERSPECTIVE 3: A HYPOTHETICAL OBSERVER IN THE DEGENERATE ERA (ROPE)
% Even in the far future, the underlying laws are still a coordination mechanism,
% albeit one governing decay and gravitational interactions rather than fusion.
% The low ε prevents a Tangled Rope classification.
constraint_indexing:constraint_classification(cosmological_evolution_alpha_omega, rope,
    context(agent_power(moderate),
            time_horizon(generational), % The 'life' of a white dwarf
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE COSMOLOGICAL THEORIST (MOUNTAIN)
% The analytical observer, viewing the entire lifecycle, sees the whole process
% as one immutable Mountain: the unfolding of a fixed set of physical laws.
constraint_indexing:constraint_classification(cosmological_evolution_alpha_omega, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cosmological_evolution_alpha_omega_tests).

test(perspectival_variance) :-
    constraint_indexing:constraint_classification(cosmological_evolution_alpha_omega, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cosmological_evolution_alpha_omega, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metric_compliance) :-
    % This test confirms that the base parameters correctly classify as a Mountain.
    ID = cosmological_evolution_alpha_omega,
    domain_priors:base_extractiveness(ID, E),
    domain_priors:suppression_score(ID, S),
    narrative_ontology:constraint_metric(ID, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ID, resistance, R),
    config:param(mountain_extractiveness_max, MaxE),
    config:param(mountain_suppression_ceiling, MaxS),
    config:param(natural_law_collapse_min, MinAC),
    config:param(natural_law_resistance_max, MaxR),
    E =< MaxE,
    S =< MaxS,
    AC >= MinAC,
    R =< MaxR,
    domain_priors:emerges_naturally(ID).

:- end_tests(cosmological_evolution_alpha_omega_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models the universe's physical laws as the constraint. The core
 *   insight is that the laws themselves are constant and non-extractive (ε=0.01),
 *   making them a fundamental Mountain. The dramatic changes over cosmological
 *   time are changes in the *state of the system* governed by these laws, not
 *   changes in the constraint itself.
 *   The temporal measurements track the evolution of the system's properties,
 *   such as the rise in 'theater' as the universe's primary function (star
 *   formation) ceases and only inertial structures remain (the Piton-like
 *   Degenerate Era). This correctly separates the invariant constraint (the laws)
 *   from the variant system state (the eras).
 *
 * PERSPECTIVAL GAP:
 *   An agent embedded *within* the system (a star, a civilization) experiences
 *   the laws as a coordination mechanism (Rope) that enables structure and
 *   action. The global, analytical observer sees the entire pre-determined
 *   trajectory as one single, immutable Mountain. There is no Snare perspective
 *   because the laws themselves are not structurally extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_cosmological_const,
    'Is the cosmological constant truly constant (a Mountain) or does it evolve (making the universe a Scaffold)?',
    'Precision measurements of cosmic expansion at high redshift.',
    'If it evolves, the ultimate fate (Heat Death vs. Big Rip) is not a fixed Mountain but a contingent outcome.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cosmological_evolution_alpha_omega, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint (physical law) is stable (ε is constant). These measurements
% track the changing properties of the *system* governed by the constraint.

% T=0-8: Stelliferous Era (Functional, Rope-like manifestation)
narrative_ontology:measurement(cosmo_ex_t0, cosmological_evolution_alpha_omega, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(cosmo_th_t0, cosmological_evolution_alpha_omega, theater_ratio, 0, 0.0).
narrative_ontology:measurement(cosmo_su_t0, cosmological_evolution_alpha_omega, suppression_score, 0, 0.01).
narrative_ontology:measurement(cosmo_ex_t8, cosmological_evolution_alpha_omega, base_extractiveness, 8, 0.01).
narrative_ontology:measurement(cosmo_th_t8, cosmological_evolution_alpha_omega, theater_ratio, 8, 0.10).
narrative_ontology:measurement(cosmo_su_t8, cosmological_evolution_alpha_omega, suppression_score, 8, 0.01).

% T=9-14: Degenerate Era (Inertial, Piton-like manifestation)
narrative_ontology:measurement(cosmo_ex_t9, cosmological_evolution_alpha_omega, base_extractiveness, 9, 0.01).
narrative_ontology:measurement(cosmo_th_t9, cosmological_evolution_alpha_omega, theater_ratio, 9, 0.70). % Function (star formation) ceases, structure remains
narrative_ontology:measurement(cosmo_su_t9, cosmological_evolution_alpha_omega, suppression_score, 9, 0.02).
narrative_ontology:measurement(cosmo_ex_t14, cosmological_evolution_alpha_omega, base_extractiveness, 14, 0.01).
narrative_ontology:measurement(cosmo_th_t14, cosmological_evolution_alpha_omega, theater_ratio, 14, 0.80).
narrative_ontology:measurement(cosmo_su_t14, cosmological_evolution_alpha_omega, suppression_score, 14, 0.03).

% T=15-20: Black Hole Era -> Heat Death (Final state, Mountain-like manifestation)
narrative_ontology:measurement(cosmo_ex_t15, cosmological_evolution_alpha_omega, base_extractiveness, 15, 0.01).
narrative_ontology:measurement(cosmo_th_t15, cosmological_evolution_alpha_omega, theater_ratio, 15, 0.40). % Activity resumes (black holes), theater drops
narrative_ontology:measurement(cosmo_su_t15, cosmological_evolution_alpha_omega, suppression_score, 15, 0.04).
narrative_ontology:measurement(cosmo_ex_t20, cosmological_evolution_alpha_omega, base_extractiveness, 20, 0.01). % Final value
narrative_ontology:measurement(cosmo_th_t20, cosmological_evolution_alpha_omega, theater_ratio, 20, 0.0).   % Final value
narrative_ontology:measurement(cosmo_su_t20, cosmological_evolution_alpha_omega, suppression_score, 20, 0.01).   % Final value (matches base property)

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cosmological_evolution_alpha_omega, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */