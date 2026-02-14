% ============================================================================
% CONSTRAINT STORY: star_to_black_hole_observational_limit
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-26
% ============================================================================

:- module(constraint_star_to_black_hole_observational_limit, []).

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
 *   constraint_id: star_to_black_hole_observational_limit
 *   human_readable: Observational Limit on Directly Observing Star-to-Black Hole Transformation
 *   domain: technological
 *
 * SUMMARY:
 *   The direct observation of a star collapsing into a black hole is extremely
 *   rare and difficult due to the speed of the event, obscuration by ejected
 *   material, and the inability to predict its occurrence. This constraint
 *   models this difficulty as a natural limit on observational capabilities,
 *   emerging from fundamental physics.
 *
 * KEY AGENTS (by structural relationship):
 *   - Observational Astronomers: The group whose scientific goals are constrained by this physical limit (analytical/mobile).
 *   - Theoretical Physicists: A group that models the constraint but is not directly impeded by the observational limit in the same way (analytical/analytical).
 *   - General Public: A group for whom the constraint is an abstract fact with no direct impact (powerless/trapped).
 *   - Analytical Observer: The system perspective viewing the full structure of the physical law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(star_to_black_hole_observational_limit, 0.15).
domain_priors:suppression_score(star_to_black_hole_observational_limit, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(star_to_black_hole_observational_limit, 0.00).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, extractiveness, 0.15).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, theater_ratio, 0.00).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, accessibility_collapse, 0.90).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(star_to_black_hole_observational_limit, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(star_to_black_hole_observational_limit).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a mountain (natural law) constraint, this
% is impersonal and has no beneficiaries or victims in the structural sense.
% The classification is invariant across all perspectives.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE GENERAL PUBLIC (POWERLESS)
% For a non-scientist, this is an unchangeable fact about the universe.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: OBSERVATIONAL ASTRONOMERS (INSTITUTIONAL)
% For the scientific community attempting to make observations, this is a
% fundamental physical barrier to data collection.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which recognizes this as a feature of
% physical law.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(star_to_black_hole_observational_limit_tests).

test(perspectival_consistency) :-
    % Verify perspectival consistency. In this mountain case all types should be the same.
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, ExtMetricName, E),
    E =< 0.25. % Mountain check.

test(nl_profile_validation) :-
    narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, resistance, R),
    domain_priors:emerges_naturally(star_to_black_hole_observational_limit),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(star_to_black_hole_observational_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The direct observation of a star collapsing into a black hole is rare for
 *   multiple reasons. The event itself is fast, it is often obscured by ejected
 *   material, and its location cannot be predicted. Given that these aspects
 *   emerge from fundamental physics, the limitation on direct observation
 *   qualifies as a Mountain constraint. The base extractiveness (0.15) represents
 *   the inherent difficulty and resource cost (lost opportunity) in gathering
 *   data. The suppression score (0.02) reflects the near-total lack of
 *   alternative means to directly witness the collapse. The theater ratio is
 *   zero as there is no performative aspect to a physical law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a natural law, the constraint is classified
 *   as a Mountain from all perspectives, from a layperson (powerless) to the
 *   scientific community (institutional) and the abstract analytical observer.
 *   Its character is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not a salient feature of a Mountain constraint. It is an
 *   impersonal feature of the environment. Unlike social constraints, it has no
 *   beneficiaries or victims in a structural sense, so those declarations are
 *   omitted. The classification engine correctly identifies this as a uniform-type
 *   constraint where perspectival differences in directionality (d) do not
 *   alter the classification outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification is robust. It prevents mislabeling this
 *   observational difficulty as a Snare (implying a malicious agent hiding
 *   the data) or a Piton (implying a degraded human system). The constraint is
 *   not due to deliberate extraction or institutional inertia, but is a
 *   fundamental property of the universe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_star_to_bh,
    'Could a future technological paradigm (e.g., advanced multi-messenger astronomy) completely overcome the challenges to directly observing black hole formation?',
    'Sustained observation of gravitational wave events with corresponding electromagnetic counterparts over centuries.',
    'If True: The constraint might transition to a Rope (a coordination problem of building and aiming the right instruments). If False: The constraint remains a Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(star_to_black_hole_observational_limit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a low-extraction constraint, so temporal data is not strictly
% required. However, for a natural law, the metrics are static, reflecting
% its time-invariant nature.
narrative_ontology:measurement(star_to_bh_tr_t0, star_to_black_hole_observational_limit, theater_ratio, 0, 0.00).
narrative_ontology:measurement(star_to_bh_tr_t5, star_to_black_hole_observational_limit, theater_ratio, 5, 0.00).
narrative_ontology:measurement(star_to_bh_tr_t10, star_to_black_hole_observational_limit, theater_ratio, 10, 0.00).

narrative_ontology:measurement(star_to_bh_ex_t0, star_to_black_hole_observational_limit, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(star_to_bh_ex_t5, star_to_black_hole_observational_limit, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(star_to_bh_ex_t10, star_to_black_hole_observational_limit, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for a natural law constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a natural law constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */