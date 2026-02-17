% ============================================================================
% CONSTRAINT STORY: hd101584_stellar_evolution
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_hd101584_stellar_evolution, []).

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
 *   constraint_id: hd101584_stellar_evolution
 *   human_readable: The Gravitational Dynamics of the HD101584 Stellar System
 *   domain: physical
 *
 * SUMMARY:
 *   The HD101584 system features a dying red giant and a smaller companion star.
 *   The companion spiraled towards the giant, causing a "stellar battle" that
 *   resulted in the premature ejection of the giant's outer layers. This
 *   interaction is governed by the unchangeable laws of physics (gravity,
 *   conservation of energy and angular momentum), which forced the system into
 *   its current, bizarre "inside-out" configuration. The constraint is this
 *   set of physical laws dictating the system's evolution and final state.
 *
 * KEY AGENTS (by structural relationship):
 *   - Red Giant Star: Primary component subject to the laws of stellar evolution (powerless/trapped).
 *   - Companion Star: Catalyst of the interaction, also subject to the same physical laws (powerless/trapped).
 *   - Ejected Gaseous Shells: The resulting structure, whose form is dictated by the interaction's physics (powerless/trapped).
 *   - Astronomers: Analytical observers studying the system (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hd101584_stellar_evolution, 0.02).
domain_priors:suppression_score(hd101584_stellar_evolution, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hd101584_stellar_evolution, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, extractiveness, 0.02).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These values ensure it passes certification.
narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hd101584_stellar_evolution, mountain).
narrative_ontology:human_readable(hd101584_stellar_evolution, "The Gravitational Dynamics of the HD101584 Stellar System").

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(hd101584_stellar_evolution).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Omitted because this is a Mountain constraint. The concepts of beneficiary
% and victim are incoherent when applied to a physical law.

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

% UNIFORM-TYPE CONSTRAINT (MOUNTAIN-ONLY)
% This is a natural law constraint, which classifies as Mountain from all
% possible perspectives. The following perspectives demonstrate this invariance.

% PERSPECTIVE 1: THE SYSTEM COMPONENTS (e.g., the stars themselves)
% From the perspective of the objects within the system, the laws are
% an unchangeable reality they are trapped within.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: A HYPOTHETICAL EXTERNAL ACTOR
% Even for a powerful, external actor, the physical laws governing the system
% would be an immutable fact, a Mountain.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ASTRONOMERS)
% The scientific observer correctly identifies the constraint as a set of
% fundamental, unchangeable physical laws.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hd101584_stellar_evolution_tests).

test(perspectival_invariance, [nondet]) :-
    % For a Mountain, there should be no perspectival gap.
    % All perspectives must resolve to the same classification.
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(mountain_threshold_validation) :-
    % Verify that the base metrics conform to the Mountain classification rules.
    domain_priors:base_extractiveness(hd101584_stellar_evolution, E),
    domain_priors:suppression_score(hd101584_stellar_evolution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_compliance) :-
    % Verify that the natural law profile metrics meet certification thresholds.
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, R),
    domain_priors:emerges_naturally(hd101584_stellar_evolution),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hd101584_stellar_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a direct application of fundamental physical laws
 *   (gravity, conservation of energy/momentum). As such, it is a canonical
 *   Mountain. The base extractiveness (ε=0.02) is near zero because physical
 *   laws do not "extract" value in a social sense; they simply define the
 *   space of possible outcomes. The suppression score (0.01) is also near
 *   zero, reflecting that this is not a coercive suppression of alternatives
 *   but a fundamental *absence* of alternatives. The Natural Law Profile
 *   metrics (accessibility_collapse=1.0, resistance=0.0) and the
 *   `emerges_naturally` flag cement its Mountain status, ensuring it passes
 *   the system's NL certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a key feature of a Mountain constraint.
 *   The laws of physics are invariant regardless of the observer's power,
 *   knowledge, or position. The star itself, a hypothetical powerful entity,
 *   and a human astronomer all perceive the same immutable reality. The
 *   uniform classification across all indices demonstrates this invariance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations are omitted as they are incoherent
 *   concepts for a physical law. The laws of gravity do not "benefit" one
 *   star at the "expense" of another; they apply universally and symmetrically
 *   to all components of the system. Consequently, the directionality `d`
 *   is not a meaningful parameter here, and effective extraction (χ) remains
 *   near zero for all observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The Deferential Realism framework correctly identifies this physical
 *   process as a Mountain, preventing anthropomorphic misinterpretations.
 *   One could mistakenly view the companion star as a "victim" of the red
 *   giant, framing the interaction as a Snare. This model avoids that error
 *   by grounding the classification in the objective, non-extractive, and
 *   non-suppressive nature of the underlying physical laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hd101584_stellar_evolution,
    'Are current models of common-envelope evolution fully predictive for this system, or are there missing secondary physical effects (e.g., from magnetic fields) that influenced the outcome?',
    'Higher-resolution spectroscopic and interferometric observations (e.g., with JWST or ELT) combined with more sophisticated magneto-hydrodynamic simulations.',
    'If current models are sufficient, it confirms our understanding. If not, it points to new physics needing to be incorporated, refining the details of this Mountain but not changing its fundamental type.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hd101584_stellar_evolution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a Mountain constraint demonstrates its stability over time.
% The underlying physical laws do not change, so the metrics are constant.
% This section is technically optional (ε < 0.46) but included for completeness.

% Theater ratio over time (stable at zero):
narrative_ontology:measurement(hd101584_tr_t0, hd101584_stellar_evolution, theater_ratio, 0, 0.0).
narrative_ontology:measurement(hd101584_tr_t5, hd101584_stellar_evolution, theater_ratio, 5, 0.0).
narrative_ontology:measurement(hd101584_tr_t10, hd101584_stellar_evolution, theater_ratio, 10, 0.0).

% Extraction over time (stable near zero):
narrative_ontology:measurement(hd101584_ex_t0, hd101584_stellar_evolution, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(hd101584_ex_t5, hd101584_stellar_evolution, base_extractiveness, 5, 0.02).
narrative_ontology:measurement(hd101584_ex_t10, hd101584_stellar_evolution, base_extractiveness, 10, 0.02).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type does not apply to a physical law.

% Network relationships (structural influence edges)
% The specific dynamics of this system are a consequence of more fundamental
% physical laws. This edge represents that dependency.
narrative_ontology:affects_constraint(newtonian_gravity, hd101584_stellar_evolution).
narrative_ontology:affects_constraint(conservation_of_angular_momentum, hd101584_stellar_evolution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. Directionality overrides are irrelevant for Mountain constraints
% where the concepts of beneficiary and victim do not apply.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */