% ============================================================================
% CONSTRAINT STORY: geophysics_superionic_core
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_geophysics_superionic_core, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geophysics_superionic_core
 *   human_readable: "Super-ionic state of matter in Earth's inner core"
 *   domain: scientific/physical
 *
 * SUMMARY:
 *   Based on advanced seismic wave analysis, geophysicists have discovered
 *   that Earth's inner core exists in a "super-ionic" state. This state
 *   consists of a solid iron lattice through which lighter elements (like
 *   hydrogen, oxygen, and carbon) flow as a liquid. This physical reality
 *   constrains all models of planetary formation and geodynamics.
 *
 * KEY AGENTS (by structural relationship):
 *   - Geophysics community: Analytical observers who must now update their models.
 *   - Theoretical physicists with competing models: Constrained by the new evidence.
 *   - Humanity: Powerless observers of a fundamental, unchangeable natural fact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geophysics_superionic_core, 0.05).
domain_priors:suppression_score(geophysics_superionic_core, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(geophysics_superionic_core, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, extractiveness, 0.05).
narrative_ontology:constraint_metric(geophysics_superionic_core, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(geophysics_superionic_core, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification, preventing a Mountain classification.
narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(geophysics_superionic_core, mountain).
narrative_ontology:human_readable(geophysics_superionic_core, "Super-ionic state of matter in Earth's inner core").
narrative_ontology:topic_domain(geophysics_superionic_core, "scientific/physical").

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges from the laws of physics under extreme pressure
% and temperature, without any human design or enforcement. This flag is
% required for the mountain metric gate to fire.
domain_priors:emerges_naturally(geophysics_superionic_core).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not applicable for this constraint as it is a Mountain (a natural law).
% Beneficiary/victim declarations are for human-constructed systems.

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

% UNIFORM-TYPE CONSTRAINT (MOUNTAIN-ONLY):
% This constraint is a feature of the physical world. As such, it classifies
% as a Mountain from all possible perspectives. The low ε and suppression
% scores ensure that even with maximum scaling from f(d) and σ(S), the
% thresholds for other classifications are never met. The following
% perspectives demonstrate this invariance.

% PERSPECTIVE 1: HUMANITY (POWERLESS OBSERVER)
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GEOPHYSICIST (ANALYTICAL OBSERVER)
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THEORIST WITH A COMPETING MODEL (MODERATE POWER)
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geophysics_superionic_core_tests).

test(classification_invariance, [nondet]) :-
    % Verify that the classification is a Mountain from all key perspectives.
    constraint_indexing:constraint_classification(geophysics_superionic_core, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geophysics_superionic_core, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(geophysics_superionic_core, mountain, context(agent_power(moderate), _, _, _)).

test(mountain_threshold_validation) :-
    % Verify that base metrics meet the strict Mountain criteria.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(geophysics_superionic_core, ExtMetricName, E),
    narrative_ontology:constraint_metric(geophysics_superionic_core, SupMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the constraint passes the Natural Law certification profile.
    narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, R),
    domain_priors:emerges_naturally(geophysics_superionic_core),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(geophysics_superionic_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint represents a fundamental scientific discovery about the
 *   physical world, making it a canonical example of a Mountain.
 *   - Base Extractiveness (ε=0.05): Extremely low. The "extraction" is the
 *     intellectual cost for scientists to discard older, incorrect models.
 *     It does not extract resources or freedom in any social sense.
 *   - Suppression (0.02): Also extremely low. Alternatives are not suppressed
 *     by coercion but are falsified by empirical evidence, which is the
 *     hallmark of scientific progress, not social control.
 *   - NL Profile: The `accessibility_collapse` (0.98) is high because the
 *     seismic data strongly invalidates prior models. `resistance` (0.05) is
 *     low, representing standard scientific skepticism, not organized
 *     opposition. The `emerges_naturally` flag is critical, as this state of
 *     matter is a product of physics, not human design.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The constraint's properties are derived
 *   from physical reality, not social agreements. Therefore, its classification
 *   is invariant across all indices (power, time, exit, scope). A geophysicist,
 *   a layperson, and a competing theorist are all equally subject to this
 *   physical fact. It is a Mountain for everyone.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality logic (beneficiary/victim) is not applicable to Mountain
 *   constraints. The concept of a "beneficiary" of the law of gravity or a
 *   "victim" of the speed of light is incoherent. The system correctly models
 *   this by tying directionality to social constructs, which are absent here.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a natural law as a Mountain. This
 *   prevents the mischaracterization of a scientific fact as a social construct,
 *   such as a Piton (a dogma maintained for theatrical reasons) or a Snare (a
 *   "truth" used to extract from a population). The ε-invariance principle is
 *   key: any attempt to measure this constraint that yielded a high ε would,
 *   by definition, be measuring a different (likely social) constraint layered
 *   on top of the science, not the scientific fact itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_geophysics_superionic_core,
    'What is the precise composition and dynamic behavior of the flowing light elements (H, O, C) within the iron lattice?',
    'Higher-resolution seismic tomography, advances in high-pressure/temperature experimental physics (diamond anvil cells), and ab initio simulations.',
    'A specific composition would refine models of Earth''s formation, primordial chemistry, and the exact mechanism of the geodynamo, but it would not change the fundamental classification of the super-ionic state as a Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(geophysics_superionic_core, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% While not required for a low-extraction constraint (ε < 0.46), these
% measurements are included to demonstrate the stability of a natural law
% over its "discovery and acceptance" lifecycle. The metrics are flat,
% indicating no drift, as expected for a physical constant.
% The interval [0, 10] represents a decade of confirmation and integration.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(gsc_tr_t0, geophysics_superionic_core, theater_ratio, 0, 0.01).
narrative_ontology:measurement(gsc_tr_t5, geophysics_superionic_core, theater_ratio, 5, 0.01).
narrative_ontology:measurement(gsc_tr_t10, geophysics_superionic_core, theater_ratio, 10, 0.01).

% Extraction over time (stable and low):
narrative_ontology:measurement(gsc_ex_t0, geophysics_superionic_core, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(gsc_ex_t5, geophysics_superionic_core, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(gsc_ex_t10, geophysics_superionic_core, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is not a coordination mechanism.
% narrative_ontology:coordination_type(geophysics_superionic_core, ...).

% Network relationships: This discovery directly impacts models of the Earth's
% magnetic field (the geodynamo).
narrative_ontology:affects_constraint(geophysics_superionic_core, geophysics_geodynamo_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. As a Mountain constraint, there are no
% beneficiary/victim groups, so the directionality derivation chain is not
% triggered. The classification is determined by the raw, unscaled metrics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */