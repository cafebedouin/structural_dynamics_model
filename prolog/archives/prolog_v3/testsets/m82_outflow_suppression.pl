% ============================================================================
% CONSTRAINT STORY: m82_outflow_suppression
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-27
% ============================================================================

:- module(constraint_m82_outflow_suppression, []).

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
 *   constraint_id: m82_outflow_suppression
 *   human_readable: M82* Explosive Outflow and Star Formation Suppression
 *   domain: physical/astronomical
 *
 * SUMMARY:
 *   Recent observations of the supermassive black hole (SMBH) M82* revealed an
 *   unprecedented phenomenon: instead of accreting matter, it is violently
 *   expelling it. This explosive outflow creates a galactic wind that clears
 *   interstellar gas from the galaxy's core, thereby suppressing the formation
 *   of new stars in that region. This constraint represents the physical law
 *   governing this process, acting as a fundamental limit on galactic evolution.
 *
 * KEY AGENTS (by structural relationship):
 *   - Nascent Star Systems: Structural target (powerless/trapped) — their formation is prevented by the physical process.
 *   - Astrophysical Community: Analytical beneficiary (institutional/arbitrage) — their understanding of galactic dynamics benefits from observing and modeling this law.
 *   - Analytical Observer: Sees the full structure as a physical law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(m82_outflow_suppression, 0.02).
domain_priors:suppression_score(m82_outflow_suppression, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(m82_outflow_suppression, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(m82_outflow_suppression, extractiveness, 0.02).
narrative_ontology:constraint_metric(m82_outflow_suppression, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(m82_outflow_suppression, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These are mandatory for a Mountain classification.
narrative_ontology:constraint_metric(m82_outflow_suppression, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(m82_outflow_suppression, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(m82_outflow_suppression, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the laws of physics without human design.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(m82_outflow_suppression).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain constraint representing a physical law, beneficiary/victim
% declarations are not applicable. The classification is determined by the
% intrinsic metrics (ε, suppression, NL profile) and is invariant.

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
% This constraint represents a fundamental physical law. As such, it classifies
% as a Mountain from all possible perspectives. The following perspectives
% demonstrate this invariance.

% PERSPECTIVE 1: THE STRUCTURAL TARGET
% The interstellar gas and potential stars whose formation is suppressed.
constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SCIENTIFIC OBSERVER
% The community of astrophysicists studying the phenomenon.
constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, viewing the constraint as a universal law.
constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(m82_outflow_suppression_tests).

test(classification_invariance, [nondet]) :-
    % Verify that the classification is Mountain from all defined perspectives.
    constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
        context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_adherence) :-
    % Verify that the base metrics conform to the Mountain classification rules.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(m82_outflow_suppression, ExtMetricName, E),
    narrative_ontology:constraint_metric(m82_outflow_suppression, SupMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_compliance) :-
    % Verify that the NL profile metrics meet the certification thresholds.
    narrative_ontology:constraint_metric(m82_outflow_suppression, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(m82_outflow_suppression, resistance, R),
    AC >= 0.85,
    R =< 0.15,
    domain_priors:emerges_naturally(m82_outflow_suppression).

:- end_tests(m82_outflow_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a newly
 *   observed physical process. The metrics reflect this:
 *   - `base_extractiveness` (ε=0.02) is near zero because a physical law does not
 *     "extract" value in a social or economic sense; it simply defines the
 *     boundaries of possibility.
 *   - `suppression_score` (0.01) is minimal because there are no coherent
 *     alternatives to suppress. One cannot opt out of the physics of a galaxy.
 *   - The Natural Law (NL) Profile is definitive: `accessibility_collapse` is 1.0
 *     as no alternative physical reality is accessible. `resistance` is 0.0 as
 *     resisting this process is incoherent. The `emerges_naturally` flag confirms
 *     it is not a human construct. These three facts are required for the
 *     system to certify the constraint as a natural law.
 *
 * PERSPECTIVAL GAP:
 *   A key feature of a Mountain constraint is the *absence* of a perspectival
 *   gap. Because the constraint is a fundamental, non-negotiable feature of
 *   reality, all observers, regardless of their power, exit options, or scope,
 *   classify it identically: as a Mountain. The powerless nascent stars and the
 *   institutional scientific community both perceive an unchangeable limit.
 *
 * DIRECTIONALITY LOGIC:
 *   For a Mountain constraint, directionality (the `d` parameter) is conceptually
 *   moot, as the classification logic for Mountains bypasses the calculation of
 *   effective extraction (χ). The constraint's immutability, defined by its
 *   low base metrics and NL profile, is the decisive factor. While the engine
 *   will derive different `d` values for the different perspectives, these do not
 *   alter the final Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a physical limit, preventing its
 *   misinterpretation. It is not a Snare, as there is no agent engineering
 *   extraction. It is not a Rope, as it is not a coordination solution. It is not
 *   a Piton, as it is fully functional and not a remnant of a past system. By
 *   classifying it as a Mountain, the system recognizes it as a fixed boundary
 *   condition upon which other, socially constructed constraints may operate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_m82_outflow,
    'Is the explosive outflow from M82* a common but previously unobserved phase in the lifecycle of SMBHs, or is it a rare anomaly specific to this galaxy?',
    'Systematic observation of other similar SMBHs over long time horizons to detect comparable outflow events.',
    'If common, it fundamentally alters models of galactic evolution (Mountain). If rare, it becomes a localized anomaly with less universal impact (still Mountain, but with a more limited scope of influence).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(m82_outflow_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal measurements are not applicable for a Mountain constraint representing
% a stable physical law operating on cosmic timescales. The constraint does not
% exhibit lifecycle drift in the human-relevant sense.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This is a physical law, not a coordination mechanism, so coordination_type
% and boltzmann_floor_override are not applicable.

% Network relationships (structural influence edges)
% This physical law directly impacts models of star formation.
narrative_ontology:affects_constraint(m82_outflow_suppression, galactic_star_formation_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The Mountain classification is invariant and does not
% depend on the derived directionality values.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */