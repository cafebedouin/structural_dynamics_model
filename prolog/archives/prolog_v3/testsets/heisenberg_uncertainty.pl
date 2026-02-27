% ============================================================================
% CONSTRAINT STORY: heisenberg_uncertainty
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_heisenberg_uncertainty, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: heisenberg_uncertainty
 *   human_readable: Heisenberg Uncertainty Principle (ΔxΔp ≥ ħ/2)
 *   domain: technological
 *
 * SUMMARY:
 *   The Heisenberg Uncertainty Principle is a fundamental limit in quantum
 *   mechanics stating that it is impossible to simultaneously know the exact
 *   position and momentum of a particle. This is not a limit of measurement
 *   technology but an intrinsic property of quantum systems. The more
 *   precisely one property is measured, the less precisely the other can be
 *   known. This constraint is a canonical example of a Mountain: an
 *   unchangeable feature of physical reality.
 *
 * KEY AGENTS (by structural relationship):
 *   - Nanotechnology Engineer: Agent attempting to work at the limit (powerless/trapped)
 *   - Quantum Cryptographer: Agent leveraging the limit (institutional/arbitrage)
 *   - Theoretical Physicist: Analytical observer mapping the limit (analytical/analytical)
 *   Note: As a Mountain, the principle itself has no beneficiaries or victims;
 *   these agents simply interact with a fixed feature of reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a physical law, it has near-zero extractiveness. It does not
% extract value, it defines the boundaries of what is possible.
domain_priors:base_extractiveness(heisenberg_uncertainty, 0.05).
% Rationale: Suppression is near-zero. It does not "suppress" classical
% mechanics; it simply describes the regime where classical mechanics is an
% invalid approximation.
domain_priors:suppression_score(heisenberg_uncertainty, 0.02).
% Rationale: The principle is purely functional with no performative aspect.
domain_priors:theater_ratio(heisenberg_uncertainty, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heisenberg_uncertainty, extractiveness, 0.05).
narrative_ontology:constraint_metric(heisenberg_uncertainty, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(heisenberg_uncertainty, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% Rationale: Alternatives are not just inaccessible, they are logically
% incoherent within the framework of quantum mechanics.
narrative_ontology:constraint_metric(heisenberg_uncertainty, accessibility_collapse, 1.0).
% Rationale: One cannot meaningfully "resist" a law of physics. Any
% resistance is simply an experimental result that fails.
narrative_ontology:constraint_metric(heisenberg_uncertainty, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(heisenberg_uncertainty, mountain).
narrative_ontology:human_readable(heisenberg_uncertainty, "Heisenberg Uncertainty Principle (ΔxΔp ≥ ħ/2)").
narrative_ontology:topic_domain(heisenberg_uncertainty, "technological").

% --- Emergence flag (required for mountain constraints) ---
% The principle is a fundamental property of the universe, not a human construct.
domain_priors:emerges_naturally(heisenberg_uncertainty).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), this constraint does not
% have beneficiaries or victims in the structural sense. It is a neutral,
% fixed feature of the environment.

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

% UNIFORM-TYPE CONSTRAINT: MOUNTAIN
% A true natural law classifies as a Mountain from ALL perspectives. The
% low base extractiveness (ε) and suppression scores ensure that even with
% scaling, the classification remains fixed. The different "experiences" of
% the law (e.g., as a tool for cryptography or a barrier for nanotechnology)
% are downstream effects, not changes in the classification of the principle
% itself. Those applications would be modeled as separate constraints that
% are AFFECTED BY this one.

% PERSPECTIVE 1: THE THEORETICAL PHYSICIST (ANALYTICAL)
constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE QUANTUM CRYPTOGRAPHER (INSTITUTIONAL)
constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE NANOTECHNOLOGY ENGINEER (POWERLESS)
constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heisenberg_uncertainty_tests).

test(perspective_invariance) :-
    % Verify that as a natural law, it classifies as Mountain from all key perspectives.
    constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain, context(agent_power(powerless), _, _, _)).

test(mountain_threshold_validation) :-
    % Verify metrics are within the required range for a Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    config:param(mountain_extractiveness_max, MountainEpsMax),
    config:param(mountain_suppression_ceiling, MountainSuppMax),
    narrative_ontology:constraint_metric(heisenberg_uncertainty, ExtMetricName, E),
    narrative_ontology:constraint_metric(heisenberg_uncertainty, SuppMetricName, S),
    E =< MountainEpsMax,
    S =< MountainSuppMax.

test(natural_law_profile_validation) :-
    % Verify the NL profile metrics are present and meet certification thresholds.
    config:param(natural_law_collapse_min, NLCollapseMin),
    config:param(natural_law_resistance_max, NLResistanceMax),
    narrative_ontology:constraint_metric(heisenberg_uncertainty, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(heisenberg_uncertainty, resistance, R),
    AC >= NLCollapseMin,
    R =< NLResistanceMax,
    domain_priors:emerges_naturally(heisenberg_uncertainty).

:- end_tests(heisenberg_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Heisenberg Uncertainty Principle is a canonical example of a Mountain.
 *   Its metrics (ε=0.05, suppression=0.02) are set to be extremely low,
 *   reflecting its status as a non-extractive, non-coercive feature of
 *   physical reality. The Natural Law (NL) profile metrics
 *   (accessibility_collapse=1.0, resistance=0.0) are set to their logical
 *   extremes, as alternatives are incoherent and resistance is futile. The
 *   `emerges_naturally` flag is asserted, completing the requirements for a
 *   robust Mountain classification that will pass the engine's NL
 *   certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. A key feature of a Mountain/Natural Law is
 *   its invariance across all observer indices. While a nanotechnologist
 *   experiences the principle as a frustrating limit and a cryptographer
 *   experiences it as a useful tool, the classification of the principle
 *   itself remains Mountain for both. Their applications or struggles are
 *   separate phenomena that are *affected by* this Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint has no beneficiaries or victims.
 *   Directionality is not a relevant concept for a fundamental physical law,
 *   hence no `constraint_beneficiary` or `constraint_victim` facts are
 *   declared. The engine will fall back to canonical `d` values for each
 *   power atom, but the extremely low base extractiveness ensures that the
 *   effective extraction χ remains negligible, preserving the Mountain
 *   classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint serves as a grounding example of what is NOT a human-
 *   constructed system of extraction. Its classification as a Mountain with
 *   near-zero metrics provides a baseline against which Snares and Tangled
 *   Ropes can be compared. Any attempt to frame this as a Snare (e.g., "it
 *   extracts certainty from engineers") is a category error that conflates a
 *   physical limit with a coercive social structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_heisenberg_uncertainty,
    'Is the uncertainty fundamentally ontic (a property of reality itself) or epistemic (a limit on our knowledge)?',
    'Resolution of the measurement problem in quantum mechanics or development of a verified theory of quantum gravity.',
    'If ontic, it remains an absolute Mountain. If epistemic, it could theoretically be re-classified as a Scaffold of our current understanding, though this is highly speculative.',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_heisenberg_uncertainty, conceptual, 'Whether quantum uncertainty is an intrinsic property of reality (ontic) or a limit on knowledge (epistemic).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(heisenberg_uncertainty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a low-extraction (ε < 0.46) and time-invariant
% physical law, temporal measurements for drift detection are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% The uncertainty principle is a foundational constraint that enables or
% limits other technological constraints. For example, it enables the
% security model of Quantum Key Distribution (QKD).
narrative_ontology:affects_constraint(heisenberg_uncertainty, quantum_key_distribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. No overrides are needed as this is a uniform-type Mountain
% constraint with no beneficiaries or victims.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */