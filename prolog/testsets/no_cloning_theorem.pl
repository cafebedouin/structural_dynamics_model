% ============================================================================
% CONSTRAINT STORY: no_cloning_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_no_cloning_theorem, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: no_cloning_theorem
 *   human_readable: The No-Cloning Theorem
 *   domain: technological
 *
 * SUMMARY:
 *   The no-cloning theorem states that it is impossible to create an independent
 *   and identical copy of an arbitrary unknown quantum state. This is a
 *   fundamental consequence of the linearity of quantum mechanics, preventing the
 *   "copy-paste" functionality central to classical information processing. As a
 *   direct consequence of physical law, it is an invariant Mountain constraint,
 *   though its impacts are perceived differently by various agents.
 *
 * KEY AGENTS (by structural relationship):
 *   - Quantum Computer Engineer: Experiences the constraint's limitations (e.g., inability to use classical error correction).
 *   - Quantum Cryptographer: Leverages the constraint's limitations to build secure systems.
 *   - Quantum Physicist: Analytical observer mapping the boundaries of physical law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a law of physics, the theorem does not extract resources, it
% merely defines the boundaries of what is possible. Its extractiveness and
% suppression scores are therefore minimal, reflecting a fixed, non-coercive
% feature of reality.
domain_priors:base_extractiveness(no_cloning_theorem, 0.05).
domain_priors:suppression_score(no_cloning_theorem, 0.02).
domain_priors:theater_ratio(no_cloning_theorem, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(no_cloning_theorem, extractiveness, 0.05).
narrative_ontology:constraint_metric(no_cloning_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(no_cloning_theorem, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: The theorem completely forecloses the alternative of
% perfect cloning within its domain.
narrative_ontology:constraint_metric(no_cloning_theorem, accessibility_collapse, 0.98).
% Resistance: Meaningful resistance is incoherent, as one cannot "oppose" a
% mathematical consequence of a physical theory.
narrative_ontology:constraint_metric(no_cloning_theorem, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(no_cloning_theorem, mountain).

% --- Emergence flag (required for mountain constraints) ---
% The theorem is a mathematical derivation from the postulates of quantum
% mechanics; it emerges naturally from the structure of the theory.
domain_priors:emerges_naturally(no_cloning_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a uniform-type Mountain constraint (a law of
% physics), the concepts of beneficiary and victim do not apply to the
% constraint's structure itself, only to the consequences of its application
% in specific technological domains.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   This is a uniform-type Mountain constraint. The classification is invariant
   across all perspectives because it is a fundamental law of physics. The
   narratives below describe the different *impacts* of this invariant
   Mountain on different agents.
   ========================================================================== */

% PERSPECTIVE 1: THE QUANTUM PHYSICIST (ANALYTICAL)
% To the physicist, the theorem is a Mountain. It is an unchangeable
% consequence of the mathematical structure of quantum mechanics. It is
% not a policy; it is a fixed peak in the topography of reality that no
% amount of engineering can level.
constraint_indexing:constraint_classification(no_cloning_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE QUANTUM CRYPTOGRAPHER (INSTITUTIONAL)
% For the security architect, the theorem's existence provides a foundation for
% secure communication protocols (e.g., QKD). While they leverage it as if it
% were a Rope for coordination, the underlying structure they are leveraging
% is an immutable Mountain of physical law.
constraint_indexing:constraint_classification(no_cloning_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE HARDWARE ENGINEER (POWERLESS)
% For the engineer debugging a quantum circuit, the theorem feels like a Snare.
% They cannot "snapshot" a state for later analysis. This inability to use
% classical redundancy techniques is a significant engineering hurdle. However,
% this perceived Snare is an encounter with an immovable Mountain, not a
% coercive, high-extraction system.
constraint_indexing:constraint_classification(no_cloning_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(no_cloning_theorem_tests).

test(classification_invariance) :-
    % Verify that as a law of physics, it is a Mountain from all perspectives.
    constraint_indexing:constraint_classification(no_cloning_theorem, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(no_cloning_theorem, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(no_cloning_theorem, mountain, context(agent_power(powerless), _, _, _)).

test(mountain_thresholds) :-
    % Verify the base metrics are within the required range for a Mountain.
    config:param(mountain_extractiveness_max, MaxE),
    config:param(mountain_suppression_ceiling, MaxS),
    narrative_ontology:constraint_metric(no_cloning_theorem, extractiveness, E),
    narrative_ontology:constraint_metric(no_cloning_theorem, suppression_requirement, S),
    E =< MaxE,
    S =< MaxS.

test(natural_law_profile_present) :-
    % Verify the required Natural Law profile metrics are declared.
    narrative_ontology:constraint_metric(no_cloning_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(no_cloning_theorem, resistance, R),
    AC > 0.85,
    R < 0.15.

:- end_tests(no_cloning_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The no-cloning theorem is a direct mathematical consequence of the linearity
 *   of quantum mechanics. As such, it is modeled as a uniform-type Mountain
 *   constraint. The base extractiveness (0.05) and suppression (0.02) are set
 *   to minimal values, reflecting that it is a non-coercive, structural feature
 *   of reality, not a system designed for extraction. The key insight is that
 *   while its *impacts* are felt differently by engineers (a cost) and
 *   cryptographers (a benefit), the underlying constraint itself is invariant.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the formal classification; all agents face
 *   a Mountain. The "gap" is in the narrative interpretation of that Mountain's
 *   impact. For a cryptographer, the Mountain is a useful barrier that protects
 *   their valley. For a hardware engineer, it is an obstacle they must build
 *   around.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, beneficiary/victim declarations are not
 *   applicable to its core structure. The directionality `d` is derived from
 *   the canonical power atom values, but because ε is so low, the resulting
 *   effective extraction χ is always near zero, ensuring a Mountain
 *   classification from all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   Modeling this as a Mountain prevents misinterpreting its negative
 *   engineering consequences as a Snare. A Snare is a contingent, coercive
 *   system with high extraction and suppression. The no-cloning theorem has
 *   neither; it is a fundamental limit. This distinction is critical for
 *   separating problems of physics from problems of policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
omega_variable(
    omega_no_cloning_theorem,
    'At what scale of system complexity does the no-cloning limit effectively vanish into classical copyability?',
    'Experimental mapping of the quantum-to-classical transition across different decoherence models.',
    'If the transition is sharp and at a low scale, quantum limits are a pervasive Mountain. If it is gradual and at a high scale, they are a local feature of microscopic systems.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_no_cloning_theorem, empirical, 'Determining the effective scale of the quantum-to-classical transition for information cloning.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(no_cloning_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a law of physics, the constraint's properties are time-invariant.
% These measurements reflect that stability.
%
% Theater ratio over time:
narrative_ontology:measurement(nct_tr_t0, no_cloning_theorem, theater_ratio, 0, 0.01).
narrative_ontology:measurement(nct_tr_t5, no_cloning_theorem, theater_ratio, 5, 0.01).
narrative_ontology:measurement(nct_tr_t10, no_cloning_theorem, theater_ratio, 10, 0.01).

% Extraction over time:
narrative_ontology:measurement(nct_ex_t0, no_cloning_theorem, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nct_ex_t5, no_cloning_theorem, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(nct_ex_t10, no_cloning_theorem, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% The no-cloning theorem is a consequence of the broader principles of quantum
% mechanics, such as the uncertainty principle which limits simultaneous
% knowledge of conjugate variables, a prerequisite for any cloning process.
narrative_ontology:affects_constraint(heisenberg_uncertainty_principle, no_cloning_theorem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The constraint is a uniform-type Mountain, and
% the canonical derivation of directionality is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */