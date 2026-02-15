% ============================================================================
% CONSTRAINT STORY: church_turing_thesis
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_church_turing_thesis, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis
 *   human_readable: Church-Turing Thesis (Computability Boundary)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Church-Turing Thesis asserts that any function that can be computed by an
 *   algorithm can be computed by a Turing machine. It establishes the absolute
 *   horizon of what is "effectively calculable," separating the solvable from
 *   the logically impossible. This constraint is a foundational limit of formal
 *   systems, analogous to a law of physics for computation.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Algorithm (Subject): A powerless sequence of instructions bound by the
 *     mechanics of the transition function. Experiences the limit as an absolute,
 *     unchangeable feature of its reality.
 *   - The Hardware Architect (Institutional): The designer of silicon or quantum
 *     substrates who uses the thesis as a design principle.
 *   - The Hypercomputationalist (Analytical): An observer seeking to
 *     transcend these bounds, for whom the thesis is the primary object of study.
 *   - The Analytical Observer: Sees the full structure as a logical limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a mathematical truth, it does not "take" in a social sense.
% The minimal extraction reflects the foreclosure of impossible alternatives
% (like solving the Halting Problem), which is a defining feature of a Mountain.
domain_priors:base_extractiveness(church_turing_thesis, 0.02).
% Rationale: It suppresses "non-effective" computational models from being
% considered valid, but this is a definitional boundary, not active coercion.
domain_priors:suppression_score(church_turing_thesis, 0.01).
% Rationale: The constraint is purely functional; there is no performative aspect.
domain_priors:theater_ratio(church_turing_thesis, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis, extractiveness, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(church_turing_thesis, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% Accessibility Collapse: Alternatives (hypercomputation) are purely theoretical
% and structurally inaccessible within standard physics and logic.
narrative_ontology:constraint_metric(church_turing_thesis, accessibility_collapse, 0.98).
% Resistance: Meaningful resistance is incoherent. One can research alternatives,
% but one cannot "oppose" a logical thesis.
narrative_ontology:constraint_metric(church_turing_thesis, resistance, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(church_turing_thesis, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the axioms of logic and set theory.
domain_priors:emerges_naturally(church_turing_thesis).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), the Church-Turing Thesis
% does not have beneficiaries or victims in the structural sense required for
% directionality derivation. Its effects are universal and symmetric.

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

% UNIFORM-TYPE CONSTRAINT: This is a natural law, classifying as Mountain
% from all perspectives. The perspectival minimum is relaxed.

% PERSPECTIVE 1: THE EXECUTING ALGORITHM
% For the algorithm in motion, the boundary is an absolute, unchangeable
% feature of its universe. It cannot "decide" to compute the uncomputable.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE COMPILER ARCHITECT
% For the architect, the thesis is a fixed design parameter. It's not a
% coordination tool (Rope) but a fundamental law (Mountain) that enables
% coordination by providing a stable, universal target for completeness.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view confirms the thesis as a foundational limit of formal
% systems, the very definition of a Mountain.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis_tests).

test(classification_invariance, [nondet]) :-
    % Verify that as a natural law, it is a Mountain from all key perspectives.
    constraint_indexing:constraint_classification(church_turing_thesis, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(church_turing_thesis, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(church_turing_thesis, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_threshold_adherence) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(church_turing_thesis, ExtMetricName, E),
    narrative_ontology:constraint_metric(church_turing_thesis, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_emergence_and_nl_profile) :-
    domain_priors:emerges_naturally(church_turing_thesis),
    narrative_ontology:constraint_metric(church_turing_thesis, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(church_turing_thesis, resistance, R), R =< 0.15.

:- end_tests(church_turing_thesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Church-Turing Thesis is a formal statement about the limits of
 *   computation, analogous to a law of physics. Its base extractiveness (0.02)
 *   and suppression (0.01) are extremely low, reflecting that it doesn't
 *   extract resources but rather defines the boundaries of possibility. These
 *   metrics, combined with its natural emergence from logic, firmly place it
 *   in the Mountain category. The required Natural Law profile metrics
 *   (accessibility_collapse=0.98, resistance=0.02) are included to pass the
 *   NL certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a fundamental limit of logic, its
 *   classification is invariant across all observers. While a security analyst
 *   might feel frustrated by its consequences (e.g., the impossibility of a
 *   perfect virus scanner, per Rice's Theorem), this frustration does not
 *   change the structural nature of the constraint. The impossibility of an
 *   alternative is the defining feature of a Mountain, not a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the constraint does not require beneficiary/victim
 *   declarations. Its effects are symmetric and universal for all agents
 *   operating within formal computational systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain is robust. The extremely low extraction
 *   and suppression scores prevent any misclassification as a Snare or
 *   Tangled Rope. The thesis is a pure limit, not a mechanism for social
 *   extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_church_turing_thesis,
    'Does the physical universe support computations beyond the Turing limit (hypercomputation)?',
    'Empirical tests of non-recursive physical processes, e.g., in quantum gravity or Malament-Hogarth spacetimes.',
    'If Yes: The "Mountain" is a "Scaffold" that falls to a higher Rope. If No: It remains a permanent Mountain.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_church_turing_thesis, empirical, 'Whether physical reality permits hypercomputation, violating the thesis.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(church_turing_thesis, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so no temporal measurements are required for
% lifecycle drift detection. The constraint is static and unchanging.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% The Church-Turing thesis is a direct logical antecedent to the Halting Problem.
narrative_ontology:affects_constraint(church_turing_thesis, halting_problem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, directionality is not a factor.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */