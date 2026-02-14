% ============================================================================
% CONSTRAINT STORY: ramsey_numbers
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_ramsey_numbers, []).

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
 *   constraint_id: ramsey_numbers
 *   human_readable: Inevitability of Order (Ramsey's Theorem)
 *   domain: mathematical
 *
 * SUMMARY:
 *   Ramsey's Theorem states that in any sufficiently large system where elements
 *   are partitioned into a finite number of classes, a large, orderly
 *   substructure must exist. This constraint represents the fundamental
 *   mathematical limit on creating truly disordered systems. The difficulty in
 *   computing Ramsey numbers (the threshold for "sufficiently large")
 *   highlights the computational hardness of predicting when this order will emerge.
 *
 * KEY AGENTS (by structural relationship):
 *   - Computational Theorists: (analytical/analytical) — Confront the extreme
 *     difficulty of computing Ramsey numbers, a hard limit on predictive power.
 *   - Students of Mathematics: (powerless/trapped) — Encounter the theorem as
 *     an immutable law of logic that must be accepted.
 *   - Research Institutions: (institutional/arbitrage) — Must allocate vast
 *     computational resources to make even marginal progress on finding bounds.
 *   - Analytical Observer: (analytical/analytical) — Sees the complete structure
 *     as a fundamental law of combinatorics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ramsey_numbers, 0.15).
domain_priors:suppression_score(ramsey_numbers, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ramsey_numbers, 0.00).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ramsey_numbers, extractiveness, 0.15).
narrative_ontology:constraint_metric(ramsey_numbers, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(ramsey_numbers, theater_ratio, 0.00).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ramsey_numbers, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ramsey_numbers, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(ramsey_numbers).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a fundamental mathematical theorem (Mountain), this
% constraint does not have structural beneficiaries or victims in the sense of
% asymmetric social or economic arrangements. It is a feature of logic itself.

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

% This is a uniform-type constraint (Mountain-only), demonstrating that the
% classification is invariant across all perspectives.

% PERSPECTIVE 1: THE STUDENT (POWERLESS)
% A student encountering the theorem for the first time is powerless to change
% it and trapped by its logical necessity. It appears as an unchangeable law.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE RESEARCH INSTITUTION (INSTITUTIONAL)
% An institution funding research into Ramsey numbers treats it as a fixed
% feature of the mathematical landscape, allocating resources to explore its
% boundaries.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context sees the theorem as a fundamental,
% unchangeable property of combinatorics.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ramsey_numbers_tests).

test(invariance_across_indices) :-
    % Verify that all perspectives classify as the same type (Mountain).
    constraint_indexing:constraint_classification(ramsey_numbers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ramsey_numbers, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ramsey_numbers, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(threshold_validation_for_mountain) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ramsey_numbers, ExtMetricName, E),
    E =< 0.25.

test(nl_profile_check) :-
    narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ramsey_numbers, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ramsey_numbers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Ramsey's Theorem is a fundamental truth of mathematics, making it a canonical
 *   example of a Mountain constraint.
 *   - Base Extractiveness (0.15): Low. The "extraction" is the cognitive and
 *     computational effort required to work within the limits it imposes. It
 *     doesn't prevent research but defines a hard boundary of inquiry.
 *   - Suppression (0.01): Near zero. The theorem doesn't suppress alternatives;
 *     it proves they are logically impossible.
 *   - Theater (0.00): Zero. There is no performative aspect.
 *   - NL Profile: Accessibility collapse is high (0.95) because there is no
 *     way to construct a system that violates the theorem. Resistance is low
 *     (0.05) because resisting a mathematical proof is incoherent.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a natural law of mathematics, its
 *   classification is invariant. A student, a research institution, and a
 *   professional mathematician all perceive it as an unchangeable feature of
 *   reality, hence the uniform 'Mountain' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations are omitted because they are not
 *   structurally applicable to a mathematical law. The theorem does not create
 *   asymmetric benefits or costs between social groups; it is a universal,
 *   symmetric constraint on logical possibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification correctly identifies this as a non-negotiable
 *   feature of the problem space, not a human-imposed rule. This prevents
 *   mislabeling the inherent difficulty of a problem (a Mountain) as a form of
 *   coercion (a Snare) or a coordination problem (a Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ramsey_bounds,
    'What are the exact values of Ramsey numbers, particularly R(5,5)?',
    'New mathematical proofs or massive computational searches, though the latter may be intractable.',
    'Finding the exact values would not change the Mountain classification but would refine our understanding of its specific parameters. The intractability itself reinforces the Mountain nature.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ramsey_numbers, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a mathematical constant, its properties do not drift over time.
% Extraction is low, so this section is for completeness.
narrative_ontology:measurement(ramsey_numbers_tr_t0, ramsey_numbers, theater_ratio, 0, 0.0).
narrative_ontology:measurement(ramsey_numbers_tr_t5, ramsey_numbers, theater_ratio, 5, 0.0).
narrative_ontology:measurement(ramsey_numbers_tr_t10, ramsey_numbers, theater_ratio, 10, 0.0).

narrative_ontology:measurement(ramsey_numbers_ex_t0, ramsey_numbers, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ramsey_numbers_ex_t5, ramsey_numbers, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(ramsey_numbers_ex_t10, ramsey_numbers, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this type of fundamental constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. The constraint is a Mountain and has no directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */