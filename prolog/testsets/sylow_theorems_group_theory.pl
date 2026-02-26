% ============================================================================
% CONSTRAINT STORY: sylow_theorems_group_theory
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sylow_theorems_group_theory, []).

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
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sylow_theorems_group_theory
 *   human_readable: Sylow Theorems (Finite Group Structure)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Sylow Theorems are a fundamental result in finite group theory that
 *   guarantee the existence and properties of subgroups of prime power order.
 *   They place rigid, unchangeable limits on the internal structure of any
 *   finite group based purely on the prime factorization of its order. This
 *   constraint is a canonical example of a mathematical truth acting as a
 *   Mountain: an immutable feature of the logical landscape.
 *
 * KEY AGENTS (by structural relationship):
 *   - Group Element (powerless/trapped): An element within a finite group whose
 *     possible behaviors (e.g., the order of the subgroup it generates) are
 *     absolutely determined by the theorems.
 *   - Group Theorist (institutional/arbitrage): A mathematician who uses the
 *     theorems as a foundational tool for classifying groups, proving their
 *     properties, or disproving the existence of certain structures (like
 *     simple groups of a given order).
 *   - Analytical Observer (analytical/analytical): Sees the theorems as a pure,
 *     non-extractive logical consequence of group axioms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The theorems "extract" possibilities (e.g., a group of order 15
% cannot be simple), but this is a consequence of logic, not coercion. ε is low.
domain_priors:base_extractiveness(sylow_theorems_group_theory, 0.05).
% Rationale: Suppression is minimal. The theorems don't suppress alternatives;
% they prove alternatives are logically impossible within the axiomatic system.
domain_priors:suppression_score(sylow_theorems_group_theory, 0.01).
% Rationale: A pure mathematical proof has zero theatrical component.
domain_priors:theater_ratio(sylow_theorems_group_theory, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sylow_theorems_group_theory, extractiveness, 0.05).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Rationale: Alternatives to the Sylow theorems are inconceivable within
% standard group theory. The collapse of alternatives is total.
narrative_ontology:constraint_metric(sylow_theorems_group_theory, accessibility_collapse, 1.0).
% Rationale: There is no meaningful resistance to a mathematical proof.
narrative_ontology:constraint_metric(sylow_theorems_group_theory, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sylow_theorems_group_theory, mountain).
narrative_ontology:human_readable(sylow_theorems_group_theory, "Sylow Theorems (Finite Group Structure)").
narrative_ontology:topic_domain(sylow_theorems_group_theory, "mathematical").

% --- Emergence flag (required for mountain constraints) ---
% The theorems are a logical consequence of the axioms of group theory.
domain_priors:emerges_naturally(sylow_theorems_group_theory).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (mathematical law), the Sylow theorems
% do not have beneficiaries or victims in the structural sense required for
% directionality derivation. The effects are symmetric and logical.

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
% As a mathematical theorem, this constraint is a Mountain from all
% perspectives. Its truth is invariant to the observer's power, time horizon,
% exit options, or scope. The following perspectives demonstrate this invariance.

% PERSPECTIVE 1: THE GROUP ELEMENT
% An element within a group is trapped by its structure, which is dictated
% by the theorems. This is an unchangeable law of its existence.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE GROUP THEORIST
% A mathematician using the theorems as a tool still perceives them as an
% immutable law of the universe they are studying.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which serves as the basis for the
% constraint_claim. It sees the theorems as a fundamental, non-extractive
% feature of logical reality.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sylow_theorems_group_theory_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from different perspectives,
    % demonstrating its uniform-type nature as a mathematical law.
    constraint_indexing:constraint_classification(sylow_theorems_group_theory, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sylow_theorems_group_theory, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain.

test(natural_law_profile_adherence) :-
    % Verify the constraint meets the metric thresholds for a Mountain.
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, extractiveness, E),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(emergence) :-
    % Verify the constraint is correctly flagged as emerging naturally.
    domain_priors:emerges_naturally(sylow_theorems_group_theory).

:- end_tests(sylow_theorems_group_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint was re-classified from a mixed-type story to a uniform-type
 *   Mountain. The original file's classifications (Tangled Rope, Snare) were
 *   narratively evocative but structurally inconsistent with the low base
 *   metrics (ε=0.15, S=0.2) and the nature of a mathematical proof.
 *
 *   The metrics have been adjusted to be compliant with Mountain thresholds:
 *   - Base Extractiveness (ε): Lowered to 0.05. The "extraction" of
 *     possibilities is a logical, not coercive, act.
 *   - Suppression (S): Lowered to 0.01. The theorems don't suppress
 *     alternatives; they demonstrate their non-existence.
 *   - NL Profile: Added `accessibility_collapse` (1.0) and `resistance` (0.0)
 *     to pass the natural_law_signature certification. These values reflect the
 *     absolute nature of a mathematical truth.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a fundamental mathematical law, the Sylow
 *   theorems are a Mountain from all possible viewpoints. The classification is
 *   invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable here. `constraint_beneficiary` and
 *   `constraint_victim` declarations were removed as they are inappropriate for
 *   a pure Mountain. A mathematical theorem does not have a structural bias;
 *   its consequences are symmetric for all who operate under its axioms.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification correctly identifies this as a feature of the
 *   logical landscape, not a system of coordination or extraction. The original
 *   file's attempt to frame it as a Snare for "simple group searchers" was a
 *   category error, confusing a logical impossibility with coercive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_sylow_constructivity,
    'Does the existence proof for Sylow subgroups (Mountain) imply an efficient algorithm to find them (Rope)?',
    'Verification of the computational complexity of known algorithms (e.g., Cannon-Holt) for finding Sylow subgroups across various group classes.',
    'If complexity is exponential, the Mountain is a practical Snare for computation. If polynomial, it is a functional Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The question is about computational complexity, which is resolvable through
% mathematical proof and empirical testing.
narrative_ontology:omega_variable(omega_sylow_constructivity, empirical, 'Is the constructive proof of Sylow subgroups computationally efficient?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sylow_theorems_group_theory, 1872, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory lifecycle drift tracking. Mathematical truths do not drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */