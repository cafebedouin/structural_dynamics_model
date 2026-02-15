% ============================================================================
% CONSTRAINT STORY: banach_tarski_paradox
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_banach_tarski_paradox, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: banach_tarski_paradox
 *   human_readable: Banach-Tarski Paradox
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Banach-Tarski Paradox is a theorem in set theory stating that a solid
 *   ball in 3D Euclidean space can be decomposed into a finite number of
 *   non-measurable point sets, which can then be reassembled into two identical
 *   copies of the original ball. This is a direct and unchangeable consequence
 *   of the Axiom of Choice (AC) within ZFC set theory. It represents a
 *   fundamental, fixed constraint on the nature of volume and measure for
 *   arbitrary sets.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Mathematical Point: A powerless subject whose membership in a
 *     non-measurable set is dictated by the theorem.
 *   - The Set Theorist: An institutional agent who accepts the ZFC axioms and
 *     must operate within their logical consequences.
 *   - The Physical Intuitionist: An analytical observer who finds the theorem
 *     counter-intuitive but cannot logically refute it within ZFC.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(banach_tarski_paradox, 0.05).
domain_priors:suppression_score(banach_tarski_paradox, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(banach_tarski_paradox, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(banach_tarski_paradox, extractiveness, 0.05).
narrative_ontology:constraint_metric(banach_tarski_paradox, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(banach_tarski_paradox, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(banach_tarski_paradox, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(banach_tarski_paradox, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(banach_tarski_paradox, mountain).

% --- Binary flags ---
% No active enforcement or sunset clause.

% --- Emergence flag (required for mountain constraints) ---
% The paradox is a logical consequence of the ZFC axioms, not a human construction.
domain_priors:emerges_naturally(banach_tarski_paradox).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not applicable. As a mountain (a logical theorem), the concepts of
% beneficiary and victim do not apply in a structural sense. The constraint
% is an invariant feature of the axiomatic system. No enrichment needed.

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

% PERSPECTIVE 1: THE MATHEMATICAL POINT
% For an individual point, its assignment to a non-measurable set is an
% unchangeable fact of the decomposition. It has no agency.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SET THEORIST
% For a mathematician working within ZFC, the paradox is a fixed feature of
% the landscape. They can choose other axioms (exit), but within ZFC, the
% theorem is an immutable law.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can choose to work in ZF without Choice
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PHYSICIST)
% For an observer trying to map mathematics to physical reality, the paradox
% is a counter-intuitive but logically necessary result of the chosen axioms.
% It is a mountain of logic that cannot be moved, only interpreted.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(banach_tarski_paradox_tests).

test(classification_invariance) :-
    % Verify that the classification is 'mountain' from all key perspectives,
    % demonstrating its status as a natural law of the axiomatic system.
    constraint_indexing:constraint_classification(banach_tarski_paradox, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(banach_tarski_paradox, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(banach_tarski_paradox, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(natural_law_profile_present) :-
    % Verify that the required metrics for NL certification are present.
    narrative_ontology:constraint_metric(banach_tarski_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(banach_tarski_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15,
    domain_priors:emerges_naturally(banach_tarski_paradox).

:- end_tests(banach_tarski_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Banach-Tarski paradox is classified as a Mountain because it is a
 *   provable theorem—a direct, unchangeable logical consequence of the ZFC
 *   axioms. It is not enforced by any agent and cannot be altered without
 *   changing the fundamental axioms of mathematics.
 *   - Base Extractiveness (0.05): The "extraction" is purely conceptual—it
 *     extracts the comfort of physical intuition from mathematics. There is
 *     no transfer of resources or value.
 *   - Suppression (0.05): It logically invalidates a "naive measure theory"
 *     where all sets are measurable, but this is a logical consequence, not
 *     active suppression of an alternative.
 *   - NL Profile: Accessibility Collapse is 1.0 because within ZFC, there is
 *     no alternative to the theorem's truth. Resistance is 0.0 because one
 *     cannot meaningfully "resist" a mathematical proof.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification; all agents perceive a
 *   Mountain. The gap is in *interpretation*. For a set theorist, it's a
 *   foundational feature of measure theory. For a physicist, it's a "paradox"
 *   that highlights the abstraction of mathematics from physical reality. The
 *   constraint itself, however, is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, directionality is not applicable. The theorem does not have
 *   beneficiaries or victims in a structural sense; it is a feature of the
 *   logical environment that all agents must contend with.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification correctly identifies the paradox as a feature
 *   of the logical landscape, not a system of coordination or extraction.
 *   Misclassifying it as a Tangled Rope or Snare would imply it is a
 *   human-designed system with winners and losers, which is structurally false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_banach_tarski_paradox,
    'Does the Axiom of Choice, which enables this paradox, describe a feature of physical reality or is it purely a formal convenience?',
    'Empirical evidence from quantum foundations or cosmology for the existence of physically real, non-measurable states.',
    'If AC is physically real, the paradox is a Mountain of physics. If it is a formal tool, the paradox is a Mountain of logic, irrelevant to the physical world.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_banach_tarski_paradox, conceptual, 'Is the Axiom of Choice a physical principle or a formal axiom?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(banach_tarski_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a mathematical theorem, its properties do not drift over time.
% Base extractiveness is low (< 0.46), so no measurements are required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */