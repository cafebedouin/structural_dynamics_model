% ============================================================================
% CONSTRAINT STORY: finite_simple_group_classification
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_finite_simple_group_classification, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: finite_simple_group_classification
 *   human_readable: The Classification of Finite Simple Groups (CFSG)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The CFSG is a landmark theorem stating that every finite simple group belongs
 *   to one of 18 infinite families, is one of the 26 sporadic groups, or is an
 *   alternating group. Often called "The Enormous Theorem," its proof spans
 *   tens of thousands of pages across hundreds of journal articles, representing
 *   one of the largest collaborative efforts in mathematics. This constraint
 *   represents the fixed, logical structure of this mathematical reality.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Individual Researcher (powerless/trapped): An agent who must accept the
 *     validity of the proof without the lifespan necessary to personally verify it.
 *   - The Mathematical Community (institutional/arbitrage): The collective that
 *     maintains, simplifies, and applies the theorem.
 *   - The Finite Simple Group (analytical/trapped): The mathematical object whose
 *     identity is strictly confined by the theorem.
 *   - Analytical Observer: Sees the full logical structure of the theorem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a mathematical theorem, the CFSG is a statement of logical fact.
% It has zero structural extractiveness. The immense cognitive cost of verifying
% the proof is a barrier to entry, not an extraction inherent in the theorem.
% It does not suppress alternatives; it proves they do not exist.
domain_priors:base_extractiveness(finite_simple_group_classification, 0.05).
domain_priors:suppression_score(finite_simple_group_classification, 0.01).
domain_priors:theater_ratio(finite_simple_group_classification, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finite_simple_group_classification, extractiveness, 0.05).
narrative_ontology:constraint_metric(finite_simple_group_classification, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(finite_simple_group_classification, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. A mathematical theorem has perfect collapse
% (no alternatives are logically possible) and zero resistance.
narrative_ontology:constraint_metric(finite_simple_group_classification, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(finite_simple_group_classification, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(finite_simple_group_classification, mountain).
narrative_ontology:human_readable(finite_simple_group_classification, "The Classification of Finite Simple Groups (CFSG)").
narrative_ontology:topic_domain(finite_simple_group_classification, "mathematical").

% --- Binary flags ---
% A mathematical theorem does not require active enforcement.

% --- Emergence flag (required for mountain constraints) ---
% A mathematical truth emerges from the structure of logic itself.
domain_priors:emerges_naturally(finite_simple_group_classification).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a pure Mountain (natural law), there are no structural beneficiaries or
% victims. The theorem is a universal, symmetric fact. The directionality
% derivation chain is not needed. No enrichment needed.

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

% PERSPECTIVE 1: THE INDIVIDUAL RESEARCHER (MOUNTAIN)
% A lone mathematician cannot choose to work in a universe where the CFSG is
% false. They are trapped by its logical necessity. While the proof's complexity
% creates a high barrier to personal verification, this is a cognitive cost,
% not a structural extraction. The theorem itself is a fixed landmark.
constraint_indexing:constraint_classification(finite_simple_group_classification, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MATHEMATICAL COMMUNITY (MOUNTAIN)
% For the institutional community, the theorem is not a coordination tool (Rope)
% but a fundamental feature of the landscape (Mountain) around which coordination
% occurs. It is an unchangeable fact that enables further work.
constraint_indexing:constraint_classification(finite_simple_group_classification, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage), % Can simplify proofs, find new applications
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical view confirms the theorem's status as a natural law of
% mathematics. Its classification is invariant across all perspectives.
constraint_indexing:constraint_classification(finite_simple_group_classification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_simple_group_classification_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(finite_simple_group_classification, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_simple_group_classification, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(finite_simple_group_classification, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(mountain_metric_thresholds) :-
    % Verify that the base metrics are within the allowed range for a Mountain.
    narrative_ontology:constraint_metric(finite_simple_group_classification, extractiveness, E),
    narrative_ontology:constraint_metric(finite_simple_group_classification, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(claim_matches_analytical_view) :-
    narrative_ontology:constraint_claim(finite_simple_group_classification, ClaimType),
    constraint_indexing:constraint_classification(finite_simple_group_classification, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimType == AnalyticalType.

:- end_tests(finite_simple_group_classification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly modeled the cognitive cost of verifying the
 *   CFSG's enormous proof as structural extraction (ε=0.35) and suppression
 *   (s=0.2). This violates the ε-invariance principle and the definition of a
 *   Mountain. A mathematical theorem is a statement of fact about a logical
 *   system; it has ε≈0 and s≈0. The difficulty of proving it does not change
 *   its structural properties. This regenerated file corrects the metrics to
 *   ε=0.05 and s=0.01, consistent with a Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is no structural perspectival gap. The CFSG is a Mountain from all
 *   perspectives. The narrative illusion of a gap (e.g., an individual feeling
 *   it is a "Snare" of trust) arises from the immense verification cost. This
 *   cost is a property of the proof's complexity, not the theorem's structure.
 *   The framework correctly identifies the theorem itself as an invariant Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   As a pure Mountain, the constraint is symmetric and applies universally.
 *   There are no structural beneficiaries or victims, so beneficiary/victim
 *   declarations are omitted as they are not applicable.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the CFSG as a pure Mountain, we avoid the error of conflating
 *   high verification costs with genuine extraction. This prevents the
 *   mislabeling of a fundamental truth as a coercive instrument (Snare) or a
 *   mere coordination tool (Rope). The theorem is a fixed landmark, not a
 *   mechanism designed by agents for coordination or extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cfsg_proof_completeness,
    "Is the 'Second Generation' proof, intended to simplify and verify the original, actually complete and error-free?",
    "Formal verification of the Gorenstein-Lyons-Solomon volumes using an automated proof assistant (e.g., Lean, Coq).",
    "If gaps remain: The Mountain's existence is not fully proven, making it a Scaffold of belief. If complete: It is confirmed as a true Mountain.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(finite_simple_group_classification, 1955, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory temporal tracking.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships or specific coordination type declared for this
% fundamental mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not required. As a pure Mountain, the constraint is symmetric and does not
% require directionality overrides.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */