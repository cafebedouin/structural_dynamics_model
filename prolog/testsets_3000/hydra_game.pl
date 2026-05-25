% ============================================================================
% CONSTRAINT STORY: hydra_game
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hydra_game, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hydra_game
 *   human_readable: The Hydra Game (Kirby-Paris Theorem)
 *   domain: mathematical_logic/proof_theory
 *
 * SUMMARY:
 *   The Hydra Game, formalized by Kirby and Paris in 1982, is a mathematical
 *   game on rooted trees in which a player removes a node and replaces it
 *   with a finite number of copies of its parent node, following a specific
 *   rule based on the depth structure. The Kirby-Paris Theorem proves that
 *   every finite strategy terminates — the Hydra eventually dies. However,
 *   this termination fact is logically true yet unprovable within Peano
 *   Arithmetic (PA). The constraint is that PA's axioms are fundamentally
 *   insufficient to formalize the termination property of all finite
 *   tree-reduction strategies. This is not a limitation of current methods or
 *   mathematical understanding — it is an immutable architectural property of
 *   first-order formal systems. The Hydra Game exemplifies a structural
 *   ceiling: the boundary between mathematical truth and formal provability
 *   is not a gap that stronger methods can eliminate, but a ceiling inherent
 *   to the relationship between finite axiom systems and certain classes of
 *   discrete processes.
 *
 * KEY AGENTS:
 *   - Peano Arithmetic: The constrained formal system (powerless/trapped) — cannot prove the Hydra termination property despite its truth
 *   - Second-Order Arithmetic: The superior formal system (institutional/arbitrage) — can prove the Hydra result by appeal to ordinals and transfinite induction
 *   - The Hydra Termination Property: The mathematical constraint (analytical/analytical) — a true statement about finite processes that exceeds PA's expressive power
 *   - Proof Theorists: Analytical observers (analytical/analytical) — recognize the constraint as a fundamental limit, not a temporary barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hydra_game, 0.12).
domain_priors:suppression_score(hydra_game, 0.02).
domain_priors:theater_ratio(hydra_game, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hydra_game, extractiveness, 0.12).
narrative_ontology:constraint_metric(hydra_game, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hydra_game, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hydra_game, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hydra_game, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hydra_game, mountain).
narrative_ontology:human_readable(hydra_game, "The Hydra Game (Kirby-Paris Theorem)").
narrative_ontology:topic_domain(hydra_game, "mathematical_logic/proof_theory").

domain_priors:emerges_naturally(hydra_game).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROOF-THEORETIC ANALYST (MOUNTAIN) — From the standpoint of formal arithmetic and recursion theory, the Hydra Game exhibits an invariant property: any finite strategy (tree reduction) must terminate, yet Peano Arithmetic cannot prove this fact. This is a fundamental limit on what PA can formalize about ordinal-indexed processes. No escape from this structural ceiling.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RECURSION THEORIST (MOUNTAIN) — The termination of all finite Hydra strategies is logically true but not Peano-provable. This reflects an immutable ceiling in formal systems: there exist true statements about finite discrete processes that no recursive axiom system can certify. The boundary between provable and true is invariant across all formal systems weaker than second-order arithmetic.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ARITHMETIC FORMALIST (MOUNTAIN) — A mathematician working entirely within Peano Arithmetic faces an irreducible barrier: the Hydra termination property is true in every model of PA, yet unprovable from PA's axioms. This is not a temporary limitation of current methods — it is a fundamental architectural feature of first-order formal systems. No procedure can overcome this ceiling.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: COMPUTER SCIENTIST (MOUNTAIN) — From the perspective of computable functions and termination proofs, the Hydra Game reveals that some terminating algorithms require non-elementary growth rates in their proof complexity. Any algorithm that provably terminates within a system weaker than second-order arithmetic will exhibit primitive-recursive or hyperexponential runtime on some inputs — a structural constraint on the relationship between proof strength and computational cost.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hydra_game_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hydra_game, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hydra_game, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hydra_game, ExtMetricName, E),
    domain_priors:suppression_score(hydra_game, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hydra_game),
    narrative_ontology:constraint_metric(hydra_game, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hydra_game, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hydra_game_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Extremely low. The Hydra Game imposes no extraction in the economic or political sense — it does not redistribute value, suppress alternatives, or create asymmetric access. It is a pure mathematical property. The value is non-zero only because the constraint does encode information about formal system hierarchies; understanding the constraint requires engagement with proof-theoretic methods that have some resource cost, but this is not extraction. Suppression (0.02): Negligible. There is no coercion, no suppression of alternatives, no penalty for non-compliance. The Hydra termination property simply is true, independent of any agent's belief, action, or formal system choice. The minimal value reflects the sheer logical necessity of the statement. Theater ratio (0.15): Very low. The Hydra Game is almost entirely functional — the proof of termination is the content, not a performance of something else. The slight theater comes from the historical contingency of which ordinal representation is chosen (ε₀ vs other ordinal notations), but once the ordinal system is fixed, the proof is transparent and non-performative.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the Hydra Game as a Mountain — there is no perspectival gap because the constraint is invariant across all observer positions. A formalist, a recursion theorist, a computer scientist, and a proof theorist all recognize the same immutable ceiling: Peano Arithmetic cannot prove the Hydra termination property, and this is not a limitation of human cleverness or available computational resources, but an invariant property of first-order formal systems. The constraint appears identical from every indexed position because it encodes a structural feature of formal systems themselves, not an institutional or economic asymmetry that different agents experience differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hydra Game does not admit beneficiary/victim declarations because it is not an extractive or coordinative constraint — it is a mathematical law. There is no agent that benefits from the constraint and no agent that bears costs. Peano Arithmetic is not a victim of its own limitation in the sense that an exploited laborer is a victim of labor extraction; rather, PA's axioms simply do not reach certain truths about ordinal-indexed processes. The constraint is invariant and universal: every formal system weaker than second-order arithmetic faces the same ceiling. Directionality is therefore not applicable — d is undefined for this constraint, and the engine applies the canonical fallback for analytical power (d ≈ 0.73) as a mere formalism. The true insight is that the Hydra Game has d-invariance: it would classify as Mountain from any observer position, with any exit options, at any time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hydra Game exemplifies the mandatrophy resolution for a constraint with zero coordination and zero asymmetric extraction: it is purely a ceiling, not an institutional arrangement. Mandatrophy asks whether we are mislabeling genuine coordination as extraction, or vice versa. For the Hydra, there is no coordination function to hide, and no extraction to mask as coordination — the constraint is a bare mathematical limit. The theorem proves that the Hydra terminates (true), PA cannot prove it (unprovable in PA), and stronger systems can (provable in ZFC, second-order arithmetic). This is not a mislabeled coordination or extraction problem; it is a classification of formal systems by proof-theoretic strength. The mandatrophy is resolved by recognizing that Mountains do not require mandatrophy analysis — they are invariant by definition, with no hidden benefits or masked asymmetries. The Hydra is a pure Mountain across all interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_indexing_necessity,
    'Is the appeal to ordinals (up to ε₀ and beyond) genuinely necessary for termination proof, or is it a methodological artifact of current proof techniques?',
    'Exhaustive search for alternative termination proof strategies within PA; investigation of whether weaker systems than second-order arithmetic could certify the result',
    'If truly necessary: the ordinal ceiling is invariant, confirming Mountain classification. If alternative PA-formalizable proofs exist: the constraint was a limitation of proof methodology, not mathematics itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_indexing_necessity, empirical, 'Whether ordinal-theoretic methods are fundamentally necessary for Hydra termination proof').

omega_variable(
    godel_incompleteness_boundary,
    'Does the Hydra Game represent a true instance of Gödel incompleteness in finitary mathematics, or is it a curious edge case that higher-order formal systems handle without conceptual strain?',
    'Comparative analysis of Hydra-like termination problems across formal systems (intuitionistic arithmetic, constructive set theory, Martin-Löf type theory); assessment of whether the phenomenon generalizes to all first-order systems',
    'If generalizable: the Mountain classification reflects an architectural limit in first-order logic applicable to broad classes of problems. If Hydra-specific: the constraint may be a special case rather than a fundamental ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(godel_incompleteness_boundary, conceptual, 'Whether Hydra incompleteness reflects fundamental Gödel limits or is a specialized phenomenon').

omega_variable(
    proof_strength_hierarchy_stability,
    'As formal systems strengthen (from PA to ATR₀ to ZFC to infinitary logics), does the Hydra termination proof remain invariant in its proof-theoretic strength, or do different system formulations reveal structural flexibility?',
    'Analysis of Hydra proofs across the Proof-Theoretic Ordinal Hierarchy; measurement of ordinal heights required in different formal frameworks',
    'If invariant: the constraint is truly immutable — the ordinal ceiling is a fundamental property of the problem. If flexible: the ceiling is relative to the formal framework, not absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_strength_hierarchy_stability, conceptual, 'Stability of the Hydra proof-theoretic strength across formal systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hydra_game, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hydra_tr_t0, hydra_game, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hydra_tr_t25, hydra_game, theater_ratio, 25, 0.15).
narrative_ontology:measurement(hydra_tr_t50, hydra_game, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(hydra_be_t0, hydra_game, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hydra_be_t25, hydra_game, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(hydra_be_t50, hydra_game, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hydra_game, information_standard).
narrative_ontology:affects_constraint(hydra_game, godel_incompleteness_first_order).
narrative_ontology:affects_constraint(hydra_game, ordinal_proof_hierarchy).

% DUAL FORMULATION NOTE:
% The Hydra Game is part of a constraint family in proof theory. The Gödel Incompleteness Theorem (ε=0.08, Mountain) establishes that formal systems have inherent limits. The Ordinal Proof Hierarchy (ε=0.06, Mountain) measures the strength of different systems. The Hydra Game (ε=0.12, Mountain) is a concrete instantiation of incompleteness in finitary mathematics. All three share the property that they represent ceilings, not extraction mechanisms. They are linked by proof-theoretic dependency: understanding the Hydra requires understanding ordinal analysis, which presupposes Gödel's results.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
