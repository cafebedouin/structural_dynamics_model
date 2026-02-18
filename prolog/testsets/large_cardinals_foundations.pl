% ============================================================================
% CONSTRAINT STORY: large_cardinal_foundations
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_large_cardinal_foundations, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: large_cardinal_foundations
 *   human_readable: Large Cardinal Axioms as a Foundational System
 *   domain: mathematical/philosophical
 *
 * SUMMARY:
 *   Large Cardinals are transfinite numbers with properties so strong that their
 *   existence cannot be proven within standard Zermelo-Fraenkel set theory (ZFC).
 *   They establish a "hierarchy of consistency strength" that acts as a
 *   structural floor for modern mathematics. This constraint is the de facto
 *   requirement to adopt these axioms to resolve independence results and
 *   establish the consistency of lower-level theories.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematical Minimalists: Primary target (moderate/constrained) — bear the "ontological tax" of accepting increasingly complex infinities to ground simpler mathematics.
 *   - Set-Theoretic Platonists: Primary beneficiary (institutional/arbitrage) — use the hierarchy as a coordination tool to map the landscape of mathematical truth and consistency.
 *   - The Inaccessible Cardinal (Metaphorical): A powerless/trapped agent representing the mathematical object itself, for whom its properties are an unchangeable law.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope, with both coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: 0.30. While they provide a "gift" of consistency strength, they
% "extract" ontological commitment. To solve "small" problems, one is taxed
% with accepting "large" existences.
domain_priors:base_extractiveness(large_cardinal_foundations, 0.30).

% Rationale: 0.40. The hierarchy suppresses the viability of "finitist-only"
% foundations by demonstrating their insufficiency for proving the consistency
% of higher-order structures.
domain_priors:suppression_score(large_cardinal_foundations, 0.40).

% Rationale: 0.01. The system is almost pure function; there is no performative
% aspect. Its value is entirely in its logical consequences.
domain_priors:theater_ratio(large_cardinal_foundations, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_cardinal_foundations, extractiveness, 0.30).
narrative_ontology:constraint_metric(large_cardinal_foundations, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(large_cardinal_foundations, theater_ratio, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(large_cardinal_foundations, tangled_rope).
narrative_ontology:human_readable(large_cardinal_foundations, "Large Cardinal Axioms as a Foundational System").
narrative_ontology:topic_domain(large_cardinal_foundations, "mathematical/philosophical").

% --- Binary flags ---
% Required for Tangled Rope. "Enforcement" in this domain is the intellectual
% work of demonstrating that these axioms are necessary to prove certain
% results, thus coercing their adoption for researchers in those areas.
domain_priors:requires_active_enforcement(large_cardinal_foundations).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(large_cardinal_foundations, set_theoretic_platonists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(large_cardinal_foundations, mathematical_minimalists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE MATHEMATICAL OBJECT (MOUNTAIN)
% A metaphorical perspective from the "point of view" of the cardinal itself.
% For the object, its properties are an unchangeable law of its existence.
constraint_indexing:constraint_classification(large_cardinal_foundations, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE SET-THEORETIC PLATONIST (ROPE)
% The beneficiary, who uses the hierarchy as a tool for coordination. They can
% choose which axioms to work with (arbitrage) to achieve desired consistency proofs.
constraint_indexing:constraint_classification(large_cardinal_foundations, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MATHEMATICAL MINIMALIST (SNARE)
% The victim, who seeks simple foundations. The hierarchy is a Snare that
% extracts ontological simplicity and suppresses finitist alternatives.
constraint_indexing:constraint_classification(large_cardinal_foundations, snare,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context, which sees both the coordination function
% (beneficiary) and the asymmetric extraction (victim), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(large_cardinal_foundations, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_cardinal_foundations_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the gap between the beneficiary (Rope) and victim (Snare).
    constraint_indexing:constraint_classification(large_cardinal_foundations, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(large_cardinal_foundations, snare, context(agent_power(moderate), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical claim must be Tangled Rope.
    constraint_indexing:constraint_classification(large_cardinal_foundations, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(large_cardinal_foundations, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(large_cardinal_foundations, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(large_cardinal_foundations).

:- end_tests(large_cardinal_foundations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this was a Mountain, but its metrics
 *   (ε=0.30, suppression=0.40) violated Mountain thresholds (ε≤0.25, supp≤0.05).
 *   The narrative clearly describes a system with both a powerful coordination
 *   function and a significant asymmetric extraction cost, which is the
 *   definitive signature of a Tangled Rope. The metrics align perfectly with
 *   Tangled Rope requirements (ε≥0.30, supp≥0.40). The claim and analytical
 *   perspective have been updated accordingly.
 *
 * PERSPECTIVAL GAP:
 *   - Set-Theoretic Platonists (Beneficiaries) see a pure Rope. For them, the
 *     hierarchy is an indispensable tool for coordinating research and establishing
 *     the relative consistency of mathematical theories. The ontological cost is
 *     not perceived as extraction but as the price of truth.
 *   - Mathematical Minimalists (Victims) see a Snare. They are forced to pay an
 *     "ontological tax" (accepting vast, unprovable infinities) to ground their
 *     work, which extracts the value of foundational simplicity and suppresses
 *     finitist approaches.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `set_theoretic_platonists`. They gain a powerful standard
 *     (consistency strength) to structure their entire field.
 *   - Victims: `mathematical_minimalists`. They lose the possibility of a simple,
 *     self-contained foundation for mathematics, a core value for their paradigm.
 *   The engine derives a low `d` for the institutional platonists (low χ, Rope)
 *   and a high `d` for the moderate minimalists (high χ, Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly identifies that the system is
 *   not a neutral "Mountain" of fact. It has a political dimension: it coordinates
 *   one group (platonists) while extracting from another (minimalists). A pure
 *   Mountain or Rope classification would miss this coercive aspect, while a pure
 *   Snare classification would ignore its genuine and powerful coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_large_cardinal_foundations,
    'Will a hidden contradiction be found in the large cardinal hierarchy, proving it inconsistent with ZFC?',
    'A formal proof of inconsistency originating from the ZFC + Large Cardinal axioms.',
    'If inconsistent, the constraint is revealed as a failed Scaffold. If consistency holds, it remains a stable Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(large_cardinal_foundations, 1908, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is not > 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The hierarchy provides a standard for measuring and comparing the consistency
% strength of different formal systems.
narrative_ontology:coordination_type(large_cardinal_foundations, information_standard).

% Network relationships (structural influence edges)
% This is a foundational constraint that affects many others in mathematics.
% narrative_ontology:affects_constraint(large_cardinal_foundations, continuum_hypothesis_undecidability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */