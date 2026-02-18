% ============================================================================
% CONSTRAINT STORY: inner_model_theory_constraints
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_inner_model_theory_constraints, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: inner_model_theory_constraints
 *   human_readable: The Axiom of Constructibility (V=L)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Inner Model Theory studies sub-universes of the set-theoretic universe (V)
 *   that satisfy the axioms of ZFC. The archetypal inner model is Gödel's
 *   Constructible Universe (L), defined by the axiom V=L. This axiom imposes
 *   a rigid, determinate structure on the transfinite, resolving questions
 *   like the Continuum Hypothesis. However, this determinacy comes at the cost
 *   of excluding more complex structures like large cardinals, creating a
 *   significant perspectival gap between different schools of set theorists.
 *
 * KEY AGENTS (by structural relationship):
 *   - Large Cardinal Pluralists (Victim): Set theorists who view V=L as a snare that extracts the potential richness of the set-theoretic universe (V) to enforce a narrow, predictable structure. (moderate/constrained)
 *   - Consistency Proof Developers (Beneficiary): Logicians and model theorists who use inner models like L as a coordination tool (a rope) to establish the relative consistency of various mathematical axioms. (institutional/mobile)
 *   - The Constructible Set (Subject): A mathematical object whose existence is rigidly defined by the L-hierarchy, for which the constraint is an unchangeable law of its nature. (powerless/trapped)
 *   - Analytical Observer: Sees the full structure as a Tangled Rope, acknowledging both its genuine coordination function for consistency proofs and its extractive nature in suppressing alternative set-theoretic ontologies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: 0.30. Inner models "extract" the combinatorial richness of the
% full universe (V) to provide a "clean" sub-model. This imposes a
% "complexity tax" by forcing researchers to differentiate between what
% is true in "L" vs what is true in "V."
domain_priors:base_extractiveness(inner_model_theory_constraints, 0.30).

% Rationale: 0.50. Historically, the axiom V=L suppressed the visibility of
% non-constructible sets (like those required for large cardinals), rendering
% them "pathological" or "non-existent" until the development of forcing.
domain_priors:suppression_score(inner_model_theory_constraints, 0.50).

% Rationale: 0.0. The constraint is a formal mathematical axiom; its function
% is entirely substantive with no performative or theatrical component.
domain_priors:theater_ratio(inner_model_theory_constraints, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inner_model_theory_constraints, extractiveness, 0.30).
narrative_ontology:constraint_metric(inner_model_theory_constraints, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(inner_model_theory_constraints, theater_ratio, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(inner_model_theory_constraints, tangled_rope).
narrative_ontology:human_readable(inner_model_theory_constraints, "The Axiom of Constructibility (V=L)").
narrative_ontology:topic_domain(inner_model_theory_constraints, "mathematical/logical").

% --- Binary flags ---
% Enforcement is definitional; accepting V=L requires enforcing its consequences.
% This is required for the Tangled Rope classification.
domain_priors:requires_active_enforcement(inner_model_theory_constraints).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(inner_model_theory_constraints, consistency_proof_developers).
narrative_ontology:constraint_beneficiary(inner_model_theory_constraints, fine_structure_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(inner_model_theory_constraints, large_cardinal_pluralists).

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

% PERSPECTIVE 1: THE CONSTRUCTIBLE SET (SUBJECT)
% For a set within L, the constraints of constructibility are a Mountain.
% It exists only because it was defined by a formula at a specific level.
% It cannot "exit" to the outer universe (V) if V=L is the governing law.
constraint_indexing:constraint_classification(inner_model_theory_constraints, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CONSISTENCY RESEARCHER (BENEFICIARY)
% For the logician, the Inner Model is a Rope—a functional coordination
% mechanism. It allows them to coordinate a "standard of achievement"
% (e.g., proving Con(ZFC) implies Con(ZFC+CH)), creating a stable
% understanding of relative consistency.
constraint_indexing:constraint_classification(inner_model_theory_constraints, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE LARGE CARDINAL PROPONENT (VICTIM)
% For those seeking a "large" universe with measurable cardinals, the axiom
% V=L is a Snare. It strangles this possibility (by Scott's Theorem). It
% extracts the "richness of V" to maintain a thin, constructible reality.
constraint_indexing:constraint_classification(inner_model_theory_constraints, snare,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (Rope for logicians)
% and the asymmetric extraction (Snare for pluralists), classifying the
% overall structure as a Tangled Rope.
constraint_indexing:constraint_classification(inner_model_theory_constraints, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inner_model_theory_constraints_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the gap between the beneficiary (logician) and victim (pluralist).
    constraint_indexing:constraint_classification(inner_model_theory_constraints, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(inner_model_theory_constraints, TypeVictim, context(agent_power(moderate), _, _, _)),
    assertion(TypeBeneficiary == rope),
    assertion(TypeVictim == snare),
    TypeBeneficiary \= TypeVictim.

test(analytical_claim_matches_tangled_rope) :-
    % The analytical perspective should resolve the conflict as Tangled Rope.
    constraint_indexing:constraint_classification(inner_model_theory_constraints, AnalyticalType, context(agent_power(analytical), _, _, _)),
    assertion(AnalyticalType == tangled_rope).

:- end_tests(inner_model_theory_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.30) and suppression (S=0.50) are too high
 *   for a Mountain, reflecting the axiom's effect of "extracting" ontological
 *   richness and suppressing alternative models (like those with large
 *   cardinals). The constraint serves a genuine coordination function for
 *   logicians (proving relative consistency), justifying the `constraint_beneficiary`
 *   and the Rope perspective. Simultaneously, it imposes a significant cost on
 *   pluralists, justifying the `constraint_victim` and the Snare perspective.
 *   The combination of a coordination function, asymmetric extraction, and
 *   active (definitional) enforcement makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a logician focused on consistency, V=L is a
 *   powerful tool (Rope) for establishing foundational results. For a set
 *   theorist interested in the maximal possible universe, V=L is a prison
 *   (Snare) that denies the existence of fascinating and powerful structures.
 *   For an object *within* L, the axiom is simply the law of its existence (Mountain).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `consistency_proof_developers` gain a stable, predictable
 *     universe where difficult questions are settled, making their work tractable.
 *   - Victims: `large_cardinal_pluralists` lose the ability to study a whole
 *     class of mathematical objects they believe to be fundamental. The
 *     constraint extracts this possibility from their domain.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two errors. It is not
 *   a pure Snare, because it has a legitimate, non-trivial coordination function
 *   for a specific community (logicians). It is not a pure Mountain, because
 *   its "truth" is not absolute but an axiomatic choice with high suppression
 *   of alternatives (the universe of non-constructible sets revealed by forcing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_inner_model_limit,
    'Does there exist an ultimate inner model (like "Ultimate L") that can accommodate all large cardinals, or is the conflict between constructibility and large cardinals fundamental?',
    'The success or failure of the "Ultimate L" research program.',
    'If resolved, the constraint could transform from a Tangled Rope into a true Rope (universal coordination) or a Mountain (fundamental law). If it remains unresolved, the perspectival conflict persists.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(inner_model_theory_constraints, 1938, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(inner_model_theory_constraints, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(inner_model_theory_constraints, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(inner_model_theory_constraints, other_constraint_id).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality for
% each agent group.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */