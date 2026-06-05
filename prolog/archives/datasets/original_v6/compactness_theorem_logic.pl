% ============================================================================
% CONSTRAINT STORY: compactness_theorem_logic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compactness_theorem_logic, []).

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
 *   constraint_id: compactness_theorem_logic
 *   human_readable: Compactness Theorem (First-Order Logic)
 *   domain: mathematical_logic/model_theory
 *
 * SUMMARY:
 *   The Compactness Theorem of first-order logic states that if every finite
 *   subset of a set of first-order sentences has a model, then the entire set
 *   has a model. This constraint is a foundational theorem in mathematical
 *   logic, proven via Gödel's completeness theorem and König's lemma, that
 *   characterizes an essential property of first-order expressivity. From all
 *   perspectives—proof-theoretic, model-theoretic, syntactic, semantic, and
 *   computational—the compactness property emerges as an invariant and
 *   unchangeable feature of first-order logic. No agent can escape or
 *   circumvent the constraint through strategic action, institutional design,
 *   or alternative frameworks without abandoning first-order logic itself.
 *   The constraint exhibits zero degrees of freedom across all indices,
 *   making it the canonical exemplar of a Mountain constraint.
 *
 * KEY AGENTS:
 *   - Finite Logical Agents: All entities (human or computational) using finite subsets of first-order theories; experience compactness as an immutable structural property that guarantees their finite derivations constrain infinite model classes
 *   - Model-Theoretic Community: Institutional agents (mathematical logicians, model theorists, proof assistants) who rely on compactness for metatheoretic results; cannot escape the constraint within first-order frameworks
 *   - Analytical Observer (Proof-Theoretic): Sees compactness as following necessarily from Gödel's completeness theorem and the deductive closure of first-order logic; observes that the constraint is logically necessary, not contingent
 *   - Analytical Observer (Semantic): Sees compactness as arising directly from König's lemma and the finitarity of logical satisfaction; observes that the constraint emerges from the structure of mathematical models themselves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compactness_theorem_logic, 0.08).
domain_priors:suppression_score(compactness_theorem_logic, 0.02).
domain_priors:theater_ratio(compactness_theorem_logic, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compactness_theorem_logic, extractiveness, 0.08).
narrative_ontology:constraint_metric(compactness_theorem_logic, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(compactness_theorem_logic, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(compactness_theorem_logic, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(compactness_theorem_logic, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compactness_theorem_logic, mountain).
narrative_ontology:human_readable(compactness_theorem_logic, "Compactness Theorem (First-Order Logic)").
narrative_ontology:topic_domain(compactness_theorem_logic, "mathematical_logic/model_theory").

domain_priors:emerges_naturally(compactness_theorem_logic).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FINITE LOGICAL AGENT (MOUNTAIN) — Any finite fragment of a first-order theory satisfiable in some model constrains all possible models satisfying the full theory. The compactness constraint is immutable from this perspective — no alternative logical architecture can escape the structural consequence that finite satisfiability entails infinite satisfiability in first-order logic. This agent is trapped by the necessity of the constraint.
constraint_indexing:constraint_classification(compactness_theorem_logic, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MODEL-THEORETIC COMMUNITY (MOUNTAIN) — Compactness is an invariant property of first-order logic across all mathematical contexts and research programs. Model theorists using first-order theories cannot escape the structural consequence that satisfiability of finite subsets entails satisfiability of the whole. The constraint is universal and unchangeable across all instantiations of first-order model theory.
constraint_indexing:constraint_classification(compactness_theorem_logic, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / PROOF-THEORETIC VIEW (MOUNTAIN) — From the perspective of formal logic and proof theory, compactness follows necessarily from the completeness theorem (Gödel) combined with the deductive closure of first-order logic. The constraint emerges as a logical necessity, not contingent on any particular mathematical choice or institutional arrangement. The constraint is invariant across all interpretations and irreducible.
constraint_indexing:constraint_classification(compactness_theorem_logic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / SEMANTIC VIEW (MOUNTAIN) — From the model-theoretic perspective, compactness is a direct consequence of the König-Rado compactness argument and the finitarity of logical derivation. Any formal system satisfying the axioms of first-order logic must exhibit compactness as an inescapable structural property. The constraint emerges from the architecture of first-order languages themselves.
constraint_indexing:constraint_classification(compactness_theorem_logic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compactness_theorem_logic_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(compactness_theorem_logic, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compactness_theorem_logic, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(compactness_theorem_logic, ExtMetricName, E),
    domain_priors:suppression_score(compactness_theorem_logic, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(compactness_theorem_logic),
    narrative_ontology:constraint_metric(compactness_theorem_logic, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(compactness_theorem_logic, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(compactness_theorem_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The compactness theorem extracts nothing from any agent; it is a pure logical law with no beneficiaries or victims. All agents experience it uniformly as a constraint on the logical system itself, not as extraction of value. The non-zero value reflects that the constraint does impose a structural cost on certain reasoning tasks (those requiring infinite derivations without finite subsets)—but this cost is a necessity of the system's architecture, not an extraction mechanism. Suppression (0.02): Minimal. The constraint suppresses no alternatives in a coercive sense; rather, it defines what first-order logic is. Agents cannot choose to violate compactness within first-order frameworks, but they can choose alternative logical systems (higher-order logic, infinitary logics) if compactness's limitations become binding. The suppression value reflects only the definitional impossibility of a first-order system lacking compactness, not institutional barriers to exit. Theater ratio (0.05): Minimal. The theorem has no performative aspect; its proof is purely formal and its statement directly describes the logical property. There is no gap between the constraint's claimed function (characterizing first-order expressivity) and its actual function (mathematical theorem).
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives converge on the same classification: Mountain. This is a uniform-type constraint. Finite logical agents, organized communities of logicians, and analytical observers from both proof-theoretic and model-theoretic approaches all experience compactness as an invariant, immutable property of first-order logic. The convergence across all (P,T,E,S) tuples to the same classification is diagnostic of a true natural law constraint. No agent benefits relative to others; no agent bears extraction; all are equally constrained by the logical necessity. This uniformity is the hallmark of a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to this constraint because there are no beneficiaries or victims. The compactness theorem imposes no extraction flow—it is a structural property of the logical system that affects all agents uniformly. The constraint does impose a cost on agents seeking infinite derivations without finite subsets (a task that compactness makes impossible within first-order logic), but this cost is necessity, not extraction. All agents experience the same constraint equally; therefore d cannot be meaningfully computed (all agents would have d ≈ 0.50 by symmetry, producing f(d) ≈ 0.65), but this would be misleading because the constraint is not asymmetric—it is universal. The absence of beneficiary/victim structure is itself a diagnostic signal that the constraint is a mountain, not a snare or tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: NOT APPLICABLE. This constraint exhibits no extractive or coordination function that could be misclassified. There is no risk of mislabeling pure extraction as coordination or vice versa, because the constraint is neither extractive nor coordinative. It is a mathematical law. The compactness theorem is a true mountain—immutable, universal, and irreducible to human institutional arrangements. There is no mandatrophy to resolve because there is no ambiguity in the constraint's nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    higher_order_transcendence,
    'Does compactness define the boundary between first-order and higher-order expressivity, or is it a deeper structural property transcending the syntax-semantics boundary?',
    'Formal comparison of compactness analogs in second-order logic, infinitary logics, and non-standard logical frameworks; identification of whether compactness is syntactic constraint or semantic necessity',
    'If syntactic boundary: compactness is specific to first-order formalization, not universal. If semantic necessity: compactness reveals a deep structural limit on finite-to-infinite inference in any logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(higher_order_transcendence, conceptual, 'Whether compactness is syntax-specific or semantically universal').

omega_variable(
    proof_versus_model_primacy,
    'Does compactness emerge from proof-theoretic properties (completeness, deductive closure) or model-theoretic properties (König''s lemma, finitarity of satisfaction)?',
    'Formal derivation analysis comparing completeness-based proofs versus König-Rado proofs; identification of which foundational property is logically prior',
    'If proof-theoretic: compactness is a logical law. If model-theoretic: compactness reflects necessary properties of mathematical structures. Either way, the constraint is immutable, but the locus of necessity shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_versus_model_primacy, conceptual, 'Proof-theoretic vs model-theoretic grounding of compactness').

omega_variable(
    finitarity_necessity,
    'Is the finitarity of logical derivation a mathematical law or a choice made in the design of first-order logic?',
    'Formal study of infinitary logic systems (L-omega-1,omega, etc.) that violate finitarity; empirical determination of whether their loss of compactness represents a cost or a feature',
    'If finitarity is necessary: compactness follows as an immutable consequence. If finitarity is a design choice: compactness is contingent on that choice, making the constraint not a mountain but a structural feature of our chosen logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finitarity_necessity, preference, 'Whether finitarity of derivation is a logical necessity or a design choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compactness_theorem_logic, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compact_tr_t0, compactness_theorem_logic, theater_ratio, 0, 0.05).
narrative_ontology:measurement(compact_tr_t50, compactness_theorem_logic, theater_ratio, 50, 0.05).
narrative_ontology:measurement(compact_tr_t100, compactness_theorem_logic, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(compact_be_t0, compactness_theorem_logic, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(compact_be_t50, compactness_theorem_logic, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(compact_be_t100, compactness_theorem_logic, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compactness_theorem_logic, information_standard).
narrative_ontology:affects_constraint(compactness_theorem_logic, loewenheim_skolem_downward).
narrative_ontology:affects_constraint(compactness_theorem_logic, godel_completeness_theorem).
narrative_ontology:affects_constraint(compactness_theorem_logic, first_order_expressivity_limits).

% DUAL FORMULATION NOTE:
% Compactness is a foundational theorem in model theory that characterizes first-order logic's essential properties. It is upstream of all results about first-order expressivity and decidability. While related constraints (Löwenheim-Skolem, Gödel completeness) have their own epistemological content, compactness represents a deeper structural property that they depend upon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
