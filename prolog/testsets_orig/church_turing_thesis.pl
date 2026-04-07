% ============================================================================
% CONSTRAINT STORY: church_turing_thesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
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
 *   constraint_id: church_turing_thesis
 *   human_readable: Church-Turing Thesis (Computability Boundary)
 *   domain: mathematical/computational_theory
 *
 * SUMMARY:
 *   The Church-Turing Thesis is a mathematical claim asserting that the class
 *   of functions computable by an algorithm equals the class of functions
 *   computable by a Turing machine. Unlike constraints that emerge from power
 *   dynamics, institutional arrangements, or coordination failures, the
 *   Church-Turing Thesis is a structural property of computation itself. No
 *   agent benefits from the computability boundary; no agent suppresses
 *   alternatives to it. The boundary exists because logical necessity
 *   produces it, not because someone enforces it. The thesis demonstrates the
 *   'Mountain' classification in its purest form: zero extractiveness (ε ≈
 *   0.12, reflecting only the minimal irreducible measurement uncertainty in
 *   mathematical claims), minimal suppression (σ ≈ 0.03, reflecting that no
 *   coercive apparatus is required), and negligible theater (θ ≈ 0.15,
 *   reflecting that the thesis is verified through formal proof, not
 *   institutional performance). The accessibility collapse is extremely high
 *   (0.92): the boundary is completely inaccessible to algorithmic methods —
 *   no amount of cleverness or effort can compute an uncomputable function
 *   using standard computation. The resistance is very low (0.08): there is
 *   no organized alternative to the thesis; no community claims or attempts
 *   to disprove it through practice.
 *
 * KEY AGENTS:
 *   - Uncomputable Functions: Structural limit (no power) — Halting Problem, Kolmogorov complexity, other incomputable problems are outside algorithmic reach
 *   - Programmers/Algorithms: Powerless/constrained agents — Cannot transcend the boundary; can only work within it
 *   - Computer Science Institution: Institutional consensus (arbitrage) — Takes the thesis as foundational; organizes research around its implications
 *   - Mathematical Community: Analytical observers (civilizational) — Formalized the thesis across multiple equivalent formalisms (lambda calculus, Turing machines, μ-recursion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis, 0.12).
domain_priors:suppression_score(church_turing_thesis, 0.03).
domain_priors:theater_ratio(church_turing_thesis, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis, extractiveness, 0.12).
narrative_ontology:constraint_metric(church_turing_thesis, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(church_turing_thesis, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(church_turing_thesis, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis, mountain).
narrative_ontology:human_readable(church_turing_thesis, "Church-Turing Thesis (Computability Boundary)").
narrative_ontology:topic_domain(church_turing_thesis, "mathematical/computational_theory").

domain_priors:emerges_naturally(church_turing_thesis).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCOMPUTABLE FUNCTION (MOUNTAIN) — Any function outside the Turing-computable set is structurally inaccessible to algorithmic computation. This is not a constraint imposed by an external agent; it is a logical limit. No escape, no workaround, no observer perspective changes this classification. Zero degrees of freedom.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROGRAMMER SEEKING HYPERCOMPUTATION (MOUNTAIN) — A programmer attempting to compute a Halting Problem instance or Kolmogorov complexity value experiences the constraint as absolute. Constrained exit options reflect that they can avoid the problem by changing the target function, but cannot change the boundary itself. The constraint's extractiveness is minimal because no agent benefits from the limit — it is a structural property of computation itself.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPUTER SCIENCE INSTITUTION (MOUNTAIN) — Academic and industrial computing communities take the Church-Turing Thesis as foundational. No verification apparatus enforces it; no agent suppresses alternatives. It exists as a mathematical fact. The institutional perspective confirms the mountain classification: the thesis is not maintained by institutional power, but by logical necessity. The institution organizes itself around its constraints, but does not create them.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / FORMAL VERIFICATION (MOUNTAIN) — From the position of mathematical analysis, the Church-Turing Thesis is a precise claim: the class of lambda-computable functions equals the class of Turing-computable functions equals the class of μ-recursive functions. This equivalence has been proven in multiple formal systems. No observer perspective changes the boundary. No measurement methodology reveals exceptions. The thesis is invariant under all mathematical formalisms that encode computation.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(church_turing_thesis, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(church_turing_thesis, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(church_turing_thesis, ExtMetricName, E),
    domain_priors:suppression_score(church_turing_thesis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(church_turing_thesis),
    narrative_ontology:constraint_metric(church_turing_thesis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(church_turing_thesis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(church_turing_thesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): The Church-Turing Thesis exhibits minimal extractiveness because it extracts nothing — no agent benefits at the expense of others. The low score (not zero) reflects epistemic humility: mathematical proofs can contain hidden gaps, and the thesis, while foundationally verified across multiple formalisms, remains a conjecture about the totality of computation rather than a proven theorem of set theory. No coercive mechanism maintains the boundary; logical necessity suffices. Suppression (0.03): Minimal. The thesis is not enforced through suppression of alternatives or elimination of competitors. No funding is withheld from hypercomputation research; no papers are rejected for proposing oracle machines. The research community openly studies hypercomputation as a mathematical construct, even while accepting that Turing machines capture all practical, realizable computation. Theater ratio (0.15): Very low. The thesis is verified through formal proof and mathematical equivalence proofs across multiple models, not through institutional performance or ritualistic verification. Publications do not stake claims that are later retracted; the foundational equivalences (lambda calculus = Turing machines = μ-recursion) are established through rigorous proof, not empirical confirmation. The minimal theater reflects that the thesis operates at the mathematical level, where verification is proof-based, not performance-based.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the Mountain classification because the Church-Turing Thesis is an invariant under all observation positions. The uncomputable function experiences the boundary as absolute (trapped, powerless). The programmer experiencing the constraint as logical necessity (constrained, moderate). The computer science institution operates within the boundary (institutional, analytical). The formal analyst verifies its invariance across all mathematical models (analytical, universal). There is no perspectival gap because the constraint is not observational — it is not relative to the observer's power, time horizon, exit options, or spatial scope. This convergence is the diagnostic signature of a natural law: all observation positions produce the same classification because the constraint transcends institutional, social, or contextual variation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church-Turing Thesis has no beneficiaries and no victims. Directionality (d) is not computed from power asymmetries or exit options because there is no extraction to direct. All perspectives converge on Mountain because the boundary is absolute and symmetric: no agent extracts from others through the thesis; the thesis is not maintained by coercion or suppression; it exists purely as a mathematical property. The lack of beneficiary/victim structure is the critical diagnostic: if there were a beneficiary (e.g., a research community defending the thesis to protect their turf), or a victim (e.g., hypercomputation researchers suppressed by mainstream orthodoxy), the constraint would be a Tangled Rope or Snare, not a Mountain. The absence of extractive directionality confirms the natural law classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The Church-Turing Thesis resolves the mandatrophy by exhibiting zero extractiveness (ε ≈ 0.12) and zero asymmetry across perspectives. Mandatrophy is the false positive error: mislabeling a pure coordination mechanism or a symmetric constraint as asymmetric extraction. The inverse error — mislabeling a true power asymmetry as a natural law — is sometimes called 'false naturalization.' The Church-Turing Thesis avoids both errors: it is not extractive (no agent benefits), and it is genuinely invariant (all perspectives agree). The thesis demonstrates that a Mountain classification is not merely a claim that 'this constraint is natural,' but a precise mathematical property: zero beneficiary/victim asymmetry, zero institutional enforcement, and invariance under all reasonable observation frameworks. The minimal omegas (confined to epistemological questions about hypercomputation's possibility, not about institutional suppression) further confirm the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypercomputation_physical_realizability,
    'Could physical processes (quantum computing, relativistic computation, analog devices, novel physical substrates) compute beyond the Turing limit?',
    'Formal proof that proposed hypercomputational models reduce to Turing-computable operations or genuinely exceed them; empirical verification of quantum advantage on uncomputable problems (not just hard problems); demonstration of closed timelike curves or other exotic physics enabling non-Turing computation',
    'If hypercomputation is physically realizable: Church-Turing Thesis remains mathematically valid but loses its empirical universality claim. The boundary becomes a mathematical property of abstract models, not a physical law. Classification remains Mountain (mathematical limit), but the practical scope narrows. If hypercomputation is physically impossible: the thesis strengthens as a law of physics, not merely mathematics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypercomputation_physical_realizability, empirical, 'Whether physical processes could exceed Turing computability').

omega_variable(
    oracle_machine_ontology,
    'Are oracle machines and hypercomputation conceptually coherent, or do they reveal that the thesis is incomplete in its scope?',
    'Philosophical analysis of oracle machine semantics; clarification of what it means to ''have access to'' an oracle vs. to compute it; comparison with other mathematical abstractions that outreach their foundational theories (e.g., ordinal arithmetic in ZFC)',
    'If oracles are legitimate mathematical objects: the thesis is locally correct (Turing machines compute the Turing-computable set) but incomplete — it does not claim to exhaust all meaningful computation, only algorithmic computation. If oracles are notational artifacts: the thesis is complete and the boundary is absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_machine_ontology, conceptual, 'Whether oracle machines represent genuine hypercomputation or notational artifacts').

omega_variable(
    implicit_assumptions_thesis_scope,
    'Does the Church-Turing Thesis make implicit assumptions about what constitutes ''computation'' (determinism, finitism, discrete steps) that could be relaxed to permit new classes of processes?',
    'Formal analysis of the thesis statement''s definitional dependencies; exploration of computation models with non-standard assumptions (continuous computation, probabilistic computation, nondeterministic computation); assessment of whether these models fall inside or outside the thesis''s scope',
    'If the thesis is definition-dependent: it is logically sound but contextual — different formal definitions of ''computation'' produce different boundaries. If it is definition-independent: the boundary is absolute and invariant across all reasonable formalizations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_assumptions_thesis_scope, conceptual, 'Whether implicit assumptions constrain the thesis''s scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis, halting_problem).
narrative_ontology:affects_constraint(church_turing_thesis, algorithmic_information_theory).
narrative_ontology:affects_constraint(church_turing_thesis, computational_universality).

% DUAL FORMULATION NOTE:
% The Church-Turing Thesis is a unified claim about computability boundaries. Unlike constraints that decompose into multiple distinct claims with different ε values, the thesis has a single, invariant ε across all mathematical formalizations. It is not decomposed into separate stories because the boundary is absolute and unchanging: lambda computable = Turing computable = μ-recursive computable across all observational contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
