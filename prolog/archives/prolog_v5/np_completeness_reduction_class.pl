% ============================================================================
% CONSTRAINT STORY: np_completeness_reduction_class
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_np_completeness_reduction_class, []).

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
 *   constraint_id: np_completeness_reduction_class
 *   human_readable: NP-Completeness Reduction Class
 *   domain: theoretical_computer_science/complexity_theory
 *
 * SUMMARY:
 *   NP-completeness reduction class defines the set of decision problems that
 *   are (1) in NP — solutions are verifiable in polynomial time, and (2)
 *   NP-hard — any NP problem can be reduced to them in polynomial time. This
 *   constraint is a natural law of theoretical computation. The core
 *   structure: if P ≠ NP (conjectured true but unproven), then no NP-complete
 *   problem admits a polynomial-time algorithm despite the verifiability of
 *   solutions. The constraint exhibits zero degrees of freedom. It is
 *   invariant across all problem instances, all algorithmic strategies, all
 *   computational substrates, and all time horizons. The reduction class
 *   emerges naturally from the definitions of polynomial-time Turing
 *   reduction and the decision problem framework. No agent benefits from the
 *   constraint; no agent can exit it; no agent experiences extraction. The
 *   constraint is a pure logical/mathematical mountain.
 *
 * KEY AGENTS:
 *   - Algorithm Designers: Structural position — attempt to solve NP-complete problems; face immutable polynomial-time barrier
 *   - Complexity Theorists: Analytical position — study the barrier; use it as a lens to understand computational limits
 *   - Cryptographers: Applied stakeholders — exploit NP-completeness for security; benefit from the barrier's existence
 *   - Computational Resource Allocators: Infrastructure position — cannot overcome the barrier through increased compute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(np_completeness_reduction_class, 0.12).
domain_priors:suppression_score(np_completeness_reduction_class, 0.03).
domain_priors:theater_ratio(np_completeness_reduction_class, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(np_completeness_reduction_class, extractiveness, 0.12).
narrative_ontology:constraint_metric(np_completeness_reduction_class, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(np_completeness_reduction_class, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(np_completeness_reduction_class, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(np_completeness_reduction_class, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(np_completeness_reduction_class, mountain).
narrative_ontology:human_readable(np_completeness_reduction_class, "NP-Completeness Reduction Class").
narrative_ontology:topic_domain(np_completeness_reduction_class, "theoretical_computer_science/complexity_theory").

domain_priors:emerges_naturally(np_completeness_reduction_class).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHM DESIGNER (MOUNTAIN) — Attempting to find a polynomial-time solution to an NP-complete problem faces an immutable structural barrier. If P ≠ NP, no algorithm of any sophistication can overcome this limit. The constraint is invariant across all problem instances, all algorithmic approaches, and all computational substrates.
constraint_indexing:constraint_classification(np_completeness_reduction_class, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL RESOURCE ALLOCATOR (MOUNTAIN) — Throwing more computational power at NP-complete problems does not overcome the polynomial-time barrier. The constraint is structural to the problem class, not to available resources. A nation-state allocating unlimited computing infrastructure still faces the same barrier.
constraint_indexing:constraint_classification(np_completeness_reduction_class, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THEORETICAL COMPUTER SCIENTIST (MOUNTAIN) — From the analytical standpoint, NP-completeness is a logical consequence of the definitions of NP, polynomial time, and reduction. The barrier is not contingent on physical law or social arrangement — it is a pure mathematical structure. The constraint is invariant across all measurement approaches and observables.
constraint_indexing:constraint_classification(np_completeness_reduction_class, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(np_completeness_reduction_class_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(np_completeness_reduction_class, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(np_completeness_reduction_class, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(np_completeness_reduction_class, ExtMetricName, E),
    domain_priors:suppression_score(np_completeness_reduction_class, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(np_completeness_reduction_class),
    narrative_ontology:constraint_metric(np_completeness_reduction_class, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(np_completeness_reduction_class, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(np_completeness_reduction_class_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The NP-completeness reduction class imposes a computational limit on algorithm design, but this limit is not 'extraction' in the Deferential Realism sense — no agent is extracting value from other agents through the constraint. The value 0.12 reflects the minimal friction of acknowledging the barrier (research effort spent on lower bounds instead of solution search) — this is not extraction overhead, merely the coordination cost of working within the constraint. Suppression (0.03): Minimal. The barrier suppresses specific algorithmic approaches (exhaustive search cannot solve NP-complete problems in polynomial time), but suppression in DR terminology means removal of alternatives to a constraint. Here, the barrier itself IS the fundamental structure; there are no suppressed alternatives. Resistance (0.08): Minimal. The barrier has resisted proof attempts for five decades, but 'resistance' in the mountain context means resistance of the barrier to violation or circumvention, not difficulty of proof. Theater ratio (0.08): Minimal. The definition is transparent; the barrier is not performative or theatrical. The research program around P vs NP includes genuine mathematical work, not ritual maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type constraint — NP-completeness classifies as mountain from all perspectives. The algorithm designer sees an immutable barrier (mountain). The computational resource allocator with arbitrary power sees the same barrier (mountain). The theoretical computer scientist analyzing from above sees a logical necessity (mountain). There is no perspectival gap because the constraint has no directionality — it does not differentiate agents or create asymmetric relationships. This uniformity is diagnostic: NP-completeness is one of the few constraints that legitimately appears as a pure natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: NP-completeness reduction class is among the clearest examples of a mountain constraint in the corpus. There is no mandatrophy risk because the constraint exhibits no coordination function that could be mislabeled as pure extraction, and no extraction that could be mislabeled as coordination. The constraint simply IS — a logical structure with no beneficiaries, no victims, and no alternative arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_vs_np_independence,
    'Is P ≠ NP provably true, or is the statement independent of standard axiomatic systems (Zermelo-Fraenkel set theory)?',
    'Logical independence proofs in the style of Cohen forcing; construction of models where P = NP or P ≠ NP both hold',
    'If independent: NP-completeness is a structural feature of some mathematical universes but not others — the mountain becomes contingent on axiomatic choice. If provably true: the mountain is absolute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_vs_np_independence, conceptual, 'Whether P ≠ NP is logically independent of ZFC').

omega_variable(
    oracle_separation_universality,
    'Do oracle separations proving P^A ≠ NP^A (for some oracle A) transfer to the standard non-relativized world, or do relativized results reflect only the structure of the oracle, not computational reality?',
    'Meta-analysis of which oracle results have led to breakthroughs in non-relativized complexity; examination of complexity-theoretic results that hold relative to all oracles but fail without oracles',
    'If oracle results are predictive: the mountain classification is slightly weakened — some barriers are oracle-dependent. If oracle results are artifacts of relativization: the mountain is confirmed as universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_separation_universality, empirical, 'Transferability of oracle separations to non-relativized complexity').

omega_variable(
    natural_proofs_barrier_absoluteness,
    'Does the Natural Proofs barrier (Razborov-Rudich) reveal a fundamental limitation of our proof techniques or a genuine structural limitation of circuit complexity?',
    'Discovery of proof techniques that circumvent the Natural Proofs barrier; construction of circuits breaking the barrier; identification of the barrier''s scope boundaries',
    'If barrier is proof-technical: NP-completeness may be provable with new techniques, downgrading mountain to scaffold. If barrier is structural: the mountain is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_proofs_barrier_absoluteness, conceptual, 'Whether Natural Proofs barrier reflects fundamental limitation or proof technique limitation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(np_completeness_reduction_class, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npc_tr_t0, np_completeness_reduction_class, theater_ratio, 0, 0.08).
narrative_ontology:measurement(npc_tr_t25, np_completeness_reduction_class, theater_ratio, 25, 0.08).
narrative_ontology:measurement(npc_tr_t50, np_completeness_reduction_class, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(npc_be_t0, np_completeness_reduction_class, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(npc_be_t25, np_completeness_reduction_class, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(npc_be_t50, np_completeness_reduction_class, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(np_completeness_reduction_class, information_standard).
narrative_ontology:affects_constraint(np_completeness_reduction_class, halting_problem_undecidability).
narrative_ontology:affects_constraint(np_completeness_reduction_class, pnp_conjecture_empirical_validation).
narrative_ontology:affects_constraint(np_completeness_reduction_class, cryptographic_key_hardness_assumption).

% DUAL FORMULATION NOTE:
% NP-completeness reduction class is upstream of specific computational hardness assumptions in cryptography and complexity theory. The constraint family includes: (1) Halting Problem undecidability (ε=0.08, Mountain, information-theoretic limit), (2) P vs NP conjecture empirical validation (ε=0.35, Tangled Rope, contingent-yet-structural, empirically unmeasured), (3) Specific NP-complete problem hardness (variable ε, depends on application domain and reduction details).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
