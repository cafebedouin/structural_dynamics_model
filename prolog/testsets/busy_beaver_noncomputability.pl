% ============================================================================
% CONSTRAINT STORY: busy_beaver_noncomputability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_busy_beaver_noncomputability, []).

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
 *   constraint_id: busy_beaver_noncomputability
 *   human_readable: The Non-Computability of the Busy Beaver Function (Σ)
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   The Busy Beaver function, Σ(n), represents the maximum number of steps a
 *   halting Turing machine with n states can execute before terminating. The
 *   non-computability of this function is a fundamental mathematical truth:
 *   no algorithm can compute Σ(n) for all n. This constraint is a candidate
 *   for pure Mountain classification because it exhibits zero degrees of
 *   freedom, emerges naturally from mathematical definitions, and imposes an
 *   absolute boundary on what computation can achieve. The constraint is
 *   invariant across all observer positions: a powerless agent, a powerful
 *   institution, and an analytical observer all face the identical boundary.
 *   There are no beneficiaries or victims — the constraint is not extractive,
 *   not suppressive, and not performative. It simply is. The theater ratio
 *   (0.05) reflects that the mathematical proof of non-computability requires
 *   no ritual, no performance, no institutional apparatus. The resistance
 *   (0.08) captures that mathematical foundations cannot be challenged by
 *   experimental refutation — they can only be questioned through formal
 *   redefiniton. The accessibility collapse (0.92) reflects that the
 *   non-computability is not a hidden or difficult-to-access property — it
 *   follows directly from the definition and standard computability theory.
 *   This constraint serves as the canonical exemplar for mathematical-logical
 *   mountains in the DR system.
 *
 * KEY AGENTS:
 *   - Turing Machines: Computational agents subject to the non-computability barrier (powerless/trapped) — cannot compute Σ(n) by definition
 *   - Mathematicians: Analytical observers of the constraint (analytical/analytical) — prove the non-computability; have no stake in whether it holds
 *   - Computational Systems: Any physical realization of computation (powerless/trapped) — subject to the same barrier as abstract Turing machines
 *   - Research Institutions: Organized knowledge systems (institutional/arbitrage) — cannot circumvent the constraint through coordination or resource aggregation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(busy_beaver_noncomputability, 0.12).
domain_priors:suppression_score(busy_beaver_noncomputability, 0.02).
domain_priors:theater_ratio(busy_beaver_noncomputability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, extractiveness, 0.12).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(busy_beaver_noncomputability, mountain).
narrative_ontology:human_readable(busy_beaver_noncomputability, "The Non-Computability of the Busy Beaver Function (Σ)").
narrative_ontology:topic_domain(busy_beaver_noncomputability, "mathematical/computational").

domain_priors:emerges_naturally(busy_beaver_noncomputability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICIAN (MOUNTAIN) — From the perspective of formal logic and computability theory, the non-computability of Σ(n) is a mathematical necessity, not a contingent institutional limit. No observer, no matter how powerful or well-resourced, can compute Σ(n) for n ≥ 5 using any Turing machine. This is not a suppression mechanism — it is an irreducible boundary of what computation itself can do. The constraint emerges naturally from the definitions of computability and halting.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL AGENT (MOUNTAIN) — A Turing machine or any physical computer attempting to compute Σ(n) for large n faces an absolute barrier, not a policy barrier or resource constraint that could be overcome with more funding or better algorithms. The barrier is ontological: the function's definition guarantees non-computability. No exit option exists. This is not extraction — it is a physical law of computation.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH COMMUNITY (MOUNTAIN) — Even a fully coordinated, well-funded research program with access to all known computational resources cannot compute Σ(6) or higher values. The non-computability is not a bottleneck that research can circumvent — it is a structural feature of mathematical reality. The constraint is invariant under aggregation: more researchers, more computers, more clever algorithms do not change the fundamental limit.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTIONAL KNOWLEDGE SYSTEM (MOUNTAIN) — Scientific institutions cannot negotiate, engineer around, or defer the non-computability of Σ(n). The constraint is not a policy that could be rewritten or a regulation that could be lobbied away. It is built into the mathematical structure of what 'computation' means. No institutional arbitrage, no regulatory capture, no paradigm shift can change this. The constraint is civilization-invariant.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(busy_beaver_noncomputability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, ExtMetricName, E),
    domain_priors:suppression_score(busy_beaver_noncomputability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(busy_beaver_noncomputability),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(busy_beaver_noncomputability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The non-computability of Σ(n) is not an extraction mechanism — it does not benefit one agent at the expense of another. The minimal non-zero value (0.12 rather than 0.0) reflects that the mathematical proof itself is created and maintained by the research community, which requires institutional resources and creates epistemic authority structures. However, these institutional features are not the constraint itself — they are scaffolding around the constraint. The core mathematical fact (non-computability) has zero extractiveness. Suppression (0.02): Negligible. The non-computability is not a suppression mechanism — it is not hiding alternatives or creating coercive barriers to exit. There is simply no alternative pathway to computation of Σ(n) that could be suppressed. Theater ratio (0.05): Minimal. The proof of non-computability requires formal mathematical reasoning with no performative elements. The proof is either correct or incorrect; there is no ritual or institutional theater required to establish it.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the same classification (Mountain) with the same underlying logic. There is no perspectival gap — the constraint looks identical from all structural positions. This uniformity is diagnostic of a true natural law: if the constraint were actually a social construction, institutional policy, or contingent arrangement, different observers would see different types (beneficiary seeing Rope, victim seeing Snare, etc.). The absence of perspectival gap confirms that we are measuring a structural feature of mathematical reality, not an institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   All four perspectives are agents subject to the same constraint with identical structural positions. No agent benefits while others bear costs. The directionality derivation chain does not apply — there are no beneficiaries or victims. All agents experience the constraint as an immutable boundary (d ≈ 0.5 for all, f(d) ≈ 0.65, but this is moot because chi is not computed for mountains). The constraint is civilization-invariant and observer-invariant.
 *
 * MANDATROPHY ANALYSIS:
 *   The Busy Beaver constraint resolves the mandatrophy trivially: it is a Mountain from all perspectives. The classification does not change based on observer position because the constraint is not extraction at all — it is an immutable boundary. The mandatrophy resolution affirms that mathematical non-computability is distinct from institutional extraction: the former is a structural law (Mountain), the latter is a social arrangement (Snare or Tangled Rope). The constraint exemplifies how to avoid the mandatrophy trap: when all perspectives converge on the same type with no perspectival gap, the classification is robust across contexts and observer positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_machine_hypercomputation,
    'Does hypercomputation (oracle machines, Turing machines with access to oracles) change the non-computability status of Σ(n)?',
    'Formal proof establishing whether oracle machines can compute Σ(n) or whether the problem remains uncomputable at higher levels of the arithmetical hierarchy',
    'If oracle machines can compute Σ(n): The constraint is relative to standard Turing machines, not absolute — it becomes a classification within a hierarchy rather than a mountain. If oracle machines cannot: The non-computability is robust across computational models and the mountain classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_machine_hypercomputation, conceptual, 'Whether hypercomputation undermines the mountain classification').

omega_variable(
    physical_realizability_ceiling,
    'Does the physical universe impose a lower ceiling on computable Busy Beaver values than mathematical non-computability suggests?',
    'Analysis of whether physical constraints (thermodynamic limits, computational speed limits, quantum decoherence) make Σ(n) practically uncomputable below the mathematical non-computability threshold',
    'If physical limits are more restrictive: The practical boundary is set by physics, not mathematics — the distinction between ''mathematically non-computable'' and ''physically impossible'' becomes empirically relevant. If mathematics is the limiting factor: The mountain classification reflects the actual constraint that agents face.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realizability_ceiling, empirical, 'Whether physical limits are more restrictive than mathematical non-computability').

omega_variable(
    definitional_circularity,
    'Is the non-computability of Σ(n) a mathematical truth or a tautology that follows from how we define the function?',
    'Examination of whether Σ(n) could be defined differently (e.g., as a computable approximation or truncated version) and whether alternative definitions would preserve the core conceptual content',
    'If definitional choice: The non-computability is partly a feature of how we chose to set up the problem — not purely a natural law. If tautological: The mountain classification is correct but needs qualification that the constraint is linguistic-conceptual in origin, not physical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_circularity, conceptual, 'Whether non-computability is mathematical truth or definitional artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(busy_beaver_noncomputability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bb_tr_t0, busy_beaver_noncomputability, theater_ratio, 0, 0.03).
narrative_ontology:measurement(bb_tr_t50, busy_beaver_noncomputability, theater_ratio, 50, 0.05).
narrative_ontology:measurement(bb_tr_t100, busy_beaver_noncomputability, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(bb_be_t0, busy_beaver_noncomputability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(bb_be_t50, busy_beaver_noncomputability, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(bb_be_t100, busy_beaver_noncomputability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(busy_beaver_noncomputability, information_standard).
narrative_ontology:affects_constraint(busy_beaver_noncomputability, halting_problem_decidability).
narrative_ontology:affects_constraint(busy_beaver_noncomputability, turing_completeness_boundary).
narrative_ontology:affects_constraint(busy_beaver_noncomputability, godel_incompleteness_theorem).

% DUAL FORMULATION NOTE:
% The Busy Beaver non-computability is a specific instance of the Halting Problem's undecidability. Σ(n) is non-computable because computing it would solve the Halting Problem. These are distinct constraints (different ε values, different measurement bases) but structurally coupled: non-computability of Σ(n) flows from the undecidability of the Halting Problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
