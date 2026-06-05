% ============================================================================
% CONSTRAINT STORY: ordinal_proof_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ordinal_proof_hierarchy, []).

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
 *   constraint_id: ordinal_proof_hierarchy
 *   human_readable: Ordinal Proof Hierarchy and Goedel's Incompleteness
 *   domain: mathematical_logic/proof_theory
 *
 * SUMMARY:
 *   The ordinal proof hierarchy and Goedel's Incompleteness Theorems
 *   establish a fundamental constraint on formal systems: no consistent,
 *   recursively axiomatizable formal system rich enough to express arithmetic
 *   can prove all truths within its domain. This constraint does not arise
 *   from institutional policy, resource scarcity, or technological
 *   limitation. It is a structural property of formal systems themselves,
 *   proven mathematically rather than discovered empirically. The ordinal
 *   proof hierarchy — the sequence of increasingly strong consistent
 *   extensions of Peano arithmetic (PA, PA + Con(PA), PA + Con(PA + Con(PA)),
 *   ...) — maps the landscape of undecidable propositions. Each ordinal level
 *   adds new provable truths but leaves new undecidables untouched. The
 *   constraint exhibits zero degrees of freedom: every consistent formal
 *   system exhibits incompleteness. No agent, institution, or choice of
 *   foundational axioms can escape this limit. The theater ratio (0.08) is
 *   minimal because there is no performative component — the proof-theoretic
 *   facts are not socially constructed or negotiable. The extractiveness
 *   (0.12) reflects the minimal 'cost' of the incompleteness constraint: some
 *   propositions cannot be decided, which imposes a bound on what can be
 *   formally known. This is among the purest examples of a natural law
 *   constraint in mathematics.
 *
 * KEY AGENTS:
 *   - Finite Agent Within System: Trapped at the system's ground level (powerless/trapped) — cannot extend beyond the system's axioms to resolve undecidables
 *   - Mathematical Observer: Analytical perspective (analytical/analytical) — sees the necessity of incompleteness across all consistent formalizations
 *   - Mathematical Community: Institutional perspective (institutional/arbitrage) — can choose axioms and foundational systems but cannot escape incompleteness itself
 *   - Undecidable Propositions: Structural invariant — persist in every consistent system of sufficient expressive power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ordinal_proof_hierarchy, 0.12).
domain_priors:suppression_score(ordinal_proof_hierarchy, 0.03).
domain_priors:theater_ratio(ordinal_proof_hierarchy, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ordinal_proof_hierarchy, extractiveness, 0.12).
narrative_ontology:constraint_metric(ordinal_proof_hierarchy, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(ordinal_proof_hierarchy, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ordinal_proof_hierarchy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ordinal_proof_hierarchy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ordinal_proof_hierarchy, mountain).
narrative_ontology:human_readable(ordinal_proof_hierarchy, "Ordinal Proof Hierarchy and Goedel's Incompleteness").
narrative_ontology:topic_domain(ordinal_proof_hierarchy, "mathematical_logic/proof_theory").

domain_priors:emerges_naturally(ordinal_proof_hierarchy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FINITE AGENT WITHIN SYSTEM (MOUNTAIN) — A proof system or formal theory operating within finitary constraints cannot prove all true statements about its own domain. This is not a policy choice or institutional arrangement — it is a structural feature of formal systems themselves. The agent within the system has zero degrees of freedom: the incompleteness gap exists regardless of effort, resources, or ingenuity applied within the system's axioms.
constraint_indexing:constraint_classification(ordinal_proof_hierarchy, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL MATHEMATICAL OBSERVER (MOUNTAIN) — From outside the system, Goedel's Incompleteness Theorems establish that any consistent formal system rich enough to express arithmetic must contain undecidable propositions. This is not a contingent fact about current mathematics or a limitation we might overcome. It is a necessary structural property following from the definition of formal systems. The observer sees zero degrees of freedom across all consistent, axiomatizable, recursively decidable theories.
constraint_indexing:constraint_classification(ordinal_proof_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY PERSPECTIVE (MOUNTAIN) — Mathematicians cannot choose to escape incompleteness by adopting a different foundation. Moving from one formal system (ZFC) to a stronger one (ZFC + large cardinal axioms) extends the undecidable set; it does not eliminate it. Every consistent system of sufficient expressive power exhibits the same property. The constraint is invariant under institutional reorganization, axiom choice, or foundational preference.
constraint_indexing:constraint_classification(ordinal_proof_hierarchy, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ordinal_proof_hierarchy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ordinal_proof_hierarchy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ordinal_proof_hierarchy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ordinal_proof_hierarchy, ExtMetricName, E),
    domain_priors:suppression_score(ordinal_proof_hierarchy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ordinal_proof_hierarchy),
    narrative_ontology:constraint_metric(ordinal_proof_hierarchy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ordinal_proof_hierarchy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ordinal_proof_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint does not extract in the sense of transferring resources from one agent to another. Rather, it imposes a structural limit: some truths are inherently unreachable through formal proof. This is a constraint on knowledge access, not an extractive transfer. Suppression (0.03): Minimal. No agents are suppressed by incompleteness — the constraint is transparent and mathematically demonstrable. Mathematicians understand why the undecidable propositions cannot be proved; incompleteness does not obscure itself. Theater ratio (0.08): Minimal. Proof theory is not performative. The incompleteness results are strictly technical — their validity does not depend on social performance or institutional ritual. The small nonzero value reflects that even formal mathematics requires some institutional scaffolding (conference presentation, journal publication, peer review of proofs), but this scaffolding is minimal and not central to the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives (finite agent, analytical observer, institutional community) converge on the same classification: Mountain. This is a uniform-type constraint where perspectival position does not alter the classification. No agent — regardless of power level or exit options — perceives incompleteness as escapable or negotiable. The finite agent experiences it as a hard structural limit. The analytical observer proves it is necessary. The institutional community observes that choosing different axioms does not eliminate it — only shifts it. This convergence is diagnostic: when a constraint classifies as Mountain from all perspectives with different power levels and exit options, the mountain classification is highly robust. The lack of perspectival gap is itself evidence of a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is not meaningfully defined for this constraint because there is no extraction dynamic — no agent benefits at another's expense. The ordinal proof hierarchy is not a zero-sum constraint where one agent's gain is another's loss. All agents (finite or infinite, individual or institutional, inside or outside the system) face the same structural limit. The mathematical community could be assigned d ≈ 0.5 (neither beneficiary nor victim) or d ≈ 0.72 (analytical observer), but these derive from the canonical fallback, not from actual beneficiary/victim dynamics. The constraint operates on a different axis: it limits knowledge access universally, not redistributes resources asymmetrically. No directionality overrides are needed because there is no structural relationship asymmetry to capture.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY: This constraint exhibits zero ambiguity between coordination and extraction. It is purely a natural law — a constraint on what formal systems can express and prove. There is no tension between a coordination function and extractive overhead. Incompleteness is not a mechanism for enabling some agents to benefit from others' labor; it is a boundary condition on mathematical knowledge itself. The constraint could not be misclassified as pure coordination (Rope) because there is no coordination problem being solved. Could not be misclassified as pure extraction (Snare) because no extraction occurs. The classification as Mountain is certain given the mathematical proof of Goedel's Incompleteness Theorems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructivism_escape,
    'Does constructive mathematics or intuitionism provide a genuine escape from incompleteness, or does incompleteness re-emerge in those frameworks under different names?',
    'Detailed proof-theoretic analysis of incompleteness phenomena in constructive formal systems; comparison of undecidable propositions across classical and intuitionistic frameworks',
    'If escape is genuine: incompleteness is a contingent feature of classical logic (reclassifies toward Rope). If incompleteness re-emerges: it is a necessary feature of formal systems regardless of foundational choice (confirms Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructivism_escape, empirical, 'Whether alternative mathematical foundations escape incompleteness').

omega_variable(
    oracle_machine_completeness,
    'Do oracle machines or hypercomputation frameworks represent a structural escape from Turing completeness limits and incompleteness, or are they theoretical idealizations that do not address the fundamental ordinal hierarchy constraint?',
    'Analysis of oracle Turing machine proof power; examination of whether oracles merely shift the undecidability boundary or constitute a genuine escape; proof-theoretic analysis of hypercomputational frameworks',
    'If oracles escape limits: incompleteness is specific to Turing-equivalent systems (contingent constraint). If boundaries shift but undecidability persists: incompleteness is necessary (confirms Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_machine_completeness, conceptual, 'Whether oracle machines represent genuine escape from incompleteness').

omega_variable(
    ordinal_hierarchy_infinity,
    'Is the ordinal proof hierarchy finite in any meaningful sense, or does it extend infinitely in a way that makes the enumeration of undecidable propositions itself undecidable?',
    'Ordinal notation analysis; proof-theoretic examination of the order type of consistent extensions; meta-analysis of the completeness of ordinal assignment systems',
    'If hierarchy is finite: there exists a maximum consistent extension (provides bounded incompleteness). If infinite: the undecidable set is itself computationally unreachable (reinforces absolute Mountain classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_hierarchy_infinity, empirical, 'Whether the ordinal proof hierarchy is finite or infinite').

omega_variable(
    meaning_vs_consistency,
    'Is undecidability in formal systems a limitation on mathematical truth (meaning), or only on formal provability? Do undecidable propositions have determinate truth values outside formal systems?',
    'Philosophical analysis of platonism vs formalism; exploration of whether Goedel''s Second Incompleteness Theorem has ontological implications beyond syntax',
    'If undecidable propositions lack determinate truth: the constraint is one of knowledge/provability (potentially escapable through meaning). If they have truth values independent of formalization: incompleteness is a constraint on access to pre-existing mathematical reality (confirms Mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meaning_vs_consistency, conceptual, 'Relationship between undecidability and mathematical truth').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ordinal_proof_hierarchy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ordproof_tr_t0, ordinal_proof_hierarchy, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ordproof_tr_t50, ordinal_proof_hierarchy, theater_ratio, 50, 0.08).
narrative_ontology:measurement(ordproof_tr_t100, ordinal_proof_hierarchy, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(ordproof_be_t0, ordinal_proof_hierarchy, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ordproof_be_t50, ordinal_proof_hierarchy, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(ordproof_be_t100, ordinal_proof_hierarchy, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ordinal_proof_hierarchy, information_standard).
narrative_ontology:affects_constraint(ordinal_proof_hierarchy, turing_halting_problem).
narrative_ontology:affects_constraint(ordinal_proof_hierarchy, computational_complexity_hierarchy).

% DUAL FORMULATION NOTE:
% The ordinal proof hierarchy is upstream of computational complexity constraints. Incompleteness in formal logic implies incompleteness in computability theory (halting problem). The undecidable propositions in first-order logic correspond structurally to uncomputable functions in recursion theory. These are not separate constraints but different formalizations of the same underlying limitation on information access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
