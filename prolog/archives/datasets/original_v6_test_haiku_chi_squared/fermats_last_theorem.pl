% ============================================================================
% CONSTRAINT STORY: fermats_last_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [RESOLVED]
% ============================================================================

:- module(constraint_fermats_last_theorem, []).

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
 *   constraint_id: fermats_last_theorem
 *   human_readable: The Difficulty of Proving Fermat's Last Theorem
 *   domain: mathematics/number_theory
 *
 * SUMMARY:
 *   Fermat's Last Theorem presents a pure mountain constraint: the objective
 *   logical difficulty of proving a statement about the structure of integer
 *   solutions to polynomial equations. From 1637 (Fermat's marginal
 *   annotation) to 1995 (Wiles' proof), the theorem's resistance to proof was
 *   not a social arrangement or coordination failure, but an inherent feature
 *   of the mathematical landscape. The theorem's truth-value was independent
 *   of any agent's belief or institutional endorsement — either no solutions
 *   exist (as Fermat claimed) or they do. The barrier was the logical
 *   distance between the statement and its proof, which required connecting
 *   to deep structures in algebraic geometry and the arithmetic of elliptic
 *   curves. This distance was a mathematical fact, not a social construct.
 *   The constraint exhibits zero degrees of freedom: no one can make the
 *   theorem easier to prove by social agreement, institutional reform, or
 *   alternative definitions. The theorem's truth cannot be negotiated. The
 *   350-year gap between statement and proof demonstrates the mountain's
 *   defining feature: emergence from mathematical structure itself, not from
 *   institutional extraction or coordination failure.
 *
 * KEY AGENTS:
 *   - Individual Mathematicians: Observers of the logical constraint (powerless/trapped) — each confronts the same objective difficulty; the barrier is not targetable at them specifically but at the mathematical structure itself
 *   - Mathematical Community: Institutional curator (institutional/analytical) — decides whether to invest attention in the problem; can allocate resources but cannot alter the logical difficulty
 *   - Logical Structure of Number Theory: The constraint's source (analytical/analytical) — not an agent but the autonomous feature that generates the barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fermats_last_theorem, 0.12).
domain_priors:suppression_score(fermats_last_theorem, 0.03).
domain_priors:theater_ratio(fermats_last_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fermats_last_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(fermats_last_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fermats_last_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fermats_last_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fermats_last_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fermats_last_theorem, mountain).
narrative_ontology:human_readable(fermats_last_theorem, "The Difficulty of Proving Fermat's Last Theorem").
narrative_ontology:topic_domain(fermats_last_theorem, "mathematics/number_theory").

domain_priors:emerges_naturally(fermats_last_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A mathematician faces the theorem's proof barrier as an objective logical structure: the gap between Fermat's claim and its proof is a genuine degree-of-freedom problem inherent to the mathematical structure itself, not a contingent social arrangement. No exit from the logical constraint; the proof must be found or not. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the institutional mathematical perspective, the theorem's proof difficulty is a pure structural property of number theory. The barrier is not a snare trapping mathematicians into extractive work — it is an immutable feature of mathematical knowledge: some theorems are harder than others because of the logical dependencies they require. The community can choose not to work on it, but the theorem's truth-value and proof structure remain independent of choice. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The analytical observer perceives Fermat's Last Theorem as a pure mountain constraint: a logical limit inherent to number theory. The theorem's statement is a precise mathematical claim; its proof difficulty emerges from the deep structural connections to algebraic geometry, modular forms, and Galois representations. The 350-year barrier was not extraction or coordination failure — it was the logical depth of the problem itself. Once Wiles discovered the connection to the Taniyama-Shimura-Weil conjecture, the proof became possible because the structural dependency was identified. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fermats_last_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fermats_last_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fermats_last_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fermats_last_theorem, ExtMetricName, E),
    domain_priors:suppression_score(fermats_last_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fermats_last_theorem),
    narrative_ontology:constraint_metric(fermats_last_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fermats_last_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fermats_last_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The theorem's proof difficulty is not a mechanism for extracting value from mathematicians. While some prestige attaches to progress on FLT, the barrier itself is not designed to extract resources or attention — it simply exists as a logical fact. Measuring extractiveness at 0.12 rather than near-zero reflects a modest acknowledgment that the famous status of FLT may have drawn disproportionate attention relative to equally difficult problems, creating some reputational asymmetry. But this is secondary to the constraint's core property. Suppression (0.03): Negligible. Mathematicians are not suppressed from working on the theorem — the problem is widely known, published, and open to any practitioner with sufficient training. The suppression index reflects only the fact that working on FLT requires advanced mathematical training (a gate but not a suppression mechanism). Theater ratio (0.15): Very low. The proof is not performative. When Wiles announced the proof, the subsequent two years of refining it to fill a gap in the original argument were genuine mathematical work, not theater. The low theater reflects that verification of Wiles' proof is a matter of logical checking, not social performance. The modest non-zero value reflects only the presentation requirements inherent to communicating proofs.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify FLT as a pure mountain. The gap is not between types but between degrees of removal from the constraint. The individual mathematician faces it directly as an obstacle (powerless/trapped). The institutional mathematical community faces it as a resource allocation problem (institutional/analytical) — should we fund FLT research or other fields? — but the underlying constraint is identical for both: the logical difficulty is the same regardless of observer position. The analytical observer perceives the mountain's true structure: Fermat's claim involves a deep statement about the rational points on the Fermat curves, and proving this required connecting to the modularity of elliptic curves — a connection that is logically necessary (not socially contingent) given the mathematical definitions involved. There is no perspectival gap in classification type because the constraint emerges identically from all observation points. This uniformity across perspectives is the defining signature of a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no extraction mechanism and no structural beneficiary/victim relationship. Directionality is not applicable. All agents encounter the same logical barrier. The theorem's difficulty is a property of mathematical structure, not a relationship between agents. d-values per perspective are computed only to demonstrate their invariance: the mountain emerges across all (P,T,E,S) tuples because the constraint's truth-value is observer-independent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_necessity_vs_contingency,
    'Is the 350-year barrier a necessary feature of the theorem''s logical structure, or a contingent feature of 17th-century mathematical knowledge?',
    'Retrospective analysis of proof techniques required (algebraic geometry, modular forms, Galois theory) and when these were developed; counterfactual: could these tools have been developed earlier?',
    'If necessary: confirms mountain classification — the barrier is inherent to the theorem itself. If contingent: suggests the barrier was partly institutional/knowledge-dependent (weakens mountain claim, suggests snare or tangled_rope elements).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_necessity_vs_contingency, empirical, 'Whether the proof barrier is logically necessary or historically contingent').

omega_variable(
    extractive_prestige_dynamics,
    'Did the famous status of Fermat''s Last Theorem create career-driven extraction dynamics (mathematicians chasing prestige rather than pursuing genuine number theory questions)?',
    'Historical analysis of mathematician motivation statements; correlation between work on FLT vs work on peer problems; assessment of whether FLT focus advanced or hindered broader number theory development.',
    'If extraction dynamics present: suggests snare or tangled_rope elements masked by mountain framing. If minimal: confirms mountain — the problem drew genuine mathematical interest aligned with its intrinsic difficulty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_prestige_dynamics, empirical, 'Whether prestige-seeking created hidden extraction dynamics').

omega_variable(
    knowledge_dependency_of_proof_concepts,
    'Were the mathematical tools required for Wiles'' proof (modular forms, Galois representations, algebraic geometry over schemes) inherently difficult to discover, or were they contingent on specific historical developments in 20th-century mathematics?',
    'Detailed analysis of the proof''s mathematical prerequisites; assessment of whether earlier mathematicians could have developed these tools if they had focused on Fermat''s theorem; comparison with other deep theorems that required equally novel machinery.',
    'If inherently difficult: mountain character is confirmed. If contingent on specific history: the barrier was partly epistemic/institutional rather than purely logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_dependency_of_proof_concepts, conceptual, 'Whether proof tools were logically necessary or historically contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fermats_last_theorem, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fermat_tr_t0, fermats_last_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fermat_tr_t175, fermats_last_theorem, theater_ratio, 175, 0.12).
narrative_ontology:measurement(fermat_tr_t350, fermats_last_theorem, theater_ratio, 350, 0.15).

% Extraction over time
narrative_ontology:measurement(fermat_be_t0, fermats_last_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fermat_be_t175, fermats_last_theorem, base_extractiveness, 175, 0.1).
narrative_ontology:measurement(fermat_be_t350, fermats_last_theorem, base_extractiveness, 350, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fermats_last_theorem, information_standard).
narrative_ontology:affects_constraint(fermats_last_theorem, mordell_conjecture_proof).
narrative_ontology:affects_constraint(fermats_last_theorem, abc_conjecture_status).

% DUAL FORMULATION NOTE:
% Fermat's Last Theorem is part of a constraint family of deep arithmetic statements. The related constraints (Mordell conjecture, ABC conjecture) have similar logical difficulty structures but different ε values and empirical status. FLT is resolved (proof exists); Mordell conjecture is resolved (Faltings, 1983); ABC conjecture remains contested (Mochizuki's IUT proof is not universally accepted as of 2026). Link them to show how similar constraint types (mountains) have different empirical closure conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
