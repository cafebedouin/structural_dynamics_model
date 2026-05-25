% ============================================================================
% CONSTRAINT STORY: collatz_conjecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collatz_conjecture, []).

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
 *   constraint_id: collatz_conjecture
 *   human_readable: The Collatz Conjecture: A Mathematical Natural Law Candidate
 *   domain: mathematics/number_theory
 *
 * SUMMARY:
 *   The Collatz Conjecture is a deceptively simple mathematical claim: for
 *   any positive integer, apply the following procedure — if even, divide by
 *   2; if odd, multiply by 3 and add 1 — and repeat. The conjecture states
 *   that this process always terminates at 1, regardless of the starting
 *   integer. Despite decades of computational verification (tested for
 *   integers up to 2^68), no proof exists, nor does a counterexample. The
 *   constraint it represents is not institutional, economic, or social — it
 *   is a logical structure embedded in the mathematics itself. The Collatz
 *   Conjecture exhibits all the hallmarks of a mountain constraint: (1) It
 *   emerges naturally from the definition of the algorithm and cannot be
 *   circumvented by organizational, political, or economic intervention. (2)
 *   Its accessibility collapse is extreme (0.92) — the statement is
 *   accessible to any person with high school arithmetic, yet the proof
 *   remains inaccessible to the global mathematical research community
 *   despite enormous effort. (3) Its resistance is minimal (0.05) — no one
 *   disputes the problem statement or argues for alternative framings; the
 *   structure is universally accepted. (4) Base extractiveness is nearly zero
 *   (0.08) — solving Collatz provides no systematic advantage to any party;
 *   the problem is not a mechanism of control or extraction. (5) Suppression
 *   is negligible (0.02) — there are no barriers preventing anyone from
 *   working on the problem; no cartel or gatekeeper controls access to the
 *   conjecture itself.
 *
 * KEY AGENTS:
 *   - Individual mathematicians (powerless/trapped): Cannot escape the logical constraint; truth-value is independent of individual effort
 *   - Mathematical research community (organized/mobile): Can pool resources and develop techniques but cannot alter the fundamental constraint
 *   - Mathematical institutions (institutional/arbitrage): Can fund research or ignore the problem but cannot change the constraint's structure
 *   - Computational verification infrastructure (institutional/arbitrage): Can extend verified range but cannot prove or disprove the conjecture
 *   - Analytical observer (analytical/analytical): Sees the constraint as a pure mathematical fact independent of all contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collatz_conjecture, 0.08).
domain_priors:suppression_score(collatz_conjecture, 0.02).
domain_priors:theater_ratio(collatz_conjecture, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collatz_conjecture, extractiveness, 0.08).
narrative_ontology:constraint_metric(collatz_conjecture, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(collatz_conjecture, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collatz_conjecture, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(collatz_conjecture, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collatz_conjecture, mountain).
narrative_ontology:human_readable(collatz_conjecture, "The Collatz Conjecture: A Mathematical Natural Law Candidate").
narrative_ontology:topic_domain(collatz_conjecture, "mathematics/number_theory").

domain_priors:emerges_naturally(collatz_conjecture).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PROBLEM-SOLVER (MOUNTAIN) — An individual mathematician confronting the Collatz Conjecture experiences an immutable logical structure. The conjecture's truth or falsity is not a function of their power, resources, or institutional position. The mathematical universe does not negotiate. The constraint is universal and unchangeable from all observational positions.
constraint_indexing:constraint_classification(collatz_conjecture, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ORGANIZED RESEARCH COMMUNITY (MOUNTAIN) — Even collectively, mathematicians cannot vote the Collatz Conjecture into or out of existence. The community can organize around the problem, pool resources, and develop new techniques, but the fundamental constraint remains: either all positive integers eventually reach 1 under the stated operations, or they do not. This is not a coordination problem or an extractive arrangement. It is a mathematical fact waiting to be discovered.
constraint_indexing:constraint_classification(collatz_conjecture, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL INSTITUTION (MOUNTAIN) — Universities, journals, and funding agencies can support or ignore Collatz research, but they cannot change the underlying constraint. The conjecture's structure is independent of institutional arrangement. No negotiation, incentive structure, or reorganization of mathematical labor can alter whether the proposition is true.
constraint_indexing:constraint_classification(collatz_conjecture, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the position of complete structural analysis, the Collatz Conjecture is a natural law of mathematical logic. It makes a precise claim about the behavior of a simple deterministic algorithm applied to positive integers. The claim is either true or false independent of any observer, measurement method, or analytical context. This is the definitive mountain classification.
constraint_indexing:constraint_classification(collatz_conjecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collatz_conjecture_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(collatz_conjecture, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collatz_conjecture, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(collatz_conjecture, ExtMetricName, E),
    domain_priors:suppression_score(collatz_conjecture, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(collatz_conjecture),
    narrative_ontology:constraint_metric(collatz_conjecture, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(collatz_conjecture, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(collatz_conjecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Collatz Conjecture classifies as mountain because: (1) EXTRACTIVENESS = 0.08: Solving or failing to solve Collatz provides no systematic extraction mechanism. No party systematically benefits from keeping the conjecture unsolved, nor does any party bear systematic costs from its remaining open. The extractiveness value reflects only the minimal 'cost' of sustained research effort without commensurate coordination benefit. (2) SUPPRESSION = 0.02: No external forces prevent research on Collatz. No gatekeeper controls access. No legal, economic, or social barriers restrict who can work on the problem. The minimal value reflects only the natural asymmetry of talent and opportunity (not all mathematicians have the ability or resources to contribute). (3) THEATER_RATIO = 0.15: The constraint has minimal performative content. Verification of Collatz behavior up to 2^68 is genuine verification, not theater. The mathematical statement is transparent — there is nothing hidden behind institutional ritual or ceremonial validation. (4) EMERGES_NATURALLY = true: The constraint is purely formal. It arises from the mathematical structure, not from social construction, institutional arrangement, or choice. (5) ACCESSIBILITY_COLLAPSE = 0.92: The statement is maximally accessible (anyone with basic arithmetic understands it), yet the truth is maximally inaccessible (the global research community cannot prove it). This 0.90+ gap is diagnostic for mountains. (6) RESISTANCE = 0.05: The problem statement is universal and non-negotiable. There are no alternative framings, no political movements to redefine the conjecture, no attempts to contest its logical structure. This minimal resistance confirms natural law status.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on mountain classification with zero perspectival gap. This is the diagnostic signature of a true natural law constraint. An individual mathematician sees the conjecture as immutable; an organized community sees it as immutable; an institution sees it as immutable; the analytical observer sees it as immutable. The uniformity across all contexts confirms that the constraint is not an artifact of observation position but a genuine feature of the mathematical universe. When all six DR types would collapse to a single classification, and that classification is mountain, the constraint has demonstrated maximal natural law status.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no beneficiary/victim relationship. The Collatz Conjecture does not create asymmetric extraction — solving it benefits no particular group at others' expense, nor does remaining unsolved systematically harm any population. The constraint is symmetric with respect to all observers. The d parameter defaults to the analytical canonical value (0.73) but this is cosmetic — the constraint's mountain status is independent of d because base extractiveness is so low that even high f(d) values cannot move chi above mountain thresholds.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: This constraint resolves mandatrophy trivially by being an actual natural law. There is no hidden extraction mechanism masquerading as coordination, no negotiable social arrangement disguised as immutable law. The Collatz Conjecture is the benchmark case: all six types collapse to mountain because the constraint is purely formal and independent of observer position. The mandatrophy resolution is automatic — there are no alternative institutional readings, no extractive cover stories, no perspectival gaps to analyze. The absence of mandatrophy is itself the diagnostic confirmation of true mountain status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decidability_boundary,
    'Could the Collatz Conjecture be undecidable within standard arithmetic axioms (Peano Arithmetic or ZFC)?',
    'Formal proof of independence from PA or ZFC; demonstration that the conjecture cannot be proven or disproven from the axiom set',
    'If undecidable: the constraint shifts from ''immutable mathematical fact'' to ''unprovable statement'' — classification remains mountain (accessibility collapse persists due to logical unprovability), but the epistemic status changes. If decidable: confirms mountain classification with full naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decidability_boundary, empirical, 'Whether Collatz is decidable or independent from standard axioms').

omega_variable(
    computational_complexity_threshold,
    'Is there a mathematical obstruction preventing efficient algorithms from solving Collatz, or is solution blockage purely computational?',
    'Proof of computational hardness class; demonstration that no polynomial-time algorithm can exist for deciding Collatz, or discovery of such an algorithm',
    'If hardness proven: computational barrier is a genuine mathematical limit, reinforcing mountain status. If algorithm discovered: the constraint remains a mountain but its epistemic status shifts from ''unsolved'' to ''solved'' — no change to classification but major change to knowledge state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_complexity_threshold, empirical, 'Computational complexity barrier for Collatz problem').

omega_variable(
    axiom_system_dependence,
    'Do different axiom systems (constructive logic, intuitionistic logic, classical logic) yield different truth values for the Collatz Conjecture?',
    'Formal derivations in alternate logical frameworks; determination of whether the conjecture''s truth is invariant across axiom systems',
    'If invariant: confirms universal mountain status. If dependent on axiom choice: the constraint is mathematically real but relative to a logical framework — still mountain (the framework itself is unchangeable), but with reduced universality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_system_dependence, conceptual, 'Axiom system dependence of Collatz truth value').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collatz_conjecture, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% The Collatz Conjecture has no dual formulation or constraint family. It is a singular mathematical claim that does not decompose into structurally distinct sub-constraints. Unlike the BGS conjecture (which separates into spectral universality and eigenvector thermalization), Collatz makes one claim about one algorithm and admits no meaningful decomposition along observable-dependent lines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
