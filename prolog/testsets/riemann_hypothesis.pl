% ============================================================================
% CONSTRAINT STORY: riemann_hypothesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_riemann_hypothesis, []).

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
 *   constraint_id: riemann_hypothesis
 *   human_readable: The Riemann Hypothesis as Mathematical Constraint
 *   domain: pure_mathematics/number_theory/analytical_structures
 *
 * SUMMARY:
 *   The Riemann Hypothesis is a mathematical constraint statement: that all
 *   non-trivial zeros of the Riemann zeta function lie on the critical line
 *   Re(s) = 1/2. This constraint exemplifies a pure mountain-type
 *   mathematical fact — it is independent of human knowledge, institutional
 *   belief, computational capacity, or social negotiation. The constraint's
 *   truth value exists prior to and independent of human discovery. Since the
 *   hypothesis was formulated in 1859, no agent has been able to alter,
 *   circumvent, or negotiate with the underlying mathematical reality. The
 *   constraint produces zero degrees of freedom for any observer across any
 *   time horizon or spatial scope because its truth is not dependent on
 *   observation, measurement, or institutional arrangement. The constraint's
 *   accessibility is maximally collapsed: there is no 'easier' version of the
 *   problem, no workaround, and no alternative formulation that preserves the
 *   problem's structure while reducing its difficulty. The resistance to the
 *   constraint (the difficulty of proving it) is real but distinct from the
 *   constraint itself — the constraint is immutable even though human access
 *   to its proof remains (as of 2026) absent.
 *
 * KEY AGENTS:
 *   - Analytical Mathematics: Discovers or contemplates the constraint structure (analytical/analytical) — the pure mathematical observer
 *   - Individual Number Theorists: Attempt to resolve the hypothesis (powerless/trapped) — constrained by mathematical truth regardless of effort
 *   - Mathematics Institutions: Allocate resources toward the problem (institutional/arbitrage) — can choose where to direct effort but cannot alter the constraint itself
 *   - Computational Verification Systems: Extend empirical bounds on zero location (analytical/analytical) — contribute data without changing the underlying mathematical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(riemann_hypothesis, 0.12).
domain_priors:suppression_score(riemann_hypothesis, 0.02).
domain_priors:theater_ratio(riemann_hypothesis, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(riemann_hypothesis, extractiveness, 0.12).
narrative_ontology:constraint_metric(riemann_hypothesis, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(riemann_hypothesis, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(riemann_hypothesis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(riemann_hypothesis, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(riemann_hypothesis, mountain).
narrative_ontology:human_readable(riemann_hypothesis, "The Riemann Hypothesis as Mathematical Constraint").
narrative_ontology:topic_domain(riemann_hypothesis, "pure_mathematics/number_theory/analytical_structures").

domain_priors:emerges_naturally(riemann_hypothesis).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — The Riemann Hypothesis is a statement about the distribution of zeros of the zeta function. Its truth or falsehood is independent of human knowledge, institutional belief, or computational resources. The constraint emerges from the mathematical structure itself: if the hypothesis is true, then prime numbers distribute according to a specific regularity pattern; if false, they exhibit a different structure. This is not a convention, a coordinate choice, or an institutional arrangement. It is a mathematical fact whose truth value exists independently of whether anyone can prove it. The constraint's zero degrees of freedom derive from the logical necessity of the mathematical statement — either the zeros lie on the critical line or they do not.
constraint_indexing:constraint_classification(riemann_hypothesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: NUMBER THEORIST (MOUNTAIN) — The individual researcher confronts the Riemann Hypothesis as an immutable mathematical constraint. No amount of effort, funding, or organizational support changes the fact that either the hypothesis is true or it is false. The researcher cannot 'negotiate' with the constraint, cannot reframe it, cannot choose to work around it without accepting the consequences (alternate prime number theorems, different asymptotic behaviors). The constraint is not extractive because it does not favor any agent — it constrains all equally. The researcher's powerlessness is universal: they are trapped by mathematical truth, not by institutional extraction.
constraint_indexing:constraint_classification(riemann_hypothesis, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICS RESEARCH ESTABLISHMENT (MOUNTAIN) — Institutions (universities, funding agencies, mathematics societies) confront the Riemann Hypothesis as a fixed target that neither their resources nor organizational power can circumvent. Funding allocations, prize announcements, or research priority directives do not change the mathematical reality. The institution has arbitrage options in how to allocate resources across other problems, but the constraint itself — the truth value of the hypothesis — remains invariant. The constraint is a mountain from the institutional perspective because it is equally immovable regardless of institutional power or negotiating position.
constraint_indexing:constraint_classification(riemann_hypothesis, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: CONTEMPORARY COMPUTATIONAL VERIFICATION (MOUNTAIN) — Modern computational verification has checked the Riemann Hypothesis to ~10^13 zeros. These checks do not prove the hypothesis but provide empirical constraint on possible counterexamples. The constraint operates at the immediate time horizon through computational exploration: the hypothesis makes a falsifiable prediction (all zeros on the critical line), and empirical data either support or contradict it. The mountain status holds because computational verification discovers mathematical truth rather than constructing it — no amount of additional computation creates degrees of freedom in the hypothesis itself. The truth value remains fixed and independent of the measurement.
constraint_indexing:constraint_classification(riemann_hypothesis, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(riemann_hypothesis_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(riemann_hypothesis, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(riemann_hypothesis, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(riemann_hypothesis, ExtMetricName, E),
    domain_priors:suppression_score(riemann_hypothesis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(riemann_hypothesis),
    narrative_ontology:constraint_metric(riemann_hypothesis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(riemann_hypothesis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(riemann_hypothesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint contains no extraction mechanism because no agent benefits asymmetrically from the constraint's truth. If the hypothesis is true, all of mathematics benefits equally from the consequent regularities in prime distribution — no agent captures monopoly value. If false, all of mathematics confronts the same problem of explaining why the counterexample exists. The small nonzero value (0.12) rather than zero reflects measurement uncertainty: the hypothesis's mathematical structure has been discussed and researched for 165+ years, and institutional arrangements (funding priorities, prestigious prizes) have emerged around it. These institutional features are not extraction by the constraint but extraction within the problem-solving ecosystem around the constraint. The core constraint itself is extractive-neutral. Suppression (0.02): Negligible. The constraint permits complete transparency — the mathematical statement is publicly known, its logical consequences are derivable by anyone with sufficient training, and the problem is fundamentally open to human scrutiny. There are no barriers to thinking about the hypothesis or publishing results on it (beyond standard academic gatekeeping, which is orthogonal to the constraint itself). Theater ratio (0.15): Minimal. The hypothesis is not performative. Its truth or falsehood is not susceptible to reframing, narrative management, or institutional theater. Mathematical proofs either work or they do not; computational verification either extends the verified region or it does not. The small nonzero value reflects that mathematical research funding and prestige mechanisms create some performative framing around the problem, but the core constraint is remarkably theater-free — perhaps the lowest theater ratio of any constraint in the corpus.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the mountain classification. This uniformity is diagnostic: it indicates that the constraint's structure is genuinely invariant across observer positions. The analytical observer and the number theorist both see a mathematical necessity that cannot be negotiated with. The institution sees a fixed target, not a manipulable problem. The computational perspective discovers facts about the hypothesis without creating degrees of freedom. The absence of perspectival gap — all perspectives yielding the same type — is characteristic of natural-law constraints. This contrasts sharply with constraints that are mountains from one perspective (powerless agent seeing immutable extraction) but ropes from another (beneficiary seeing coordination). Here, all see an immutable mathematical fact.
 *
 * DIRECTIONALITY LOGIC:
 *   The Riemann Hypothesis does not have beneficiaries or victims in the structural sense. No agent benefits from the hypothesis being true; no agent suffers extraction from the hypothesis being false. The constraint's structure is symmetric across all agents — it is equally binding on the number theorist, the institution, the computational system, and the abstract mathematical universe. This symmetry is a strong indicator of the mountain type: the constraint distributes its force equally across all positions, creating zero asymmetry in who bears the cost. The directional analysis yields d ≈ 0.5 (symmetric position) for all agents, which in the canonical mapping produces institutional power equivalence and renders the constraint as a shared fixed target rather than an extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy trivially: it is mountain from all perspectives, and no mislabeling is possible. The constraint does not risk being confused with coordination (rope) because it has no coordination function — agents do not solve a collective action problem through the hypothesis, they confront a mathematical fact. It does not risk being confused with extraction (tangled rope, snare, scaffold) because there is no asymmetry in who bears the cost of the constraint or who benefits from its resolution. The mandatrophy is not 'how do we classify this correctly?' but rather 'why is this so different from all other constraints in the corpus?' The answer: it is a pure mathematical necessity with no institutional, extractive, or coordination layer. The Riemann Hypothesis is a baseline exemplar of what a genuine natural-law constraint looks like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_status_of_undecidability,
    'Is the Riemann Hypothesis mathematically undecidable within ZFC (Zermelo-Fraenkel set theory with Choice)?',
    'Gödel-completeness analysis, proof of independence from ZFC axioms, or construction of a ZFC model in which the hypothesis is false',
    'If undecidable: the constraint becomes a boundary case — mountain in structure but with internal indeterminacy. The hypothesis is not ''false'' but unprovable within the axiom system. If decidable: standard mountain classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_status_of_undecidability, conceptual, 'Whether RH is independent of ZFC axioms').

omega_variable(
    computational_verification_asymptote,
    'Does computational verification of ~10^13 zeros provide meaningful constraint on possible counterexamples, or does the hypothesis remain effectively undetermined by finite computation?',
    'Analysis of tail distribution of zeros, heuristic arguments for counterexample location if they exist, comparison with other number-theoretic conjectures settled or refuted by computation',
    'If verification provides strong heuristic confidence: effective constraint strength increases. If verification is asymptotically uninformative: computational perspective contributes minimal constraint force, and mountain classification persists only through theoretical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_verification_asymptote, empirical, 'Information content of computational zero verification').

omega_variable(
    mathematical_necessity_vs_contingency,
    'Is mathematical truth a discovery of objective necessity or a convention emerging from axiom systems humans chose?',
    'Philosophical analysis of mathematical ontology, comparison of outcomes across different axiomatic frameworks, investigation of whether alternative mathematics would yield different Riemann-type constraints',
    'If necessity: mountain classification is fundamental — the constraint is immutable across all possible mathematical universes. If contingency: the constraint is relative to the chosen axiom system, and the mountain status is perspectival rather than absolute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mathematical_necessity_vs_contingency, conceptual, 'Ontological status of mathematical truth').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(riemann_hypothesis, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rh_tr_t0, riemann_hypothesis, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rh_tr_t50, riemann_hypothesis, theater_ratio, 50, 0.1).
narrative_ontology:measurement(rh_tr_t100, riemann_hypothesis, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(rh_be_t0, riemann_hypothesis, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(rh_be_t50, riemann_hypothesis, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(rh_be_t100, riemann_hypothesis, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(riemann_hypothesis, information_standard).
narrative_ontology:affects_constraint(riemann_hypothesis, prime_number_distribution).
narrative_ontology:affects_constraint(riemann_hypothesis, dirichlet_class_number_conjecture).
narrative_ontology:affects_constraint(riemann_hypothesis, generalised_riemann_hypothesis).

% DUAL FORMULATION NOTE:
% The Riemann Hypothesis is the foundational constraint in a family of number-theoretic claims about zero distribution in L-functions. Related constraints (prime number distribution, generalized Riemann hypothesis, Dirichlet class number conjecture) are mathematically downstream: their truth or falsehood depends on the Riemann Hypothesis being resolved. These form a constraint family linked by mathematical implication rather than institutional relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
