% ============================================================================
% CONSTRAINT STORY: monty_hall_conditional_probability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monty_hall_conditional_probability, []).

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
 *   constraint_id: monty_hall_conditional_probability
 *   human_readable: The Monty Hall Problem (Conditional Probability)
 *   domain: mathematical/probability_theory
 *
 * SUMMARY:
 *   The Monty Hall problem is a mathematical constraint that demonstrates how
 *   conditional probability restructures the sample space. A contestant
 *   chooses one of three doors; the host (knowing where the prize is) opens
 *   one of the other two doors, revealing a goat. The contestant is offered
 *   the option to switch. The mathematical fact is invariant: switching
 *   doubles the probability of winning from 1/3 to 2/3. This is not a matter
 *   of institutional enforcement, convention, or opinion — it is a logical
 *   necessity that emerges from the combinatorial structure of the problem.
 *   The Monty Hall problem exemplifies a pure mountain constraint: it
 *   exhibits zero degrees of freedom, complete accessibility collapse
 *   (mathematical proof is transparent to any sufficiently educated agent),
 *   minimal resistance (the proof cannot be defeated by social organization
 *   or institutional pressure), and emerges naturally from the axioms of
 *   probability theory. The cognitive difficulty humans experience — the
 *   near-universal initial intuition that switching makes no difference —
 *   represents not a weakness in the constraint but a gap between the
 *   constraint's logical structure and human probability intuition. The
 *   constraint itself is unambiguous and unchangeable; human understanding of
 *   it may evolve, but the constraint does not.
 *
 * KEY AGENTS:
 *   - The Contestant: Agent making the decision (powerless/analytical) — faces the constraint but cannot change its underlying structure
 *   - The Host: Agent revealing information (organized/analytical) — implements the constraint but does not create or alter its mathematical basis
 *   - The Mathematician: Agent proving the theorem (analytical/analytical) — observes and formalizes the constraint
 *   - The Cognitive Psychologist: Agent studying intuition failure (analytical/analytical) — investigates the gap between constraint and human cognition
 *   - The Logical Universe: Structural baseline (analytical/analytical) — the constraint exists independent of any observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monty_hall_conditional_probability, 0.08).
domain_priors:suppression_score(monty_hall_conditional_probability, 0.02).
domain_priors:theater_ratio(monty_hall_conditional_probability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, extractiveness, 0.08).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monty_hall_conditional_probability, mountain).
narrative_ontology:human_readable(monty_hall_conditional_probability, "The Monty Hall Problem (Conditional Probability)").
narrative_ontology:topic_domain(monty_hall_conditional_probability, "mathematical/probability_theory").

domain_priors:emerges_naturally(monty_hall_conditional_probability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTESTANT (MOUNTAIN) — Even a contestant with no mathematical background faces an immutable logical structure. The contestant cannot escape the mathematical reality that switching doubles their probability of winning regardless of their intuition. The constraint is not enforcement but logical necessity. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICIAN (MOUNTAIN) — From the standpoint of formal probability theory, conditional probability is a logical law. The Monty Hall problem demonstrates Bayes' theorem operating on a concrete problem. Once the host reveals a goat, the posterior probability that the original choice was correct remains 1/3, while the unchosen remaining door has probability 2/3. This is not contingent on the observer or context — it is a necessary truth. d≈0.00, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: GAME SHOW PRODUCER (MOUNTAIN) — Regardless of whether the host knows where the prize is, the mathematical structure is invariant. If the host always reveals a goat and offers the switch, contestants who switch win 2/3 of the time. This is empirically testable and universally reproducible. No institutional leverage can change this ratio — it emerges from the combinatorial structure of the problem. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: EPISTEMOLOGIST (MOUNTAIN) — The constraint is deeper: new information (a goat revealed) restructures the probability space. This is not a social convention or institutional artifact but a fundamental property of how information updates probability. Even if all human institutions disappeared, the relationship between posterior and prior probability would remain. d≈0.73, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monty_hall_conditional_probability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, ExtMetricName, E),
    domain_priors:suppression_score(monty_hall_conditional_probability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monty_hall_conditional_probability),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monty_hall_conditional_probability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Monty Hall problem exhibits the lowest extractiveness because no agent can bend the probability distribution to their advantage through social means. The mathematical truth is invariant across all instantiations. The small non-zero value (0.08 rather than 0.00) reflects minimal measurement overhead — translating the abstract constraint into specific choice situations requires clarity of problem statement. Suppression (0.02): Negligible. Once the problem is stated, there is almost no coercion or suppression. Agents can understand the solution through pure reasoning. The minimal non-zero value reflects only the unavoidable cognitive friction of learning unfamiliar mathematical ideas. Theater ratio (0.15): Very low. The constraint has minimal performative content. A contestant either switches or doesn't; the mathematical outcome determines the result independent of ritual, ceremony, or institutional performance. The slight theater content (0.15 rather than 0.00) reflects the pedagogical theater of teaching the problem — how it is presented to students, the narrative framing of the puzzle, and the social context in which it is learned may include some performance element, but this is epistemologically separable from the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the Monty Hall problem from any mathematically coherent viewpoint. The mountain classification is uniform across all perspectives because the constraint is a logical truth, not an institutional fact. A contestant, a mathematician, a game show producer, and an epistemologist all face the same probability distribution — switching wins 2/3 of the time. The appearance of a gap emerges from cognitive bias (the contestant initially believes switching is irrelevant) and language ambiguity (different problem phrasings can yield subtly different mathematical structures), but these are failures of cognition and communication, not alternative valid observations of the constraint itself. Once the problem is precisely specified and understood, all perspectives converge on the same classification: mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is largely undefined in a pure mountain constraint because there is no asymmetric extraction or beneficiary/victim relationship. All agents face the same logical structure. The minimal d values derived for each perspective reflect the absence of power asymmetry: even a powerless contestant cannot be exploited through the mathematical constraint itself. The analyst's d≈0.72-0.73 reflects only that the analytical perspective is observing rather than participating — there is no extortion or suppression, only transparent logical necessity. No agent benefits from the constraint's existence, and no agent bears an unfair cost. The constraint simply is.
 *
 * MANDATROPHY ANALYSIS:
 *   The Monty Hall problem resolves the mandatrophy trivially: it is a pure mountain, not a hidden snare masquerading as coordination. The constraint exhibits zero extractiveness (0.08), zero suppression (0.02), and universal agreement across all perspectives. There is no risk that calling it a mountain conceals institutional exploitation, because the mathematical structure is fully transparent. The mandatrophy is not a concern here because there is nothing hidden. The cognitive illusion (the near-universal human error) is a separate phenomenon — it reflects human cognition, not institutional extraction. The mathematical constraint itself is clean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    host_knowledge_assumption,
    'Does the Monty Hall probability constraint depend on the assumption that the host knows where the prize is?',
    'Formal proof that switching probability equals 2/3 regardless of host knowledge if host always reveals a goat; empirical verification with random host vs informed host',
    'If dependent: the constraint is partially contingent on institutional knowledge. If independent: the constraint is a pure logical property of the problem structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(host_knowledge_assumption, empirical, 'Whether host knowledge is required for the 2/3 switching probability').

omega_variable(
    problem_statement_ambiguity,
    'Does the classical Monty Hall formulation uniquely specify the probability distribution, or does problem statement ambiguity permit multiple mathematically valid interpretations?',
    'Rigorous enumeration of all possible problem instantiations consistent with the colloquial statement; proof of which interpretations yield 2/3 vs other ratios',
    'If unique: the constraint is pure logic. If multiple valid interpretations: the constraint is partially linguistic/conventional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(problem_statement_ambiguity, conceptual, 'Whether problem ambiguity permits multiple valid mathematical interpretations').

omega_variable(
    cognitive_accessibility_gap,
    'Why do intelligent reasoners consistently misjudge the Monty Hall problem despite its logical simplicity, and what does this reveal about the relationship between mathematical constraint and cognitive bias?',
    'Cognitive science analysis of the mental model mismatch; investigation of whether the ''illusion'' is a feature of human reasoning or an artifact of problem presentation',
    'If cognitive bias is systematic: the constraint exists in mathematics but is obscured by cognition. If presentation artifact: reformulating the problem may make the constraint transparent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_accessibility_gap, empirical, 'Why the Monty Hall problem is cognitively difficult despite mathematical simplicity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monty_hall_conditional_probability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mh_tr_t0, monty_hall_conditional_probability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mh_tr_t50, monty_hall_conditional_probability, theater_ratio, 50, 0.1).
narrative_ontology:measurement(mh_tr_t100, monty_hall_conditional_probability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(mh_be_t0, monty_hall_conditional_probability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mh_be_t50, monty_hall_conditional_probability, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(mh_be_t100, monty_hall_conditional_probability, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monty_hall_conditional_probability, information_standard).
narrative_ontology:affects_constraint(monty_hall_conditional_probability, bayes_theorem_fundamental).
narrative_ontology:affects_constraint(monty_hall_conditional_probability, conditional_probability_axioms).

% DUAL FORMULATION NOTE:
% The Monty Hall problem is a downstream application of conditional probability and Bayes' theorem. The upstream constraints are the axioms of probability theory itself (conditional_probability_axioms, ε≈0.05, mountain). The Monty Hall problem (ε≈0.08, mountain) applies these axioms to a concrete discrete problem. A third constraint, bayes_theorem_fundamental (ε≈0.06, mountain), is a more general form. All three form a mountain family with near-identical ε values, linked by logical dependency, not by observable-dependent measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
