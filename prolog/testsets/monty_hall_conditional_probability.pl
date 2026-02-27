% ============================================================================
% CONSTRAINT STORY: monty_hall_conditional_probability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The Monty Hall problem exemplifies a pure mathematical constraint: a
 *   logical structure that determines outcomes regardless of the observer's
 *   power, time horizon, exit options, or spatial scope. The constraint
 *   arises from the interaction of three elements: (1) initial uniform
 *   distribution over three equally probable door locations, (2) the host's
 *   knowledge of the prize location and commitment to reveal a non-winning
 *   door, and (3) the contestant's ability to observe which door the host
 *   opened and to update their beliefs via Bayes' theorem. The posterior
 *   probability of winning by switching is 2/3, derived necessarily from
 *   conditional expectation. This is not a social contract, institutional
 *   arrangement, or coordination mechanism — it is a fact about information
 *   and probability. The constraint is invariant across all observables and
 *   measurement methodologies that preserve the problem structure. Changing
 *   the setup rules (number of doors, host knowledge, contestant options)
 *   produces a different problem with a different posterior, not a different
 *   measurement of the same constraint.
 *
 * KEY AGENTS:
 *   - The Contestant: Powerless/analytical agent attempting to maximize winning probability through strategic door selection
 *   - The Host: Institutional agent controlling information revelation, bound by the rule to reveal a non-winning door
 *   - The Prize Distribution: The underlying random variable whose structure determines all posterior calculations
 *   - The Mathematical System: The logical/formal system in which Bayesian updating occurs — the constraint itself
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

% PERSPECTIVE 1: THE CONFUSED CONTESTANT — A contestant selecting one of three doors faces irreducible mathematical constraint. The probabilities are determined by conditional expectation given the host's action. No amount of social power, institutional authority, or temporal advantage can alter the mathematical structure. The constraint appears as immutable natural law.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MATHEMATICAL ANALYST — From the perspective of formal probability theory, the Monty Hall constraint is a mountain: the posterior probability P(prize behind door 1 | host opened door 3) = 2/3 follows necessarily from Bayes' theorem. This is not a social constraint, institutional arrangement, or contingent empirical claim. It is a logical necessity derivable from the problem statement.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE GAME SHOW HOST (INSTITUTIONAL VIEW) — Even the host, who controls which door to reveal, cannot escape the mathematical constraint. If the host must reveal a non-winning door (the canonical interpretation), the host's action mechanically alters the information structure. The host has no freedom to change the conditional probability — only to choose which losing door to reveal. The constraint is binding at every level of agency.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE PROBLEM DESIGNER — Even the authority who created the problem cannot alter its logical structure. Changing the setup rules changes the problem itself, not the solution to the original problem. The constraint is independent of power — it persists under all attempts to override it through authority.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerful),
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
 *   Extractiveness (0.08): Extremely low. The constraint imposes no extraction in the classical sense. No agent benefits at another's expense through the constraint — the probability structure is neutral. The value reflects minimal measurement ambiguity in specifying what 'conditional probability' means in a formal system. Suppression (0.02): Minimal. The mathematical system suppresses nothing — it is transparent. The value reflects only notational or pedagogical barriers (human difficulty understanding conditional probability), not structural barriers in the constraint itself. Theater ratio (0.15): Low and increasing slightly over time. The core mathematical structure requires no performative elements — the derivation is direct. The small theater ratio and its slight increase reflect growing pedagogical elaboration: explanations, simulations, and intuitive framings that add scaffolding but don't alter the mathematical fact. The mountain profile (accessibility_collapse ≥ 0.85, resistance ≤ 0.15) is fully satisfied: the constraint cannot be escaped through institutional redesign, social power, or temporal advantage. It is logically rigid.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, there is NO perspectival gap in this constraint. All four perspectives produce the same classification (Mountain) and the same posterior probability (2/3 to switch). The powerless contestant, the analytical mathematician, the institutional host, and the powerful problem designer all face the same logical necessity. This invariance across perspectives is the signature of a true mountain — the constraint does not decompose into beneficiaries and victims, does not vary with agent power or exit options, and does not change under different temporal horizons. If perspectival gaps were to emerge (e.g., if different agents computed different posteriors), that would indicate misspecification of the problem or decomposition into multiple constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The Monty Hall constraint is a gold-standard mountain: it exhibits zero degrees of freedom under all reasonable perspectives. It does not risk mandatrophy (mislabeling as pure extraction or pure coordination) because it has neither extraction nor coordination — it is a pure logical structure. The constraint serves as a diagnostic anchor for the system: if a supposedly complex constraint can be shown to have mountain structure across all perspectives, it has been fully understood mathematically. Conversely, constraints that remain controversial (human intuitions diverge, domain experts disagree) are candidates for decomposition: the label may be hiding multiple constraints with different ε values, different beneficiary structures, or different logical foundations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    host_knowledge_assumption,
    'Is the assumption that the host knows the prize location an essential or contingent feature of the constraint?',
    'Formal analysis of the problem under variants: (a) host knows and never opens the prize, (b) host is ignorant and may open the prize by chance, (c) host has partial information. Compare posterior distributions for each variant.',
    'If essential: the constraint is the logical interdependence of the host''s knowledge and the contestant''s updating. If contingent: the core constraint is Bayesian updating itself, and the host''s knowledge is merely one instantiation. Resolves whether the mountain is about conditional probability or about epistemic dependencies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(host_knowledge_assumption, conceptual, 'Whether host knowledge is essential or contingent to the constraint').

omega_variable(
    interpretation_sensitivity,
    'Do different framings of ''what the host knows'' or ''what the contestant observes'' produce substantively different probability values, or only apparent differences due to notation?',
    'Formal derivation of posterior under all reasonable interpretations: (a) door numbers are arbitrary labels, (b) door positions carry physical significance, (c) the host''s choice reveals information about the initial distribution. Compute chi-squared distance between posterior distributions.',
    'If only notational: the constraint is fully robust — a true mountain. If substantive differences: multiple conditional probability structures are lurking in the problem formulation, suggesting decomposition into separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_sensitivity, conceptual, 'Sensitivity of posterior probability to problem interpretation').

omega_variable(
    empirical_replication_fidelity,
    'Why do human intuitions so strongly violate the mathematical posterior? Is this a failure of intuitive reasoning or a sign that humans are solving a different problem (e.g., one with asymmetric priors or different information states)?',
    'Cognitive science experiments with varied problem framings: (a) original Monty Hall, (b) 100-door variant, (c) explicit Bayesian updating instructions, (d) visual/interactive simulations. Measure convergence to 2/3 posterior under each framing.',
    'If failure of reasoning: the constraint is mathematically rigid but empirically fragile (humans don''t integrate information correctly). If different problem: humans are implicitly solving under different assumptions (e.g., uncertainty about host intentions), and the ''constraint'' is actually an ambiguity in the problem specification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_replication_fidelity, empirical, 'Why human intuitions violate the mathematical posterior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monty_hall_conditional_probability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mh_tr_t0, monty_hall_conditional_probability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mh_tr_t50, monty_hall_conditional_probability, theater_ratio, 50, 0.15).
narrative_ontology:measurement(mh_tr_t100, monty_hall_conditional_probability, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(mh_be_t0, monty_hall_conditional_probability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mh_be_t50, monty_hall_conditional_probability, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(mh_be_t100, monty_hall_conditional_probability, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monty_hall_conditional_probability, information_standard).
narrative_ontology:affects_constraint(monty_hall_conditional_probability, bayes_theorem_formal_derivation).
narrative_ontology:affects_constraint(monty_hall_conditional_probability, conditional_expectation_mathematical_structure).

% DUAL FORMULATION NOTE:
% The Monty Hall problem is a canonical application of Bayes' theorem to a finite probability space. It affects the comprehension and application of Bayesian updating in decision-making under uncertainty. The constraint family includes (1) Bayes' theorem itself (the mathematical foundation, ε ≈ 0.05, Mountain), (2) the Monty Hall problem (the well-known pedagogical exemplar, ε ≈ 0.08, Mountain), and (3) empirical decision-making under uncertainty (how humans actually update beliefs, ε ≈ 0.35, Tangled Rope or Snare depending on institutional context). This story focuses on the mathematical constraint; the downstream stories would address the cognitive and institutional barriers to applying it correctly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
