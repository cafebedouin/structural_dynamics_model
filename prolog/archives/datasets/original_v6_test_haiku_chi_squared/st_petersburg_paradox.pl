% ============================================================================
% CONSTRAINT STORY: st_petersburg_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_st_petersburg_paradox, []).

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
 *   constraint_id: st_petersburg_paradox
 *   human_readable: St. Petersburg Paradox: Divergent Expected Value in Infinite-Tailed Distributions
 *   domain: mathematical_economics/probability_theory
 *
 * SUMMARY:
 *   The St. Petersburg Paradox (first formulated by Nicolaus Bernoulli in
 *   1713, published by Daniel Bernoulli in 1738) is a mathematical
 *   constraint, not a social or institutional one. A game is defined: a fair
 *   coin is flipped repeatedly. If it shows heads on the nth flip, the player
 *   receives 2^n rubles and the game ends. What is the fair price for the
 *   right to play this game? Classical expected value theory says E[X] = Σ
 *   (1/2)^n × 2^n = Σ 1 = ∞. Yet empirical observation and rational actors
 *   consistently refuse to pay more than a modest sum (typically $10-40 in
 *   modern surveys). This constraint — the divergence of expected value in
 *   infinite-tailed distributions — is one of the purest examples of a
 *   mathematical mountain: it emerges inevitably from the structure of
 *   probability measure and geometric series, it admits no workaround for any
 *   observer, and it cannot be suppressed or evaded by any institutional
 *   arrangement. The paradox is 'paradoxical' only because classical expected
 *   utility theory appears to recommend infinitely high willingness-to-pay,
 *   contradicting observed behavior. The resolution is not that the math is
 *   wrong, but that expected value maximization is not the right decision
 *   criterion for this game — bounded utility, entropy considerations, or
 *   information-theoretic constraints provide the true ground.
 *
 * KEY AGENTS:
 *   - Mathematicians: Analytical observers (analytical/analytical) — derive and confirm the divergence; no exit from the constraint
 *   - Classical Economists: Institutional stakeholders (institutional/arbitrage) — attempt to price the game under EU theory; face the paradox
 *   - Empirical Researchers: Moderate observers (moderate/mobile) — document that humans refuse to pay high prices; identify bounded rationality as the relevant constraint
 *   - Insurance/Betting Industry: Institutional actors (institutional/arbitrage) — cannot price the game at EV; must use alternatives; face capital finiteness as a hard constraint
 *   - Economics Education: Organized community (organized/constrained) — uses the paradox as a teaching tool to motivate utility theory, bounded rationality, and decision-making refinements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(st_petersburg_paradox, 0.15).
domain_priors:suppression_score(st_petersburg_paradox, 0.02).
domain_priors:theater_ratio(st_petersburg_paradox, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(st_petersburg_paradox, extractiveness, 0.15).
narrative_ontology:constraint_metric(st_petersburg_paradox, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(st_petersburg_paradox, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(st_petersburg_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(st_petersburg_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(st_petersburg_paradox, mountain).
narrative_ontology:human_readable(st_petersburg_paradox, "St. Petersburg Paradox: Divergent Expected Value in Infinite-Tailed Distributions").
narrative_ontology:topic_domain(st_petersburg_paradox, "mathematical_economics/probability_theory").

domain_priors:emerges_naturally(st_petersburg_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICIAN'S NATURAL LAW (MOUNTAIN) — From pure probability theory, the divergence of expected value in the St. Petersburg game is a mathematical inevitability. Given a fair coin and doubling payoff rule, E[X] = ∞ by the law of total expectation. This follows from the structure of geometric series with ratio ≥ 1. No observer can escape this constraint; no exit option exists. ε=0.15, suppression=0.02. Emerges naturally from the axioms of probability measure.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RATIONAL BETTOR / CLASSICAL EU THEORY (MOUNTAIN) — An agent committed to maximizing expected utility under the classical von Neumann-Morgenstern axioms has no choice: the fair price of the St. Petersburg game IS infinite (or undefined in finite analysis). The constraint is the axiom system itself. Trapped: cannot violate the axioms without abandoning rational choice theory. d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.21. The paradox is a limit of the framework, not an extraction mechanism.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: EMPIRICAL ECONOMIST / BEHAVIORAL OBSERVATION (MOUNTAIN) — In practice, people consistently refuse to pay more than a few dollars for the St. Petersburg game. This observed behavior reflects an irreducible feature of human decision-making under uncertainty: we use bounded evaluation heuristics (bounded rationality, risk aversion, finite-time horizons). The constraint that human utility functions are bounded or that attention is finite is a natural limit on cognition. No amount of training or incentives can make a human pay infinite value for this game in real-world conditions. ε=0.15, suppression ≈0. This is a mountain of human nature, not a snare.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE/BETTING INDUSTRY (MOUNTAIN) — No insurance company or betting operation can price the St. Petersburg game at its mathematical expected value. They face an absolute ceiling: finite capital reserves. A counterparty cannot be paid $2^n$ if n→∞ and capital is finite. This is not extraction or suppression—it is an immutable constraint of the physical world (finite matter, finite time horizon for institutions). Exit option 'arbitrage' means they can use alternative pricing mechanisms (utility-based, quantile-based, capital-constraint models), but the underlying constraint (divergence of EV) remains. ε=0.15.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MEASURE-THEORETIC ANALYSIS (MOUNTAIN) — In measure theory and functional analysis, divergent integrals are a structural feature of Lebesgue spaces. The St. Petersburg payoff function is integrable only up to some finite bound; beyond that bound, it diverges. This is not a contingent feature of our probability assignment—it follows from the structure of measure spaces themselves. A function with decay slower than any exponential will have infinite integral. ε=0.15, accessibility_collapse=0.92 (the structure is difficult to access conceptually, leading to persistent paradox). Resistance=0.08 (once understood, the constraint is nearly impossible to resist or evade).
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ECONOMICS EDUCATION (ROPE) — The St. Petersburg paradox functions as a coordination device in economic theory pedagogy. It coordinates on the insight that expected value maximization has limits; it teaches bounded rationality, utility theory refinements, and the need for alternative decision frameworks. No extraction occurs—the constraint is purely coordinating (teaching students that divergent payoffs require non-classical evaluation). ε≈0.05, suppression=0, theater≈0.1. Beneficiary: the field of economics. This is a rope perspective, not a mountain, because it reflects institutional coordination rather than mathematical necessity.
constraint_indexing:constraint_classification(st_petersburg_paradox, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(st_petersburg_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(st_petersburg_paradox, ExtMetricName, E),
    domain_priors:suppression_score(st_petersburg_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(st_petersburg_paradox),
    narrative_ontology:constraint_metric(st_petersburg_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(st_petersburg_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(st_petersburg_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The constraint is purely mathematical. There is no agent extracting value from another; the divergence affects all participants symmetrically. It is a limit of the framework, not a mechanism of exploitation. Suppression (0.02): Minimal. The constraint has no suppressive mechanism. No alternatives are blocked; no actors are prevented from using other decision criteria (utility-based, quantile-based, information-theoretic). The suppression score reflects only the cognitive barrier to understanding the limit of classical expected value theory. Theater ratio (0.08): Very low. The paradox is not performative. The mathematics is fully transparent; the game rules are clear; the divergence follows directly from the axioms. No institutional performance or ritual is required to maintain the constraint. The small non-zero value reflects only the fact that pedagogical exposition of the paradox takes effort and can be obfuscated by presentation.
 *
 * PERSPECTIVAL GAP:
 *   Despite being a single mathematical fact, the St. Petersburg Paradox classifies as Mountain from five perspectives and as Rope from one (the education perspective). The gap is not between disagreement on whether the constraint exists, but between whether it is experienced as a law of nature (Mountain) or as a coordination/teaching device (Rope). From the mathematician's view, divergence is an inevitable property of the axioms. From the empirical economist's view, bounded utility is an inevitable property of human cognition and physics. From the insurance industry's view, finite capital is an inevitable property of the physical world. From the education perspective, the paradox is a useful tool for teaching, not a suppressive force. All six perspectives confirm the mathematical fact; they differ only in whether the constraint is experienced as limitative (Mountain) or coordinating (Rope). This is a rare case where the perspectival gap is purely about framing, not about genuine disagreement on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the St. Petersburg Paradox is a constraint on a hypothetical game, not on agents with conflicting interests. The divergence of expected value affects all participants equally — it is not a mechanism of extraction. Any agent who plays the game faces the same mathematical limit. Directionality is therefore not applicable in the standard way. The constraint classifies as Mountain because it imposes an irreducible mathematical limit (E[X] = ∞), not because of any directionality asymmetry. If the paradox were reframed as a game-design constraint (e.g., 'a casino must price this game'), then directionality would apply — the casino would be a victim of the divergence (unable to price fairly), and the player would be a potential beneficiary (unable to be exploited). But the constraint as formulated is about the mathematical fact of divergence, not about game participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The St. Petersburg Paradox resolves mandatrophy by being a pure Mountain — it has no extractive or coordinative function to confuse. The 'paradox' arises because classical expected value theory appears to recommend infinite willingness-to-pay, which contradicts observed behavior. This apparent paradox is resolved by recognizing that expected value maximization is not the correct decision criterion for this game. The resolution is not that the math is wrong (the divergence is mathematically certain), but that the framework of unbounded expected utility is incomplete. Alternative frameworks (bounded utility, quantile-based pricing, information-theoretic constraints) eliminate the paradox by accepting the mathematical limit and working within it. The constraint is a Mountain: no institutional arrangement, incentive structure, or coordinative mechanism can make it go away. A rational actor must either (a) refuse to play, (b) use a bounded utility function, (c) apply a quantile-based pricing rule, or (d) accept the risk of unbounded loss. None of these choices violates the mathematical law; they merely acknowledge it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divergence_vs_convention,
    'Is the divergence of expected value in the St. Petersburg game a mathematical fact or a consequence of the probability measure chosen (the fair coin assumption)?',
    'Formalize the measure space. If we assign non-uniform probabilities to outcomes (e.g., exponentially decaying probabilities), does the expected value remain infinite? If not, the divergence is a feature of the measure, not a mathematical law.',
    'If divergence is measure-independent: Mountain classification holds universally. If measure-dependent: the constraint moves toward Rope or Tangled Rope (the measure choice is a coordination/enforcement mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divergence_vs_convention, conceptual, 'Whether divergence is mathematical fact or measure convention').

omega_variable(
    utility_finiteness_boundary,
    'Is there a formal lower bound on the growth rate of utility functions that preserves rational decision-making? Or is bounded utility a contingent feature of human cognition rather than a mathematical necessity?',
    'Investigate whether axiom systems for rational choice (Von Neumann-Morgenstern, Savage, etc.) can accommodate unbounded utility. If yes: boundedness is a design choice, not a law. If no: boundedness is inherent to rational choice itself.',
    'If boundedness is necessary: the constraint is a mountain of rationality. If contingent: it is a human-side constraint (a piton of evolved cognition), not a mathematical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_finiteness_boundary, conceptual, 'Whether utility boundedness is mathematical necessity or cognitive contingency').

omega_variable(
    physical_realizability_threshold,
    'In a physically realistic game, can we actually construct a mechanism that pays $2^n$ for arbitrary n? Or is there a hard physical limit (entropy, causality, thermodynamics) that makes arbitrarily large payoffs impossible?',
    'Information-theoretic and thermodynamic analysis of the resources required to encode and pay out $2^n. Determine the scaling relationship between n and required matter/energy.',
    'If physical realizability fails for large n: the constraint has a physical component (mountain of physics) separate from the mathematical component. If realizability is merely economically infeasible: the constraint is a human/institutional constraint, not a law of nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_realizability_threshold, empirical, 'Physical feasibility of arbitrarily large payouts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(st_petersburg_paradox, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stpete_tr_t0, st_petersburg_paradox, theater_ratio, 0, 0.05).
narrative_ontology:measurement(stpete_tr_t150, st_petersburg_paradox, theater_ratio, 150, 0.08).
narrative_ontology:measurement(stpete_tr_t300, st_petersburg_paradox, theater_ratio, 300, 0.08).

% Extraction over time
narrative_ontology:measurement(stpete_be_t0, st_petersburg_paradox, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stpete_be_t150, st_petersburg_paradox, base_extractiveness, 150, 0.15).
narrative_ontology:measurement(stpete_be_t300, st_petersburg_paradox, base_extractiveness, 300, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(st_petersburg_paradox, information_standard).
narrative_ontology:affects_constraint(st_petersburg_paradox, expected_utility_theory_limits).
narrative_ontology:affects_constraint(st_petersburg_paradox, bounded_rationality_framework).

% DUAL FORMULATION NOTE:
% The St. Petersburg Paradox can be decomposed into two related constraints: (1) the mathematical divergence of expected value in infinite-tailed distributions (ε≈0.15, Mountain), and (2) the cognitive/institutional constraint that bounded utility is necessary for rational decision-making (ε≈0.20-0.30, potentially Rope or Scaffold depending on whether bounded utility is viewed as a natural law or a design choice). This story addresses constraint (1) — the pure mathematical limit. The downstream constraints address how decision theory accommodates and coordinates around this limit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
