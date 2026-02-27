% ============================================================================
% CONSTRAINT STORY: prisoners_dilemma_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prisoners_dilemma_equilibrium, []).

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
 *   constraint_id: prisoners_dilemma_equilibrium
 *   human_readable: The Prisoner's Dilemma (Nash Equilibrium)
 *   domain: logical/economic
 *
 * SUMMARY:
 *   The Prisoner's Dilemma is a foundational logical structure in game theory
 *   demonstrating why rational individual incentives can produce collectively
 *   suboptimal outcomes. In its canonical form, two suspects are arrested and
 *   interrogated separately. Each faces a choice: confess or remain silent.
 *   If both remain silent, each serves 1 year. If one confesses and the other
 *   remains silent, the confessor goes free and the silent agent serves 3
 *   years. If both confess, each serves 2 years. The payoff structure creates
 *   a dominant strategy equilibrium at mutual confession (mutual defection),
 *   even though mutual silence (mutual cooperation) is Pareto-superior. This
 *   constraint exhibits the full range of DR classifications depending on
 *   observational context and game structure (one-shot vs repeated, symmetric
 *   vs asymmetric, with or without enforcement institutions). The primary
 *   classification is Mountain: the Nash equilibrium property is a logical
 *   theorem about the payoff matrix, not contingent on institutional design,
 *   enforcement, or agent behavior. The constraint emerges necessarily from
 *   the mathematical structure of games satisfying the canonical payoff
 *   ordering.
 *
 * KEY AGENTS:
 *   - Agent 1 (Suspect A): Rational actor (powerless/immediate/trapped/universal) — faces a logical inevitability of defection despite mutual cooperation being superior
 *   - Agent 2 (Suspect B): Rational actor (powerless/immediate/trapped/universal) — symmetric position to Agent 1; both derive the same dominant strategy
 *   - The Payoff Matrix: The logical structure itself — neither an agent nor an institution, but the constraint's substrate
 *   - The Repeated-Game Coalition: Organized agents (organized/generational/constrained/regional) — cooperate through iterated strategies and reputation systems
 *   - The Legal Institution: Formal system (institutional/generational/arbitrage/national) — transforms incentives through external enforcement, making cooperation rational
 *   - The Mathematical Logician: Analytical observer (analytical/civilizational/analytical/universal) — sees the constraint as a theorem, invariant across all instantiations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, 0.08).
domain_priors:suppression_score(prisoners_dilemma_equilibrium, 0.03).
domain_priors:theater_ratio(prisoners_dilemma_equilibrium, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, extractiveness, 0.08).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prisoners_dilemma_equilibrium, mountain).
narrative_ontology:human_readable(prisoners_dilemma_equilibrium, "The Prisoner's Dilemma (Nash Equilibrium)").
narrative_ontology:topic_domain(prisoners_dilemma_equilibrium, "logical/economic").

domain_priors:emerges_naturally(prisoners_dilemma_equilibrium).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONAL AGENT (MOUNTAIN) — From the viewpoint of a single agent analyzing their payoff matrix, mutual defection is the only dominant strategy that survives iterated elimination of dominated strategies. The constraint is the logical structure of the payoff matrix itself: cooperation yields a lower payoff than defection regardless of what the other agent does. This is a logical law, not contingent on enforcement or institutional design. Every rational agent in a one-shot game, facing this payoff structure, must defect.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The Nash equilibrium at mutual defection is a mathematical theorem: given the payoff structure, no agent can unilaterally improve their outcome by changing strategy. The constraint emerges necessarily from the logical structure of symmetric games with this payoff ordering. The observer sees an invariant: mutual defection IS the Nash equilibrium in one-shot play for any payoff matrix satisfying Sucker's Payoff < Mutual Defection < Mutual Cooperation < Temptation to Defect. This is a law of rational choice, not dependent on observable or measurement basis.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: REPEATED-GAME COALITION (ROPE) — When agents interact repeatedly and can condition future behavior on history, cooperation becomes rational through reciprocal strategies (tit-for-tat, generous tit-for-tat). The constraint shifts from the payoff matrix to the discount factor: agents must value future payoffs sufficiently relative to present payoffs. Organized agents (firms, nations, communities) solve the prisoner's dilemma through repeated interaction, reputation systems, and norm enforcement. This is pure coordination — no extraction, just collective action enabling mutual benefit.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEGAL INSTITUTION (SCAFFOLD) — Formal institutions (contracts, courts, police) can transform the payoff matrix by making defection costly and cooperation rewarding through external enforcement. This is temporary support for cooperation: the institution provides the scaffolding that makes cooperation rational. As agents internalize cooperative norms and repeated-game logic becomes salient, institutional enforcement becomes redundant — the institution's role declines (sunset clause). Legal systems represent a transitional solution: they work, but only as long as enforcement capacity is maintained.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOMINANT PLAYER (MOUNTAIN) — An agent with sufficient power (military, economic, or informational advantage) can defect and escape retaliation, making defection strictly dominant even in repeated games. The constraint becomes: defection is optimal for the powerful agent regardless of the other agent's strategy or time horizon. This is a logical law governing asymmetric games, not a contingent institutional outcome.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: MATHEMATICAL LOGICIAN (MOUNTAIN) — The prisoner's dilemma is a theorem in combinatorial game theory: for any symmetric game with payoff matrix satisfying Sucker's Payoff < Mutual Defection Payoff < Mutual Cooperation Payoff < Temptation Payoff, the unique Nash equilibrium in pure strategies is mutual defection in one-shot play. This is logically necessary — not dependent on agent behavior, measurement, or context. The constraint is the proof itself.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prisoners_dilemma_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, ExtMetricName, E),
    domain_priors:suppression_score(prisoners_dilemma_equilibrium, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prisoners_dilemma_equilibrium),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prisoners_dilemma_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint is a logical property of the payoff structure, not an extractive mechanism that transfers value. No agent 'extracts' from another — the constraint is the mathematical structure itself. Base extraction is near-zero because there is no beneficiary or victim in the one-shot game; mutual defection is equally suboptimal for both. Suppression (0.03): Minimal. The constraint does not rely on coercion or suppression; it emerges from rational analysis of dominant strategies. Agents 'choose' defection through logical reasoning, not through force. Theater ratio (0.15): Minimal. The constraint has no performative component; the logical analysis is transparent and verifiable. The Nash equilibrium theorem can be proven and tested directly, not through theatrical display.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is between temporal and structural contexts, not between beneficiaries and victims. The one-shot game (immediate/trapped/universal) classifies as Mountain — defection is logically dominant. The repeated game (generational/constrained/regional) classifies as Rope — cooperation becomes rational through iteration and reputation. The legal institution perspective (generational/arbitrage/national) classifies as Scaffold — formal enforcement temporarily supports cooperation until internalized norms take over. The dominant player perspective (immediate/mobile/local) classifies as Mountain — the powerful agent's defection is strictly dominant regardless of context. The mathematical logician classifies as Mountain — the Nash equilibrium is a logical theorem invariant across all instantiations. All perspectives are consistent with the base properties (low extraction, low suppression, low theater) but differ in whether they emphasize the logical inevitability (Mountain) or the institutional/iterative solutions (Rope, Scaffold).
 *
 * DIRECTIONALITY LOGIC:
 *   In a one-shot symmetric prisoner's dilemma, there is no meaningful beneficiary or victim — mutual defection is equally suboptimal for both agents. Directionality derivation does not apply to this symmetric structure in the one-shot case. In asymmetric variants (one agent stronger or more patient), the more powerful agent is a de facto beneficiary (defection against a cooperative opponent yields the Temptation payoff), but this is not extraction in the classical sense — it is simply the consequence of structural asymmetry. In repeated games with coalitions, the beneficiaries are the organized agents (those capable of maintaining reciprocal strategies), and the victims are unorganized agents (those unable to sustain cooperation). In legal institution perspectives, the beneficiary is the institution itself (gains enforcement authority), and the victim is the agent paying enforcement costs. However, the primary Mountain classification is invariant across all these variants: the logical structure of the Nash equilibrium does not depend on directionality or agent power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payoff_matrix_specification,
    'Does the constraint depend on the specific numerical payoff values, or is it a logical property invariant across all matrices satisfying the ordering?',
    'Formal proof that the Nash equilibrium property holds for any payoff matrix satisfying the ordering constraint T > R > P > S (Temptation > Reward > Punishment > Sucker''s Payoff)',
    'If invariant: The constraint is a pure logical law (Mountain confirmed). If dependent on specific values: The constraint is domain-sensitive and may classify differently under alternative payoff structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payoff_matrix_specification, conceptual, 'Whether the constraint is invariant across payoff matrix specifications').

omega_variable(
    one_shot_vs_repeated_decomposition,
    'Should the one-shot prisoner''s dilemma and the repeated-game variant be treated as a single constraint or decomposed into separate constraint stories?',
    'Examine whether epsilon values differ: one-shot game (pure Nash equilibrium, no coordination function) vs repeated game (iterated equilibrium, coordination possible). If epsilon differs by a factor > 2, decompose.',
    'If single constraint: Rope and Mountain perspectives both apply to the same structure (perspectival gap reflects game horizon). If decomposed: Two separate constraints with different ε values linked by network relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(one_shot_vs_repeated_decomposition, conceptual, 'Whether one-shot and repeated games form a single constraint or separate structures').

omega_variable(
    rationality_assumption_grounding,
    'Is the prisoner''s dilemma constraint about the logical structure of games (independent of how humans or institutions behave) or about rational agency as a behavioral model?',
    'Formal analysis: Does the Nash equilibrium theorem require any empirical assumptions about agent behavior, or is it purely a logical statement about payoff structures? Empirical test: Do agents in laboratory prisoner''s dilemma games actually defect as the theory predicts?',
    'If logical/mathematical: Mountain classification is robust (independent of human behavior). If behavioral/empirical: The constraint may degrade to Piton (theater persists while function changes) if real agents cooperate despite the theory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationality_assumption_grounding, conceptual, 'Whether the constraint is logical or behavioral').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prisoners_dilemma_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pd_tr_t0, prisoners_dilemma_equilibrium, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pd_tr_t5, prisoners_dilemma_equilibrium, theater_ratio, 5, 0.15).
narrative_ontology:measurement(pd_tr_t10, prisoners_dilemma_equilibrium, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(pd_be_t0, prisoners_dilemma_equilibrium, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(pd_be_t5, prisoners_dilemma_equilibrium, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(pd_be_t10, prisoners_dilemma_equilibrium, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prisoners_dilemma_equilibrium, information_standard).
narrative_ontology:affects_constraint(prisoners_dilemma_equilibrium, collective_action_problem_general).
narrative_ontology:affects_constraint(prisoners_dilemma_equilibrium, tragedy_of_the_commons).
narrative_ontology:affects_constraint(prisoners_dilemma_equilibrium, arms_race_dynamics).

% DUAL FORMULATION NOTE:
% The prisoner's dilemma decomposes into: (1) one-shot equilibrium analysis (pure logical theorem, Mountain), (2) repeated-game coordination (institutional scaffolding and repeated-game Rope), and (3) asymmetric power variants (dominant-player Mountain). These are structurally distinct but share the same payoff-ordering substrate. The network links to downstream applications: tragedy of the commons (extended to N-player), arms races (repeated asymmetric variant), and collective action (general coordination failure). Each downstream constraint inherits the logical structure but adds domain-specific complications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
