% ============================================================================
% CONSTRAINT STORY: nash_equilibrium_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nash_equilibrium_coordination, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nash_equilibrium_coordination
 *   human_readable: Nash Equilibrium as Coordination Mechanism
 *   domain: economic/social
 *
 * SUMMARY:
 *   The Nash equilibrium is a mathematical concept that describes a state in
 *   which no player can unilaterally improve their outcome given the
 *   strategies of others. However, Nash equilibrium simultaneously functions
 *   as a coordination mechanism (pure Rope), an explanatory framework of
 *   limited predictive power (Piton), and in certain structures, a trap that
 *   locks rational agents into collectively suboptimal outcomes (Snare or
 *   Tangled Rope). The constraint's classification depends critically on
 *   whether the equilibrium being studied is Pareto-efficient, whether agents
 *   can communicate, whether institutional mechanisms enable commitment to
 *   alternative equilibria, and whether agents actually converge to the
 *   mathematical prediction. The theater ratio has risen from 0.35 to 0.45
 *   over the interval as game theory has become increasingly applied to
 *   real-world coordination problems while simultaneously documented to fail
 *   in practice, creating a gap between theoretical prediction and empirical
 *   realization.
 *
 * KEY AGENTS:
 *   - Individual Rational Players: Primary beneficiaries (moderate/mobile) — experience Nash equilibrium as self-enforcing coordination with no need for external enforcement
 *   - Players Trapped in Suboptimal Equilibrium: Secondary victims (powerless/constrained) — experience the equilibrium as coordination that is simultaneously extractive due to inability to commit to joint deviations
 *   - Institutional Architects: Secondary beneficiaries (institutional/arbitrage) — benefit from predictability and stability provided by Nash equilibrium framework
 *   - Behavioral Agents: Primary victims in the trapped frame (powerless/immediate) — face immediate pressure toward equilibrium strategies regardless of long-term efficiency
 *   - Game Theorists: Analytical framework maintainers (analytical/analytical) — maintain the theory despite empirical failures to predict convergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nash_equilibrium_coordination, 0.35).
domain_priors:suppression_score(nash_equilibrium_coordination, 0.3).
domain_priors:theater_ratio(nash_equilibrium_coordination, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nash_equilibrium_coordination, extractiveness, 0.35).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nash_equilibrium_coordination, rope).
narrative_ontology:human_readable(nash_equilibrium_coordination, "Nash Equilibrium as Coordination Mechanism").
narrative_ontology:topic_domain(nash_equilibrium_coordination, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nash_equilibrium_coordination, equilibrium_achieving_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONAL INDIVIDUAL PLAYER (ROPE) — Each player can unilaterally shift strategies if motivated, but finds no incentive to do so given others' choices. The constraint is experienced as stable coordination without coercion. Exit options exist (players can experiment with alternative strategies) but prove unprofitable. Pure coordination benefit with minimal overhead.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: TRAPPED PLAYER IN SUBOPTIMAL EQUILIBRIUM (TANGLED ROPE) — When the Nash equilibrium is Pareto-inefficient (prisoner's dilemma, tragedy of the commons), individual players are locked into outcomes worse than coordination-at-a-different-point would achieve. Players experience this as both coordination (equilibrium is self-enforcing) and extraction (they cannot escape without collective action). Suppression arises from the inability to commit to joint deviations.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VICTIM OF COORDINATION FAILURE (SNARE) — In hawk-dove, arms races, or races-to-the-bottom, the Nash equilibrium itself is a trap. Individual incentives push toward outcomes (aggression, overspending, quality degradation) that harm all participants. No unilateral deviation helps; collective escape requires commitment devices none possess. High suppression, minimal coordination benefit, pure extraction.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: GAME DESIGNER/MARKET ARCHITECT (ROPE) — Institutional actors (platforms, regulators, market designers) benefit from establishing equilibria that serve their objectives. They see the Nash equilibrium as a pure coordination tool: it simplifies their landscape by making agent behavior predictable and self-stabilizing. Low extraction because the architecture solves their problem elegantly.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THEORETICAL FRAMEWORK (PITON) — Game theory itself treats Nash equilibrium as foundational, but empirical play often deviates: agents use bounded rationality, apply heuristics, learn slowly, or fail to reach equilibrium. The concept persists as a standard despite frequent non-convergence to its predictions. Theater ratio reflects that Nash equilibrium is often invoked as an explanatory device for outcomes it does not actually predict in practice.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MATHEMATICAL NECESSITY (MOUNTAIN) — From pure game-theoretic logic, every finite game has at least one Nash equilibrium (mixed or pure). This is a mathematical theorem, not a contingent empirical fact. No agent can violate the structure: if players follow their incentives, an equilibrium-like configuration must exist. However, this perspective confuses mathematical existence with empirical realization. Most real coordination does NOT reach Nash equilibrium. The mountain classification is a false summit masking institutional and cognitive factors that prevent convergence.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nash_equilibrium_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nash_equilibrium_coordination, TR),
    TR >= 0.70.

:- end_tests(nash_equilibrium_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Nash equilibrium is primarily a coordination mechanism — it enables equilibration without requiring an external enforcer. However, extractiveness is non-zero because (1) in suboptimal equilibria (prisoner's dilemma, arms races), the equilibrium itself is a trap that extracts welfare from all players relative to feasible alternatives, and (2) institutional designers can exploit equilibrium-selection problems to steer outcomes in their favor. Suppression (0.30): Moderate. The suppression arises from inability to commit to joint deviations when multiple equilibria exist, and from the difficulty of coordinating on alternative equilibria when the current Nash point is self-enforcing. However, suppression is not high because (a) in efficient equilibria, no alternative is preferable anyway, and (b) communication and institutional mechanisms can partially overcome commitment barriers. Theater ratio (0.45): Moderate. Game theory's Nash concept is increasingly applied to real systems, but empirical convergence to equilibrium is slower and more uncertain than the mathematical theorem suggests. The theory is intellectually coherent but partially performative in application — it explains post-hoc why an outcome occurred, less often predicts which outcome will occur.
 *
 * PERSPECTIVAL GAP:
 *   The trapped player in a suboptimal equilibrium experiences the maximum perspectival gap. From their individual-rational perspective, they cannot unilaterally deviate profitably — the constraint appears unbreakable, making it experientially equivalent to a snare. From the collective-rational perspective (if all players could commit jointly), the equilibrium is inefficient and escape is desirable. From the institutional architect's perspective, the suboptimal equilibrium may still be valuable because it is predictable and stable, making it appear as Rope. The game theorist's mountain perspective ('mathematically guaranteed to exist') misses that the real constraint is behavioral: agents don't converge to equilibrium because they are boundedly rational, lack information, or face commitment barriers. The false summit occurs when mathematical necessity is confused with behavioral inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by equilibrium type. In efficient coordination games (e.g., driving conventions), the equilibrium is genuinely beneficial for all players — beneficiaries and victims collapse into a single group, and d is intermediate (symmetric benefit). In suboptimal equilibria (prisoner's dilemma, arms races), players experience extraction toward the equilibrium despite preferring alternatives — d is high for trapped players (they bear full cost relative to feasible alternatives). For institutional architects selecting which equilibrium is realized when multiple exist, d is low (they benefit from the equilibrium selection). The engine derives d from beneficiary/victim declarations: in efficient equilibrium contexts, minimal declarations are needed; in suboptimal equilibrium contexts, all agents are victims of the trap.
 *
 * MANDATROPHY ANALYSIS:
 *   Nash equilibrium resolves the mandatrophy by decomposing the single concept into six structurally distinct perspectives. In efficient coordination games (e.g., currency standards, driving conventions), the constraint is pure Rope from all perspectives. In suboptimal equilibria (prisoner's dilemma), it is Snare from the individual-player perspective and Rope from the institutional-architect perspective — revealing that the same mathematical structure serves different functions for different agents. The piton perspective detects that game theory's explanatory power in predicting actual convergence is degraded: the theory explains why behavior is rational ex-post, but does not reliably predict ex-ante which equilibrium will be selected or when convergence occurs. The mountain perspective is a false summit: mathematical existence of equilibrium is guaranteed, but behavioral convergence is contingent on cognitive, informational, and institutional factors. The mandatrophy resolution lies in recognizing that 'the Nash equilibrium' is not a single constraint but a family of structurally distinct constraints indexed by equilibrium efficiency and player access to commitment mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_efficiency_gap,
    'When do agents rationally prefer the outcome of a suboptimal Nash equilibrium over the Pareto-superior alternative, and when are they trapped by inability to coordinate commitment?',
    'Experimental and empirical analysis of repeated game play, communication effects, and institutional mechanisms (contracts, governance, reputation) that enable or prevent coordination on superior equilibria',
    'If agents prefer the Nash equilibrium due to credible commitment problems: classification is Rope for all perspectives. If agents trapped by inability to communicate: classification becomes Tangled Rope or Snare depending on power level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equilibrium_efficiency_gap, empirical, 'Distinguishing rational preference for equilibrium from coordination failure').

omega_variable(
    convergence_mechanism,
    'What determines whether agents actually converge to a Nash equilibrium, and is mathematical existence sufficient to explain empirical equilibration?',
    'Historical case analysis of economic/social coordination (markets, protocols, norms); behavioral economics experiments on equilibrium convergence; identification of learning mechanisms and information structures required for convergence',
    'If convergence depends critically on institutional support (communication, monitoring, punishment): Nash equilibrium is Rope only for well-designed systems; becomes Piton when institutional support degrades. If no convergence occurs: Piton classification dominates empirically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_mechanism, empirical, 'Whether game-theoretic equilibrium predicts actual coordination outcomes').

omega_variable(
    multiple_equilibrium_selection,
    'When multiple Nash equilibria exist (coordination games, battle-of-the-sexes), what mechanism selects which equilibrium is realized, and can this selection itself be exploited for extraction?',
    'Institutional analysis of equilibrium-selection rules; behavioral experiments on focal points and default options; economic analysis of who benefits when a particular equilibrium is selected vs alternatives',
    'If selection is costless and symmetric: multiple equilibria are coordination games (Rope). If selection mechanism favors one agent over another: coordination becomes Tangled Rope or Snare depending on bargaining power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multiple_equilibrium_selection, empirical, 'Equilibrium selection mechanisms and asymmetric benefit allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nash_equilibrium_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nash_tr_t0, nash_equilibrium_coordination, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nash_tr_t5, nash_equilibrium_coordination, theater_ratio, 5, 0.4).
narrative_ontology:measurement(nash_tr_t10, nash_equilibrium_coordination, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(nash_be_t0, nash_equilibrium_coordination, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nash_be_t5, nash_equilibrium_coordination, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(nash_be_t10, nash_equilibrium_coordination, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nash_equilibrium_coordination, resource_allocation).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, prisoner_dilemma_trap).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, tragedy_of_commons_mechanism).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, equilibrium_selection_problem).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, coordination_game_standard).

% DUAL FORMULATION NOTE:
% Nash equilibrium as a single concept decomposes into two structurally distinct constraints: (1) efficient_coordination_equilibrium (ε ≤ 0.15, Rope for all perspectives), representing cases where the Nash equilibrium is Pareto-optimal and serves pure coordination; (2) suboptimal_equilibrium_trap (ε ≥ 0.40, Snare for powerless agents, Tangled Rope for moderate agents), representing prisoner's dilemma and arms-race structures where the equilibrium is self-enforcing but collectively suboptimal. These are not the same constraint viewed from different angles; their ε values differ by a factor of 3. The efficient equilibrium is genuinely a coordination rope. The suboptimal equilibrium is genuinely a trap. The game-theoretic framework applies to both, but the structural logic differs. Stories should be decomposed unless the specific game structure being analyzed is explicitly mixed (e.g., coordination game where players disagree on which efficient equilibrium to select, creating both coordination and extraction layers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
