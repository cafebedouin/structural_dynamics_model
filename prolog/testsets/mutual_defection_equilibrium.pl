% ============================================================================
% CONSTRAINT STORY: mutual_defection_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mutual_defection_equilibrium, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mutual_defection_equilibrium
 *   human_readable: The Infinite Prisoner's Dilemma Stalemate
 *   domain: economic/social
 *
 * SUMMARY:
 *   The infinite prisoner's dilemma stalemate represents a fundamental
 *   coordination failure where rational individual behavior produces
 *   collectively irrational outcomes. Unlike finite games where backward
 *   induction can sustain cooperation, or one-shot games where cooperation is
 *   irrational, the infinite dilemma creates a stable state where all agents
 *   defect despite mutual awareness that cooperation would improve collective
 *   welfare. The constraint manifests across economic systems (trade wars,
 *   price wars), social systems (arms races, collective action problems), and
 *   environmental systems (tragedy of the commons, climate negotiations). The
 *   stalemate is self-enforcing: each agent defects because others defect,
 *   and rational agents cannot unilaterally improve their position by
 *   switching to cooperation. This creates a trap that persists without
 *   external intervention to change payoff structures or enable enforcement
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Individual Agents: Primary victims (powerless/trapped) — rationally choose defection despite collective irrationality
 *   - Collective Welfare: Primary victim (powerless/trapped) — bears full cost of mutual defection equilibrium
 *   - Organized Coalition: Secondary agent (moderate/constrained) — attempts to solve coordination problem but faces free-rider incentives
 *   - Rule-Setter Institution: Potential solution provider (institutional/arbitrage) — can redesign payoffs or enable enforcement
 *   - Defection Beneficiary: Power asymmetry actor (powerful/mobile) — captures higher payoff by defecting while others attempt cooperation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — views constraint as mathematical necessity or social contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mutual_defection_equilibrium, 0.58).
domain_priors:suppression_score(mutual_defection_equilibrium, 0.65).
domain_priors:theater_ratio(mutual_defection_equilibrium, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mutual_defection_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mutual_defection_equilibrium, snare).
narrative_ontology:human_readable(mutual_defection_equilibrium, "The Infinite Prisoner's Dilemma Stalemate").
narrative_ontology:topic_domain(mutual_defection_equilibrium, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(mutual_defection_equilibrium, all_agents).
narrative_ontology:constraint_victim(mutual_defection_equilibrium, collective_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL DEFECTOR (SNARE) — Each agent rationally defects because unilateral cooperation yields the worst payoff (sucker's payoff). Trapped in a structure where defection is individually rational but collectively catastrophic. No exit option exists that improves outcomes for this agent without coordinating with others, which the structure prevents.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED COALITION ATTEMPT (SNARE) — Groups attempting to organize cooperation face asymmetric payoff structure: they benefit only if all members cooperate, but any member has incentive to defect and capture the defection payoff. Organization cannot change the underlying payoff matrix. Extraction operates through the mathematical structure itself.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RULE-SETTER INSTITUTION (ROPE) — An external enforcer (contract law, reputation system, repeated-game mechanism) can convert the one-shot dilemma into a coordination problem by changing payoff structure or enabling retaliation. From this institutional view, the stalemate is solvable through rule-making, making the constraint appear as pure coordination (Rope) rather than trap.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The mutual defection equilibrium is a fixed point of the payoff structure. In a one-shot game with these payoffs, defection is a dominant strategy regardless of agent preferences or power. The constraint appears as an immutable law of rational choice — a mathematical necessity rather than a social construction.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: DEFECTION BENEFICIARY (TANGLED ROPE) — An agent with superior resources can defect while others attempt cooperation, capturing the highest payoff (temptation payoff) before retaliation. This agent benefits from the stalemate's existence while possessing exit options. The constraint provides both extraction (from others' mutual defection) and coordination (enabling their defection strategy).
constraint_indexing:constraint_classification(mutual_defection_equilibrium, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mutual_defection_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mutual_defection_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mutual_defection_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint does not extract resources to a single beneficiary (unlike typical Snares), but rather prevents mutual gain, making the extraction structural rather than directional. The value reflects how much welfare is lost relative to the cooperation outcome. The trajectory from 0.42 to 0.58 shows increasing severity as agents become more confident in the defection equilibrium. Suppression (0.65): High. The payoff structure itself constitutes suppression — individual agents cannot escape defection without collective coordination, which the payoff structure prevents. There is no unilateral exit path that improves outcomes. Theater ratio (0.38): Low-moderate. The constraint is functionally severe (not primarily performative), but as agents rationalize their defection over time, they develop narratives and justifications that mask the underlying payoff structure, explaining the modest increase in theater over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap lies between the individual agent (snare: trapped, no unilateral exit) and the institutional rule-setter (rope: can change payoff structure). From the individual agent's perspective, defection is inevitable given the payoffs. From the institution's perspective, mutual defection is a design choice that can be reversed by altering rewards, enabling punishment, or restructuring the game entirely. The powerful beneficiary experiences the constraint as partially beneficial (tangled rope), gaining from asymmetric payoffs while possessing exit options. The analytical observer risks naturalizing a constructed game as a law of nature (mountain), when the payoff structure itself is a choice point. The Snare classification holds across most perspectives, indicating a severe constraint, but the diversity of classifications reveals that the constraint is not immutable — it depends on institutional structures and payoff design.
 *
 * DIRECTIONALITY LOGIC:
 *   The mutual defection equilibrium exhibits unusual directionality: it extracts from all agents equally rather than flowing from victims to beneficiaries. Each agent's d value depends on whether they perceive the payoff structure as exogenous (immutable, high d toward being victimized) or endogenous (changeable, lower d if they have influence). A powerless agent trapped in an unchangeable payoff structure experiences d ≈ 0.90 (near-complete victimhood). An institutional rule-setter with capacity to redesign payoffs experiences d ≈ 0.15 (near-beneficiary position). A powerful agent able to capture asymmetric gains while defecting experiences d ≈ 0.45 (mixed). The derivation chain produces higher d for agents with fewer exit options, lower d for agents who can escape via institutional redesign or superior resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution: mutual defection is NOT pure extraction (Snare) when viewed from the institutional level where payoff structures are chosen. It IS pure extraction when viewed from individual agents trapped in an exogenous payoff structure. The constraint resolves the mandatrophy by revealing that 'Snare vs Rope' depends on whether the payoff matrix is treated as exogenous (Snare from below) or endogenous (Rope from above). The rule-setter institution can convert the stalemate into a coordination problem (Rope) by restructuring payoffs, enabling reputation systems, or creating enforcement mechanisms. The analytics avoid false naturalization by treating the payoff structure as a contingent choice, not as a law of nature. The mountain perspective is a false summit — it mistakes the logical necessity of mutual defection given certain payoffs for the necessity of those payoffs themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payoff_structure_externality,
    'Are the payoff matrices exogenously determined or endogenously constructed by the agents themselves?',
    'Historical analysis of how payoff structures were established; examination of whether agents can collectively revise payoffs',
    'If exogenous: constraint is Mountain-like (immutable). If endogenous: constraint is Snare-like (constructed extraction). If mixed: constraint is Tangled Rope (both features).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payoff_structure_externality, conceptual, 'Whether payoff matrices are exogenous or endogenously constructed').

omega_variable(
    iteration_empirical_prevalence,
    'In real iterated dilemmas (trade relationships, arms races, public goods), do agents actually achieve mutual defection equilibrium or do repeated interactions produce cooperation?',
    'Empirical study of historical examples (trade wars, arms control, climate negotiations); laboratory experiments with repeated interactions',
    'If mutual defection is rare: the theoretical constraint is weaker than presented. If mutual defection is common: the constraint''s real-world severity is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iteration_empirical_prevalence, empirical, 'Empirical prevalence of mutual defection in iterated dilemmas').

omega_variable(
    coalition_enforcement_mechanism,
    'What enforcement mechanisms (reputation, punishment, contract, third-party enforcement) can shift the equilibrium from mutual defection to cooperation?',
    'Game-theoretic analysis of modified payoff structures; empirical testing of which enforcement mechanisms work in which domains',
    'If enforcement is cheap and reliable: Rope solution dominates, constraint is temporary. If enforcement is expensive or unreliable: Snare persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_enforcement_mechanism, empirical, 'Effectiveness of enforcement mechanisms to shift equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mutual_defection_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mudef_tr_t0, mutual_defection_equilibrium, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mudef_tr_t5, mutual_defection_equilibrium, theater_ratio, 5, 0.32).
narrative_ontology:measurement(mudef_tr_t10, mutual_defection_equilibrium, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(mudef_be_t0, mutual_defection_equilibrium, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mudef_be_t5, mutual_defection_equilibrium, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mudef_be_t10, mutual_defection_equilibrium, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mutual_defection_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(mutual_defection_equilibrium, collective_action_problem).
narrative_ontology:affects_constraint(mutual_defection_equilibrium, tragedy_of_the_commons).
narrative_ontology:affects_constraint(mutual_defection_equilibrium, arms_race_escalation).

% DUAL FORMULATION NOTE:
% The infinite prisoner's dilemma stalemate is upstream to multiple applied constraints (tragedy of commons, arms races, trade wars) that manifest the same payoff structure in specific domains. Each downstream constraint has its own ε reflecting domain-specific extraction severity, but all share the foundational structure of the dilemma.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mutual_defection_equilibrium, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
