% ============================================================================
% CONSTRAINT STORY: equilibrium_selection_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equilibrium_selection_problem, []).

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
 *   constraint_id: equilibrium_selection_problem
 *   human_readable: Equilibrium Selection Problem
 *   domain: game_theory/institutional_coordination
 *
 * SUMMARY:
 *   The equilibrium selection problem arises when multiple Nash equilibria
 *   exist in a coordination game and agents must converge on one without
 *   explicit communication or centralized authority. This constraint exhibits
 *   the full spectrum of DR types from different structural positions. A
 *   suboptimal but stable equilibrium locks in through convention, fear of
 *   unilateral defection, and information asymmetries about whether
 *   alternatives exist or whether others will switch. The same structural
 *   phenomenon — the inability to coordinate on a Pareto-superior outcome —
 *   appears as pure coordination (Rope), temporary coordination failure with
 *   transition pathways (Scaffold), mixed coordination with extraction
 *   (Tangled Rope), pure extraction (Snare), or immutable mathematical
 *   property (Mountain), depending on the observer's power level and exit
 *   capacity. The constraint's theater ratio (0.68) reflects that academic
 *   game theory literature performs high sophistication in analyzing
 *   refinement concepts and focal point selection, while the actual mechanism
 *   by which real-world equilibrium locks in and persists remains
 *   structurally unchanged. The extractiveness trajectory (0.35 → 0.52) shows
 *   accumulating welfare loss as suboptimal equilibrium persists and agents
 *   internalize convention-lock as inevitable.
 *
 * KEY AGENTS:
 *   - Trapped Coordinators: Powerless agents (powerless/trapped) — individual players bound by convention with no capacity for unilateral exit or coordination
 *   - Reform Coalition: Organized agents (organized/constrained) — groups capable of organizing equilibrium transition but facing concentrated costs and defection risk
 *   - Convention Beneficiary: Institutional winners (institutional/arbitrage) — actors positioned favorably by current equilibrium, benefit from status quo clarity and stability
 *   - Academic Theorist: Sophisticated observers (powerful/mobile) — game theorists analyzing equilibrium selection as mathematical problem; theater-high perspective treats proof as mechanism explanation
 *   - Defector Anticipator: Individual optimizers (moderate/constrained) — agents recognizing superior equilibrium but unable to switch without simultaneous mass coordination
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent convention-lock as inherent mathematical property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equilibrium_selection_problem, 0.52).
domain_priors:suppression_score(equilibrium_selection_problem, 0.58).
domain_priors:theater_ratio(equilibrium_selection_problem, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equilibrium_selection_problem, extractiveness, 0.52).
narrative_ontology:constraint_metric(equilibrium_selection_problem, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equilibrium_selection_problem, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equilibrium_selection_problem, tangled_rope).
narrative_ontology:human_readable(equilibrium_selection_problem, "Equilibrium Selection Problem").
narrative_ontology:topic_domain(equilibrium_selection_problem, "game_theory/institutional_coordination").

domain_priors:requires_active_enforcement(equilibrium_selection_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equilibrium_selection_problem, convention_lock_beneficiaries).
narrative_ontology:constraint_victim(equilibrium_selection_problem, coordination_agents).
narrative_ontology:constraint_victim(equilibrium_selection_problem, welfare_suboptimal_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED COORDINATOR (SNARE) — Agents bound by convention with no exit capacity. The selected equilibrium may be Pareto-dominated by an alternative, but switching requires coordinated defection. No single agent can escape without collective action. Maximum extraction experienced through convention lock.
constraint_indexing:constraint_classification(equilibrium_selection_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORM COALITION (TANGLED ROPE) — Organized agents capable of coordinating transition to superior equilibrium. Experience both coordination benefit (shared interest in moving to better outcome) and extraction cost (transition requires concentrated effort and assumes information burden). Can exit but at high organizational cost.
constraint_indexing:constraint_classification(equilibrium_selection_problem, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONVENTION BENEFICIARY (ROPE) — Institutional actors positioned favorably by the current equilibrium selection. Experience the constraint as pure coordination mechanism — maintaining the convention enables predictable interaction and locks in their advantage. Low experienced extraction; benefits from status quo clarity.
constraint_indexing:constraint_classification(equilibrium_selection_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC THEORIST (PITON) — Game theorists studying equilibrium selection as pure mathematical problem. The literature performs sophistication (refinement concepts, focal points, epistemic conditions) while the actual mechanism of real-world equilibrium locking persists unchanged. Theater ratio high: proof of existence ≠ analysis of why specific equilibria persist.
constraint_indexing:constraint_classification(equilibrium_selection_problem, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 5: DEFECTOR ANTICIPATOR (TANGLED ROPE) — Individual agents who recognize superior alternative equilibrium but face payoff reduction if others don't switch simultaneously. Coordination benefit (mutual switching improves all payoffs) and extraction cost (vulnerability to defection if switching fails). Exit requires mass coordination.
constraint_indexing:constraint_classification(equilibrium_selection_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: NATURAL LAW FRAMING (MOUNTAIN) — Frames equilibrium selection as inherent to strategic interaction. Multiple equilibria and selection problem appear as fundamental features of game structure, like mathematical underdetermination. However, this naturalizes what is a contingent institutional fact: societies successfully coordinate on equilibrium transitions when power distributions and information asymmetries permit it.
constraint_indexing:constraint_classification(equilibrium_selection_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equilibrium_selection_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equilibrium_selection_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equilibrium_selection_problem, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equilibrium_selection_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equilibrium_selection_problem, TR),
    TR >= 0.70.

:- end_tests(equilibrium_selection_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The equilibrium selection problem generates welfare loss through coordination failure, but the loss is not maximally extractive because no agent deliberately designed the lock — it emerges from decentralized strategic interaction. The beneficiary gains advantage but through convention-lock rather than active rent-seeking. The trajectory shows accumulation over time as agents internalize the equilibrium as inevitable. Suppression (0.58): Moderate-high. Significant barriers to equilibrium transition include defection risk (collective action problem), information asymmetry about alternatives, coordination costs, and institutional embedding of the current equilibrium. But suppression is not total — historical examples show equilibrium transitions when power asymmetries favor it or institutional design enables it. Theater ratio (0.68): High. Game theory literature demonstrates substantial sophistication in equilibrium analysis (refinement concepts, epistemic foundations, focal point theory) while the actual real-world selection mechanism remains opaque. Proving multiple equilibria exist ≠ explaining why specific equilibria persist. The performative content is in the mathematical models' elegance masking institutional contingency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces opposing classifications based on observational position. Trapped agents see Snare (no exit, maximum extraction). Beneficiaries see Rope (coordination mechanism, arbitrage advantage). Organized defectors see Tangled Rope (mixed coordination benefit and extraction cost, but capacity to exit at high organizational cost). Theorists see Piton (performative analysis, theater-high mechanism). Analytical observer risks seeing Mountain (immutable mathematical property) but structural data reveals false summit: equilibrium lock is reconstructible through institutional intervention. The perspectival gap is not measurement ambiguity — it reflects real differences in how agents experience the same constraint based on their power and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position in the coordination problem. Trapped coordinators with no exit (exit_options=trapped, power=powerless) experience maximum extraction (d≈0.95, f(d)≈1.42). Convention beneficiaries with arbitrage exits experience minimum extraction (d≈0.05, f(d)≈-0.12) — the constraint is pure coordination from their perspective. Organized defectors with constrained exits occupy middle ground (d≈0.55, f(d)≈0.75) — they perceive both coordination benefit and extraction cost. The powerful academic theorist with mobile exits (d≈0.48, f(d)≈0.60) experiences the constraint as sophistication opportunity, not extraction. The analytical observer (d≈0.73, f(d)≈1.15) sees the full structure but risks naturalizing contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   Equilibrium selection resolves the mandatrophy by revealing that all six types are legitimate perspectival readings. The mandatrophy question 'Is this coordination or extraction?' has a two-part answer: (1) it is genuine coordination (multiple agents benefit from convergence on common equilibrium) AND (2) it functions as extraction mechanism (the specific equilibrium selected benefits some agents asymmetrically, and powerless agents cannot escape). The constraint is not misclassified by any perspective — each perspective describes a real structural feature. The analytical observer's mountain classification is a false summit: the underdetermination of equilibria is mathematical, but the selection of which equilibrium persists is institutional. Institutional design can enable transition (Scaffold), organized power can force transition (Tangled Rope), or powerlessness can lock agents in Snare. The constraint is not naturally determined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    focal_point_mechanism,
    'What determines which equilibrium becomes focal in real-world coordination problems — Schelling salience, power asymmetry, path dependence, or institutional design?',
    'Comparative case analysis of successful vs failed equilibrium transitions; identification of common factors predicting which equilibrium agents converge on',
    'If salience/convention dominates: equilibrium selection is primarily coordination problem (Rope). If power asymmetry dominates: equilibrium selection is primarily extraction mechanism (Snare). If institutional design dominates: constraint is reconstructible (Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(focal_point_mechanism, empirical, 'Determinants of focal equilibrium selection').

omega_variable(
    payoff_dominance_vs_risk_dominance,
    'When payoff-dominant and risk-dominant equilibria diverge, which one do actual agents converge on, and why?',
    'Experimental game theory evidence; longitudinal observation of real-world coordination failures where payoff-superior equilibrium exists but agents remain locked in risk-dominant alternative',
    'If payoff-dominant prevails: agents rationally recognize superior outcome and organize transition. If risk-dominant prevails: information asymmetry and defection fear prevent transition despite recognized superiority — extraction mechanism strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payoff_dominance_vs_risk_dominance, empirical, 'Payoff-dominance vs risk-dominance in equilibrium selection').

omega_variable(
    transition_cost_distribution,
    'Who bears the costs of transitioning from one equilibrium to a superior alternative — are costs distributed or concentrated on vulnerable agents?',
    'Analysis of historical equilibrium transitions (monetary standards, technical standards, organizational forms); measurement of who gains/loses in transition window',
    'If costs distributed: constraint is scaffolding problem with sunset. If costs concentrated on powerless: constraint is snare masquerading as coordination. Determines whether reform coalition can form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_cost_distribution, empirical, 'Distribution of transition costs across agent populations').

omega_variable(
    information_asymmetry_persistence,
    'Do agents locked in suboptimal equilibrium lack information about superior alternative, or do they have information but face defection risk that prevents switching?',
    'Qualitative analysis of agent beliefs and expectations; measurement of information availability vs behavioral switching; controlled experiments testing information provision',
    'If information constraint: providing information enables transition (low suppression, Rope predominates). If defection-risk constraint: information alone insufficient — requires institutional guarantee or power asymmetry to enforce (high suppression, Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Information availability vs defection-risk in equilibrium lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equilibrium_selection_problem, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqsel_tr_t0, equilibrium_selection_problem, theater_ratio, 0, 0.55).
narrative_ontology:measurement(eqsel_tr_t3, equilibrium_selection_problem, theater_ratio, 3, 0.62).
narrative_ontology:measurement(eqsel_tr_t6, equilibrium_selection_problem, theater_ratio, 6, 0.68).
narrative_ontology:measurement(eqsel_tr_t9, equilibrium_selection_problem, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(eqsel_be_t0, equilibrium_selection_problem, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eqsel_be_t3, equilibrium_selection_problem, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(eqsel_be_t6, equilibrium_selection_problem, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(eqsel_be_t9, equilibrium_selection_problem, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equilibrium_selection_problem, information_standard).
narrative_ontology:affects_constraint(equilibrium_selection_problem, coordination_failure).
narrative_ontology:affects_constraint(equilibrium_selection_problem, convention_lock).
narrative_ontology:affects_constraint(equilibrium_selection_problem, institutional_path_dependence).

% DUAL FORMULATION NOTE:
% Equilibrium selection problem decomposes into three structurally distinct constraints: (1) multiple equilibria existence (mathematical fact, ε≈0.08, Mountain); (2) coordination failure to superior equilibrium (behavioral/institutional fact, ε≈0.52, this story); (3) path dependence preventing transition (institutional entrenchment, ε≈0.65, Snare-dominant). Each has different epsilon and different resolution mechanisms. This story focuses on the behavioral coordination aspect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
