% ============================================================================
% CONSTRAINT STORY: prisoners_dilemma_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Prisoner's Dilemma is a canonical logical constraint in game theory
 *   that demonstrates why two rational agents, each acting independently in
 *   their own self-interest, will fail to cooperate even when mutual
 *   cooperation yields a better outcome. The constraint is axiomatic — it
 *   follows from the payoff structure and the assumption of rational
 *   decision-making under common knowledge of rationality. Unlike
 *   institutional or social constraints that can be reformed or escaped, the
 *   prisoner's dilemma is a structural property of certain game forms. The
 *   dilemma does not require enforcement; it emerges logically. The
 *   constraint has zero degrees of freedom: given the payoff matrix, the
 *   mutual defection equilibrium is inevitable. This makes it a candidate for
 *   Mountain classification if and only if the fundamental assumption (that
 *   rational agents will defect) holds. However, from perspectives that
 *   question this assumption (evolutionary, institutional, behavioral), the
 *   dilemma becomes a problem to be solved rather than an immutable law.
 *
 * KEY AGENTS:
 *   - Rational Agent A (analytical/analytical) — Makes individual payoff-maximizing choice; defects regardless of other agent's choice
 *   - Rational Agent B (analytical/analytical) — Makes individual payoff-maximizing choice; defects regardless of other agent's choice
 *   - The Collective Good (powerless/trapped) — Abstract beneficiary of mutual cooperation; cannot impose coordination
 *   - Institutional Designer (organized/constrained) — Can modify game form through repeated play, reputation, or enforcement
 *   - Evolutionary Fitness Pressure (analytical/analytical) — Selects for cooperation mechanisms over pure rationality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, 0.08).
domain_priors:suppression_score(prisoners_dilemma_equilibrium, 0.03).
domain_priors:theater_ratio(prisoners_dilemma_equilibrium, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, extractiveness, 0.08).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, resistance, 0.05).

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

% PERSPECTIVE 1: LOGICAL STRUCTURE (MOUNTAIN) — The prisoner's dilemma is a pure logical consequence of the payoff matrix and rational decision theory. Given the canonical payoff structure (mutual defection worse than mutual cooperation, but unilateral defection best), the Nash equilibrium at mutual defection is mathematically inevitable under common knowledge of rationality. This is not a constraint imposed by institutions or enforcement — it emerges from the logical structure itself. ε≈0.08, accessibility_collapse≈0.90, resistance≈0.05. Mountain gate satisfied.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: GAME THEORIST (MOUNTAIN) — From the perspective of formal game theory, the prisoner's dilemma is an unchangeable structural property of certain payoff configurations. No institution, no amount of enforcement, no side agreement can eliminate the logical dilemma inherent to the game form itself. The constraint is the logical inevitability of the outcome given rationality assumptions. The escape routes (repeated games, communication, reputation) are not solutions to the dilemma — they are exits from the game form, not resolutions within it.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL DESIGNER (ROPE) — From the perspective of someone designing institutions to solve cooperation problems, the prisoner's dilemma becomes a coordination challenge. By introducing reputation mechanisms, repeated interactions, communication protocols, or enforcement structures, an institutional designer can shift from the defection equilibrium to a cooperation equilibrium. This perspective sees the dilemma not as an immutable law but as a solvable coordination problem. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05. The constraint here is the design problem itself, and it is addressable through institutional innovation.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: EVOLUTIONARY BIOLOGIST (ROPE) — Over evolutionary timescales, organisms that solve the prisoner's dilemma (through kinship, reciprocal altruism, group selection, or reputation) outcompete pure defectors. The dilemma is not immutable — it is a selection pressure that shaped cooperation mechanisms. From this view, the constraint is how evolution coordinates cooperation despite the logic of individual incentives. The dilemma itself is not a mountain but a recurring problem that life solves repeatedly.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prisoners_dilemma_equilibrium_tests).

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
 *   Extractiveness (0.08): Extremely low. The prisoner's dilemma does not extract value — it represents a failure of coordination that leaves both players worse off than the cooperative outcome. If the dilemma 'extracts' anything, it is the forgone cooperative surplus from both agents, but this is not asymmetric extraction. The constraint operates symmetrically on both players. Suppression (0.03): Minimal. There is no coercive mechanism suppressing alternatives — the dilemma is purely logical. Each agent is free to cooperate; the incentive structure simply makes defection dominant. Theater ratio (0.0): Zero. The dilemma is functionally pure — there is no performative component. The equilibrium outcome (mutual defection) is exactly what the theory predicts without appeal to any hidden mechanism or narrative cover. Accessibility collapse (0.90): High. The prisoner's dilemma demonstrates maximum transparency about the constraint structure. Every rational agent can see the full payoff matrix and calculate the dominant strategy. The constraint is maximally accessible. Resistance (0.05): Minimal. Once the payoff structure is understood, there is no meaningful resistance to the logic. The resistance value reflects only that humans sometimes cooperate despite the logic, indicating the assumption of rationality itself is not universally valid.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is between the logician (who sees the dilemma as an immutable consequence of rational choice) and the institutional/evolutionary observer (who sees it as a solvable problem). The logician correctly identifies that within the one-shot game form with common knowledge of rationality, mutual defection is inevitable. The institutional designer correctly identifies that by changing the game form (repeating it, adding reputation, introducing enforcement), cooperation can be sustained. Neither perspective is wrong — they are addressing different structural questions. The mountain classification applies to the game form itself (the logical structure is invariant). The rope classification applies to the problem of designing institutions that support cooperation (this is a coordination challenge). The key insight is that the dilemma is not a constraint ON rational agents but a constraint ABOUT what rational choice theory predicts in one-shot simultaneous games.
 *
 * DIRECTIONALITY LOGIC:
 *   Both agents occupy identical structural positions relative to the dilemma: neither is a beneficiary and neither is a victim in the sense of asymmetric extraction. Both are losers relative to the cooperative outcome, and both are equal losers. The constraint is symmetric. From the logical perspective, both agents have d≈0.50 (symmetric cost), and since this is an analytical observation, they map to the Mountain classification. From the institutional perspective, the designer wants to shift the equilibrium, making the designer d≈0.50 (wants to improve both players), which maps to Rope. The dilemma itself has no natural beneficiary — it is a pure coordination failure affecting all parties equally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prisoners_dilemma_equilibrium, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prisoners_dilemma_equilibrium, information_standard).
narrative_ontology:affects_constraint(prisoners_dilemma_equilibrium, tragedy_of_the_commons).
narrative_ontology:affects_constraint(prisoners_dilemma_equilibrium, arms_race_equilibrium).
narrative_ontology:affects_constraint(prisoners_dilemma_equilibrium, collective_action_problem).

% DUAL FORMULATION NOTE:
% The prisoner's dilemma is the foundational logical structure for coordination failures. It affects all downstream coordination problems because it establishes the baseline incentive incompatibility. However, the dilemma itself is a mountain — the logical structure is invariant. Specific instantiations (arms races, commons tragedies) inherit the dilemma's structure but add institutional, ecological, or temporal complexity that can create different ε values and classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
