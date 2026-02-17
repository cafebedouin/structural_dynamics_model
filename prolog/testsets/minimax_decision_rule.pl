% ============================================================================
% CONSTRAINT STORY: minimax_decision_rule
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_minimax_decision_rule, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: minimax_decision_rule
 *   human_readable: Minimax Decision Rule
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Minimax is a decision rule from game theory for minimizing the possible
 *   loss in a worst-case scenario. In a two-player, zero-sum game, it
 *   identifies the strategy that guarantees a certain minimum outcome,
 *   regardless of the opponent's actions. This story models the mathematical
 *   principle itself as a constraint, which is a logical limit on optimal
 *   play within such games.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Strategic Innovator (powerless/constrained): Primary target of the
 *     constraint's logic. Their high-risk, high-reward strategies are
 *     invalidated by an opponent playing optimally to minimize loss.
 *   - The System Architect (institutional/arbitrage): Primary beneficiary.
 *     Uses Minimax as a coordination principle to build secure, predictable,
 *     and provably safe systems (e.g., in AI or cybersecurity).
 *   - The AI Search Node (powerless/trapped): An agent for whom the rule is
 *     an inescapable law of its operation.
 *   - The Analytical Observer: Sees the full structure as a logical mountain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a mathematical theorem, Minimax has near-zero intrinsic
% extractiveness. It doesn't take value, it describes an equilibrium. The
% low value reflects the "opportunity cost" of not using a riskier strategy.
domain_priors:base_extractiveness(minimax_decision_rule, 0.05).

% Rationale: Suppression is also very low. The rule doesn't suppress
% alternatives through coercion; it simply renders them logically suboptimal
% within the confines of a zero-sum game.
domain_priors:suppression_score(minimax_decision_rule, 0.02).
domain_priors:theater_ratio(minimax_decision_rule, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(minimax_decision_rule, extractiveness, 0.05).
narrative_ontology:constraint_metric(minimax_decision_rule, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(minimax_decision_rule, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% accessibility_collapse: Within a zero-sum game, the logic of minimax is
%   inescapable for optimal play. Alternatives are logically foreclosed.
narrative_ontology:constraint_metric(minimax_decision_rule, accessibility_collapse, 0.98).
% resistance: Meaningful resistance to a mathematical theorem is incoherent.
narrative_ontology:constraint_metric(minimax_decision_rule, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(minimax_decision_rule, mountain).
narrative_ontology:human_readable(minimax_decision_rule, "Minimax Decision Rule").

% --- Binary flags ---
% No active enforcement needed; it's a principle of logic.

% --- Emergence flag (required for mountain constraints) ---
% The minimax theorem emerges naturally from the logical structure of
% zero-sum games. It was discovered, not designed or enforced.
domain_priors:emerges_naturally(minimax_decision_rule).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% For a Mountain, these are not strictly required, but they are included here
% to explain the perspectival gap where beneficiaries see it as a Rope.
% They represent perceived, rather than structural, roles.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(minimax_decision_rule, system_architects).
narrative_ontology:constraint_beneficiary(minimax_decision_rule, defensive_security_systems).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(minimax_decision_rule, innovative_strategists).
narrative_ontology:constraint_victim(minimax_decision_rule, high_variance_traders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE AI SEARCH NODE (MOUNTAIN)
% For an individual node in a search tree, the minimax value is a natural law.
% It has no agency to "hope" for a better outcome; it is bound by the
% recursive logic. The metrics (ε=0.05, S=0.02) place this firmly in the
% Mountain category.
constraint_indexing:constraint_classification(minimax_decision_rule, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SYSTEM ARCHITECT (ROPE)
% For the architect, Minimax is a "Rope"—a tool for functional coordination.
% It allows them to coordinate a standard of security by assuming a worst-case
% adversary, pulling the system toward a stable, defensible state. The
% engine derives a negative chi (d≈0.05 -> f(d)≈-0.12), classifying this as Rope.
constraint_indexing:constraint_classification(minimax_decision_rule, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STRATEGIC INNOVATOR (MOUNTAIN)
% For the innovator seeking a high-risk breakthrough, the rule feels like a
% Snare, but is structurally a Mountain. It's a logical limit of the game.
% The algorithm’s refusal to deviate from optimal play isn't extraction; it's
% the boundary of the problem space. The innovator is not being trapped by a
% mechanism, but is confronting an unchangeable feature of the environment.
constraint_indexing:constraint_classification(minimax_decision_rule, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical view confirms the constraint's fundamental nature. With
% ε=0.05 and suppression=0.02, the metrics fall squarely within the Mountain
% thresholds, regardless of perspective scaling. This classification must
% match the constraint_claim.
constraint_indexing:constraint_classification(minimax_decision_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(minimax_decision_rule_tests).

test(perspectival_gap) :-
    % Verify the gap between the agent confronting the logic and the agent using it.
    constraint_indexing:constraint_classification(minimax_decision_rule, TypeTarget,
        context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(minimax_decision_rule, TypeBeneficiary,
        context(agent_power(institutional), _, arbitrage, _)),
    TypeTarget == mountain,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(mountain_threshold_validation) :-
    % Verify that the base metrics are consistent with the Mountain claim.
    narrative_ontology:constraint_metric(minimax_decision_rule, extractiveness, E),
    narrative_ontology:constraint_metric(minimax_decision_rule, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(minimax_decision_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This regeneration resolves a structural contradiction in the original file.
 *   The original claimed the constraint was a 'mountain' but gave it metrics
 *   (ε=0.3, S=0.2) that violated mountain thresholds, causing lint errors.
 *   It also incorrectly claimed the constraint could be a 'snare' from one
 *   perspective, which is impossible for a constraint with mountain-level
 *   base metrics.
 *
 *   This version commits to a single, coherent interpretation: the Minimax
 *   *theorem* is a Mountain of logic (ε=0.05, S=0.02). The "extraction" of
 *   opportunity cost is a feature of the zero-sum environment, not the rule
 *   itself.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between those who experience the rule as an unchangeable
 *   boundary (Mountain) and those who choose to use it as a tool for creating
 *   order and safety (Rope). The System Architect sees a Rope because they
 *   have arbitrage exit—they can choose other rules (e.g., expectimax) for
 *   different games. The Strategic Innovator, stuck within a zero-sum game,
 *   sees an impassable Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (system_architects) are those who leverage the logical
 *   guarantee of Minimax for stability. Victims (innovative_strategists) are
 *   those whose strategies are rendered suboptimal by this same logic. The
 *   beneficiary/victim declarations allow the engine to derive the Rope vs.
 *   Mountain perspectives correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the mathematical principle as a Mountain, we avoid
 *   mislabeling a logical limit as a coercive Snare. The feeling of being
 *   "snared" is a subjective response to the rules of the game, not a
 *   property of the decision algorithm. A separate constraint story could be
 *   written for the *policy* of misapplying Minimax to non-zero-sum games,
 *   which would likely be a Tangled Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_minimax_decision_rule,
    'Is this constraint the mathematical theorem (Mountain) or the policy of applying it (Tangled Rope)?',
    'Decomposition analysis. Write a separate story for "policy_of_minimax_in_nongeneral_games" and compare its metrics. If its ε is significantly higher, the decomposition is valid.',
    'If they are the same constraint, then logic itself can be extractive. If they are different, it confirms that extraction arises from misapplication, not from the logic itself.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(minimax_decision_rule, 1928, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required, as base_extractiveness (0.05) is below the 0.46 threshold.
% The metrics of a mathematical theorem are stable over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(minimax_decision_rule, information_standard).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(minimax_decision_rule, zero_sum_game_assumption).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options correctly models the perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */