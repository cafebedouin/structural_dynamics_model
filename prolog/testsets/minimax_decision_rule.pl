% ============================================================================
% CONSTRAINT STORY: minimax_decision_rule
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: John von Neumann (1928) / Game Theory / Decision Theory
% ============================================================================

:- module(constraint_minimax_decision_rule, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: minimax_decision_rule
 * human_readable: Minimax Decision Rule
 * domain: technological/mathematical
 * temporal_scope: 1928 - Present
 * spatial_scope: Global/Abstract (Zero-sum strategic environments)
 * * SUMMARY:
 * Minimax is a decision rule used in game theory and artificial intelligence for 
 * minimizing the possible loss for a worst-case (maximum loss) scenario. In a 
 * zero-sum game, it represents the point where one player's gain is exactly equal 
 * to the other's loss, establishing a stable but often conservative equilibrium.
 * * KEY AGENTS:
 * - The Search Algorithm (Subject): A powerless agent bound by the recursive 
 * logic of the search tree.
 * - The System Architect (Institutional): Uses Minimax as a "Rope" to guarantee 
 * baseline security in adversarial environments.
 * - The Opponent (Victim): An agent whose creative or aggressive strategies are 
 * "strangled" by an algorithm that plays perfectly to minimize their potential wins.
 * * NARRATIVE ARC:
 * Minimax functions as a "Mountain" of logical inevitability—in a zero-sum 
 * universe, it is the fundamental floor of safety. In cybersecurity and 
 * high-stakes negotiation, it is a "Rope" for functional coordination. 
 * However, for the creative agent seeking a "big win," the rule acts as a 
 * "Snare," extracting the possibility of high-reward variance and "strangling" 
 * the game into a predictable, risk-averse stalemate.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required structural anchor
narrative_ontology:interval(minimax_era, 1928, 2026).
narrative_ontology:constraint_claim(minimax_decision_rule, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.3. It "extracts" the potential for maximum gain (Maximax) in 
% favor of guaranteed minimums, imposing a "tax" of conservatism on the agent.
domain_priors:base_extractiveness(minimax_decision_rule, 0.3).

% Suppression score (0.0-1.0)
% Rationale: 0.2. It suppresses high-risk, high-reward "exploitative" strategies 
% in favor of "optimal" defensive play.
domain_priors:suppression_score(minimax_decision_rule, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(minimax_decision_rule, extractiveness, 0.3).
narrative_ontology:constraint_metric(minimax_decision_rule, suppression_requirement, 0.2).

% Enforcement: Emerges naturally from the desire to survive adversarial encounters.
domain_priors:emerges_naturally(minimax_decision_rule).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(minimax_decision_rule, conservative_investors).
constraint_beneficiary(minimax_decision_rule, adversarial_security_systems).
constraint_victim(minimax_decision_rule, high_variance_gamblers).
constraint_victim(minimax_decision_rule, innovative_asymmetric_attackers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE AI SEARCH NODE - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For an individual node in a minimax search tree, the value assigned by the 
   algorithm is a natural law. It has no agency to "hope" for a better outcome; 
   it is bound by the recursive $V = \min_{a} \max_{b} U(a,b)$.
   
   NARRATIVE EVIDENCE:
   "The algorithm does not play to win; it plays to ensure it does not lose 
   more than the value of the game."
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    minimax_decision_rule,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NETWORK DEFENDER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the defender, Minimax is a "Rope"—a tool for functional coordination. 
   It allows them to coordinate a standard of achievement (security) by 
   assuming the worst-case attacker behavior, pulling the system toward a 
   stable, defensible state.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    minimax_decision_rule,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STRATEGIC INNOVATOR - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless
   WHEN: immediate
   WHERE: constrained
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For the innovator, the rule is a "Snare." It "strangles" creative 
   risk-taking. The algorithm’s refusal to leave its conservative 
   equilibrium extracts the potential for breakthrough "big wins," trapping 
   the innovator in a cycle of marginal, defensive gains.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    minimax_decision_rule,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- 
    domain_priors:base_extractiveness(minimax_decision_rule, E),
    E >= 0.2,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(minimax_decision_rule_tests).

test(multi_perspective_variance) :-
    % Node -> Mountain
    constraint_indexing:constraint_classification(minimax_decision_rule, Type1, context(powerless, immediate, trapped, local)),
    % Defender -> Rope
    constraint_indexing:constraint_classification(minimax_decision_rule, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, global),
    constraint_indexing:extractiveness_for_agent(minimax_decision_rule, ContextPowerless, S1),
    constraint_indexing:extractiveness_for_agent(minimax_decision_rule, ContextPowerful, S2),
    S1 > S2. % Powerless agent (innovator) feels more extraction of opportunity.

test(emergence) :-
    domain_priors:emerges_naturally(minimax_decision_rule).

:- end_tests(minimax_decision_rule_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.3): Chose this because Minimax is inherently about 
 * sacrificing potential upside for safety. It extracts the "dream" 
 * of the agent to enforce the "reality" of the opponent.
 * 2. CLASSIFICATION: Captured the transition from a "Mountain" of logic for 
 * the machine to a "Snare" of boredom/stalemate for the player.
 */

% OMEGA IDENTIFICATION
omega_variable(
    adversarial_rationality,
    "Does Minimax remain a 'Mountain' if the opponent is irrational (Scaffold)?",
    resolution_mechanism("Comparison of Minimax vs Expectiminimax performance against stochastic/irrational agents."),
    impact("If irrationality prevails, Minimax is a 'Snare' that extracts unnecessary caution."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Maximax (Aggressive/Risk-seeking)
 * Viability: Useful in non-zero-sum games with high rewards.
 * Suppression: Actively suppressed in formal security/adversarial training 
 * because it leads to catastrophic failure in worst-case scenarios.
 * * ALTERNATIVE 2: Expected Utility (Bayesian/Probabilistic)
 * Viability: The standard when the opponent's strategy is predictable.
 * Conclusion: The existence of probabilistic alternatives (Ropes) makes 
 * the Minimax "Snare" only relevant when the opponent is a "Perfect Demon."
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [minimax_decision_rule].
% Analyze: ?- constraint_indexing:multi_index_report(minimax_decision_rule).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Technical constraint — mostly substantive, minimal implementation theater
domain_priors:theater_ratio(minimax_decision_rule, 0.02).
narrative_ontology:constraint_metric(minimax_decision_rule, theater_ratio, 0.02).

% --- Analytical perspective classification (missing) ---
% chi = 0.3 * 1.15 (analytical) * 1.2 (global) = 0.414
% Classification: rope
constraint_indexing:constraint_classification(minimax_decision_rule, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
