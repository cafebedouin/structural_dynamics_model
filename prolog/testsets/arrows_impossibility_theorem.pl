% ============================================================================
% CONSTRAINT STORY: arrows_impossibility_theorem
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Kenneth Arrow (1951) / Social Choice and Individual Values
% ============================================================================

:- module(constraint_arrows_impossibility, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: arrows_impossibility_theorem
 * human_readable: Arrow's Impossibility Theorem
 * domain: political/economic/social
 * temporal_scope: 1951 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Social Choice Theory)
 * * SUMMARY:
 * Arrow's Impossibility Theorem proves that no rank-order voting system can 
 * convert the ranked preferences of individuals into a community-wide 
 * ranking while meeting a specific set of fair criteria: non-dictatorship, 
 * Pareto efficiency, and independence of irrelevant alternatives. It 
 * establishes an absolute mathematical limit on the fairness of collective 
 * decision-making.
 * * KEY AGENTS:
 * - The Voter (Subject): A powerless agent whose ranked preferences are 
 * inevitably "distorted" or ignored by any systemic aggregation.
 * - The Constitutional Designer (Institutional): An agent who uses the 
 * theorem as a "Rope" to choose which specific "unfairness" (e.g., 
 * susceptibility to tactical voting) to tolerate in a stable system.
 * - The Democratic Reformer (Victim): An agent for whom the theorem acts 
 * as a "Snare," as it "strangles" the search for a perfect voting system.
 * * NARRATIVE ARC:
 * Arrow's Theorem is the "Mountain" of political reality—the impossibility 
 * of a perfect social welfare function is a fixed feature of logic. In 
 * legislative design, it is a "Rope" for functional coordination, allowing 
 * architects to prioritize stability over theoretical purity. However, 
 * for the idealist, the theorem acts as a "Snare," extracting the hope 
 * of a "true" collective will (extraction) and "choking" political 
 * energy by proving that every choice is a compromise of axioms.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(arrows_era, 1951, 2026).
narrative_ontology:constraint_claim(arrows_impossibility_theorem, mountain).

% Base extractiveness: 0.6
% Rationale: High. The theorem "extracts" the possibility of a pure 
% democratic signal, forcing a permanent "tax" of systematic bias or 
% strategic vulnerability on all collective choice mechanisms.
domain_priors:base_extractiveness(arrows_impossibility_theorem, 0.6).

% Suppression: 0.4
% Rationale: Moderate. It suppresses the visibility of "perfect" 
% democracy as a viable alternative, rendering such claims 
% mathematically fraudulent.
domain_priors:suppression_score(arrows_impossibility_theorem, 0.4).

% Enforcement: Emerges naturally from the axioms of social choice.
domain_priors:emerges_naturally(arrows_impossibility_theorem).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(arrows_impossibility_theorem, extractiveness, 0.6).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(arrows_impossibility_theorem, stable_incumbents). % Systemic flaws favor the status quo.
constraint_beneficiary(arrows_impossibility_theorem, game_theorists). 
constraint_victim(arrows_impossibility_theorem, democratic_idealists).
constraint_victim(arrows_impossibility_theorem, third_party_candidates). % Strangled by IIA violations (spoilers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL VOTER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The voter cannot change the mathematical laws of aggregation.
   WHEN: immediate - True at every election cycle.
   WHERE: trapped - Bound within the rules of the existing voting system.
   SCOPE: local - Immediate impact of their ballot.
   
   WHY THIS CLASSIFICATION:
   For the voter, the impossibility of a perfectly fair aggregation is an 
   absolute Mountain. No matter how sincerely they rank their candidates, 
   the "Mountain" of Arrow's logic ensures that the system may still 
   produce a paradoxical result (e.g., the Condorcet Paradox).
   
   
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    arrows_impossibility_theorem,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CONSTITUTIONAL DESIGNER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to select the voting method (FPTP, IRV, Borda).
   WHEN: biographical - Planning the long-term stability of a state.
   WHERE: mobile - Can choose which axioms to relax to achieve stability.
   SCOPE: national - Country-wide application.
   
   WHY THIS CLASSIFICATION:
   For the designer, the theorem is a "Rope"—a functional coordination tool. 
   Knowing that a perfect system is impossible, they coordinate a "standard 
   of achievement" by selecting a method that minimizes the specific 
   unfairness they find most dangerous (e.g., prioritizing "Pareto" over "IIA").
   
   
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    arrows_impossibility_theorem,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE VOTING SYSTEM REFORMER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the agency to campaign for reform but is bound by the theorem.
   WHEN: historical - Facing a century of failed "perfect" reforms.
   WHERE: constrained - The "exit" (a fair system) is mathematically barred.
   SCOPE: global - Universal limit on collective choice.
   
   WHY THIS CLASSIFICATION:
   For the reformer, Arrow's Theorem is a "Snare." It "strangles" the hope 
   for a truly just outcome. It extracts enormous political and intellectual 
   capital (extraction) while "choking" the movement with the realization 
   that every "fix" introduces a new, equally fundamental unfairness.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    arrows_impossibility_theorem,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(arrows_impossibility_theorem, E),
    E >= 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(arrows_impossibility_tests).

test(multi_perspective_variance) :-
    % Voter -> Mountain
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, Type1, context(individual_powerless, immediate, trapped, local)),
    % Designer -> Rope
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, Type2, context(institutional, biographical, mobile, national)),
    Type1 \= Type2.

test(reform_extraction_penalty) :-
    % Reformers experience the "Snare" of high extraction (0.6).
    Context = context(individual_moderate, historical, constrained, global),
    constraint_indexing:extractiveness_for_agent(arrows_impossibility_theorem, Context, Score),
    Score >= 0.5.

test(natural_emergence) :-
    domain_priors:emerges_naturally(arrows_impossibility_theorem).

:- end_tests(arrows_impossibility_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.6):
 * Reasoning: Arrow's theorem is a "tax" on democratic legitimacy. It 
 * extracts the possibility of a consensus will to empower the 
 * arbitrariness of the system designer. It is highly extractive of 
 * political utility.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Voter (Subject), Designer (Institution), and Reformer (Victim) 
 * to show how a "Mountain" of logic becomes a "Snare" for 
 * political aspiration.
 * * 3. OMEGA IDENTIFICATION:
 * Formalized the "Cardinal Utility" uncertainty—does the Mountain 
 * hold if we stop using ranked ballots?
 */

% OMEGA IDENTIFICATION
omega_variable(
    cardinal_utility_escape,
    "Is Arrow's 'Mountain' stable if we use Cardinal Utility (e.g., Score Voting) (Scaffold)?",
    resolution_mechanism("Investigation into whether Approval or Score voting evades the IIA/Dictatorship trade-off by including intensity of preference."),
    impact("If Yes: Arrow is a 'Snare' specifically for ordinal systems. If No: It is a universal Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Range/Score Voting
 * Viability: By using cardinal scores (0-10) instead of rankings, 
 * systems like Range Voting bypass the specific "impossibility" proof, 
 * as they do not rely on ordinal rankings.
 * Suppression: Often suppressed in public discourse in favor of 
 * Ranked Choice (ordinal) which remains subject to the "Snare" of Arrow.
 * * ALTERNATIVE 2: Quadratic Voting
 * Viability: Uses "buying" votes with points to reveal preference intensity.
 * * CONCLUSION:
 * The existence of Cardinal Utility alternatives (Alternative 1) proves 
 * that the Arrow "Snare" is the specific price we pay for the "Rope" 
 * of rank-order simplicity.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_arrows_impossibility].
 * 2. Multi-perspective: ?- multi_index_report(arrows_impossibility_theorem).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
