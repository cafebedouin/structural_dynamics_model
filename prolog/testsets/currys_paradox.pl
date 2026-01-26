% ============================================================================
% CONSTRAINT STORY: currys_paradox
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Logic / Epistemology / Curry's Paradox
% ============================================================================

:- module(constraint_currys_paradox, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: currys_paradox
 * human_readable: Curry's Paradox
 * domain: analytical/logic/mathematics
 * temporal_scope: Permanent (Historical to Civilizational)
 * spatial_scope: Universal (Formal Systems)
 * * SUMMARY:
 * Curry's Paradox is a logical "trap" that proves any arbitrary claim 
 * (e.g., "The moon is made of cheese") using a self-referential sentence of 
 * the form "If this sentence is true, then X is true." Unlike the Liar 
 * Paradox, it does not require negation, making it a more fundamental threat 
 * to naive set theory and unrestricted comprehension.
 * * KEY AGENTS:
 * - The Foundationalist: Attempting to build a system where only "true" things 
 * can be derived.
 * - The System Architect: Designing programming languages or formal logics 
 * that must avoid trivialism (where everything is provable).
 * - The Trivialist: An agent who accepts all propositions (the "victim" of 
 * total systemic collapse).
 * * NARRATIVE ARC:
 * The paradox acts as a structural "No-Go" theorem. It forces formal systems 
 * to either limit self-reference, restrict the rules of implication (like 
 * contraction), or face "explosion," where the system loses all meaning because 
 * it permits every possible statement to be true.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(currys_paradox_interval, 0, 10).
narrative_ontology:constraint_claim(currys_paradox, snare).

% Base extractiveness: 0.2 (Low-Moderate)
% Rationale: It extracts "meaning" from a system by making it trivial. If a 
% system is trivialized, the value of every true statement is diluted to zero.
domain_priors:base_extractiveness(currys_paradox, 0.2).

% Suppression score: 0.5 (Moderate)
% Rationale: Alternatives that allow both self-reference and standard 
% implication are actively suppressed because they lead to immediate systemic 
% collapse (Explosion).
domain_priors:suppression_score(currys_paradox, 0.5).

% Enforcement requirements: Emerges naturally from the interaction of 
% implication, self-reference, and contraction.
domain_priors:emerges_naturally(currys_paradox).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(currys_paradox, extractiveness, 0.2).
narrative_ontology:constraint_metric(currys_paradox, suppression_requirement, 0.5).

% Beneficiaries: Paraconsistent Logicians (those who study systems that 
% tolerate contradictions without collapsing).
constraint_beneficiary(currys_paradox, paraconsistent_logicians).

% Victims: Naive System Architects (those whose systems "explode" under 
% simple self-reference).
constraint_victim(currys_paradox, naive_set_theorists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICIAN (Institutional) - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: Institutional/Analytical power (The gatekeepers of formal proof).
   WHEN: Civilizational (Permanent structural reality).
   WHERE: Trapped (Mathematics cannot "vote" the paradox away).
   SCOPE: Global/Universal.
   
   WHY THIS CLASSIFICATION:
   For the mathematician, the paradox is a Mountain. It is an inherent feature 
   of the landscape of logic. One does not "fix" Curry's Paradox; one builds 
   fences (types, hierarchies) to ensure they never accidentally walk into it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    currys_paradox,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROGRAMMER (Individual Moderate) - ROPE
   --------------------------------------------------------------------------
   
   WHO: Individual Moderate (Builds software using logical rules).
   WHEN: Biographical/Immediate (Focused on the runtime of a system).
   WHERE: Mobile (Can switch languages or type systems).
   SCOPE: Regional (Specific to a language environment).
   
   WHY THIS CLASSIFICATION:
   To a programmer, the paradox is a Rope. It is a tool for understanding the 
   risks of recursion and untyped lambdas. It is a coordination mechanism 
   that helps them design safer compilers. If a specific logic is "Curry-unstable," 
   they simply exit to a different language.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    currys_paradox,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LOGICAL REFORMER (Individual Powerless) - NOOSE
   --------------------------------------------------------------------------
   
   WHO: Individual Powerless (Trying to propose new logical foundations).
   WHEN: Historical.
   WHERE: Constrained (By the "Explosion Principle").
   SCOPE: National (Within the academic discipline).
   
   WHY THIS CLASSIFICATION:
   For a reformer who wants to allow natural language's "unrestricted 
   comprehension," Curry's Paradox is a Snare. It is the "threat" used by the 
   establishment to shut down more flexible logics. "If you allow that rule," 
   they say, "the Snare tightens and your entire system explodes into triviality."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    currys_paradox,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(currys_paradox_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(currys_paradox, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(currys_paradox, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(currys_paradox, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % For a powerless reformer, the "explosion" (extraction of meaning) is total.
    % For a powerful institution, they've already mitigated it.
    ContextPowerless = context(individual_powerless, historical, constrained, national),
    ContextPowerful = context(institutional, civilizational, trapped, global),
    domain_priors:base_extractiveness(currys_paradox, Score),
    Score > 0.0.

test(time_immutability) :-
    % Long-term civilizational view = Mountain (immutable)
    constraint_indexing:constraint_classification(currys_paradox, mountain, context(_, civilizational, _, _)).

:- end_tests(currys_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.2): Chose a low-moderate score. While it doesn't 
 * extract labor, it extracts "information utility" by rendering a 
 * system useless if triggered.
 * * 2. SUPPRESSION (0.5): This is higher than the Liar Paradox because the 
 * logical community is very aggressive about suppressing systems that 
 * cannot handle Curry, as "Explosion" is seen as the death of logic.
 * * 3. NOOSE SELECTION: The "Snare" classification arises from the way 
 * the paradox is used to gatekeep which logics are "respectable."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    relevance_logic_viability,
    "Can 'Relevance Logic' (which rejects contraction) be a stable foundation for human-AI coordination?",
    resolution_mechanism("Development and large-scale deployment of a relevance-based theorem prover"),
    impact("If Yes: Curry's Paradox moves from Mountain to Rope (optional tool). If No: It remains a Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Rejecting 'Contraction' (Relevance Logic)
 * Viability: High in niche logic, Low in mainstream math.
 * Suppression: High. Most mathematicians view the rule (A -> (A -> B)) |- (A -> B) 
 * as "natural law."
 * * ALTERNATIVE 2: Fixed-point hierarchies (Tarski-style)
 * Viability: High. The standard way to avoid the paradox.
 * Suppression: Low (it is the dominant paradigm).
 * * CONCLUSION:
 * Because Alternatives exist (like rejecting contraction) but are cognitively 
 * suppressed by the "intuitive" nature of standard implication, the paradox 
 * often functions as a Snare for alternative logical architectures.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_currys_paradox].
 * 2. Run multi-perspective report: ?- constraint_indexing:multi_index_report(currys_paradox).
 * 3. Run tests: ?- run_tests(currys_paradox_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
