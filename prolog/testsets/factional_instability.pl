% ============================================================================
% CONSTRAINT STORY: factional_instability
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Federalist Paper No. 10 (James Madison)
% ============================================================================

:- module(constraint_factional_instability, []).

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
 * * constraint_id: factional_instability
 * human_readable: The Violence of Faction
 * domain: political/economic
 * temporal_scope: 1787-1788 (Constitutional Ratification)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * Madison identifies "faction" as a group of citizens, whether a majority or 
 * minority, united by a common passion or interest adverse to the rights of 
 * [cite_start]other citizens or the permanent interests of the community[cite: 1]. 
 * The constraint is the inherent tendency of popular governments to be 
 * unstable and unjust due to these conflicting interests, particularly 
 * [cite_start]those arising from the unequal distribution of property[cite: 1].
 * * KEY AGENTS:
 * - The Enlightened Statesman: The institutional ideal capable of adjusting 
 * [cite_start]clashing interests, though not always available[cite: 1].
 * - [cite_start]The Creditor Class: A faction seeking to protect property rights[cite: 1].
 * - The Debtor Class: A faction often seeking "wicked projects" like paper 
 * [cite_start]money or abolition of debts[cite: 1].
 * - The Majority Faction: The most dangerous agent in a democracy, capable 
 * [cite_start]of sacrificing the public good to its ruling passion[cite: 1].
 * * NARRATIVE ARC:
 * The text moves from the "mortal disease" of factional strife toward a 
 * [cite_start]technological solution: the "Republican remedy"[cite: 1]. It argues that 
 * since the causes of faction (liberty and human nature) cannot be removed 
 * without destroying liberty, the effects must be controlled through a 
 * [cite_start]large-scale representative republic[cite: 1].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(federalist_10_analysis, 0, 10).
narrative_ontology:constraint_claim(factional_instability, noose).

% Base extractiveness score: High
% Rationale: Factions inherently seek to extract value or rights from others 
[cite_start]% to serve their own "ruling passion"[cite: 1].
domain_priors:base_extractiveness(factional_instability, 0.7).

% Suppression score: Moderate
% Rationale: Madison argues that suppressing the *causes* of faction is 
% impossible without destroying liberty, but the *effects* can be diluted 
[cite_start]% by the system[cite: 1].
domain_priors:suppression_score(factional_instability, 0.5).

% Enforcement requirements: Emerges naturally from human nature
domain_priors:emerges_naturally(factional_instability).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(factional_instability, extractiveness, 0.7).
narrative_ontology:constraint_metric(factional_instability, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(factional_instability, majority_coalitions).
constraint_victim(factional_instability, minority_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MINORITY (Property Owner) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - In a pure democracy, a minority has no 
   [cite_start]structural defense[cite: 1].
   WHEN: immediate - The threat of debt relief or redistribution is 
   [cite_start]ever-present[cite: 1].
   WHERE: trapped - Within a small democracy, there is no physical or 
   [cite_start]conceptual exit from majority rule[cite: 1].
   [cite_start]SCOPE: local - Small societies where a common passion is easily felt[cite: 1].
   
   WHY THIS CLASSIFICATION:
   For the minority owner, faction is a "Noose" because a majority can 
   [cite_start]legally use the mechanism of government to extract his property[cite: 1]. 
   It is asymmetric and coercive.
   
   NARRATIVE EVIDENCE:
   "A rage for paper money, for an abolition of debts, for an equal 
   [cite_start]division of property"[cite: 1].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    factional_instability,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(factional_instability, majority_faction),
        constraint_victim(factional_instability, property_owners),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(factional_instability, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FOUNDING ARCHITECT (Madison) - Rope
   --------------------------------------------------------------------------
   
   [cite_start]WHO: institutional - Designing the "structure of the government"[cite: 1].
   [cite_start]WHEN: historical - Building a system to last for generations[cite: 1].
   WHERE: arbitrage - Using the "extent of the Union" to play factions 
   [cite_start]against each other[cite: 1].
   [cite_start]SCOPE: national - The "large over the small republic"[cite: 1].
   
   WHY THIS CLASSIFICATION:
   Madison views the *control* of faction as a "Rope"—a functional 
   mechanism of representative government that coordinates diverse 
   [cite_start]interests to achieve the public good[cite: 1].
   
   NARRATIVE EVIDENCE:
   "The latent causes of faction are thus sown in the nature of man... 
   The regulation of these various and interfering interests forms the 
   [cite_start]principal task of modern legislation"[cite: 1].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    factional_instability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(factional_instability, public_good),
        constraint_victim(factional_instability, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: HUMAN NATURE - Mountain
   --------------------------------------------------------------------------
   
   [cite_start]WHO: analytical - Observing the "nature of man"[cite: 1].
   [cite_start]WHEN: civilizational - A permanent feature of human society[cite: 1].
   [cite_start]WHERE: trapped - "As long as the reason of man continues fallible"[cite: 1].
   [cite_start]SCOPE: global - Applicable to all societies with liberty[cite: 1].
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the *existence* of faction is a 
   "Mountain." It is an unchangeable law of nature that cannot be removed 
   [cite_start]without destroying the liberty that is essential to political life[cite: 1].
   
   NARRATIVE EVIDENCE:
   "Liberty is to faction, what air is to fire... But it could not be a 
   [cite_start]less folly to abolish liberty"[cite: 1].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    factional_instability,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        constraint_beneficiary(factional_instability, []),
        constraint_victim(factional_instability, []),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(factional_instability, S),
    S < 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(factional_instability_tests).

test(multi_perspective_variance) :-
    % Test that Madison (Institutional) and Minority (Powerless) see different types
    constraint_indexing:constraint_classification(factional_instability, rope, context(agent_power(institutional), _, _, _, _, _)),
    constraint_indexing:constraint_classification(factional_instability, noose, context(agent_power(individual_powerless), _, _, _, _, _)).

test(extraction_experience) :-
    % Minority experiences high extraction (Noose)
    constraint_indexing:extractiveness_for_agent(factional_instability, context(individual_powerless, _, trapped, _, _, _), Score),
    Score > 0.5.

test(immutability_of_liberty) :-
    % The source of faction is as unchangeable as air (Mountain)
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(factional_instability_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.7): Madison explicitly defines factions as 
 * [cite_start]adverse to the rights of others[cite: 1]. The "debtor/creditor" example 
 * [cite_start]highlights a zero-sum extractive struggle[cite: 1].
 * * 2. PERSPECTIVE SELECTION: Analyzed the Minority (Noose) vs. the Legislator 
 * (Rope) to demonstrate the core tension of the Federalist project. Added 
 * the Analytical (Mountain) view to ground the argument in Madison's 
 * [cite_start]"human nature" premise[cite: 1].
 * * 3. CLASSIFICATION RATIONALE:
 * - Minority -> Noose: They are the specific targets of factional 
 * [cite_start]extraction in small democracies[cite: 1].
 * - Madison -> Rope: He treats faction as a manageable energy that 
 * [cite_start]can be coordinated through constitutional "filtration"[cite: 1].
 * - [cite_start]Human Nature -> Mountain: Madison admits the causes cannot be removed[cite: 1].
 * * 4. OMEGAS:
 * omega_variable(statesman_availability,
 *	"Can 'enlightened statesmen' be relied upon to adjust interests?",
 *	resolution_mechanism("Historical track record of legislative impartiality"),
 *	impact("If yes: Rope is stable. If no: System reverts to Noose."),
 *	confidence_without_resolution(low)
 *	).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Removing the Causes (Uniformity of Passion)
 * [cite_start]Viability: Theoretically possible through totalitarianism[cite: 1].
 * [cite_start]Suppression: Madison rejects this because it is "worse than the disease"[cite: 1].
 * * ALTERNATIVE 2: Pure Democracy
 * [cite_start]Viability: Possible in small scales[cite: 1].
 * Suppression: Rejected due to the inability to control the effects 
 * [cite_start]of majority faction[cite: 1].
 * * CONCLUSION:
 * Madison's rejection of these alternatives as either "impossible" or 
 * "undesirable" reinforces his view of the Large Republic as the only 
 * [cite_start]viable "Rope"[cite: 1].
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraints/factional_instability].
% Report: ?- constraint_indexing:multi_index_report(factional_instability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
