% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: us_two_party_duopoly
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Pew Research (2026), CFR Task Force Report 83 (2025), and Election Analysis
% ============================================================================

:- module(constraint_us_two_party_duopoly, []).

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
 * * constraint_id: us_two_party_duopoly
 * human_readable: The U.S. Two-Party Duopoly and the Third-Party "Spoiler" Constraint
 * domain: political
 * temporal_scope: 2024-2026 (Strategic landscape)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * The U.S. electoral landscape is structurally locked into a two-party system that 37% of Americans 
 * explicitly wish to expand. This constraint functions through "winner-take-all" 
 * mechanics and ballot access barriers, creating a "spoiler effect" where third-party 
 * support often benefits the voter's least preferred major candidate.
 * * KEY AGENTS:
 * - Disillusioned Voter ("Double Hater"): Voters who view both parties as "too extreme" and feel trapped.
 * - Major Party Strategist (Institutional): Actors who maintain the system by "appropriating" third-party issues to neutralize threats.
 * - Third-Party Candidate (The "Sting"): Candidates who highlight neglected issues but rarely achieve electoral viability.
 * * NARRATIVE ARC:
 * The 2024 election and 2026 midterm cycle show an "inflection point" where high partisan 
 * antipathy and dim views of both parties (61% see GOP as extreme; 57% see Dems as extreme) 
 * drive record interest in alternatives, yet the structural "Noose" of the duopoly keeps 
 * power concentrated in the two-party stack.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(us_election_cycle_2026, 2024, 2026).
narrative_ontology:constraint_claim(us_two_party_duopoly, noose).

% Base extractiveness score: High (0.75)
% Rationale: The two major parties capture nearly 100% of legislative and executive seats 
% despite significant public demand for more options (37%-44% among younger adults).
domain_priors:base_extractiveness(us_two_party_duopoly, 0.75).

% Suppression score: High (0.80)
% Rationale: Intentional ballot access barriers, the 15% debate threshold, and the 
% mathematical "spoiler" penalty actively suppress the visibility and viability of alternatives.
domain_priors:suppression_score(us_two_party_duopoly, 0.80).

% Enforcement: Requires active enforcement
% Rationale: Major parties actively use litigation to keep challengers off ballots and 
% re-draw districts to maintain control.
domain_priors:requires_active_enforcement(us_two_party_duopoly).

% Beneficiaries and Victims
constraint_beneficiary(us_two_party_duopoly, [republican_party, democratic_party]).
constraint_victim(us_two_party_duopoly, [independent_voters, third_party_candidates, young_voters]).

narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_two_party_duopoly, suppression_requirement, 0.80).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: YOUNG DISILLUSIONED VOTER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (Price-takers in a binary choice system)
   WHEN: immediate (Voting in the 2026 midterms)
   WHERE: trapped ( estrutural barriers prevent a viable third option)
   SCOPE: national (The entire federal system)
   
   WHY THIS CLASSIFICATION:
   For this agent, the system is a Noose because it forces them to "light their ballot 
   on fire" or vote for an "extreme" major party they don't like.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    us_two_party_duopoly,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MAJOR PARTY LEADERSHIP - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making power over the electoral system)
   WHEN: biographical (Long-term party coalition building)
   WHERE: mobile (Can shift platforms to absorb external threats)
   SCOPE: national (Federal governance)
   
   WHY THIS CLASSIFICATION:
   To the parties, the duopoly is a Rope—a functional coordination mechanism that 
   creates a "big tent" to build majorities and prevents radical fragmentation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    us_two_party_duopoly,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: POLITICAL SCIENTIST / ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of the structural "laws" of voting)
   WHEN: historical (The endurance of the system since 1856)
   WHERE: analytical (Unconstrained by the choice, observing the math)
   SCOPE: national (US-specific plurality rules)
   
   WHY THIS CLASSIFICATION:
   From an analytical view, the duopoly is a Mountain—the natural and unchangeable 
   result of Duverger's Law in a winner-take-all, first-past-the-post system.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    us_two_party_duopoly,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (POLITICAL SCIENTIST) - Mountain
   --------------------------------------------------------------------------
   WHO: agent_power(analytical) - Observes the unyielding electoral physics.
   WHEN: generational - The FPTP structure has been fixed for centuries.
   WHERE: trapped - Bound by the constitutional and statutory electoral rules.
   SCOPE: global - Affects the policy direction of the global superpower.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is Duverger's Law and the FPTP architecture. The 
   mathematical pressure toward two-party stability is an unchangeable 
   structural invariant of the American terrain. While voters feel this 
   as a Noose and parties use it as a Rope for gatekeeping, the analyst 
   recognizes it as an immutable fact: the system is mathematically 
   designed to suppress third-party viability regardless of voter intent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    us_two_party_duopoly,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(us_two_party_duopoly),
    !.

% Explicit priors reflecting the rigid, high-friction nature of the duopoly.
domain_priors:base_extractiveness(us_two_party_duopoly, 0.4).
domain_priors:suppression_score(us_two_party_duopoly, 0.8).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(duopoly_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(us_two_party_duopoly, noose, context(individual_powerless, immediate, trapped, national)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, rope, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, mountain, context(analytical, historical, analytical, national)).

test(power_mobility_correlation) :-
    % Institutional power sees mobile exit options (Rope); Powerless sees trapped (Noose)
    constraint_indexing:constraint_classification(us_two_party_duopoly, noose, context(individual_powerless, _, trapped, _)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, rope, context(institutional, _, mobile, _)).

:- end_tests(duopoly_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-20
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * The report data shows a massive gap between public preference (37% want more 
 * parties) and structural reality (0% major third-party seats). This 100% 
 * concentration of power by the duopoly is highly extractive.
 * * 2. PERSPECTIVE SELECTION:
 * The "Double Hater" (individual_powerless) vs. "Party Strategist" (institutional) 
 * best illustrates the Perspectival Gap. One group feels suffocated by the choice, 
 * while the other sees it as a stabilizing "coordination tool."
 * * 3. CLASSIFICATION RATIONALE:
 * Categorized as a Mountain for the Analyst because without changing the "first-past-
 * the-post" rules, the two-party outcome is mathematically forced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rcv_adoption_tipping_point,
    "Will Ranked Choice Voting (RCV) reach a critical mass of states to break the spoiler math?",
    resolution_mechanism("Monitor state-level ballot initiatives through 2028."),
    impact("If yes, the Mountain (structural lock) becomes a Rope (changeable choice)."),
    confidence_without_resolution(medium)
).

omega_variable(
    latino_voter_realignment,
    "Will the demographic shift of Latino voters toward the GOP consolidate the two-party system or fracture it?",
    resolution_mechanism("Analyze 2026 midterm precinct-level data."),
    impact("High impact on whether third-party alternatives (e.g. Workers parties) gain local traction."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Ranked Choice Voting (RCV)
 * Viability: Actively promoted as the only way to avoid "wasted" third-party votes.
 * Suppression: Blocked by major party legislation in several states to maintain binary control.
 * * ALTERNATIVE 2: "Vocal and Local" Strategy
 * Viability: Focus on state/county levels to build a local imprint before national runs.
 * Suppression: Ignored by major donors and national media who focus only on presidential "spoilers."
 * * CONCLUSION:
 * The existence of suppressed alternatives like RCV confirms the Noose classification 
 * for the powerless voter.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
