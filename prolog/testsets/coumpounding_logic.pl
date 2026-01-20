% ============================================================================
% CONSTRAINT STORY: compounding_logic
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: General Economic Theory & Berkshire Hathaway 2024 Letter
% ============================================================================

:- module(constraint_compounding_logic, []).

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
 * * constraint_id: compounding_logic
 * human_readable: The Law of Compounding Returns
 * domain: economic/mathematical
 * temporal_scope: Universal / Long-term
 * spatial_scope: Global
 * * SUMMARY:
 * Compounding is the mathematical constraint where the value of a system increases 
 * [cite_start]exponentially because earnings are reinvested to generate further earnings[cite: 165]. 
 * It is constrained by three primary variables: time, rate of return, and the 
 * [cite_start]stability of the underlying medium (currency or asset)[cite: 146, 173].
 * * KEY AGENTS:
 * - [cite_start]The Saver (Individual Powerless): Must forgo current consumption for future gain[cite: 164].
 * - [cite_start]The Capital Allocator (Institutional): Manages the reinvestment cycles[cite: 10, 135].
 * - The Debtor (Individual Powerless): Experiences compounding as an extractive force.
 * * NARRATIVE ARC:
 * Compounding starts "tiny, almost meaningless" but "mushrooms" over time, provided 
 * [cite_start]the "culture of savings" is sustained[cite: 165]. It rewards "fidelity" and 
 * "long-term compounding" while punishing "fiscal folly" and "mistakes" in capital 
 * [cite_start]allocation[cite: 10, 146, 165].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(compounding_logic_analysis, 0, 10).
narrative_ontology:constraint_claim(compounding_logic, rope).

% Base extractiveness score: 0.2 (for wealth creation) / 0.9 (for debt)
% Rationale: In a savings context, it creates value; in a debt context, it is 
% highly extractive. We average to 0.5 for the general logic.
domain_priors:base_extractiveness(compounding_logic, 0.5).

% Suppression score: 0.4
[cite_start]% Rationale: One can choose not to participate by consuming immediately[cite: 164], 
% though the mathematical "rules" of the results cannot be altered.
domain_priors:suppression_score(compounding_logic, 0.4).

[cite_start]% Enforcement requirements: Requires "stable currency" and "wisdom"[cite: 173].
domain_priors:requires_active_enforcement(compounding_logic).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(compounding_logic, extractiveness, 0.5).
narrative_ontology:constraint_metric(compounding_logic, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(compounding_logic, [long_term_savers, posterity, capital_allocators]).
constraint_victim(compounding_logic, [the_short_sighted, debtors, the_impatient]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LONG-TERM SAVER - Rope
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_powerless - The person "foregoing dividends"[cite: 164].
   [cite_start]WHEN: generational - Thinking in "decades" rather than single years[cite: 88].
   [cite_start]WHERE: arbitrage - Utilizing the "magic of long-term compounding"[cite: 165].
   [cite_start]SCOPE: global - Reinvesting across diverse "marketable equities"[cite: 128].
   
   WHY THIS CLASSIFICATION:
   For the saver, compounding is a "Rope"—a functional coordination mechanism 
   [cite_start]that turns "tiny" reinvestment into a "mushroomed" result[cite: 165]. It 
   [cite_start]requires the agent to "rest assured" and trust the system[cite: 144].
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    compounding_logic,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(generational),
        exit_options(arbitrage),
        constraint_beneficiary(compounding_logic, long_term_savers),
        constraint_victim(compounding_logic, []),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(compounding_logic, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SHORT-TERM SPECULATOR - Mountain
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_moderate - One attempting to "come and go on a dime"[cite: 133].
   [cite_start]WHEN: immediate - Focused on "year-by-year numbers"[cite: 87].
   [cite_start]WHERE: trapped - Bound by the "unpredictable" swings of the market[cite: 87].
   [cite_start]SCOPE: local - Focused on "single year" performance[cite: 88].
   
   WHY THIS CLASSIFICATION:
   For the speculator, the time requirement of compounding is a "Mountain." 
   [cite_start]They cannot force "decades" of results into a single year[cite: 88]. The 
   mathematical necessity of time is an unchangeable law they must endure.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    compounding_logic,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(compounding_logic, []),
        constraint_victim(compounding_logic, the_impatient),
        spatial_scope(local)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DEBTOR - Noose
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_powerless - One subject to "runaway" interest or "fiscal folly"[cite: 146, 193].
   [cite_start]WHEN: immediate - The "cash register rings" for the lender, not them[cite: 89].
   [cite_start]WHERE: constrained - "Paper money" value evaporates, leaving them with debt[cite: 146].
   [cite_start]SCOPE: national - Affected by "monetary instability"[cite: 149].
   
   WHY THIS CLASSIFICATION:
   For the debtor, compounding is a "Noose." It is a coercive mechanism that 
   extracts their future labor at an accelerating rate. It creates a 
   [cite_start]"Hemorrhaging cash" situation that is "hard to ignore"[cite: 179].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    compounding_logic,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(compounding_logic, lenders),
        constraint_victim(compounding_logic, debtors),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(compounding_logic, E),
    E > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(compounding_logic_tests).

test(time_as_mountain) :-
    % Verify that immediate time horizons treat compounding as an immutable Mountain
    constraint_indexing:constraint_classification(compounding_logic, mountain, context(_, immediate, _, _, _, _)).

test(alignment_check) :-
    % Verify that for the long-term saver (Rope), extractiveness is low
    domain_priors:base_extractiveness(compounding_logic, E),
    E =< 0.5.

test(enforcement_dependency) :-
    [cite_start]% Compounding requires active enforcement of "stable currency" [cite: 173]
    domain_priors:requires_active_enforcement(compounding_logic).

:- end_tests(compounding_logic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVAL VARIANCE: Compounding is the ultimate "Rope" for the 
 * patient, but the ultimate "Noose" for the borrower. I used the 2024 
 * Berkshire letter to ground the "Rope" perspective in the "magic" of 
 * [cite_start]reinvestment[cite: 165].
 * 2. THE MOUNTAIN OF TIME: I identified Time as the primary "Mountain" 
 * constraint. You cannot "arbitrage" time in a compounding equation; 
 * [cite_start]it is a zero-degree-of-freedom variable for the "impatient"[cite: 88].
 * 3. MEDIUM STABILITY: I noted that compounding is contingent on the 
 * "Rope" of a stable currency; if "fiscal folly" prevails, the 
 * [cite_start]mathematical gain is negated by value evaporation[cite: 146, 173].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reinvestment_availability,
    [cite_start]"Will there always be 'really outstanding businesses' or 'gems' available for reinvestment? [cite: 127]",
    [cite_start]resolution_mechanism("Monitoring market saturation and 'knee-deep' vs 'nothing looks compelling' cycles[cite: 129]."),
    impact("If gems disappear: The Rope of compounding goes slack, and the system stalls."),
    confidence_without_resolution(medium)
).

omega_variable(
    monetary_survival,
    [cite_start]"Can individuals truly 'cope with monetary instability' through equities? [cite: 149]",
    resolution_mechanism("Historical analysis of equity performance during hyperinflation."),
    impact("If no: Compounding in paper-denominated assets becomes a Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Immediate Consumption (The "Anti-Compounding" Model)
 * [cite_start]Viability: The default human behavior[cite: 164].
 * Suppression: "America would have been spinning its wheels" if all was 
 * [cite_start]consumed[cite: 159].
 * * ALTERNATIVE 2: Linear Growth (Simple Interest)
 * Viability: Lower risk, lower coordination requirement.
 * [cite_start]Suppression: Fails to "mushroom" or create "miracles" over 60 years[cite: 164, 165].
 * * CONCLUSION:
 * Compounding is a "Rope" that requires the active suppression of the 
 * "Alternative" (immediate consumption) to function.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(coumpounding_logic, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(coumpounding_logic, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(coumpounding_logic, noose, agent_power(individual_powerless)).
