% ============================================================================
% CONSTRAINT STORY: sorites_paradox
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Eubulides of Miletus / Vagueness in Language and Logic
% ============================================================================

:- module(constraint_sorites_paradox, []).

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
 * * constraint_id: sorites_paradox
 * human_readable: The Sorites Paradox (The Paradox of the Heap)
 * domain: social/technological/legal
 * temporal_scope: Permanent (Linguistic/Logical Architecture)
 * spatial_scope: Global (All Categorical Systems)
 * * SUMMARY:
 * The Sorites Paradox arises from vague predicates where a small change (e.g., 
 * removing one grain of sand) does not seemingly change the category (a "heap"), 
 * yet successive small changes eventually lead to a total categorical shift. 
 * It represents the fundamental difficulty of defining hard boundaries in a 
 * continuous reality.
 * * KEY AGENTS:
 * - The Analytic Philosopher: Analytical observer mapping the "Mountain" 
 * of logical vagueness and the failure of bivalence.
 * - The Legislative Drafter: Institutional agent using arbitrary "Ropes" 
 * (thresholds) to coordinate legal certainty (e.g., defining "adulthood" 
 * at exactly 18 years).
 * - The Marginal Subject: Individual powerless agent "strangled" by an 
 * arbitrary boundary where a single second or dollar makes them a criminal 
 * or ineligible for aid.
 * * NARRATIVE ARC:
 * Language treats reality as a "Mountain" of discrete categories. To manage 
 * the vagueness, institutions create "Ropes"—arbitrary sharp lines. However, 
 * for the person standing at the 17-year, 364-day mark, the line is a "Noose," 
 * as the system extracts their agency based on a single grain of time that 
 * lacks any biological distinction.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(sorites_interval, 0, 10).
narrative_ontology:constraint_claim(sorites_paradox, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The paradox extracts "conceptual precision." 
% It forces a cost on all legal and social systems to maintain arbitrary 
% lines, extracting fairness from those at the margins.
domain_priors:base_extractiveness(sorites_paradox, 0.4).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). It suppresses "Continuous Truth." Societies 
% must ignore the reality of gradual change to maintain the binary 
% "A vs B" logic required for law and software.
domain_priors:suppression_score(sorites_paradox, 0.7).

% Enforcement: Emerges naturally from the structure of categorical language.
domain_priors:emerges_naturally(sorites_paradox).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(sorites_paradox, extractiveness, 0.4).
narrative_ontology:constraint_metric(sorites_paradox, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(sorites_paradox, [bureaucrats, software_engineers]).
constraint_victim(sorites_paradox, [marginal_subjects, qualitative_nuance]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LOGICIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the fundamental laws of thought.
   WHEN: civilizational - Viewing vagueness as a permanent logical substrate.
   WHERE: trapped - Human language cannot exist without vague predicates.
   SCOPE: global - Universal across all human symbolic systems.
   
   WHY THIS CLASSIFICATION:
   To the logician, the paradox is a Mountain. It is an unchangeable 
   feature of "information hardware." One cannot "solve" the vagueness 
   of a heap; it is a fixed peak of ambiguity in the landscape of 
   communication that persists regardless of technology.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sorites_paradox,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TAX AUTHORITY - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define and enforce arbitrary brackets.
   WHEN: biographical - Managing a national budget over decades.
   WHERE: arbitrage - Can shift thresholds (e.g., $40,000 vs $40,001) to optimize revenue.
   SCOPE: national - Country-wide fiscal policy.
   
   WHY THIS CLASSIFICATION:
   For the institution, the paradox is solved with a Rope. They create 
   an arbitrary "sharp" line (e.g., "taxable income starts at $X"). 
   This is a coordination mechanism that pulls the system toward 
   certainty and scale, ignoring the "heap" of continuous income levels.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sorites_paradox,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(sorites_paradox, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE IMMIGRATION APPLICANT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "sharp" lines of the state.
   WHEN: immediate - The date of a deadline or an age limit.
   WHERE: trapped - Cannot exit the legal jurisdiction's binary logic.
   SCOPE: national - The reach of the state's border rules.
   
   WHY THIS CLASSIFICATION:
   For the person who misses a "cutoff" by a single day, the paradox is 
   a Noose. The law treats them as "Not Resident" or "Too Old" based 
   on a grain of sand that has no material reality. The arbitrary 
   line strangles their future, extracting their mobility and life 
   quality via the "Noose" of bureaucratic binary logic.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sorites_paradox,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(sorites_paradox, E),
    E > 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(sorites_paradox_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(sorites_paradox, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(sorites_paradox, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(sorites_paradox, noose, context(individual_powerless, immediate, trapped, national)).

test(power_extractiveness_vagueness) :-
    % Powerless individuals feel the total extraction of their agency (Noose).
    % Institutional actors use arbitrary lines for coordination (Rope).
    ContextPowerless = context(individual_powerless, immediate, trapped, national),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(sorites_paradox, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(sorites_paradox, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(sorites_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): The paradox extracts "Fairness." Because reality 
 * is a spectrum but law is a binary, those near the boundary are 
 * effectively robbed of their true status.
 * 2. PERSPECTIVES: Selected the Logician (Hardware), the Bureaucrat 
 * (Tool), and the Applicant (Victim) to highlight how vagueness 
 * becomes a structural trap.
 * 3. CLASSIFICATION RATIONALE:
 * Analytical → Mountain: Language cannot escape vague predicates.
 * Institutional → Rope: Arbitrary thresholds are the "ropes" of civilization.
 * Powerless → Noose: Cutoff points are the "nooses" for those on the wrong side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precision_logic_limit,
    "Can 'Fuzzy Logic' or probabilistic systems untie the Sorites Noose 
    (Rope), or is binary decision-making a Mountain of social efficiency?",
    resolution_mechanism("Monitor the adoption of non-binary decision 
    algorithms in high-stakes legal or medical insurance adjudication"),
    impact("If Rope: Arbitrary cutoffs can be socialized into 'dimmers.' 
    If Mountain: Binary switches are the only way to scale society."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Degree-based Legal Systems (Continuum)
 * Viability: Low. Instead of "Legal vs. Illegal," use "0.8 Legal" with 
 * proportional fines/rights.
 * Suppression: Extreme. Historically suppressed because it is too 
 * complex to calculate and enforce at scale.
 * * CONCLUSION:
 * The existence of suppressed "Continuum" models confirms that for the 
 * marginal individual, the Sorites Paradox is a Noose—their reality 
 * is continuous, but the system's Rope (the Threshold) is a trap.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_sorites_paradox].
 * 2. Multi-perspective: ?- multi_index_report(sorites_paradox).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
