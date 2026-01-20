% ============================================================================
% CONSTRAINT STORY: viral_transmission_rates
% ============================================================================
% Generated: 2024-05-20
% Model: Gemini 2.0 Flash
% Source: Epidemiological Modeling / Public Health Policy
% ============================================================================

:- module(constraint_viral_transmission_rates, []).

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
 * * constraint_id: viral_transmission_rates
 * human_readable: Viral Transmission Rates (R0/Rt)
 * domain: technological/biological/political
 * temporal_scope: Pandemic Periods (e.g., 1918, 2020-2023)
 * spatial_scope: Global
 * * SUMMARY:
 * Viral transmission rates (represented by the basic reproduction number R0 and 
 * effective reproduction number Rt) dictate the speed and scale of a pathogen's 
 * spread through a population. These rates act as a hard biological constraint 
 * that forces societal re-organization, policy intervention, and individual 
 * behavioral change to prevent systemic collapse.
 * * KEY AGENTS:
 * - The Epidemiologist (Analytical): Observes the mathematical inevitability of exponential growth.
 * - The Public Health Official (Institutional): Uses Rt as a "Rope" to coordinate lockdowns and vaccinations.
 * - The Quarantined Citizen (Individual Powerless): Experiences the transmission rate as a "Noose" that restricts freedom and extracts labor/income.
 * * NARRATIVE ARC:
 * Initially, R0 appears as a "Mountain"—a natural law of biology. As the virus 
 * spreads, the state attempts to transform this into a "Rope" through social 
 * distancing and mandates to lower Rt. However, for those lacking financial 
 * safety nets, these mandates tighten into a "Noose," where the biological risk 
 * and the political response both extract survival capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor
narrative_ontology:interval(viral_transmission_rates, 0, 10).
narrative_ontology:constraint_claim(viral_transmission_rates, mountain).

% Base extractiveness: 0.7 (High)
% Rationale: Pathogens extract health, life, and labor from the host population. 
% The societal response (lockdowns) further extracts economic agency.
domain_priors:base_extractiveness(viral_transmission_rates, 0.7).

% Suppression: 0.8 (High)
% Rationale: During high transmission, alternatives to isolation are actively 
% punished or banned. The "natural" spread suppresses the visibility of 
% normal social functioning.
domain_priors:suppression_score(viral_transmission_rates, 0.8).

% Enforcement: Emerges naturally from biology but requires active political enforcement to mitigate.
domain_priors:emerges_naturally(viral_transmission_rates).
domain_priors:requires_active_enforcement(viral_transmission_rates).

% BENEFICIARIES & VICTIMS
% The pathogen benefits biologically; institutional power may benefit from increased emergency control.
constraint_beneficiary(viral_transmission_rates, institutional_governance).
% The primary victims are the vulnerable and the economically precarious.
constraint_victim(viral_transmission_rates, individual_powerless).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EPIDEMIOLOGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of statistical and biological laws.
   WHEN: historical - Evaluating trends over decades of outbreaks.
   WHERE: analytical - Not personally constrained by the policy, but the math.
   SCOPE: global - The virus ignores borders.
   
   WHY THIS CLASSIFICATION:
   To the scientist, R0 is a Mountain. It is an intrinsic property of the virus 
   in a given environment. One cannot "negotiate" with exponential growth; 
   it is a feature of the biological landscape that must be respected as fact.
   
   NARRATIVE EVIDENCE:
   "The virus has its own timeline." "Mathematics is not a suggestion."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, historical, analytical, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HEALTH MINISTRY - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power.
   WHEN: immediate - Managing the current hospital capacity (1 year).
   WHERE: arbitrage - Can shift resources and laws to modify the rate.
   SCOPE: national - Country-wide mandates.
   
   WHY THIS CLASSIFICATION:
   For the institution, Rt is a Rope—a functional coordination mechanism. 
   By tracking and influencing the rate, they can pull the population toward 
   safety, justifying collective action and resource deployment.
   
   NARRATIVE EVIDENCE:
   "We need to flatten the curve." "Rt must stay below 1.0 to reopen the economy."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(viral_transmission_rates, E),
    E > 0.5, % High extraction is managed through institutional coordination
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE WAGE LABORER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to mandates, no savings.
   WHEN: immediate - Survival depends on this month's rent.
   WHERE: trapped - No option but to work or be restricted.
   SCOPE: local - Immediate neighborhood/worksite.
   
   WHY THIS CLASSIFICATION:
   For the laborer, the transmission rate is a Noose. The biological risk of 
   infection is high (extraction of health), and the legal response to that risk 
   (lockdown) prevents them from earning (extraction of labor). Both the virus 
   and the policy tighten the pressure on their survival.
   
   NARRATIVE EVIDENCE:
   "I can't afford to stay home." "The restrictions are killing my business 
   faster than the flu."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(viral_transmission_rates, S),
    S > 0.6, % Alternatives to the official policy are suppressed
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(viral_transmission_rates_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(viral_transmission_rates, mountain, context(analytical, historical, analytical, global)),
    constraint_indexing:constraint_classification(viral_transmission_rates, rope, context(institutional, immediate, arbitrage, national)),
    constraint_indexing:constraint_classification(viral_transmission_rates, noose, context(individual_powerless, immediate, trapped, local)),
    !.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, immediate, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(viral_transmission_rates, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(viral_transmission_rates, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability) :-
    % Over historical time for an analytical observer, it's a fixed law (Mountain)
    constraint_indexing:effective_immutability_for_context(context(analytical, historical, analytical, global), mountain).

:- end_tests(viral_transmission_rates_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2024-05-20
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.7):
 * Reasoning: Viruses are biologically extractive by nature. When paired with 
 * the economic shutdown required to stop them, the net extraction of 
 * human potential/labor is severe.
 * * 2. SUPPRESSION SCORE (0.8):
 * Reasoning: Pandemics create "emergency states" where dissent or 
 * alternative behavioral paths (e.g., ignoring distancing) are not just 
 * discouraged but often criminalized or socially pathologized.
 * * 3. PERSPECTIVE SELECTION:
 * Chose a three-way split (Scientist/State/Citizen) because this constraint 
 * perfectly illustrates how one "fact" (R0) is a law, a tool, and a trap 
 * simultaneously.
 * * 4. CONFIDENCE:
 * High: The biological/mathematical side (Mountain).
 * Medium: The institutional benefit (some states struggle, but power 
 * generally centralizes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega declaration
omega_variable(
    herd_immunity_threshold,
    "Is the threshold for herd immunity reached via natural infection (Noose) or vaccination (Rope)?",
    resolution_mechanism("Longitudinal seroprevalence study vs. vaccination uptake data"),
    impact("If Natural: massive extraction (victims). If Vaccine: coordinated escape (Rope)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Focused Protection (The Barrington Approach)
 * Viability: Theoretically allows low-risk individuals to build immunity 
 * while shielding the high-risk.
 * Suppression: Actively suppressed by institutional "Zero-Covid" or 
 * "Hammer and Dance" strategies as being too risky or unethical.
 * Evidence: Scientific debate in 2020-2021 regarding lockdowns.
 * * CONCLUSION:
 * The existence of suppressed alternatives (Focused Protection) shifts the 
 * experience of the laborer from a Rope (necessary sacrifice) to a Noose 
 * (enforced sacrifice when other paths were blocked).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/viral_transmission_rates].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(viral_transmission_rates).
 * 3. Run tests: ?- run_tests(viral_transmission_rates_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
