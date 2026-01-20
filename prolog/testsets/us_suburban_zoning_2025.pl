% ============================================================================
% CONSTRAINT STORY: us_suburban_zoning_2025
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: NYT Article "Would the Housing Crisis Ease if Boomers Rented Out Their Empty Rooms?" (May 4, 2025)
% ============================================================================

:- module(constraint_us_suburban_zoning_2025, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: us_suburban_zoning_2025
 * human_readable: Single-Family Suburban Zoning
 * domain: political/legal/social
 * temporal_scope: 1970s - 2025 (Active/Under Pressure)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * Local zoning codes restrict large swaths of metropolitan land to low-density, 
 * single-family homes, creating a mismatch between existing housing stock 
 * and modern demographic needs. This legal constraint effectively bans 
 * apartment buildings and "roommate houses" in many areas, driving up 
 * [cite_start]affordability issues. [cite: 10, 13]
 * * KEY AGENTS:
 * - [cite_start]Monte Anderson: A "serial rehabber" developer using loopholes (hidden doors) to retrofit suburban homes into multi-unit apartments. [cite: 3, 4]
 * - [cite_start]Boomers/Older Homeowners: Owners of large lots and empty bedrooms, currently incentivized (or trapped) by the single-family status quo. [cite: 10, 16]
 * - [cite_start]Local Planning Boards/Zoning Codes: The institutional enforcers of the single-family mandate. [cite: 3, 10]
 * - [cite_start]Renters/Middle-Income Adults: Agents seeking smaller, affordable housing but priced out by a market biased toward luxury or high-cost single units. [cite: 6, 10]
 * * NARRATIVE ARC:
 * Originally designed for the 1970s nuclear family, zoning now acts as a 
 * [cite_start]"Mountain" of physical and legal inertia. [cite: 10, 11] Entrepreneurs like Anderson 
 * treat it as a "Noose" to be escaped via architectural loopholes, while 
 * states like California are attempting to convert it into a "Rope" (flexible 
 * [cite_start]coordination) through ADU laws. [cite: 12, 14]
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% ID for Python extraction
narrative_ontology:interval(us_suburban_zoning_2025, 1970, 2026).
narrative_ontology:constraint_claim(us_suburban_zoning_2025, noose).

% Base extractiveness: 0.7
% Rationale: High asymmetry. [cite_start]Homeowners with large lots gain "billions in value" [cite: 16] 
[cite_start]% while renters face a shortage of 4-8 million homes, keeping costs high. [cite: 5]
domain_priors:base_extractiveness(us_suburban_zoning_2025, 0.7).

% Suppression: 0.65
[cite_start]% Rationale: Apartment buildings are "essentially banned" in large swaths[cite: 10], 
% and alternatives like "roommate houses" require hidden doors in broom closets 
[cite_start]% to avoid legal detection. [cite: 1, 3]
domain_priors:suppression_score(us_suburban_zoning_2025, 0.65).

[cite_start]% Enforcement: Requires active maintenance (Permits, zoning codes, municipal inspections). [cite: 3, 6]
domain_priors:requires_active_enforcement(us_suburban_zoning_2025).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(us_suburban_zoning_2025, boomer_homeowners). [cite_start]% Own the biggest lots. [cite: 16]
constraint_beneficiary(us_suburban_zoning_2025, local_nimby_residents). [cite_start]% Residents who "don't like change". [cite: 12]
constraint_victim(us_suburban_zoning_2025, middle_income_renters). [cite_start]% Priced out by lack of small units. [cite: 6]
constraint_victim(us_suburban_zoning_2025, multifamily_developers). [cite_start]% Restricted by density limits. [cite: 10]

% Metrics for Executive Summary
narrative_ontology:constraint_metric(us_suburban_zoning_2025, extractiveness, 0.7).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, suppression_requirement, 0.65).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: MONTE ANDERSON (DEVELOPER) - Noose
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has resources to build, but constrained by law.
   WHEN: biographical - His career is built on navigating these specific codes.
   WHERE: constrained - He must hide features (broom closet doors) to survive.
   SCOPE: national - Sees the "America's affordable housing crisis" at large.
   
   WHY THIS CLASSIFICATION:
   Anderson sees zoning as a coercive limit that prevents the "logical" use of 
   existing property. He must "find a loophole" to provide what the market 
   [cite_start]actually needs (multifamily life). [cite: 3, 10]
   
   NARRATIVE EVIDENCE:
   [cite_start]"The main reason for their existence is that they allow Mr. Anderson to claim he lives in a single-family home... when in reality the home contains four apartments." [cite: 3]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    us_suburban_zoning_2025,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(us_suburban_zoning_2025, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: YOUNG RENTERS / GRANDDAUGHTER - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - Subjects of the housing shortage.
   WHEN: immediate - Focused on monthly rent ($1,800).
   WHERE: trapped - Housing supply "remained essentially flat" outside ADUs.
   SCOPE: regional - Limited to where they work (e.g., Dallas suburbs).
   
   WHY THIS CLASSIFICATION:
   For those without property, zoning is not a "choice" but a fixed reality 
   of the landscape that dictates where they can live and what they must pay. 
   [cite_start]The shortage is a "grinding" reality they simply endure. [cite: 11, 15]
   
   NARRATIVE EVIDENCE:
   [cite_start]"Construction costs have exploded... most new homes tend to be luxury rentals... rather than something a person with a middle or lower income can afford." [cite: 6]
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    us_suburban_zoning_2025,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(us_suburban_zoning_2025, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CALIFORNIA LEGISLATORS / ENTREPRENEURS - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to rewrite the rules.
   WHEN: generational - Aiming to add "millions of units" over time.
   WHERE: mobile - Moving toward ADU laws and subdivision flexibility.
   SCOPE: continental - (Representing broad US state-level policy shifts).
   
   WHY THIS CLASSIFICATION:
   They view zoning as a tool that was once useful but now needs "tuning" or 
   "retrofitting." By passing ADU laws, they turn the constraint into a 
   [cite_start]mechanism for coordination and value-unlocking. [cite: 10, 14, 16]
   
   NARRATIVE EVIDENCE:
   [cite_start]"State legislators have proposed a blizzard of housing laws... the humble backyard cottage... is the main bright spot." [cite: 12, 14]
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    us_suburban_zoning_2025,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(continental)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(us_suburban_zoning_2025_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, Type1, context(individual_moderate, biographical, constrained, national)), % Noose
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, Type2, context(individual_powerless, immediate, trapped, regional)), % Mountain
    Type1 \= Type2.

test(power_extractiveness_scaling) :-
    % Middle-income renters (powerless) suffer more from the shortage than institutional brokers (BuildCasa).
    ContextPowerless = context(individual_powerless, immediate, trapped, regional),
    ContextPowerful = context(institutional, generational, mobile, continental),
    domain_priors:base_extractiveness(us_suburban_zoning_2025, Base),
    % Extraction for the powerless is effectively the base + penalty for lack of exit.
    % (Simulation of the index logic)
    true.

test(loophole_as_exit) :-
    % If an agent has a "loophole" (hidden door), they move from Trapped to Constrained.
    % This shifts the classification from Mountain (for the renter) to Noose (for the developer).
    true.

:- end_tests(us_suburban_zoning_2025_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. BASE EXTRACTIVENESS (0.7): I chose a high score because the text 
 * explicitly notes "yawning inequality" and how the current system 
 * benefits older homeowners (Boomers) while creating a "grinding" 
 * [cite_start]shortage for everyone else. [cite: 15, 16]
 * * 2. SUPPRESSION SCORE (0.65): Alternatives (apartments) aren't just 
 * invisible; they are "essentially banned." The physical concealment 
 * [cite_start]of doors in closets is a literal manifestation of suppression. [cite: 3, 10]
 * * 3. PERSPECTIVES:
 * - [cite_start]Monte Anderson (Noose): He is the primary case study of navigating a coercive law. [cite: 3]
 * - Renters (Mountain): Represents the millions of people who cannot change 
 * [cite_start]the zoning but are forced to "double up" to survive. [cite: 15]
 * - Legislators (Rope): Represents the institutional effort to regain 
 * [cite_start]functional control of the housing stock. [cite: 12]
 * * 4. AMBIGUITIES:
 * - The article suggests Anderson's $1,800 units are "not affordable for low-income tenants," 
 * [cite_start]yet he views his work as a "proof of concept" for affordability. [cite: 9] 
 * I formalized this as an Omega regarding the scalability of his model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    scalability_of_roommate_model,
    [cite_start]"Will high-cost retrofits ($1M for 4 units) actually lower costs for low-income tenants, or just middle-income ones?", [cite: 9]
    resolution_mechanism("Track average rent per square foot in retrofitted homes vs. traditional apartment builds over 5 years."),
    impact("If Mountain: The retrofit is just a niche luxury. If Rope: It becomes a viable national policy lever."),
    confidence_without_resolution(medium)
).

omega_variable(
    nimby_political_will,
    [cite_start]"Will local residents continue to successfully block zoning changes, or will state-level overrides become the new norm?", [cite: 12]
    resolution_mechanism("Measure the ratio of local zoning vetoes vs. state pre-emption overrides in CA/TX by 2030."),
    impact("If Noose: Residents keep a lock on supply. If Rope: Institutional agents successfully modernize the system."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: High-Density Upzoning (Transit-Oriented Development)
 * [cite_start]Viability: Proposed in CA laws but often met with flat production numbers. [cite: 12, 14]
 * [cite_start]Suppression: Rejected or stalled by local governments and "residents who don't like change." [cite: 12]
 * * ALTERNATIVE 2: Post-War "Mass-Built" Suburbs
 * [cite_start]Viability: The 1950s model that created the shortage. [cite: 15]
 * [cite_start]Evidence: The text notes this model is now "reversed" as households double up. [cite: 15]
 * * CONCLUSION:
 * The existence of ADU laws as a "bright spot" confirms that the rigid 
 * Single-Family zoning is a Noose being partially unraveled. If there 
 * [cite_start]were no alternatives, it would remain a Mountain. [cite: 14]
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
