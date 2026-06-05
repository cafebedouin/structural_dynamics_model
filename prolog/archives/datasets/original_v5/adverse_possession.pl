% ============================================================================
% CONSTRAINT STORY: adverse_possession
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Real Property Law / Common Law / Squatter's Rights
% ============================================================================

:- module(constraint_adverse_possession, []).

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
 * * constraint_id: adverse_possession
 * human_readable: Adverse Possession (Squatter's Rights)
 * domain: economic/political/social
 * temporal_scope: Historical/Statutory (typically 7-21 years)
 * spatial_scope: National/Regional (Jurisdiction-dependent)
 * * SUMMARY:
 * Adverse possession is a legal principle where a person who does not have legal 
 * title to a piece of property—usually land—acquires legal ownership based on 
 * continuous possession or occupation of the land without the permission of its 
 * legal owner. It functions as a "statute of limitations" on property rights to 
 * ensure land is put to productive use.
 * * KEY AGENTS:
 * - The Statutory Legislator (Institutional): Sets the time and condition 
 * requirements to clear title and encourage land utility.
 * - The Absentee Landlord (Individual Powerful): The owner whose property 
 * rights are extracted due to neglect.
 * - The Adverse Possessor (Individual Moderate): The "squatter" who 
 * coordinates a long-term claim through open and notorious use.
 * - The Evicted Owner (Individual Powerless): A subject who loses their 
 * right to exit a property dispute after the clock runs out.
 * * NARRATIVE ARC:
 * Adverse possession begins as a "Rope"—a legal coordination tool to prevent 
 * stale claims and clarify land titles. For the successful squatter, it remains 
 * a Rope to pull themselves into ownership. However, for the negligent owner 
 * who discovers the claim too late, it becomes a "Snare," as the law 
 * irreversibly extracts their deed. For the legal system, it is a 
 * "Mountain"—a fixed temporal wall where property rights simply expire.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(adverse_possession, 0, 10).
narrative_ontology:constraint_claim(adverse_possession, snare).

% Base Properties
% Rationale: High extraction (0.65) as the legal title is liquidated and 
% transferred to the possessor without compensation.
domain_priors:base_extractiveness(adverse_possession, 0.65).
domain_priors:suppression_score(adverse_possession, 0.45).
domain_priors:theater_ratio(adverse_possession, 0.65). % Reflects the ritual of "open and notorious" possession.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(adverse_possession, extractiveness, 0.65).
narrative_ontology:constraint_metric(adverse_possession, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(adverse_possession, theater_ratio, 0.65).

% Mandatory keys for classification engine v3.4
% These resolved the [FAIL] Schema mismatch by anchoring the measurement keys.
domain_priors:requires_active_enforcement(adverse_possession).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(adverse_possession, squatter_or_possessor).
narrative_ontology:constraint_victim(adverse_possession, negligent_property_owner).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE REAL ESTATE LAWYER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of legal precedent and statutory timelines.
   WHEN: historical - Viewing title chains over decades.
   WHERE: analytical - Not personally losing or gaining land.
   SCOPE: national - Bound by state/country property codes.
   
   WHY THIS CLASSIFICATION:
   To the lawyer, adverse possession is a Mountain. It is an unchangeable 
   feature of the legal landscape. Once the statutory period (e.g., 21 years) 
   has passed with all conditions met, the outcome is a fixed peak of law 
   that cannot be argued away; the original title has effectively eroded.
   
   NARRATIVE EVIDENCE:
   "The law does not help those who sleep on their rights." The statute 
   of limitations creates a hard wall that bars any future legal action.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    adverse_possession,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, historical, analytical, national),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMMUNITY DEVELOPER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-shaping power to clear urban blight.
   WHEN: biographical - Revitalizing a neighborhood over 10-20 years.
   WHERE: arbitrage - Can use legal "clearing" tools to acquire stalled assets.
   SCOPE: local - Immediate neighborhood/municipality.
   
   WHY THIS CLASSIFICATION:
   For the developer or a city land bank, this is a Rope. It is a coordination 
   mechanism to return abandoned "dead" land to the tax rolls and productive 
   use. It is a tool used to pull a neighborhood out of stagnation by 
   extinguishing the claims of absentee owners who do not contribute.
   
   NARRATIVE EVIDENCE:
   The doctrine of "Laches" and adverse possession are used as tethers 
   to ensure land does not remain idle and title remains marketable.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    adverse_possession,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(adverse_possession, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE NEGLIGENT HEIR - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to a law they didn't know was running.
   WHEN: immediate - The moment they discover their "inheritance" is gone.
   WHERE: trapped - Cannot exit the statutory reality after the time expires.
   SCOPE: local - Immediate family property.
   
   WHY THIS CLASSIFICATION:
   For the owner who discovers a squatter has met the requirements, the 
   law is a Snare. They are "strangled" by the passage of time they 
   ignored. The law extracts 100% of their equity without compensation. 
   There is no "exit" or negotiation possible; the trap has already snapped.
   
   NARRATIVE EVIDENCE:
   "I went to check on my late father's cabin only to find a fence and 
   a court order saying I no longer own the land."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    adverse_possession,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(adverse_possession, E),
    E > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(adverse_possession_tests).

test(multi_perspective_variance) :-
    % Lawyer sees Mountain, Developer sees Rope, Heir sees Snare
    constraint_indexing:constraint_classification(adverse_possession, mountain, context(analytical, historical, analytical, national)),
    constraint_indexing:constraint_classification(adverse_possession, rope, context(institutional, biographical, arbitrage, local)),
    constraint_indexing:constraint_classification(adverse_possession, snare, context(powerless, immediate, trapped, local)).

test(power_extractiveness_property) :-
    % Powerless individuals feel the total extraction of their land (Snare).
    % Institutional actors use the law to clear title for growth (Rope).
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, local),
    constraint_indexing:extractiveness_for_agent(adverse_possession, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(adverse_possession, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_property) :-
    % Short horizon discovery = Snare.
    % Historical lookback = Mountain.
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(adverse_possession_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.7):
 * Reasoning: Adverse possession is a total-loss extraction. Unlike a tax 
 * or a fine, it transfers the entire underlying asset from one party 
 * to another.
 * Evidence: Common law requirements for "Actual, Open, Notorious, Exclusive, 
 * and Hostile" possession.
 * * 2. SUPPRESSION SCORE (0.4):
 * Reasoning: The alternatives (selling, renting) are clear, but the 
 * law itself suppresses the "Titled Owner's" rights after a specific 
 * period of time to favor "Utility."
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Lawyer (Law), the Developer (Utility Tool), and the Heir 
 * (Victim) to highlight how a property "rope" for a community 
 * becomes a "snare" for an individual.
 * * 4. AMBIGUITIES:
 * - The "Good Faith" vs. "Hostile" requirement varies by jurisdiction. 
 * I assumed the traditional "Hostile" (meaning without permission) 
 * common law standard for this analysis.
 * * 5. CONFIDENCE:
 * High: Classification as Snare for the losing owner.
 * Medium: Extractiveness (varies based on land value).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_requirement,
    "Does the squatter need to intend to steal the land (Snare) or simply 
    believe they own it (Rope)?",
    resolution_mechanism("Legislative review of state-specific 'mental state' 
    requirements for adverse possession"),
    impact("If Intentional: It's a predatory Snare. If Mistaken: It's a 
    protective Rope for boundaries."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Eminent Domain
 * Viability: State acquisition for public use with compensation.
 * Suppression: Bypassed because Adverse Possession requires NO 
 * compensation, making it a more "extractive" Snare for the owner.
 * * ALTERNATIVE 2: Market Purchase
 * Viability: The squatter could simply buy the land.
 * Suppression: Ignored by the squatter because the "Free" legal 
 * mechanism of time is more beneficial to them.
 * * CONCLUSION:
 * The existence of paid alternatives (Purchase) that are bypassed for a 
 * free legal extraction confirms that for the titled owner, Adverse 
 * Possession is a Snare.
 */

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Declining from "Posturing/Trespassing" (0.65) to "Established Occupation" (0.20).
% As the occupation becomes "Open and Notorious," the performative ambiguity fades.
narrative_ontology:measurement(adv_pos_tr_t0, adverse_possession, theater_ratio, 0, 0.65).
narrative_ontology:measurement(adv_pos_tr_t5, adverse_possession, theater_ratio, 5, 0.40).
narrative_ontology:measurement(adv_pos_tr_t10, adverse_possession, theater_ratio, 10, 0.20).

% Extraction: The "Clock" effect. At T=0, the owner has full rights. 
% By T=10, the legal "Snare" has tightened, liquidating the owner's equity.
narrative_ontology:measurement(adv_pos_ex_t0, adverse_possession, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(adv_pos_ex_t5, adverse_possession, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(adv_pos_ex_t10, adverse_possession, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_adverse_possession].
 * 2. Multi-perspective: ?- multi_index_report(adverse_possession).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
