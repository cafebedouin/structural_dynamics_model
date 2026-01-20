% ============================================================================
% CONSTRAINT STORY: cobra_effect
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Economics / Behavioral Science / Colonial History (India)
% ============================================================================

:- module(constraint_cobra_effect, []).

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
 * * constraint_id: cobra_effect
 * human_readable: The Cobra Effect (Perverse Incentives)
 * domain: economic/political/social
 * temporal_scope: 19th Century Colonial India / Modern Policy
 * spatial_scope: Regional (Delhi) / Global (Economic Systems)
 * * SUMMARY:
 * The Cobra Effect occurs when an incentive has an unintended and undesirable 
 * result which is contrary to the intentions of its designers. Named after 
 * a colonial-era bounty on cobras in Delhi, it represents a failure to 
 * account for the strategic gaming of rules by rational agents.
 * * KEY AGENTS:
 * - The Colonial Administrator (Institutional): Designs the bounty to reduce the snake population.
 * - The Cobra Breeder (Individual Moderate): Exploits the incentive by breeding snakes for profit.
 * - The Bitten Citizen (Individual Powerless): Suffers from the eventual surge in the wild snake population.
 * - The Behavioral Economist (Analytical): Observes the predictable failure of linear incentives in complex systems.
 * * NARRATIVE ARC:
 * To solve a safety crisis, the state creates a "Rope" (a bounty). Agents 
 * treat the Rope as an opportunity for arbitrage, breeding more cobras. 
 * When the state realizes the ruse and cuts the bounty, the breeders release 
 * the worthless snakes, turning the initial policy into a "Noose" for public 
 * safety that leaves the environment more dangerous than before.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(cobra_effect_interval, 0, 10).
narrative_ontology:constraint_claim(cobra_effect, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.6 (Moderate-High). The policy extracts capital from the state 
% while eventually extracting safety and health from the public.
domain_priors:base_extractiveness(cobra_effect, 0.6).

% Suppression score (0.0-1.0)
% Rationale: 0.4 (Moderate). Alternatives (like professional pest control) 
% were visible but suppressed by the perceived simplicity and speed of the 
% market-based incentive.
domain_priors:suppression_score(cobra_effect, 0.4).

% Enforcement requirements
% Requires active maintenance (the bounty program) to persist as a mechanism.
domain_priors:requires_active_enforcement(cobra_effect).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cobra_effect, [snake_breeders, short_term_opportunists]).
constraint_victim(cobra_effect, [taxpayers, public_safety_subjects]).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(cobra_effect, extractiveness, 0.6).
narrative_ontology:constraint_metric(cobra_effect, suppression_requirement, 0.4).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COLONIAL ADMINISTRATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power attempting to manage a crisis.
   WHEN: immediate - Solving the "snake problem" this year.
   WHERE: arbitrage - Can set the bounty and adjust the budget.
   SCOPE: regional - The city of Delhi.
   
   WHY THIS CLASSIFICATION:
   For the administrator, the bounty is a Rope. It is a coordination mechanism 
   intended to mobilize the local population to solve a shared problem. It 
   is a tool used to pull the community toward safety.
   
   NARRATIVE EVIDENCE:
   "We will pay for every dead cobra brought to the station; the market 
   will solve our safety problem."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cobra_effect,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(cobra_effect, E),
    E < 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COBRA BREEDER - Rope/Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Agent with some agency to exploit rules.
   WHEN: biographical - Seeking income for their family.
   WHERE: constrained - Limited by the state's rules, but mobile enough to game them.
   SCOPE: local - Immediate neighborhood/breeding site.
   
   WHY THIS CLASSIFICATION:
   Initially, the breeder sees the bounty as a Rope—a way to pull wealth 
   into their household. However, when the bounty is canceled, they are 
   left with a worthless and dangerous "product," turning the economic 
   logic into a Noose that forces them to release the snakes and ruin 
   the policy's intent.
   
   NARRATIVE EVIDENCE:
   "If they pay for dead snakes, it is easier to raise them than to 
   find them in the grass."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cobra_effect,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(cobra_effect, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SKEPTICAL ECONOMIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal human incentives.
   WHEN: civilizational - Viewing human nature as a constant.
   WHERE: analytical - Not constrained by the bounty itself.
   SCOPE: global - Universal behavior of agents.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the Cobra Effect is a Mountain. It represents an 
   unchangeable law of incentives: agents will always optimize for the 
   metric (dead snakes) rather than the intended outcome (fewer live snakes). 
   It is a fixed peak in the landscape of human social behavior.
   
   NARRATIVE EVIDENCE:
   "Goodhart's Law: When a measure becomes a target, it ceases to be a 
   good measure."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cobra_effect,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, analytical, global),
        mountain
    ),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cobra_effect_tests).

test(multi_perspective_variance) :-
    % Institutional sees Rope, Powerless/Moderate sees Noose, Analytical sees Mountain
    constraint_indexing:constraint_classification(cobra_effect, rope, context(institutional, immediate, arbitrage, regional)),
    constraint_indexing:constraint_classification(cobra_effect, noose, context(individual_moderate, biographical, constrained, local)),
    constraint_indexing:constraint_classification(cobra_effect, mountain, context(analytical, civilizational, analytical, global)).

test(power_extractiveness_cobra) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, immediate, arbitrage, regional),
    % While the model reports a base score, the experienced extraction scales
    % for those who must live with the increased snake population.
    domain_priors:base_extractiveness(cobra_effect, Score),
    Score > 0.5.

test(time_immutability_incentives) :-
    % Civilizational scale = Mountain (Human Nature is fixed)
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(cobra_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.6):
 * The Cobra Effect is highly extractive because it wastes public capital 
 * while making the underlying problem (danger) worse. It extracts 
 * utility from the system and leaves only friction.
 * * 2. SUPPRESSION SCORE (0.4):
 * Alternatives were visible but ignored due to the "administrative 
 * simplicity" bias. 
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Administrator (Rope), the Breeder (Noose), and the 
 * Economist (Mountain) to show how an incentive is a tool for the 
 * state, a trap for the subject, and a law for the observer.
 * * 4. AMBIGUITIES:
 * The term "Cobra Effect" is often used broadly for any failed incentive. 
 * I grounded this specifically in the historical Delhi scenario to 
 * ensure classification accuracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_breeding_threshold,
    "At what price point does 'finding' snakes transition to 'breeding' 
    snakes (Rope vs Noose transition)?",
    resolution_mechanism("Experimental behavior economics study of 
    supply-response to varying bounty levels"),
    impact("If Low: Any bounty is a Noose. If High: Small bounties remain 
    useful Ropes."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Direct State Employment (Pest Control)
 * Viability: Professional snake catchers paid a salary, not per head.
 * Suppression: Suppressed by the neoliberal/market-bias of the colonial 
 * administration which preferred "outsourcing" the risk.
 * * ALTERNATIVE 2: Habitat Management
 * Viability: Reducing the conditions that allow cobras to thrive in Delhi.
 * Suppression: Ignored due to the long time-horizon required compared 
 * to the immediate political pressure for a "quick fix."
 * * CONCLUSION:
 * The existence of professional alternatives (Rope) that were suppressed 
 * confirms that for the citizen, the Cobra Effect is a Noose of 
 * administrative incompetence.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_cobra_effect].
 * 2. Multi-perspective: ?- multi_index_report(cobra_effect).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
