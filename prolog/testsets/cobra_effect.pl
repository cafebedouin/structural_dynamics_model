% ============================================================================
% CONSTRAINT STORY: cobra_effect
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
 * 
 * constraint_id: cobra_effect
 * human_readable: The Cobra Effect (Perverse Incentives)
 * domain: economic/political/social
 * temporal_scope: 19th Century Colonial India / Modern Policy
 * spatial_scope: Regional (Delhi) / Global (Economic Systems)
 * 
 * SUMMARY:
 * The Cobra Effect occurs when an incentive has an unintended and undesirable 
 * result which is contrary to the intentions of its designers. Named after 
 * a colonial-era bounty on cobras in Delhi, it represents a failure to 
 * account for the strategic gaming of rules by rational agents.
 * 
 * KEY AGENTS:
 * - The Colonial Administrator (Institutional): Designs the bounty to reduce the snake population.
 * - The Cobra Breeder (Individual Moderate): Exploits the incentive by breeding snakes for profit.
 * - The Bitten Citizen (Individual Powerless): Suffers from the eventual surge in the wild snake population.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(cobra_effect, 0, 10).
narrative_ontology:constraint_claim(cobra_effect, snare).

% Base extractiveness: 0.6 (Moderate-High). The policy extracts capital from the state 
% while eventually extracting safety and health from the public.
domain_priors:base_extractiveness(cobra_effect, 0.6).

% Suppression: 0.4 (Moderate). Alternatives (like professional pest control) 
% were visible but suppressed by the perceived simplicity and speed of the 
% market-based incentive.
domain_priors:suppression_score(cobra_effect, 0.4).

% Enforcement: Requires active maintenance (the bounty program) to persist as a mechanism.
domain_priors:requires_active_enforcement(cobra_effect).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cobra_effect, cobra_breeders).
constraint_victim(cobra_effect, public_safety_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COLONIAL ADMINISTRATOR - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Rule-making power attempting to manage a crisis.
   WHEN: immediate - Solving the "snake problem" this year.
   WHERE: arbitrage - Can set the bounty and adjust the budget.
   
   WHY THIS CLASSIFICATION:
   For the administrator, the bounty is a 'Rope'. It is a coordination mechanism 
   intended to mobilize the local population to solve a shared problem. It 
   is a tool used to pull the community toward safety.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BITTEN CITIZEN - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (A citizen whose safety is directly threatened)
   WHEN: immediate (The moment of encounter with a cobra)
   WHERE: trapped (Cannot escape the increased danger in the environment)
   
   WHY THIS CLASSIFICATION:
   For the citizen, the eventual surge in the wild snake population (a result
   of the failed policy) makes the Cobra Effect a 'Snare' that directly threatens
   their safety and well-being. They are trapped in a more dangerous environment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cobra_effect,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SKEPTICAL ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of universal human incentives.
   WHEN: civilizational - Viewing human nature as a constant.
   WHERE: analytical - Not constrained by the bounty itself.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the Cobra Effect is a 'Mountain'. It represents an 
   unchangeable law of incentives: agents will always optimize for the 
   metric (dead snakes) rather than the intended outcome (fewer live snakes). 
   It is a fixed peak in the landscape of human social behavior.
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
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(cobra_effect_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(cobra_effect, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cobra_effect, Type2, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(cobra_effect, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(cobra_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'The Bitten Citizen' to represent
 *    the 'individual_powerless' agent. Their experience is the ultimate 'Snare'
 *    of a failed policy.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Administrator (Rope): A tool for problem-solving.
 *    - Citizen (Snare): A direct threat to safety.
 *    - Economist (Mountain): An immutable law of incentives.
 * 
 * 3. CORE INSIGHT: The Cobra Effect is a cautionary tale about perverse incentives.
 *    What starts as a seemingly beneficial 'Rope' for an administrator can quickly
 *    become a deadly 'Snare' for the public due to unexamined assumptions about
 *    human behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the behavioral threshold at which unintended consequences emerge.
 */

omega_variable(
    intentional_breeding_threshold,
    "At what price point does 'finding' snakes transition to 'breeding' snakes (the point where the bounty shifts from a 'Rope' to a 'Snare' for the bounty hunter)?",
    resolution_mechanism("Experimental behavioral economics study of supply-response to varying bounty levels, tracking the emergence of perverse incentives."),
    impact("If Low: Any bounty is a 'Snare'. If High: Small bounties remain useful 'Ropes'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Direct State Employment (Professional Pest Control)
 *    Viability: Professional snake catchers paid a salary, not per head.
 *    Suppression: Suppressed by the neoliberal/market-bias of the colonial
 *    administration which preferred "outsourcing" the risk.
 *
 * CONCLUSION:
 * The existence of professional alternatives that were suppressed confirms
 * that the Cobra Effect is a 'Snare' of administrative incompetence, rather than
 * an unavoidable 'Mountain'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/cobra_effect].
 * 2. Multi-perspective: ?- multi_index_report(cobra_effect).
 * 3. Run tests: ?- run_tests(cobra_effect_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */