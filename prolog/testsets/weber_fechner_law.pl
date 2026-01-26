% ============================================================================
% CONSTRAINT STORY: weber_fechner_law
% ============================================================================
% Generated: 2026-01-22
% Model: Gemini
% Source: Psychophysics / Weber (1834) / Fechner (1860)
% ============================================================================

:- module(constraint_weber_fechner_law, []).

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
 * * constraint_id: weber_fechner_law
 * human_readable: Weber-Fechner Law (Logarithmic Perception)
 * domain: technological/social/biological
 * temporal_scope: Permanent (Evolutionary sensory architecture)
 * spatial_scope: Global (Universal human biology)
 * * SUMMARY:
 * The Weber-Fechner law describes the logarithmic relationship between stimulus 
 * intensity and perception. It dictates that humans perceive relative rather than 
 * absolute changes, creating a "Just Noticeable Difference" (JND) threshold that 
 * acts as a biological filter for all sensory and economic inputs.
 * * KEY AGENTS:
 * - The Neuroscientist: Analytical observer of the biological sensory substrate.
 * - The Industrial Designer: Institutional actor using the law as a "Rope" to 
 * align technical systems with human comfort.
 * - The Corporate Strategist: Rule-shaper using the law as a "Tangled Rope" to 
 * coordinate prices while extracting value via JND masking.
 * - The Supermarket Shopper: Individual powerless subject trapped by 
 * shrinkflation and psychological value distortion.
 * * NARRATIVE ARC:
 * Originally an evolutionary efficiency for data compression, the law is 
 * weaponized in the modern market. What begins as a "Mountain" of neural 
 * reality is leveraged as a "Rope" for design utility, but eventually 
 * tightens into a "Snare" for consumers when used to hide the extraction 
 * of product value.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(weber_fechner_law, 0, 10).
narrative_ontology:constraint_claim(weber_fechner_law, mountain).

% Base Properties
% Rationale: Moderate extraction (0.4) through "stealth inflation" and JND masking.
domain_priors:base_extractiveness(weber_fechner_law, 0.4).
% Rationale: Moderate suppression (0.5) as it masks the linear objective truth.
domain_priors:suppression_score(weber_fechner_law, 0.5).

% Emerges naturally from neural firing rates and sensory compression.
domain_priors:emerges_naturally(weber_fechner_law).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(weber_fechner_law, [advertising_agencies, industrial_designers, retail_strategists]).
constraint_victim(weber_fechner_law, [uninformed_consumers, classical_utility_models]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */


/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEUROSCIENTIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of the biological sensory substrate.
   WHEN: civilizational - Viewing the evolution of the mammalian nervous system.
   WHERE: trapped - Sensory compression is a fixed feature of neural hardware.
   SCOPE: global - Universal to the human species.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the law is an unchangeable feature of biological topography. 
   The brain's hardware is hardwired for logarithmic response; no amount of 
   social reform can "linearize" human hearing or vision.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UI/UX DESIGNER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design interfaces and sensory experiences.
   WHEN: biographical - Creating products for long-term usage.
   WHERE: arbitrage - Can choose to use logarithmic or linear scales.
   SCOPE: national - Digital and physical product ecosystems.
   
   WHY THIS CLASSIFICATION:
   For the designer, the law is a vital coordination mechanism. By aligning 
   a volume slider or brightness control with the JND, they create a 
   "natural" bridge between the user and the machine.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PRICING STRATEGIST - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional - Designing market signals and price points.
   WHEN: generational - Managing brand value and market share.
   WHERE: mobile - Can adjust strategies across different markets.
   SCOPE: national - National retail networks.
   
   WHY THIS CLASSIFICATION:
   The strategist uses the law to coordinate consumer expectations (Rope) 
   while simultaneously extracting surplus by keeping price/quantity 
   adjustments below the JND (Extraction). It is a hybrid tool of 
   market stability and predatory margin-management.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE SUPERMARKET SHOPPER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Subject to masking and value extraction.
   WHEN: immediate - Today's grocery bill.
   WHERE: constrained - High cost to verify absolute weights/volumes.
   SCOPE: local - Immediate household purchasing.
   
   WHY THIS CLASSIFICATION:
   For the consumer, the law functions as an invisible trap. Shrinkflation 
   exploits their biological "blindness," tightening the budget as 
   value is extracted in increments they are biologically incapable of 
   noticing without external measurement tools.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    weber_fechner_law,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(weber_fechner_law_tests).

test(multi_perspective_variance) :-
    % Verify different indices yield different types
    constraint_indexing:constraint_classification(weber_fechner_law, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(weber_fechner_law, rope, context(agent_power(institutional), time_horizon(biographical), _, _)),
    constraint_indexing:constraint_classification(weber_fechner_law, snare, context(agent_power(individual_powerless), _, _, _)).

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(weber_fechner_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(weber_fechner_law, ContextPowerful, Score2),
    Score1 > Score2.

test(hybrid_tangled_rope_detection) :-
    % Verify that long-term institutional pricing is detected as a Tangled Rope
    constraint_indexing:constraint_classification(weber_fechner_law, tangled_rope, context(institutional, generational, mobile, national)).

:- end_tests(weber_fechner_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-22
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.4):
 * While the law itself is natural, its economic application extracts 
 * "value-perception" from consumers. It is not as severe as total 
 * apocalypse (0.7), but persistent.
 * * 2. PERSPECTIVE SELECTION:
 * Introduced 'Tangled Rope' for the strategist to demonstrate how 
 * coordination (market pricing) and extraction (JND masking) are 
 * surgerically intertwined.
 * * 3. NOOSE LOGIC:
 * Focuses on 'Shrinkflation' as the primary manifestation of the 
 * predatory use of biological sensory limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precision_training_limit,
    "Can humans be trained to bypass Weber-Fechner 'blindness' through comparative analytical tools, or is the logarithmic floor absolute?",
    resolution_mechanism("Long-term sensory training studies using digital feedback to linearize perception"),
    impact("If trainable: The Snare is untieable. If absolute: We require external regulation (External Rope)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Stevens' Power Law
 * Viability: High. A scientific refinement rather than a social alternative.
 * Suppression: None. It co-exists as a more accurate scientific model.
 * * CONCLUSION:
 * Since there is no "Linear Sensation" biological alternative, the 
 * constraint remains a Mountain of nature. The "Snare" arises purely 
 * from the social exploitation of this biological fixed-point.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [weber_fechner_law].
% Report: ?- constraint_indexing:multi_index_report(weber_fechner_law).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
