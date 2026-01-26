% ============================================================================
% CONSTRAINT STORY: endowment_effect
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Thaler, R. (1980) / Kahneman, Knetsch, & Thaler (1990)
% ============================================================================

:- module(constraint_endowment_effect, []).

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
 * * constraint_id: endowment_effect
 * human_readable: The Endowment Effect
 * domain: economic/social/cognitive
 * temporal_scope: Permanent (Human Cognitive Architecture)
 * spatial_scope: Global (Human Decision Systems)
 * * SUMMARY:
 * The endowment effect is a finding in behavioral economics where individuals 
 * ascribe more value to things merely because they own them. This leads to 
 * an asymmetry between the "Willingness to Pay" (WTP) and the "Willingness 
 * to Accept" (WTA), often resulting in market inefficiencies where 
 * beneficial trades do not occur.
 * * KEY AGENTS:
 * - The Owner: Ascribes sentimental or psychological "bonus" value to an object.
 * - The Potential Buyer: Values the object based on its objective utility or 
 * market price.
 * - The Market Optimizer: An analytical agent observing the failure of 
 * Coasian bargaining due to this psychological gap.
 * * NARRATIVE ARC:
 * The effect functions as a "Mountain" of psychological inertia. For the 
 * salesperson, it is a "Rope" (using "try-before-you-buy" to create ownership). 
 * For the person trapped in an old, inefficient house or bad investment due to 
 * sentiment, it is a "Snare" that strangles their financial mobility.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural identification
narrative_ontology:interval(endowment_interval, 0, 10).
narrative_ontology:constraint_claim(endowment_effect, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts "liquidity" and "efficient allocation" from the market. 
% It forces the buyer to pay more than utility warrants or prevents a 
% mutually beneficial sale.
domain_priors:base_extractiveness(endowment_effect, 0.3).

% Suppression score (0.0-1.0)
% Rationale: It suppresses the alternative of "rational market valuation." 
% The brain's loss aversion makes the fair market price appear as a loss, 
% making the rational trade invisible or emotionally painful.
domain_priors:suppression_score(endowment_effect, 0.4).

% Enforcement requirements
domain_priors:emerges_naturally(endowment_effect).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(endowment_effect, extractiveness, 0.3).
narrative_ontology:constraint_metric(endowment_effect, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(endowment_effect, existing_owners).
constraint_beneficiary(endowment_effect, retailers_offering_trials).
constraint_victim(endowment_effect, market_efficiency).
constraint_victim(endowment_effect, asset_purchasers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE BEHAVIORAL ECONOMIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer, not participant
   WHEN: civilizational - Observing a universal cognitive invariant
   WHERE: trapped - Human cognitive architecture is currently fixed
   SCOPE: global - Worldwide
   
   WHY THIS CLASSIFICATION:
   To the scientist, this is a Mountain. It is an unchangeable consequence of 
   how the human brain processes loss and ownership. You cannot simply 
   "choose" not to feel the loss aversion that drives the effect; it is 
   a feature of the biological "terrain".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    endowment_effect,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CAR SALESMAN - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making/process-shaping power over a sale
   WHEN: biographical - Achieving sales targets over a career
   WHERE: arbitrage - Can use "test drives" to play with ownership psychology
   SCOPE: local - Immediate environment
   
   WHY THIS CLASSIFICATION:
   For the institutional seller, the effect is a Rope. It is a functional 
   coordination mechanism for closing deals. By encouraging a "test drive" 
   or "free home trial," they induce a sense of ownership, pulling the 
   customer toward a purchase through the psychological "tether" of 
   the endowment effect.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    endowment_effect,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE IMPRISONED HEIR - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to emotional/cultural rules
   WHEN: immediate - Today's financial crisis vs. the object
   WHERE: constrained - Exit possible but psychologically/culturally costly
   SCOPE: local - Village or immediate household
   
   WHY THIS CLASSIFICATION:
   For an heir who desperately needs money but cannot bring themselves to 
   sell a family estate, the effect is a Snare. Their own psychology 
   strangles their financial future. The value they ascribe to the "endowment" 
   prevents them from taking the "exit" that market reality offers, 
   extracting their potential wealth and freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    endowment_effect,
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

:- begin_tests(endowment_effect_tests).

test(multi_perspective_variance) :-
    % Test that different perspectives yield different classifications
    constraint_indexing:constraint_classification(endowment_effect, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(endowment_effect, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(endowment_effect, snare, context(agent_power(individual_powerless), _, _, _)).

test(power_extractiveness_asymmetry) :-
    % Powerless experience more extraction as the "Snare" of sentiment
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, local),
    constraint_indexing:extractiveness_for_agent(endowment_effect, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(endowment_effect, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_scale) :-
    % Biological traits are Mountains on civilizational scales
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(endowment_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.3):
 * Reasoning: Low-moderate. While it prevents efficient market clearing, 
 * it is often a "voluntary" internal extraction of opportunity cost 
 * rather than a coercive external theft.
 * * 2. SUPPRESSION SCORE (0.4):
 * Reasoning: Moderate. Traditional economics (alternatives) is taught 
 * universally, but the internal "noise" of the bias suppresses its 
 * application in high-stakes personal decisions.
 * * 3. PERSPECTIVE SELECTION:
 * Selected Analytical (Mountain), Institutional (Rope), and Powerless (Snare) 
 * to demonstrate the full spread of indexical types.
 * * 4. OMEGAS:
 * omega_variable(
 * biological_origin,
 * "Is the endowment effect an evolved biological heuristic (Mountain) or a 
 * learned social norm related to property rights (Rope)?",
 * resolution_mechanism("Comparative cross-cultural studies of societies with 
 * differing property concepts"),
 * impact("If Mountain: Fixed barrier. If Rope: Can be de-conditioned."),
 * confidence_without_resolution(medium)
 * ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Coasian Bargaining
 * Viability: Theoretically, initial allocation shouldn't matter for 
 * efficiency if transaction costs are zero.
 * Suppression: Suppressed by the internal psychological "cost" of parting 
 * with owned goods.
 * * CONCLUSION:
 * The presence of a rational market alternative (Coase) that is suppressed 
 * by a natural cognitive bias confirms the "Snare" type for the individual 
 * actor while remaining a "Mountain" of reality for the observer.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_endowment_effect].
 * 2. Multi-perspective: ?- multi_index_report(endowment_effect).
 * 3. Run tests: ?- run_tests(endowment_effect_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
