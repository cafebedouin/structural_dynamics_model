% ============================================================================
% CONSTRAINT STORY: ergot_grain_poisoning
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Ergot on Rye" from cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_ergot_grain_poisoning, []).

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
 * * constraint_id: ergot_grain_poisoning
 * human_readable: The Ergot/Famine Dilemma
 * domain: social/technological/biological
 * temporal_scope: 857 A.D. to late 1800s
 * spatial_scope: Continental Europe (primarily the Rhine Valley and France)
 * * SUMMARY:
 * Ergot (Claviceps) is a fungal parasite of grains—primarily rye—that causes hallucinations, 
 * convulsions, and gangrenous death (St. Anthony's Fire). Historically, 
 * population booms forced the cultivation of marginal lands where rye flourished, 
 * leading to a systemic trade-off: widespread famine vs. widespread madness/chaos.
 * * KEY AGENTS:
 * - The Peasantry (Subject): Forced to eat "spurred rye" during "the starving time" 
 * when all other food stores were exhausted.
 * - The Miller (Institutional): Controlled the distribution of grain, keeping clean 
 * flour for the affluent and selling "spurred" flour to the poor.
 * - The King/Policy-Maker: Faced with the dilemma of destroying harvests (famine) 
 * or preserving life through poisoned grain (madness).
 * - The Analytical Historian: Observes the pattern of "dancing manias" and 
 * the transition to the potato as a resolution to food insecurity.
 * * NARRATIVE ARC:
 * The constraint emerges from biological ignorance and resource scarcity. 
 * In the Middle Ages, ergotism was misidentified as demonic possession or divine 
 * punishment. It served as a social filter where the wealthy avoided the 
 * "Snare" of poisoning by shifting the risk onto the poor until the adoption 
 * of the potato and scientific agriculture provided a "Rope" of exit.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(ergot_grain_poisoning_interval, 0, 10).
narrative_ontology:constraint_claim([ergot_grain_poisoning], [biological_economic_trap]).

% Base extractiveness score (0.8 = High)
% Rationale: The system extracts the health, limbs, and sanity of the poor to 
% maintain the biological survival of the kingdom and the profits of millers.
domain_priors:base_extractiveness(ergot_grain_poisoning, 0.8).

% Suppression score (0.7 = High)
% Rationale: Scientific causes were unknown for centuries; alternatives like 
% deep tilling were hidden by ignorance, and "madness" was suppressed by religious 
% interpretations (Inquisition/Devil).
domain_priors:suppression_score(ergot_grain_poisoning, 0.7).

% Enforcement requirements
% While the fungus is natural, the social enforcement (selling sparked rye to the poor) 
% was an active institutional choice by millers.
domain_priors:requires_active_enforcement(ergot_grain_poisoning).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(ergot_grain_poisoning, extractiveness, 0.8).
narrative_ontology:constraint_metric(ergot_grain_poisoning, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(ergot_grain_poisoning, affluent_customers). % Received clean flour
constraint_beneficiary(ergot_grain_poisoning, millers).            % Avoided losses by selling bad grain
constraint_victim(ergot_grain_poisoning, peasantry).               % Suffered ergotism/death

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PEASANT - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (No control over grain supply or soil knowledge)
   WHEN: immediate (Short-term survival during "the starving time")
   WHERE: trapped (Bounded by marginal lands and famine)
   SCOPE: local (Immediate village/town)
   
   WHY THIS CLASSIFICATION:
   For the poor, ergot is a "Snare." They must eat the poisoned grain to avoid 
   famine, accepting disfigurement or madness as the price of physical survival. 
   The extraction is absolute: up to 40% mortality in some epidemics.
   
   NARRATIVE EVIDENCE:
   "Millers... frequently kept clean rye flour for the affluent, selling flour 
   made from ‘spurred rye’... to poorer customers".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    ergot_grain_poisoning,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(ergot_grain_poisoning, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MILLER / THE KING - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Control over grain distribution/policy)
   WHEN: biographical (Managing harvests and trade over decades)
   WHERE: mobile (Access to clean stores; ability to choose who eats what)
   SCOPE: regional/national (Rhine Valley / Kingdom scale)
   
   WHY THIS CLASSIFICATION:
   For the institutional agent, ergot is a "Rope"—a coordination mechanism to 
   prevent total kingdom collapse. By sacrificing the sanity of the 
   many for the "memories" of the few (the unpoisoned sane), they manage the 
   civilizational threat of famine.
   
   NARRATIVE EVIDENCE:
   "The kingdom would face either widespread famine... or widespread madness... 
   the king reluctantly decided to have the people go ahead and eat the grain".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergot_grain_poisoning,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of long-term biological/social trends)
   WHEN: historical/civilizational (Viewing the High Middle Ages to present)
   WHERE: analytical (Observer stance)
   SCOPE: global (Grain genetics and soil alkalinity effects)
   
   WHY THIS CLASSIFICATION:
   To the analytical observer, ergotism is a "Mountain"—a fixed biological 
   reality of the Claviceps genus interacting with human population expansion. 
   Before the discovery of saline soaking and crop rotation, it was an 
   unchangeable natural law of the environment.
   
   NARRATIVE EVIDENCE:
   "Ergot is a fungi of the genus Claviceps... it has two major effects... 
   decimating urban populations every few years for centuries".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    ergot_grain_poisoning,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ergot_grain_poisoning_tests).

test(multi_perspective_madness) :-
    % Peasant (Powerless) sees Snare
    constraint_indexing:constraint_classification(ergot_grain_poisoning, snare, context(individual_powerless, immediate, trapped, local)),
    % King (Institutional) sees Rope (survival tool)
    constraint_indexing:constraint_classification(ergot_grain_poisoning, rope, context(institutional, biographical, mobile, national)),
    % Historian (Analytical) sees Mountain
    constraint_indexing:constraint_classification(ergot_grain_poisoning, mountain, context(analytical, historical, analytical, global)),
    !.

test(power_extractiveness_scaling) :-
    % The powerless experience the full 0.8 extraction, while millers benefit.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, national),
    constraint_indexing:extractiveness_for_agent(ergot_grain_poisoning, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(ergot_grain_poisoning, ContextPowerful, E2),
    E1 > E2.

test(time_immutability_discovery) :-
    % 1021 CE (Immediate) = inescapable madness/Mountain
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    % Evolution of Agriculture (Historical) = avoidable/Rope
    constraint_indexing:effective_immutability(historical, analytical, rope).

:- end_tests(ergot_grain_poisoning_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * Reasoning: High-asymmetry extraction where the peasantry pays with their 
 * bodies and minds so the affluent can remain sane/clean. 
 * The 40% mortality rate in epidemics confirms severe extraction.
 * * 2. SUPPRESSION SCORE (0.7):
 * Reasoning: For centuries, the biological cause was suppressed by 
 * religious narratives and the general lack of scientific understanding 
 * of fungi.
 * * 3. PERSPECTIVE SELECTION:
 * Contrasted the Peasant (Snare) who has no exit during "the starving time" 
 * with the Miller (Rope) who uses the "spurred" rye to maintain profit 
 * margins and the King who uses it as a tragic tool of coordination.
 * * 4. AMBIGUITIES:
 * - The "dancing plague" in 1021 CE could be apocryphal, but its symptoms 
 * match hallucinogenic ergotism.
 * - The connection to the Cathars is speculative but plausible within the 
 * system of religious purity and grain poisoning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction:
omega_variable(
    ergot_famine_intent,
    "Was the distribution of spurred rye to the poor a functional necessity for survival (Mountain) or an intentional predatory extraction (Snare/Mandatrophy)?",
    resolution_mechanism("Audit of medieval grain stores and distribution logs vs. famine mortality rates"),
    impact("If necessity: Survival Mountain. If predatory: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    hallucinogenic_personality_permanence,
    "Does ergot/psilocybin produce permanent personality shifts (Mountain) or temporary cognitive Ropes?",
    resolution_mechanism("Longitudinal tracking of Openness To Experience in participants 20+ years after exposure"),
    impact("If Mountain: Fixed shift in civilizational openness. If Rope: Temporary tool for state-shifting."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Potato (Solanum tuberosum)
 * Viability: Adopted in the 1770s as a superior food source that resisted 
 * the ergot/rye famine cycle.
 * Suppression: Initially rejected/ignored until acute famine pressure made 
 * food security a national priority.
 * * ALTERNATIVE 2: Scientific Prevention (Saline Soaking/Crop Rotation)
 * Viability: Deep tilling kills spores; saline soaking separates ergot.
 * Suppression: Suppressed by general ignorance and lack of microscopic 
 * understanding of fungi before 1676 C.E.
 * * CONCLUSION:
 * The presence of these alternatives shifts ergot from a "Mountain" of 
 * biological fate into a "Snare" created by institutional ignorance and 
 * social stratification (millers choosing to sell sparked rye).
 */



/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [ergot_grain_poisoning].
% Multi-perspective: ?- constraint_indexing:multi_index_report(ergot_grain_poisoning).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
