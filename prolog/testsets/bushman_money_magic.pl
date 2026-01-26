% ============================================================================
% CONSTRAINT STORY: bushman_money_magic
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Bushman Money Magic" from cafebedouin.org (quoting James Suzman)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_bushman_money_magic, []).

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
 * * constraint_id: bushman_money_magic
 * human_readable: The Trickster's Asymmetric Scam
 * domain: economic/social
 * temporal_scope: Traditional/Mythic (Bushman folklore context)
 * spatial_scope: The Kalahari / Rural Frontier
 * * SUMMARY:
 * This constraint represents the "magic scam" where a trickster figure (Jackal) utilizes 
 * deception and temporary narrative control to extract wealth from victims (cattle ranchers). 
 * It functions by hiding physical causality (fire under sand, money in a donkey) to present 
 * a "magic" alternative that appears highly beneficial but is functionally empty.
 * * KEY AGENTS:
 * - Jackal (The Trickster): The rule-shaper who constructs the "magic" constraint to extract wealth.
 * - Cattle Ranchers (The Victims): Subjects who "agree" to the asymmetric exchange due to greed and 
 * suppressed context.
 * - The Donkey (The Prop): The physical vehicle used to manifest the "magic" result.
 * * NARRATIVE ARC:
 * Jackal creates a fake "magic pot" and "magic donkey" to sell to ranchers for $1,000 each. 
 * The constraint is a temporary information asymmetry where the physical reality is 
 * actively suppressed (sand over fire, money hidden in donkey). Once the exchange 
 * is complete, the "magic" vanishes, leaving the victims in a state of loss.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bushman_money_magic_interval, 0, 10).
narrative_ontology:constraint_claim([bushman_money_magic], [asymmetric_scam]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.9). The exchange is entirely one-sided; Jackal extracts $2,000 
% in exchange for zero functional value, utilizing the victims' labor (wealth) 
% for his own survival.
domain_priors:base_extractiveness(bushman_money_magic, 0.9).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: High (0.8). The physical reality (the fire under the sand) is 
% actively hidden to maintain the "magic" narrative. The truth is suppressed 
% until the trickster is gone.
domain_priors:suppression_score(bushman_money_magic, 0.8).

% Enforcement requirements
% Requires active enforcement (Jackal must actively perform the "hits" and 
% stage the evidence to maintain the scam).
domain_priors:requires_active_enforcement(bushman_money_magic).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(bushman_money_magic, extractiveness, 0.9).
narrative_ontology:constraint_metric(bushman_money_magic, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bushman_money_magic, jackal_the_trickster).
constraint_victim(bushman_money_magic, cattle_ranchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: JACKAL (THE TRICKSTER) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (In this context, Jackal is the "author" of the rules of the scam)
   WHEN: immediate (Short-term survival/extraction)
   WHERE: mobile (He "fled with the money" once the exchange was done)
   SCOPE: local (The immediate camp/interaction site)
   
   WHY THIS CLASSIFICATION:
   For Jackal, the "magic" items are a "Rope"—a functional coordination mechanism 
   he uses to pull resources from the ranchers to himself. It is a tool of agency 
   to achieve a "full belly" in the desert.
   
   NARRATIVE EVIDENCE:
   "Jackal saw them coming, he was scared... quickly took the money... and hid it 
   in his donkey’s anus". He uses the situation to his advantage.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    bushman_money_magic,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CATTLE RANCHERS (THE VICTIMS) - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (In the context of the scam, they have zero agency)
   WHEN: immediate (Focused on the "wonderful" magic pot right now)
   WHERE: trapped (Bounded by their own gullibility and lack of information)
   SCOPE: local (The interaction site)
   
   WHY THIS CLASSIFICATION:
   The scam is a "Snare." The ranchers "agree" to the rules, but those rules 
   systematically extract their wealth while providing a "magic" illusion that 
   tightens their loss. They are trapped by the trickster's narrative control.
   
   NARRATIVE EVIDENCE:
   "So they put some raw meat in the pot and hit it three times... But the meat 
   was still raw... 'We have been tricked!' they shouted".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bushman_money_magic,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (THE HISTORIAN/ANTHROPOLOGIST) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the archetype of the trickster across cultures)
   WHEN: historical/civilizational (The trickster is a "millennia-old" role)
   WHERE: analytical (Observer stance)
   SCOPE: global (The human condition of greed meeting deception)
   
   WHY THIS CLASSIFICATION:
   To the observer, the "scam" is a "Mountain"—an immutable feature of human 
   social systems where information asymmetry and greed inevitably produce 
   asymmetric extraction. It is a natural law of the "jungle".
   
   NARRATIVE EVIDENCE:
   "Jackal [is the] Bushman trickster god... he called it the churn, when 
   the rules of the game change".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bushman_money_magic,
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

:- begin_tests(bushman_money_magic_tests).

test(multi_perspective_scam) :-
    % Jackal (Institutional/Mobile) sees a Rope
    constraint_indexing:constraint_classification(bushman_money_magic, rope, context(institutional, immediate, mobile, local)),
    % Ranchers (Powerless/Trapped) see a Snare
    constraint_indexing:constraint_classification(bushman_money_magic, snare, context(individual_powerless, immediate, trapped, local)),
    % Historian (Analytical) sees a Mountain
    constraint_indexing:constraint_classification(bushman_money_magic, mountain, context(analytical, historical, analytical, global)).

test(power_extractiveness_scam) :-
    % The powerless victim experiences the full 0.9 extraction.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, immediate, mobile, local),
    constraint_indexing:extractiveness_for_agent(bushman_money_magic, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(bushman_money_magic, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_scam) :-
    % In the immediate interaction, the scam is a fact (Mountain/Snare).
    % Over a historical horizon, it's a social coordination mechanism (Rope).
    constraint_indexing:effective_immutability(historical, analytical, rope).

:- end_tests(bushman_money_magic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================= */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.9):
 * Reasoning: Chose high because the scam is purely predatory; no real value is 
 * exchanged, and it relies on the complete loss of the victim's resources.
 * * 2. SUPPRESSION SCORE (0.8):
 * Reasoning: The trick depends on the active suppression of the fire's presence. 
 * If the ranchers saw the fire, the "magic" (the constraint) would dissolve 
 * immediately.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Jackal (Rope) to show how extraction is a tool for the powerful/clever, 
 * the Ranchers (Snare) to show how gullibility traps the subject, and the 
 * Analyst (Mountain) to show the archetypal inevitability of the trickster.
 * * 4. MANDATROPHY RESOLUTION:
 * The status is [RESOLVED MANDATROPHY] because while the system is an 
 * absolute Snare for the ranchers, it is a life-sustaining Rope for Jackal 
 * (the beneficiary).
 * * 5. AMBIGUITIES:
 * - The text mentions "$1,000" in a Bushman myth context, which implies an 
 * integration of modern currency into traditional story structures. 
 * I have treated this as a generic unit of wealth extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    bushman_money_magic_extraction_intent,
    "Is Jackal's scam a functional necessity for survival in a hyper-arid 'Empty Quarter' or an intentional predatory choice?",
    resolution_mechanism("Audit of Jackal's resource alternatives vs. the frequency of the 'trickster' behavior"),
    impact("If necessity: Survival Mountain. If predatory choice: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    rancher_gullibility_persistence,
    "Do the ranchers learn to bypass the trickster in the next encounter (Rope), or is their greed a fixed biological trait (Mountain)?",
    resolution_mechanism("Follow-up on the 'successive occasions' of ranchers meeting Jackal"),
    impact("If they learn: Trickery is a temporary Snare. If not: It is a systemic Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Honest Trade
 * Viability: Jackal could have sold the meat directly or traded donkey labor.
 * Suppression: Rejected because the "magic" narrative allows for a 1000x 
 * premium over physical value.
 * * ALTERNATIVE 2: Social Cooperation
 * Viability: The "comradeship inherent in the circumstances" mentioned by 
 * Thesiger elsewhere.
 * Suppression: Actively suppressed by Jackal's fear and "scared" reaction to 
 * the ranchers, leading him to choose the "side door" of trickery.
 * * CONCLUSION:
 * The existence of honest alternatives makes the scam a definitive "Snare." 
 * The trickster chooses extraction because it is more efficient than cooperation 
 * in the short term.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system: ?- [constraint_bushman_money_magic].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
