% ============================================================================
% CONSTRAINT STORY: khantivadin_radical_patience
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Ayya Khema, "Being Nobody, Going Nowhere"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_khantivadin_radical_patience, []).

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
 * * constraint_id: khantivadin_radical_patience
 * human_readable: The Teacher of Patience (Khantivadin)
 * domain: religious/ethical
 * temporal_scope: Perennial / Folklore
 * spatial_scope: Kingdom of Kausala
 * * SUMMARY:
 * This constraint defines a state of radical patience (Khanti) in the face of absolute physical 
 * liquidation. A sage, Khantivadin, is dismembered by a jealous king for 
 * preaching to the king's wives, yet he refuses to respond with anger or curses, identifying 
 * patience as a quality of mind rather than a property of the body.
 * * KEY AGENTS:
 * - Khantivadin: The "Teacher of Patience" who remains unmoved during dismemberment.
 * - The King of Kausala: An institutional agent whose rage and intoxication drive 
 * terminal extraction.
 * - The Royal Wives: Five hundred agents whose search for beauty leads them to the 
 * sage's sermon.
 * - The Soldiers: Powerless agents caught between the King's commands and their 
 * fear of the sage's spiritual power.
 * * NARRATIVE ARC:
 * 
 * A royal picnic turns into an execution when the King finds his wives listening to 
 * Khantivadin's sermon. The King's jealousy triggers a terminal "Noose" 
 * for the sage, but the sage's detachment transforms the physical extraction into a 
 * "Mountain" of moral law.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(khantivadin_patience_interval, 0, 10).
narrative_ontology:constraint_claim([khantivadin_radical_patience], [ontological_resilience]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Absolute (1.0). The king literally cuts the sage "to pieces" until 
% he is "on the point of dying".
domain_priors:base_extractiveness(khantivadin_radical_patience, 1.0).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: High (0.9). The King is "under the influence of all that food and 
% drink" and "couldn't listen to reason," suppressing all alternatives to violence.
domain_priors:suppression_score(khantivadin_radical_patience, 0.9).

% Enforcement requirements
% Requires active enforcement by the King and his soldiers.
domain_priors:requires_active_enforcement(khantivadin_radical_patience).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(khantivadin_radical_patience, extractiveness, 1.0).
narrative_ontology:constraint_metric(khantivadin_radical_patience, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(khantivadin_radical_patience, spiritual_legacy). % The Buddha uses this as a teaching.
constraint_victim(khantivadin_radical_patience, khantivadin_physical_body). % Dismembered and killed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: KHANTIVADIN - Noose (Physical) / Rope (Mind)
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Tied to a tree, physically helpless)
   WHEN: immediate (The moment of dismemberment)
   WHERE: trapped (Bound by the King's soldiers)
   SCOPE: local (The meadow)
   
   WHY THIS CLASSIFICATION:
   For Khantivadin's body, the King's knife is a "Noose" of terminal extraction. 
   However, for his mind, patience is a "Rope"—a tool for coordination with the 
   divine/dhamma that allows him to "not curse anyone" even at death.
   
   NARRATIVE EVIDENCE:
   "he cut off one foot and said, ‘And where is your patience now?’ Khantivadin 
   replied, ‘Not in my foot, your Majesty'".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    khantivadin_radical_patience,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(khantivadin_radical_patience, E),
    E > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE KING OF KAUSALA - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power over the kingdom)
   WHEN: biographical (A day of picnic/jealousy)
   WHERE: mobile (He has the agency to "take a huge knife" and act)
   SCOPE: national (His kingdom, which the soldiers fear will be cursed)
   
   WHY THIS CLASSIFICATION:
   For the King, violence is a "Rope"—a tool used to maintain his perceived 
   "ownership" of his wives and to punish those who "take my wives away".
   
   NARRATIVE EVIDENCE:
   "He called the soldiers and said ‘Go! Get my wives back immediately’... the 
   soldiers... were in the employ of the king".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    khantivadin_radical_patience,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BUDDHA - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The "ancient and wise" observer of truth)
   WHEN: civilizational (A law of the teaching for all who understand)
   WHERE: analytical (Observer stance)
   SCOPE: global (Applies to all practitioners)
   
   WHY THIS CLASSIFICATION:
   To the Buddha, this act is a "Mountain"—an unchangeable natural law of 
   awakening. True patience is absolute and does not reside in the limbs; 
   it is a zero-degree-of-freedom requirement for his disciples.
   
   NARRATIVE EVIDENCE:
   "Who does not act in this way has not understood my teaching".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    khantivadin_radical_patience,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(khantivadin_radical_patience_tests).

test(multi_perspective_patience) :-
    % Physical victim sees Noose
    constraint_indexing:constraint_classification(khantivadin_radical_patience, noose, context(individual_powerless, immediate, trapped, local)),
    % King (Institutional) sees Rope
    constraint_indexing:constraint_classification(khantivadin_radical_patience, rope, context(institutional, biographical, mobile, national)),
    % Buddha (Analytical) sees Mountain
    constraint_indexing:constraint_classification(khantivadin_radical_patience, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_liquidation) :-
    % The powerless sage experiences the full 1.0 extraction of his life
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, national),
    constraint_indexing:extractiveness_for_agent(khantivadin_radical_patience, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(khantivadin_radical_patience, ContextPowerful, E2),
    E1 > E2.

test(time_immutability_dhamma) :-
    % In the short term, the king's knife is a Mountain (fact)
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    % Over civilizational time, the Sage's patience is the Mountain (truth)
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

:- end_tests(khantivadin_radical_patience_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (1.0):
 * Reasoning: Dismemberment followed by death is the terminal point of extraction. 
 * There is no higher asymmetry than the liquidation of an agent's life.
 * * 2. STATUS: [RESOLVED MANDATROPHY]:
 * The extraction is absolute. However, it is "resolved" by the Sage's 
 * indexed detachment (Rope of the mind), which allows him to bypass the 
 * standard "suffering" usually associated with such a Noose.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Khantivadin (Noose) to show the cost, the King (Rope) to show 
 * predatory utility, and the Buddha (Mountain) to show the eternal lesson.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    khantivadin_extraction_intent,
    "Is the dismemberment a functional necessity for the King's authority (Mountain) or an intoxicated predatory choice (Noose)?",
    resolution_mechanism("Audit of the King's behavior post-sobering to see if the violence remains a policy tool"),
    impact("If necessity: Political Mountain. If intoxicated choice: Predatory Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    patience_origin,
    "Is Khantivadin's patience a biological anomaly (Mountain) or a learned skill available to all (Rope)?",
    resolution_mechanism(" Buddha's assertion suggests it is a learnable requirement: 'Who does not act in this way has not understood'"),
    impact("If anomaly: Personal Mountain. If skill: Universal Rope."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Resistance / Curse
 * Viability: The soldiers' suggestion: "please do not curse the whole kingdom".
 * Suppression: Explicitly rejected by Khantivadin: "I do not curse anyone".
 * * ALTERNATIVE 2: Royal Reason
 * Viability: Listening to the sermon of loving-kindness.
 * Suppression: Suppressed by the King's intoxication: "couldn't listen to reason".
 * * CONCLUSION:
 * The King's intoxicated state makes Alternatives 1 and 2 invisible, forcing 
 * the interaction into a Noose for the Sage. Khantivadin's choice to remain 
 * patient is a deliberate "Rope" that prevents him from being "poisoned" by the 
 * King's rage.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [khantivadin_radical_patience].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(khantivadin_radical_patience).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
