% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: trojan_war_spoils
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Trojan Women of Euripides (Gilbert Murray translation)
% ============================================================================

:- module(constraint_trojan_war_spoils, []).

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
 * * constraint_id: trojan_war_spoils
 * human_readable: The Allocation of the Captive
 * domain: military/social/existential
 * temporal_scope: Mythic Antiquity (Post-Fall of Troy)
 * spatial_scope: Regional (Troy/Run-down Citadel)
 * * SUMMARY:
 * The total defeat of Troy transforms its survivors into "spoils of war." 
 * The constraint is the Greek lottery and subsequent "Master-Slave" 
 * relationship that dictates the survivors' destination, labor, and life-span.
 * * KEY AGENTS:
 * - Hecuba: Former Queen, now a "slave of Odysseus," representing total loss of agency.
 * - Talthybius: The Greek Herald, an agent of the institution who manages the constraint.
 * - The Greek Masters (Odysseus, Neoptolemus): The rule-making power.
 * - Astyanax: The child-victim, whose existence is suppressed by the "Greeks' law."
 * * NARRATIVE ARC:
 * The play opens after the city's destruction. The survivors await the "lottery." 
 * The constraint is revealed through a series of "choices" made by the Greeks 
 * (Cassandra to Agamemnon, Andromache to Neoptolemus) that are experienced 
 * by the women as immutable fate and by the Greeks as administrative logistics.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(trojan_collapse_interval, 0, 10).
narrative_ontology:constraint_claim(trojan_war_spoils, snare).

% Base extractiveness score: 1.0
% Rationale: Total extraction. The city is burned, the men killed, the wealth 
% looted, and the women/children converted into human property.
domain_priors:base_extractiveness(trojan_war_spoils, 1.0).

% Suppression score: 1.0
% Rationale: Any alternative (survival of the Trojan line) is physically 
% suppressed, culminating in the throwing of Astyanax from the walls.
domain_priors:suppression_score(trojan_war_spoils, 1.0).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(trojan_war_spoils, extractiveness, 1.0).
narrative_ontology:constraint_metric(trojan_war_spoils, suppression_requirement, 1.0).

% Enforcement requirements: Active enforcement through the Greek fleet and "The Heralds."
domain_priors:requires_active_enforcement(trojan_war_spoils).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(trojan_war_spoils, [greek_army, agamemnon, odysseus]).
constraint_victim(trojan_war_spoils, [hecuba, andromache, cassandra, astyanax]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: HECUBA / THE WOMEN - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - "A slave of a slave".
   WHEN: immediate - The "dashing of the surge" of current misery.
   WHERE: trapped - The "Greekish tents" and the "dead city."
   SCOPE: regional - The limited horizon of the Aegean world.
   
   WHY THIS CLASSIFICATION:
   For the women, the fall of Troy and their enslavement is a "Mountain." 
   It is a law of nature/gods that has crushed their world. They have 
   zero degrees of freedom; even their prayers "are wind".
   
   NARRATIVE EVIDENCE:
   "What else is there? They have cast the lot... the lot that determines 
   where each must go".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trojan_war_spoils,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(trojan_war_spoils, greek_army),
        constraint_victim(trojan_war_spoils, hecuba),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(trojan_war_spoils, S),
    S > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GREEK HIGH COMMAND - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - The "Kings" who cast the lots.
   WHEN: historical - Thinking of the return to Greece and future security.
   WHERE: mobile - Moving their "prizes" onto the ships.
   SCOPE: global - Across the "Hellenic sea."
   
   WHY THIS CLASSIFICATION:
   For the Greek leadership, the "spoils system" is a "Rope"—a functional 
   coordination mechanism used to divide the loot fairly among the kings 
   to prevent civil strife in the army.
   
   NARRATIVE EVIDENCE:
   "Selected for the Kings... The lords of the host have divided them".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trojan_war_spoils,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        constraint_beneficiary(trojan_war_spoils, agamemnon),
        constraint_victim(trojan_war_spoils, []),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: TALTHYBIUS - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - A messenger who feels "unclean" in his duty.
   WHEN: immediate - The moment he must take Astyanax.
   WHERE: constrained - He is "bound" by the order of Odysseus.
   SCOPE: local - Within the walls of Troy.
   
   WHY THIS CLASSIFICATION:
   For Talthybius, the constraint is a "Snare." He recognizes it as a 
   coercive and "bitter" mechanism that extracts the life of an innocent 
   child. He has limited agency—he cannot stop it, but he can feel its weight.
   
   NARRATIVE EVIDENCE:
   "I am not fit to carry out such a word... There should be some other man, 
   less pitiless".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trojan_war_spoils,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(trojan_war_spoils, greek_army),
        constraint_victim(trojan_war_spoils, astyanax),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(trojan_war_spoils, E),
    E > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(trojan_war_spoils_tests).

test(total_extraction_check) :-
    % Verify that the system registers maximum extraction (Enslavement)
    domain_priors:base_extractiveness(trojan_war_spoils, 1.0).

test(heralds_gap) :-
    % Talthybius (Moderate) should see a Snare (coercion), 
    % while Hecuba (Powerless) sees a Mountain (fate).
    constraint_indexing:constraint_classification(trojan_war_spoils, mountain, context(agent_power(powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(trojan_war_spoils, snare, context(agent_power(individual_moderate), _, _, _, _, _)).

test(suppression_of_alternative) :-
    % Verify that the alternative (Trojan restoration) has zero survival chance.
    domain_priors:suppression_score(trojan_war_spoils, 1.0).

:- end_tests(trojan_war_spoils_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (1.0): This is the upper bound of the system. 
 * Slavery and the infanticide of the heir (Astyanax) represent the 
 * complete removal of an agent's future value for the benefit of the 
 * institution.
 * 2. PERSPECTIVAL GAP: The Herald (Talthybius) is the most interesting 
 * data point. He is the "operator" of the Snare. His discomfort shows 
 * that the constraint is not a Rope (coordination) for him, but a 
 * psychological Snare that forces him into "pitiless" action.
 * 3. MOUNTAIN LOGIC: Hecuba's transition from Queen (Institutional) to 
 * Slave (Powerless) represents a collapse from a rule-maker to a 
 * rule-receiver, turning her social "Rope" into a physical "Mountain."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_intent,
    "Is the Trojan defeat a literal Mountain of divine will (Poseidon/Athena) or a geopolitical Snare constructed by Greek strategy?",
    resolution_mechanism("Textual ambiguity in the Prologue between Poseidon's lament and the Greek outcome."),
    impact("If Divine: The play is about man vs. nature. If Geopolitical: The play is about the ethics of power."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Diplomatic Surrender / Ransom
 * Viability: Historically common in Greek warfare.
 * Suppression: Rejected by Odysseus and the "Council."
 * Evidence: Odysseus' specific argument for the death of Astyanax to 
 * prevent a future war.
 * * CONCLUSION:
 * The active suppression of Alternative 1 (Ransom) confirms the Greeks 
 * are operating an extractive "Snare" rather than a functional "Rope."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
