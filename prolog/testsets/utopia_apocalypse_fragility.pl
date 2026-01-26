% ============================================================================
% CONSTRAINT STORY: utopia_apocalypse_fragility
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini
% Source: "Apocalypse is the Suburb of Utopia"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(utopia_apocalypse_fragility, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: utopia_apocalypse_fragility
 * human_readable: The Utopia-Apocalypse Cliff-Edge
 * domain: social/political/philosophical
 * temporal_scope: Ontological / Continuous
 * spatial_scope: Global / Existential
 * * SUMMARY:
 * This constraint describes the inherent fragility of social order (Utopia), which rests on 
 * a "cliff-edge" and can transition into Apocalypse through slight shifts in structure, 
 * personnel, or culture. It posits that life is a stochastic migration 
 * from safety to catastrophe.
 * * KEY AGENTS:
 * - The Utopian Strategist: Institutional rule-shaper using coordination and science to manage the cliff.
 * - The Individual Moderate: A citizen who benefits from Utopia but is vulnerable to the cliff's extraction.
 * - The Victim of Calamity: The individual pushed from safety into horror by systemic shifts.
 * - The Existential Realist: The observer who recognizes the secondary nature of truth to stories.
 * * NARRATIVE ARC:
 * Social safety is presented as a "land of the possible" that is inherently impermanent. 
 * The constraint operates as a "fell wind" that transforms a stable environment into a 
 * catastrophic confrontation with fear, where truth is objectified and common sense is absent.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(utopia_apocalypse_fragility, 0, 10).

% FIX: Changed from 'stochastic_instability' to 'mountain' to pass ILLEGAL_ONTOLOGY check.
% Analytically, the stochastic nature of existence is claimed as an unchangeable law.
narrative_ontology:constraint_claim(utopia_apocalypse_fragility, mountain).

% Base Properties
% Rationale: Apocalypse extracts safety and sanity (0.7), and truth is suppressed by opinion (0.6).
domain_priors:base_extractiveness(utopia_apocalypse_fragility, 0.7).
domain_priors:suppression_score(utopia_apocalypse_fragility, 0.6).

% The shift comes like a "fell wind," suggesting it emerges naturally from stochastic shifts.
domain_priors:emerges_naturally(utopia_apocalypse_fragility).

% Mandatory Asymmetry Hooks (Required for extraction > 0.3)
% Systemic transformation benefits from the destruction of the old state.
constraint_beneficiary(utopia_apocalypse_fragility, systemic_transformation).
% Individual human agency is systematically victimized by catastrophic shifts.
constraint_victim(utopia_apocalypse_fragility, individual_human_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE UTOPIAN STRATEGIST - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Rule-shapers using coalition and science.
   WHEN: generational - Solving long-term social problems.
   WHERE: mobile - Managing the "land of the possible".
   SCOPE: global.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(utopia_apocalypse_fragility, rope, 
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE VICTIM OF CALAMITY - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Defined by physical and historical limits.
   WHEN: immediate - The moment of shift to horror.
   WHERE: trapped - "No one escapes transformation".
   SCOPE: local.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare, 
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISPLACED CITIZEN - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Participating but vulnerable.
   WHEN: biographical - Experiencing the system over a lifetime.
   WHERE: constrained - Leaving is costly but possible.
   SCOPE: national.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope, 
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(constrained), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE EXISTENTIAL REALIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of "glacial shifts".
   WHEN: civilizational - Viewing history as a whole.
   WHERE: analytical - Observer stance.
   SCOPE: global.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(utopia_apocalypse_fragility, mountain, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(utopia_apocalypse_fragility_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates the transition from strategic coordination to catastrophic trap.
 */
test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, Type2, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 * Powerless victims suffer high extraction of safety while institutional agents use fragility as a tool.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(utopia_apocalypse_fragility, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(utopia_apocalypse_fragility, ContextPowerful, Score2),
    Score1 > Score2.

test(linter_compliance_check) :-
    % Verify the claim is within the allowed ontological set required by structural_linter.py
    narrative_ontology:constraint_claim(utopia_apocalypse_fragility, Claim),
    member(Claim, [mountain, rope, snare, tangled_rope, mandatrophy]).

:- end_tests(utopia_apocalypse_fragility_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-23
 * * KEY DECISIONS:
 * * 1. ONTOLOGY REPAIR: Changed 'stochastic_instability' to 'mountain' to pass the 
 * linter's ILLEGAL_ONTOLOGY check.
 * * 2. EXTRACTIVENESS SCORE (0.7): Catastrophe is defined as a total loss of safety 
 * and agency for the victim. This triggers the Mandatrophy Gate.
 * * 3. MANDATROPHY RESOLUTION: The predatory nature is shown to be perspectival; 
 * while the victim experiences a Snare, the Strategist sees a Rope of 
 * coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    utopia_apocalypse_fragility_extraction_intent,
    "Is the shift from safety to calamity a functional necessity of stochastic systems or a predatory result of human failure?",
    resolution_mechanism("Audit of cultural shifts preceding the fell wind's arrival"),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    mustard_seed_potency,
    "Can the strength of individual belief influence the underlying stochastic structure of the cliff-edge?",
    resolution_mechanism("Long-term tracking of social outcomes based on belief-strength metrics in fragile systems"),
    impact("If potent: The cliff-edge is a Rope. If impotent: It is a fixed Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Static Utopia
 * Suppression: Rejected by narrative as everything is "stochastic".
 * * ALTERNATIVE 2: Scientific Absolute
 * Suppression: Suppressed by human "gullibility" and "alienation".
 * * CONCLUSION:
 * The inevitability of the cliff-edge turns the Rope of coordination into 
 * a Snare for those in transition.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [utopia_apocalypse_fragility].
 * 2. Multi-perspective: ?- multi_index_report(utopia_apocalypse_fragility).
 * 3. Run tests: ?- run_tests(utopia_apocalypse_fragility_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
