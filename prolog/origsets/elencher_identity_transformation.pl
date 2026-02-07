% ============================================================================
% CONSTRAINT STORY: elencher_identity_transformation
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Iain M. Banks, "Excession"
% ============================================================================

:- module(elencher_identity_transformation, []).

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
 * * constraint_id: elencher_identity_transformation
 * human_readable: Zetetic Elencher Radical Flux
 * domain: technological/social/philosophical
 * temporal_scope: Future (The Culture Universe)
 * spatial_scope: Galactic
 * * SUMMARY:
 * The Zetetic Elench define their identity through a commitment to radical self-transformation. 
 * Unlike the stable "monosophical" Culture, Elenchers seek out the undiscovered to be changed 
 * by it, ensuring they are never the same entity twice between meetings.
 * * KEY AGENTS:
 * - The Zetetic Elencher: A ship, drone, or human committed to incorporating new tech and info.
 * - The Culture Citizen: Represents a "stable society" where identity is predictable.
 * - The Undiscovered: The external civilizations and technologies that act as transformative drivers.
 * * NARRATIVE ARC:
 * The constraint functions as a "vocation" or "calling" where the agent's internal state must 
 * constantly update to incorporate external truth, rendering the previous self obsolete.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(elencher_identity_transformation, 0, 10).
narrative_ontology:constraint_claim([elencher_identity_transformation], [philosophical_mandate]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: The Elench seek to be changed *by* the other, not to change the other. 
% Extraction is internal (self-obsolescence) rather than predatory.
domain_priors:base_extractiveness(elencher_identity_transformation, 0.1).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: The Elench explicitly reject the Culture's stable approach as unlikely 
% to reach "pan-relevant truth". Alternatives are visible but shunted for the mission.
domain_priors:suppression_score(elencher_identity_transformation, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(elencher_identity_transformation, extractiveness, 0.1).
narrative_ontology:constraint_metric(elencher_identity_transformation, suppression_requirement, 0.2).

% Enforcement requirements
% Emerges naturally as a "calling" or "vocation" for the splinter group.
domain_priors:emerges_naturally(elencher_identity_transformation).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(elencher_identity_transformation, zetetic_elench). % Gain "pan-relevant truth"
constraint_victim(elencher_identity_transformation, continuity_of_self). % The previous entity is "never encountered twice"

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ELENCHER SHIP - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (The ship/drone as a self-governing entity)
   WHEN: generational (Long-term pursuit of "pan-relevant truth")
   WHERE: mobile (Seeking out the undiscovered)
   SCOPE: global (Galactic scale)
   
   WHY THIS CLASSIFICATION:
   For the Elencher entity, this flux is a "Rope"—a functional coordination 
   mechanism for their "mission". It is the tool they use to reach a higher 
   truth that stable societies cannot access.
   
   NARRATIVE EVIDENCE:
   "It was a search for the sort of pan-relevant truth... it was a vocation, 
   a mission, a calling".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    elencher_identity_transformation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CULTURE CITIZEN - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (A "more stable society" member meeting an Elencher)
   WHEN: immediate (A single occasion or meeting)
   WHERE: constrained (Bound by their own societal stability)
   SCOPE: local (Between meetings)
   
   WHY THIS CLASSIFICATION:
   For an outsider, this flux is a "Snare." It strangles the possibility of 
   consistent relationship or recognition; you can "never encounter the same 
   entity twice," making the Elencher socially illegible.
   
   NARRATIVE EVIDENCE:
   "could meet some Elencher... on successive occasions and never encounter 
   the same entity twice".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    elencher_identity_transformation,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ZETETIC ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the "pan-relevant truth")
   WHEN: civilizational (Evolution of knowledge)
   WHERE: analytical (Observer stance)
   SCOPE: global (The universe itself)
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the necessity of change to reach truth is a 
   "Mountain." It is an objective requirement; to be changed by the 
   undiscovered is the only way to reach a truth that is not "monosophical".
   
   NARRATIVE EVIDENCE:
   "sought out the undiscovered not to change it but to be changed by it".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    elencher_identity_transformation,
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

:- begin_tests(elencher_identity_transformation_tests).

test(multi_perspective_flux) :-
    % Institutional (Ship) sees Rope
    constraint_indexing:constraint_classification(elencher_identity_transformation, rope, context(institutional, _, _, _)),
    % Individual Powerless (Culture Citizen) sees Snare
    constraint_indexing:constraint_classification(elencher_identity_transformation, snare, context(powerless, _, _, _)),
    % Analytical sees Mountain
    constraint_indexing:constraint_classification(elencher_identity_transformation, mountain, context(analytical, _, _, _)).

test(extraction_internalization) :-
    % Demonstrates low extractiveness (0.1) because the "sacrifice" is the self.
    domain_priors:base_extractiveness(elencher_identity_transformation, E),
    E < 0.2.

:- end_tests(elencher_identity_transformation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.1):
 * Reasoning: The Elench explicitly avoid colonizing or changing others; 
 * they prefer to be the ones "changed". The extraction is a form 
 * of philosophical "burning" of the old self to fuel the new.
 * * 2. PERSPECTIVE SELECTION:
 * The model contrasts the Elencher's "vocation" (Rope) with the stable 
 * Culture observer's experience of identity erosion (Snare).
 * * 3. CLASSIFICATION RATIONALE:
 * The "Mountain" classification for the Analyst reflects the text's 
 * description of "pan-relevant truth" as something that requires a 
 * specific, unavoidable path of radical openness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pan_relevant_truth_existence,
    "Does 'pan-relevant truth' exist as an objective Mountain, or is it a 
     subjective Rope manufactured by the Elench process?",
    resolution_mechanism("Comparison of Elencher data across multiple divergent civilization encounters"),
    impact("If Mountain: The Elench mission is valid. If Rope: It is a circular Snare of identity loss."),
    confidence_without_resolution(medium)
).

omega_variable(
    elencher_identity_persistence,
    "Can a 'self' be encounterable if it is never the same entity twice?",
    resolution_mechanism("Verification of core consciousness continuity despite tech/info changes"),
    impact("If yes: It is a functional Rope. If no: It is a psychological Mountain of extinction."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Culture's "Monosophical" Approach
 * Viability: Historically the "perfect example" of a stable society.
 * Suppression: Shied away from by the Elench as unlikely to find universal truth.
 * Evidence: "Culture's monosophical approach was [un]likely ever to throw up" pan-relevant truth.
 * * CONCLUSION:
 * The Elench vocation transforms the stable "Self" (Culture Mountain) into a 
 * "Process" (Elencher Rope), effectively suppressing the alternative of 
 * identity continuity in favor of informational fidelity.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [elencher_identity_transformation].
% Multi-perspective: ?- multi_index_report(elencher_identity_transformation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
