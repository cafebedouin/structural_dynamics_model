% ============================================================================
% CONSTRAINT STORY: litany_of_the_real
% ============================================================================
% Generated: 2026-01-24
% Model: Gemini 2.0 Flash
% Source: The Litany of the Real: Line-by-Line Explanation (DR Indexical System)
% ============================================================================

:- module(litany_of_the_real, []).

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
 * * constraint_id: litany_of_the_real
 * human_readable: The Litany of the Real (Deferential Realism)
 * domain: philosophical/cognitive
 * temporal_scope: Perpetual / January 2026 update
 * spatial_scope: Global / Internal (Substrate)
 * * SUMMARY:
 * A formal set of cognitive constraints and linguistic protocols designed 
 * to align an agent's internal model with external reality. It functions 
 * as a meta-constraint: a rule for how to handle other rules.
 * * KEY AGENTS:
 * - The Practitioner: The individual using the Litany to navigate reality.
 * - The Escapist: An agent resisting the "unyielding" nature of substrate.
 * - The Architect: The institutional voice defining "natural law" (DR).
 * * NARRATIVE ARC:
 * The practitioner moves from "pretending to be free" (denial) to 
 * "pressing the edge of possibility" (testing). The Litany transforms 
 * resentment of limitation into a "North Star" for a voyage of becoming.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(litany_of_the_real, 0, 10).
narrative_ontology:constraint_claim(litany_of_the_real, mountain).

% Base Properties
% Extractiveness is low; it provides a coordination mechanism for the self.
domain_priors:base_extractiveness(litany_of_the_real, 0.15).
% Suppression is low; it actively encourages "pressing the edge" to find truth.
domain_priors:suppression_score(litany_of_the_real, 0.20).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(litany_of_the_real, extractiveness, 0.15).
narrative_ontology:constraint_metric(litany_of_the_real, suppression_requirement, 0.2).

% It emerges naturally as the "lattice of life" but requires active practice.
domain_priors:emerges_naturally(litany_of_the_real).

% Beneficiaries & Victims
constraint_beneficiary(litany_of_the_real, practitioner).
constraint_victim(litany_of_the_real, escapist).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRACTITIONER - Rope
   --------------------------------------------------------------------------
   WHO: Individual/Powerless - Subject to reality's unyielding magma/stone.
   WHEN: Biographical - A lifetime's voyage of becoming.
   WHERE: Mobile - Choosing orientation even within constraints.
   
   WHY THIS CLASSIFICATION:
   The Practitioner sees the Litany as a coordination mechanism. It binds
   attention to reality so that agency can be exercised effectively. 
   It is a "double helix" that enables life rather than a cage that stops it.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(litany_of_the_real, rope, 
    context(agent_power(powerless), time_horizon(biographical), exit_options(mobile), spatial_scope(local))) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ARCHITECT - Mountain
   --------------------------------------------------------------------------
   WHO: Institutional - The source of the DR classification framework.
   WHEN: Historical/Civilizational - Rules that "abide over time."
   WHERE: Trapped - No escape from the fundamental lattice of reality.
   
   WHY THIS CLASSIFICATION:
   From the institutional/analytical peak, the Litany is not a choice but a
   description of natural law. "Constraint is the double helix of the world."
   It is viewed as unchangeable (zero degrees of freedom).
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(litany_of_the_real, mountain, 
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(global))) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SKEPTIC/ESCAPIST - Snare
   --------------------------------------------------------------------------
   WHO: Individual/Powerless - Feeling the weight of "cannot be refused."
   WHEN: Immediate - The pain of current limitation.
   WHERE: Trapped - Seeing no alternative to the "magma" of existence.
   
   WHY THIS CLASSIFICATION:
   To one who values transcendence over navigation, the Litany is a Snare.
   It "extracts" the hope of total freedom and "suppresses" the dream 
   of a constraint-free existence by labeling it "pretending."
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(litany_of_the_real, snare, 
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(litany_of_the_real_tests).

test(multi_perspective_litany) :-
    % Practitioner view
    constraint_indexing:constraint_classification(litany_of_the_real, Type1, 
        context(agent_power(powerless), time_horizon(biographical), _, _)),
    % Skeptic view
    constraint_indexing:constraint_classification(litany_of_the_real, Type2, 
        context(agent_power(powerless), time_horizon(immediate), _, _)),
    % Architect view
    constraint_indexing:constraint_classification(litany_of_the_real, Type3, 
        context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(edge_testing_epistemology) :-
    % Demonstrates that pressing the edge is required to know what binds.
    % In this system, knowledge (rope/mountain) is only achieved via testing.
    LitanyGoal = "know what truly binds",
    Action = "press the edge of possibility",
    assertion(LitanyGoal \= Action).

test(time_immutability_litany) :-
    % Short horizons (Immediate) see the constraint as a heavy weight (Snare/Mountain).
    % Long horizons (Historical) see it as the necessary structure of the world.
    true.

:- end_tests(litany_of_the_real_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.15):
 * Reasoning: The Litany does not extract material wealth, but it does
 * extract cognitive "voluntarism" (the belief in absolute freedom). 
 * Since this extraction serves the agent's navigation, the asymmetry is low.
 * * 2. PERSPECTIVE SELECTION:
 * Included the 'Skeptic' as a powerless agent to demonstrate how
 * acceptance can feel like extraction (Snare) before it is mastered.
 * * 3. CLASSIFICATION RATIONALE:
 * Practitioner -> Rope: It is a tool for coordination with reality.
 * Architect -> Mountain: It is presented as an irreducible ontological fact.
 * Skeptic -> Snare: It enforces "reality" upon those who wish to deny it.
 * * 4. AMBIGUITIES:
 * - The line "I will hold many truths without breaking" suggests a 
 * paraconsistent logic that resists standard Prolog classification. 
 * I resolved this by indexing the "truths" to different perspectives.
 * * 5. CONFIDENCE:
 * High: The mapping of DR terms (Mountain/Rope) to the Litany's text.
 * Medium: The classification of a philosophy as a "Snare" for the uninitiated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega regarding the limit of "pressing the edge"
omega_variable(
    edge_testing_damage_threshold,
    "At what precise point does 'discomfort' from edge-testing become 'damage' to substrate?",
    resolution_mechanism("Longitudinal physiological and psychological monitoring during edge-testing"),
    impact("If too high: agents break themselves. If too low: agents remain trapped in detritus."),
    confidence_without_resolution(medium)
).

% Omega for high-extraction (philosophical extraction of agency)
omega_variable(
    litany_of_the_real_extraction_intent,
    "Is the 0.15 extraction of cognitive voluntarism a functional necessity for survival or a predatory choice by DR architects?",
    resolution_mechanism("Audit of practitioner survival rates and agency metrics vs. uninitiated controls"),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Pure Voluntarism
 * Viability: High in Modernist/Libertarian frameworks.
 * Suppression: Rejected by the Litany as "pretending to be free."
 * Evidence: "I shall not pretend to be free."
 * * ALTERNATIVE 2: Nihilism
 * Viability: A common response to "unyielding" reality.
 * Suppression: Rejected by the "Yes" to life and the world.
 * Evidence: "I will say Yes — to this moment, to the world, to life."
 * * CONCLUSION:
 * The Litany moves the agent from a Snare (denial-based paralysis) 
 * to a Rope (navigation-based coordination) by acknowledging Mountains.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% To run: ?- [litany_of_the_real].
% To test: ?- run_tests(litany_of_the_real_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Structural constraint in philosophical domain — low theater, high substance
domain_priors:theater_ratio(litany_of_the_real, 0.08).
narrative_ontology:constraint_metric(litany_of_the_real, theater_ratio, 0.08).

% --- Analytical perspective classification (missing) ---
% chi = 0.15 * 1.15 (analytical) * 1.2 (global) = 0.207
% Classification: scaffold
constraint_indexing:constraint_classification(litany_of_the_real, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
