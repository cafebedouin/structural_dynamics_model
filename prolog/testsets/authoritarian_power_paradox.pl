% ============================================================================
% CONSTRAINT STORY: authoritarian_power_paradox
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini 2.0 Flash
% Source: Cross-Domain Synthesis (Political, Narrative, and Institutional)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(authoritarian_power_paradox, []).

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
 * * constraint_id: authoritarian_power_paradox
 * human_readable: The Paradox of Authoritarian Centralization
 * domain: political/social/technological
 * temporal_scope: Universal / Multi-Era
 * spatial_scope: Global / Institutional
 * * SUMMARY:
 * The paradox of power describes a state where an institution or leader 
 * increases centralization to ensure stability but, in doing so, suppresses 
 * the diagnostic sensors required to perceive threats. This creates 
 * "Brittle Stability"—a system that appears unyielding until it collapses 
 * instantaneously upon hitting an unmodeled reality.
 * * KEY AGENTS:
 * - The Institutional Core: Rule-making power seeking total coordination (Rope) 
 * but effectively creating systemic blindness (Snare).
 * - The Individual Outlier: Powerless agents with diagnostic capacity who are 
 * filtered out by conformity requirements.
 * - The Analytical Observer: External or historical perspective that identifies 
 * the terminal collapse.
 * * NARRATIVE ARC:
 * Systems move from "Adaptive Ropes" (coordination) to "Domesticated Mountains" 
 * (unquestioned dogma). The paradox is that the more a system 
 * eliminates "friction" (dissent), the more it extracts the intelligence 
 * required for its own survival.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(authoritarian_power_paradox, 0, 10).
narrative_ontology:constraint_claim(authoritarian_power_paradox, snare).

% Base extractiveness: 0.80 (High)
% Rationale: High extraction of cognitive diversity and local agency to 
% maintain the unified narrative of the central core.
domain_priors:base_extractiveness(authoritarian_power_paradox, 0.8).

% Suppression score: 0.90 (Extreme)
% Rationale: Severe suppression of boundary-testing and non-conforming 
% feedback loops.
domain_priors:suppression_score(authoritarian_power_paradox, 0.9).

% Enforcement: Requires active maintenance (surveillance, purges, censorship).
domain_priors:requires_active_enforcement(authoritarian_power_paradox).

% Mandatory Asymmetry Hooks (Required for any score > 0.3)
constraint_beneficiary(authoritarian_power_paradox, central_bureaucracy).
constraint_beneficiary(authoritarian_power_paradox, short_term_social_order).
constraint_victim(authoritarian_power_paradox, systemic_adaptability).
constraint_victim(authoritarian_power_paradox, individual_outliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL OUTLIER - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - Low-level workers or dissenters.
   WHEN: immediate - Tactical awareness of a specific threat.
   WHERE: trapped - Bound by the domestication gradient of the system.
   SCOPE: local - Specific area of observation.
   
   WHY THIS CLASSIFICATION:
   To the outlier seeing a systemic flaw, the central denial is a Mountain. 
   No signal can move the institution because the institution has defined 
   the outlier's sensor as "noise".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(authoritarian_power_paradox, mountain, 
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))) :-
    domain_priors:suppression_score(authoritarian_power_paradox, S), S > 0.7, !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTIONAL CORE - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Rule-making power.
   WHEN: generational - Maintaining long-term goals or legacies.
   WHERE: mobile - Projecting power across sectors.
   SCOPE: national - Managing the aggregate population.
   
   WHY THIS CLASSIFICATION:
   The center views its centralization as a Rope—the only mechanism capable 
   of weaving a disparate population into a coherent mission. 
   The elimination of dissent is seen as beneficial optimization.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(authoritarian_power_paradox, rope, 
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(national))) :-
    domain_priors:base_extractiveness(authoritarian_power_paradox, E), E < 0.95, !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Snare
   --------------------------------------------------------------------------
   WHO: analytical - Observer of the terminal collapse.
   WHEN: historical - Evaluating the rupture between stability and crash.
   WHERE: analytical - Free from institutional "glow".
   SCOPE: global - Impact of failure on the broader world.
   
   WHY THIS CLASSIFICATION:
   The observer sees the Snare. Centralization extracted the margin of safety 
   and intelligence until the first unanticipated crisis became terminal. 
   The coordination tool (Rope) acted as the instrument of strangulation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(authoritarian_power_paradox, snare, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :-
    domain_priors:base_extractiveness(authoritarian_power_paradox, E), E > 0.6, !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(authoritarian_power_paradox_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates the transition from strategic tool to immutable dogman to trap.
 */
test(brittle_stability_variance) :-
    constraint_indexing:constraint_classification(authoritarian_power_paradox, T1, context(institutional, generational, mobile, national)),
    constraint_indexing:constraint_classification(authoritarian_power_paradox, T2, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(authoritarian_power_paradox, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

/**
 * TEST 2: Power-based extractiveness scaling
 * Powerless agents lose agency to maintain the center's unified narrative.
 */
test(powerless_agency_extraction) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextInstitutional = context(institutional, generational, mobile, national),
    constraint_indexing:extractiveness_for_agent(authoritarian_power_paradox, ContextPowerless, S1),
    constraint_indexing:extractiveness_for_agent(authoritarian_power_paradox, ContextInstitutional, S2),
    S1 > S2.

/**
 * TEST 3: Diagnostic Blindness Delta
 * High suppression leads to the inability to perceive terminal risks.
 */
test(diagnostic_blindness_check) :-
    domain_priors:suppression_score(authoritarian_power_paradox, S),
    domain_priors:base_extractiveness(authoritarian_power_paradox, E),
    S > 0.8, E > 0.7.

:- end_tests(authoritarian_power_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. MANDATROPHY RESOLUTION: The 0.8 extractiveness is justified as the 
 * systemic "harvesting" of local intelligence to fuel a centralized 
 * narrative. 
 * 2. PATTERN SYNTHESIS: Unified themes from bureaucratic blindness in 
 * science and high-stakes engineering where margins are extracted 
 * until failure.
 * 3. PERSPECTIVE SELECTION: Chose to highlight the "Outlier" (Mountain) 
 * vs. "Observer" (Snare) to show how systemic blindness is invisible 
 * from within the Institutional Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    friction_utility_threshold,
    "How much 'friction' (dissent/error) must a system retain to remain diagnostic without losing coordination?",
    resolution_mechanism("Comparative simulation of institutional survival rates against 'dissent-tolerance' parameters"),
    impact("If low friction is ideal: The Rope is successful. If high friction is required: The Rope is a deceptive Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    authoritarian_power_paradox_extraction_intent,
    "Is high extraction (0.8) an intentional predatory choice by the bureaucracy or a functional side-effect of seeking stability?",
    resolution_mechanism("Audit of resource allocation: does the core protect internal efficiency at the expense of external reality?"),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Multi-Polar Collective Leadership
 * Viability: Provides internal checks and multiple diagnostic sensors.
 * Suppression: Actively dismantled by centralization to ensure "unity of action".
 * * ALTERNATIVE 2: Radical Transparency / Open Science
 * Viability: Prevents technical silos and error concealment.
 * Suppression: Suppressed to prevent "social instability" or loss of prestige.
 * * CONCLUSION:
 * Rejection of these alternatives confirms the paradox is a self-imposed 
 * Snare disguised as an institutional Rope.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into system: ?- [authoritarian_power_paradox].
% Generate report: ?- multi_index_report(authoritarian_power_paradox).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
