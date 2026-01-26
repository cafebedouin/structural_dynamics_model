% ============================================================================
% CONSTRAINT STORY: apartheid_nuclear_program
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini 2.0 Flash
% Source: Renfrew Christie Obituary / Christie (1984) / South Africa Truth and Reconciliation Commission
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(apartheid_nuclear_program, []).

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
 * * constraint_id: apartheid_nuclear_program
 * human_readable: Apartheid South Africa's Clandestine Nuclear Program
 * domain: political/military/technological
 * temporal_scope: 1970-1991 (Era of the "Cheapest Electricity")
 * spatial_scope: National / Continental (Southern Africa)
 * * SUMMARY:
 * A clandestine weapons program aimed at securing the survival of the minority 
 * regime through nuclear deterrence. The program was fueled by an extractive 
 * energy economy—specifically coal mining and power generation subsidized by 
 * forced black labor—under the guise of civil electrification.
 * * KEY AGENTS:
 * - The Apartheid State (Institutional): Seeking civilizational survival via 
 * technological deterrence and energy dominance.
 * - Renfrew Christie (Scholar-Spy): An analytical agent who mapped the 
 * program's energy requirements to reveal its military core.
 * - Forced Laborers/Political Prisoners (Powerless): The subjects who 
 * experienced the extraction of labor and the suppression of the Terrorism Act.
 * * NARRATIVE ARC:
 * The program functioned as a strategic "Rope" for the regime, coordinating 
 * industrial might toward a "Mountain" of nuclear reality. However, for 
 * dissenters and laborers, it was a "Snare" of state violence and labor 
 * exploitation.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(apartheid_nuclear_program, 0, 10).
narrative_ontology:constraint_claim(apartheid_nuclear_program, snare).

% Base extractiveness: 0.85 (High)
% Rationale: The program was funded by "the cheapest electricity in the world," 
% achieved through the extreme exploitation of black labor in coal mines.
domain_priors:base_extractiveness(apartheid_nuclear_program, 0.85).

% Suppression score: 0.90 (Severe)
% Rationale: The state utilized the Terrorism Act, solitary confinement, and 
% the death penalty to hide the program's true nature.
domain_priors:suppression_score(apartheid_nuclear_program, 0.90).

% Enforcement: Required active state violence and intelligence monitoring.
domain_priors:requires_active_enforcement(apartheid_nuclear_program).

% Mandatory Asymmetry Hooks (Required for score > 0.3)
constraint_beneficiary(apartheid_nuclear_program, apartheid_regime).
constraint_beneficiary(apartheid_nuclear_program, cold_war_allies).
constraint_victim(apartheid_nuclear_program, forced_laborers).
constraint_victim(apartheid_nuclear_program, renfrew_christie).
constraint_victim(apartheid_nuclear_program, political_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE APARTHEID REGIME - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making power, state survival focus)
   WHEN: historical (Projected survival of the white minority state)
   WHERE: arbitrage (Moving between international treaties and secret trade)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   From the state's perspective, the program was a coordination mechanism (Rope) 
   to align national industry, energy, and military research for sovereign 
   security. The extraction from labor was viewed as a necessary input.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(apartheid_nuclear_program, rope, 
    context(agent_power(institutional), time_horizon(historical), exit_options(arbitrage), spatial_scope(national))) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E), E > 0.8, !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE POLITICAL PRISONER (CHRISTIE IN SOLITARY) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (In solitary confinement, subject to the state)
   WHEN: biographical (Spent 7 years in prison, facing a potential death row)
   WHERE: trapped (No physical or legal exit)
   SCOPE: local (The prison cell)
   
   WHY THIS CLASSIFICATION:
   To the dissident in solitary confinement, the program is a pure Snare. 
   It offers zero coordination benefit and maximum coercive extraction 
   of freedom and life.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(apartheid_nuclear_program, snare, 
    context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local))) :-
    domain_priors:suppression_score(apartheid_nuclear_program, S), S > 0.8, !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Tangled Rope
   --------------------------------------------------------------------------
   WHO: analytical (External researcher/historian)
   WHEN: historical (Evaluating the system post-1994)
   WHERE: analytical (Unconstrained observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   As a Tangled Rope, the program solved a genuine coordination problem 
   (industrial electrification) but did so through high-asymmetry extraction 
   and systemic suppression. The "core" of the rope (nuclear capability) 
   was inextricably wound around the "snare" of apartheid labor.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E), E > 0.4,
    domain_priors:suppression_score(apartheid_nuclear_program, S), S > 0.5, !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE NUCLEAR PHYSICIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Focusing on the physical constraints of uranium)
   WHEN: civilizational (Physics doesn't change)
   WHERE: analytical
   SCOPE: continental
   
   WHY THIS CLASSIFICATION:
   The physical requirements for uranium enrichment and the energy yields 
   of the Pelindaba facility were unchangeable natural laws. Christie 
   targeted this "Mountain" to prove the program's existence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(apartheid_nuclear_program, mountain, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(continental))).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(apartheid_nuclear_tests).

/**
 * TEST 1: Perspectival Variance
 * Demonstrates that the state sees a tool (Rope) while the victim sees a trap (Snare).
 */
test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypeRegime, context(institutional, historical, arbitrage, national)),
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypePrisoner, context(individual_powerless, biographical, trapped, local)),
    TypeRegime \= TypePrisoner.

/**
 * TEST 2: Power-based Extractiveness Scaling
 * Demonstrates that extraction is felt most acutely by those without exit options.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, biographical, trapped, local),
    ContextInstitutional = context(institutional, historical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(apartheid_nuclear_program, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(apartheid_nuclear_program, ContextInstitutional, Score2),
    Score1 > Score2.

/**
 * TEST 3: Suppression Impact on Immutaiblity
 * Demonstrates how high suppression (0.9) makes the system appear as a Mountain to the uninformed.
 */
test(suppression_visibility_gap) :-
    domain_priors:suppression_score(apartheid_nuclear_program, S),
    S > 0.85.

/**
 * TEST 4: Mandatrophy Verification
 * Confirms the high extraction triggers the mandatory audit requirement.
 */
test(mandatrophy_trigger) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E),
    E >= 0.7.

:- end_tests(apartheid_nuclear_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. MANDATROPHY RESOLUTION: I chose 0.85 for extractiveness because the program 
 * literally ran on "the cheapest electricity" extracted through forced labor.
 * This is a textbook case of Mandatrophy where institutional coordination 
 * is built on predatory extraction.
 * 2. PERSPECTIVES: I separated Christie as a 'Prisoner' from Christie as a 
 * 'Researcher' to show how the same individual shifts from seeing a 'Mountain' 
 * (science) to a 'Snare' (prison).
 * 3. TANGLED ROPE: Included for the analytical view to capture the 1970s dual-use 
 * logic (civilian nuclear power vs. military bombs).
 * * AMBIGUITIES:
 * The extent to which Cold War allies were aware of the forced labor subsidies 
 * remains a medium-confidence area.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    apartheid_extraction_intent,
    "Was the 0.85 extraction a functional necessity for nuclear survival or a predatory byproduct of racial ideology?",
    resolution_mechanism("Audit of state energy expenditures vs. private mining profits during the clandestine phase."),
    impact("If necessity: Mountain (Survival Law). If predatory: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    clandestine_knowledge_scope,
    "How many 'institutional' actors viewed the program as a Snare rather than a Rope?",
    resolution_mechanism("Analysis of internal dissension records in Eskom and the Atomic Energy Board."),
    impact("High dissension would shift the Institutional classification toward Tangled Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Integration / NPT Adherence
 * Viability: Historically available, but ideologically rejected.
 * Suppression: High. Advocacy for international nuclear transparency 
 * was prosecuted as espionage or treason.
 * * ALTERNATIVE 2: Renewable/Diversified Energy (Non-Extractive)
 * Viability: Low during the 1970s given the strategic reliance on domestic coal.
 * * CONCLUSION:
 * The active suppression of Alternative 1 (transparency) confirms the 
 * classification of the program as a Snare for the South African public.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [apartheid_nuclear_program].
 * 2. Multi-perspective: ?- multi_index_report(apartheid_nuclear_program).
 * 3. Run tests: ?- run_tests(apartheid_nuclear_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
