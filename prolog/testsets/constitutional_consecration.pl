% ============================================================================
% CONSTRAINT STORY: constitutional_consecration
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Gettysburg Address (Abraham Lincoln, 1863)
% ============================================================================

:- module(constraint_constitutional_consecration, []).

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
 * * constraint_id: constitutional_consecration
 * human_readable: The Proposition of Equality
 * domain: political/legal/existential
 * temporal_scope: 1863 (American Civil War)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * Lincoln reinterprets the American constraint system not as a mere legal 
 * compact, but as a "proposition" that all men are created equal. This 
 * constraint acts as the "unfinished work" that binds the living to the 
 * sacrifice of the dead, creating a "new birth of freedom" that 
 * necessitates the survival of the democratic "Rope."
 * * KEY AGENTS:
 * - Abraham Lincoln: The institutional architect seeking to preserve the Union.
 * - The Honored Dead: The ultimate victims/beneficiaries of the constraint.
 * - "The People": The collective agent tasked with ensuring the government 
 * does not "perish from the earth."
 * * NARRATIVE ARC:
 * The speech moves from the past ("Four score and seven years ago") to 
 * a present state of "testing" via civil war. The constraint is identified 
 * as the "proposition" which is currently under existential threat. 
 * The resolution is a commitment to "highly resolve" that the sacrifice 
 * of the dead creates a permanent obligation for the living.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(gettysburg_address_analysis, 0, 10).
narrative_ontology:constraint_claim(constitutional_consecration, rope).

% Base extractiveness score: 0.6
% Rationale: The constraint extracts "the last full measure of devotion" 
% (life itself) from its subjects to ensure the survival of the system.
domain_priors:base_extractiveness(constitutional_consecration, 0.6).

% Suppression score: 0.9
% Rationale: The war itself is the suppression mechanism for the 
% "alternative" (secession/inequality). The speech argues that the 
% world "can never forget" what happened here, effectively locking 
% the narrative.
domain_priors:suppression_score(constitutional_consecration, 0.9).

% Enforcement requirements: Active enforcement through "war" and "devotion."
domain_priors:requires_active_enforcement(constitutional_consecration).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(constitutional_consecration, extractiveness, 0.6).
narrative_ontology:constraint_metric(constitutional_consecration, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(constitutional_consecration, [human_liberty, the_union]).
constraint_victim(constitutional_consecration, [soldier_dead]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SOLDIER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The soldier caught in the "great battlefield."
   WHEN: immediate - The moment of sacrifice/death.
   WHERE: trapped - Bound to the earth by their own blood ("hallow this ground").
   SCOPE: local - A specific cemetery plot.
   
   WHY THIS CLASSIFICATION:
   For the soldier who died at Gettysburg, the "proposition" of equality 
   became a "Mountain." It was an unchangeable reality that dictated their 
   end with zero degrees of freedom. They can no longer "add or detract" 
   from it.
   
   NARRATIVE EVIDENCE:
   "The brave men, living and dead... have consecrated it, far above 
   our poor power to add or detract."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_consecration,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(constitutional_consecration, the_union),
        constraint_victim(constitutional_consecration, soldier_dead),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(constitutional_consecration, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRESIDENT (Lincoln) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - The leader articulating the "new birth of freedom."
   WHEN: historical - Looking back to 1776 and forward to the future.
   WHERE: arbitrage - Managing the "testing" of the nation's endurance.
   SCOPE: national - "Any nation so conceived and so dedicated."
   
   WHY THIS CLASSIFICATION:
   For Lincoln, the Union is a "Rope"—a functional, though strained, 
   mechanism for coordinating a free people. It is a "work" that must 
   be "dedicated" and "carried on" through deliberate effort.
   
   NARRATIVE EVIDENCE:
   "It is for us the living, rather, to be dedicated here to the 
   unfinished work... to the great task remaining before us."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_consecration,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(constitutional_consecration, human_liberty),
        constraint_victim(constitutional_consecration, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANTI-UNIONIST - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - Leaders of the rebellion (implied).
   WHEN: immediate - The "testing" of war.
   WHERE: constrained - The military necessity of the Union's "endurance."
   SCOPE: regional - The divided states.
   
   WHY THIS CLASSIFICATION:
   For those who sought to leave the Union, Lincoln's "proposition" is a 
   "Noose." It is a coercive claim that their region cannot exist 
   outside the national "dedication," enforced by "great battlefield" 
   violence.
   
   NARRATIVE EVIDENCE:
   "Now we are engaged in a great civil war, testing whether that nation, 
   or any nation so conceived and so dedicated, can long endure."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_consecration,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(constitutional_consecration, the_union),
        constraint_victim(constitutional_consecration, secessionists),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(constitutional_consecration, S),
    S > 0.8,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(constitutional_consecration_tests).

test(sacrifice_index) :-
    % Verify that for the dead, the constraint is classified as a Mountain
    constraint_indexing:constraint_classification(constitutional_consecration, mountain, context(agent_power(individual_powerless), _, trapped, _, _, _)).

test(functional_survival) :-
    % Verify that for the institution, the goal is the survival of the Rope
    constraint_indexing:constraint_classification(constitutional_consecration, rope, context(agent_power(institutional), _, _, _, _, _)).

test(extraction_limit) :-
    % The "last full measure" represents the maximum extraction value (0.6+)
    domain_priors:base_extractiveness(constitutional_consecration, 0.6).

:- end_tests(constitutional_consecration_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE CONTRADICTION OF CONSECRATION: Lincoln argues the living cannot 
 * hallow the ground (Rope-level agency), but the dead already did 
 * (Mountain-level fact). This demonstrates how human sacrifice turns a 
 * political "Rope" into a moral "Mountain."
 * 2. EXTRACTIVENESS: While the tone is redemptive, the extraction (death) 
 * is total. However, because it is "conceived in liberty," the score 
 * remains in the 0.6 range—high, but ostensibly for the beneficiary's own 
 * ultimate freedom.
 * 3. PERSPECTIVES: I included the implied Secessionist as a "Noose" 
 * perspective because the "testing" of the Union's "endurance" represents 
 * a coercive restriction on their exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equality_definition,
    "Does 'all men' include the four million enslaved people, or is the 'proposition' limited to the legal status of states?",
    resolution_mechanism("Requires the Reconstruction Amendments to transition the proposition from a 'Rope' to a 'Mountain' of law."),
    impact("If the proposition is narrow, the Noose of the Civil War only benefits white citizens."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Secession / Disintegration: The idea that the Union is a "contract" 
 * that can be rescinded at will (Rope with high exit options).
 * * VIABILITY: Physically viable, as evidenced by the ongoing war.
 * * SUPPRESSION: Actively suppressed by military force and the 
 * narrative of "consecration" provided in the address.
 * * CONCLUSION: 
 * Lincoln's address is a "suppression event" that seeks to convert a 
 * fraying Rope back into a permanent Mountain.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
