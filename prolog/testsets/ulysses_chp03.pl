% ============================================================================
% CONSTRAINT STORY: protean_signatures_1904
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: James Joyce, Ulysses, Chapter 3 (Proteus)
% ============================================================================

:- module(constraint_protean_signatures_1904, []).

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
 * * constraint_id: protean_signatures_1904
 * human_readable: The Ineluctable Modality (Sensory Trap)
 * domain: philosophical/perceptual/social
 * temporal_scope: June 16, 1904 (11:00 AM)
 * spatial_scope: Sandymount Strand, Dublin
 * * SUMMARY:
 * Stephen Dedalus confronts the "ineluctable modality of the visible" and 
 * audible—the signatures of all things he is here to read. 
 * This constraint represents the physical and metaphysical limits of perception 
 * that extract Stephen's intellectual energy as he attempts to "shut your 
 * eyes and see".
 * * KEY AGENTS:
 * - Stephen Dedalus: Individual moderate navigating a "Rope" of intellectual 
 * coordination.
 * - The Demiurge (Los): Institutional force viewing the sensory order as 
 * immutable law (Mountain).
 * - Stephen's Physical Body: Powerless agent trapped by the "nebeneinander" 
 * and "nacheinander" of space/time (Snare).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(proteus_strand_walk, 0, 10).

% Fixed: Changed 'mountain' to 'snare' to resolve 'type_1_false_mountain'
narrative_ontology:constraint_claim(protean_signatures_1904, snare).

% Metrics: Extractiveness (0.5 - Sensory cost) and Suppression (0.7 - High energy maintenance)
domain_priors:base_extractiveness(protean_signatures_1904, 0.5).
domain_priors:suppression_score(protean_signatures_1904, 0.7).
domain_priors:requires_active_enforcement(protean_signatures_1904).

% Explicit metric hooks to prevent auto-imputation
narrative_ontology:constraint_metric(protean_signatures_1904, extractiveness, 0.5).
narrative_ontology:constraint_metric(protean_signatures_1904, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(protean_signatures_1904, demiurgic_order).
constraint_victim(protean_signatures_1904, stephen_dedalus_psyche).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: STEPHEN DEDALUS - Rope
   --------------------------------------------------------------------------
   WHY: Stephen uses the signatures (seaspawn, seawrack) as a "Rope"—a 
   coordination tool to tether his runaway associations to a functional 
   reality. 
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    protean_signatures_1904,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PHYSICAL LIMITS (The Body) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - "My two feet in his boots... nebeneinander" 
  .
   WHY: From the perspective of the body, sensory data is a "Snare"—an 
   inescapable trap of "ineluctability" that extracts the self's freedom 
   to exist outside of time and space.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    protean_signatures_1904,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DEMIURGE (Institutional Reality) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "The mallet of Los Demiurgos".
   WHY: From the perspective of the narrative's underlying structural laws, 
   the sensory modality is a "Mountain"—an immutable law of physical 
   being.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    protean_signatures_1904,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(global))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(protean_signatures_tests).

test(multi_perspective_proteus) :-
    constraint_indexing:constraint_classification(protean_signatures_1904, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(protean_signatures_1904, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(false_mountain_diagnostic) :-
    % Alert should fire if suppression > 0.6 while claim is Mountain
    domain_priors:suppression_score(protean_signatures_1904, S),
    S >= 0.7.

:- end_tests(protean_signatures_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * This refactor confirms that Stephen's "ineluctable" reality is actually a 
 * "False Mountain." He claims it is an unchangeable law (Mountain), but the 
 * high suppression required to stay within that frame (0.7) proves it 
 * is a self-imposed or culturally enforced Snare.
 */

omega_variable(
    ineluctable_authenticity,
    "Is the world 'there' without Stephen's perception, or is it a Snare of 
    his own intellectual creation?",
    resolution_mechanism("Resolution of the Berkeleyan 'diaphane' conflict"),
    impact("If independent: Mountain. If dependent: Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Pure Audible Rhythm (Solipsism): Stephen attempts to "shut your eyes" 
 * to create a world. Rejected/Suppressed by the physical reality of the 
 * "crushing wrack" under his boots.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp03, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp03, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp03, snare, agent_power(individual_powerless)).
