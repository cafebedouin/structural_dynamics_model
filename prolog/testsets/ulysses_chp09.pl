% ============================================================================
% CONSTRAINT STORY: stephen_shakespeare_ghost
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 9 (Scylla and Charybdis)
% ============================================================================

:- module(stephen_shakespeare_ghost, []).

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
 * * constraint_id: stephen_shakespeare_ghost
 * human_readable: The Ineluctability of Paternity (The Ghost's Theory)
 * domain: ontological/social
 * temporal_scope: June 16, 1904 (Afternoon)
 * spatial_scope: National Library of Ireland, Dublin
 * * SUMMARY:
 * Stephen Dedalus presents a complex theory of Shakespeare's work that hinges on 
 * the inescapable, "ghostly" nature of paternity. The constraint 
 * is the ontological weight of the father—a state of being that is impalpable 
 * yet legally and spiritually binding[cite: 1].
 * * KEY AGENTS:
 * - Stephen Dedalus: The individual moderate performing a "Rope" of intellectual 
 * argument to manage his own guilt[cite: 1].
 * - The Library/Tradition: The institutional "Mountain" of established 
 * literary and moral law[cite: 1].
 * - Stephen’s Internal Self: The powerless recipient of his own father-guilt (Snare)[cite: 1].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Fixed: Changed 'ontological_paternity' to 'mountain' to satisfy schema [cite: 2, 3]
narrative_ontology:interval(stephen_shakespeare_ghost, 0, 10).
narrative_ontology:constraint_claim(stephen_shakespeare_ghost, mountain).

% Metrics: Extractiveness (0.4) and Suppression (0.6)
domain_priors:base_extractiveness(stephen_shakespeare_ghost, 0.4).
domain_priors:suppression_score(stephen_shakespeare_ghost, 0.6).
domain_priors:requires_active_enforcement(stephen_shakespeare_ghost).

% Explicit metric hooks to prevent auto-imputation [cite: 3]
narrative_ontology:constraint_metric(stephen_shakespeare_ghost, extractiveness, 0.4).
narrative_ontology:constraint_metric(stephen_shakespeare_ghost, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(stephen_shakespeare_ghost, intellectual_status).
constraint_victim(stephen_shakespeare_ghost, stephen_dedalus_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: STEPHEN DEDALUS - Rope
   --------------------------------------------------------------------------
   WHY: Stephen uses his theory as a "Rope"—a functional coordination tool 
   to navigate the library’s social dynamics and process his personal trauma 
   through performance[cite: 1]. 
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    stephen_shakespeare_ghost,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LIBRARY CIRCLE - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - "The quaker librarian purred... True in the larger analysis".
   WHY: For the academic institution, the weight of Shakespeare and the laws of 
   historical tradition are immutable facts of reality—a Mountain[cite: 1].
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    stephen_shakespeare_ghost,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE "GHOST" (Stephen's Guilt) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Stephen as a "servant of two masters"[cite: 1].
   WHY: Internally, the ineluctability of his past and his father is a "Snare" 
   that extracts his peace and traps him in a cycle of "agenbite of inwit"[cite: 1].
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    stephen_shakespeare_ghost,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(stephen_shakespeare_ghost_tests).

test(multi_perspective_conflict) :-
    constraint_indexing:constraint_classification(stephen_shakespeare_ghost, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(stephen_shakespeare_ghost, T2, context(agent_power(institutional), _, _, _)),
    T1 = rope, T2 = mountain.

test(immutability_scaling) :-
    % Historical/Institutional view sees paternity/tradition as a Mountain
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(stephen_shakespeare_ghost_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to fix the 'ontological_paternity' mismatch. By declaring the 
 * claim as 'mountain', the audit can evaluate if Stephen's perceived 
 * 'Natural Law' of the father is actually an extractive trap (Snare) 
 * fueled by his own suppression of belief[cite: 1].
 */

omega_variable(
    stephen_belief_sincerity,
    "Does Stephen actually believe his own theory, or is it a rhetorical Rope?",
    resolution_mechanism("Analysis of his confession: 'No', when asked if he believes his theory [cite: 1]"),
    impact("If insincere: The 'Mountain' is a 'Snare' of self-deception."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Intellectual Sincerity: Rejecting the "peril" of the theory to speak 
 * plainly. Suppressed by Stephen's need for artistic performance[cite: 1].
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp09, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp09, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp09, snare, agent_power(individual_powerless)).
