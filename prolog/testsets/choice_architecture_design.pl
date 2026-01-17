% ============================================================================
% CONSTRAINT STORY: choice_architecture_design
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Thaler, R. H., & Sunstein, C. R. (2008). Nudge.
% ============================================================================

:- module(constraint_choice_architecture, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: choice_architecture_design
 * human_readable: Choice Architecture (The Silent Director)
 * domain: psychological/economic/social
 * temporal_scope: Immediate to Biographical
 * spatial_scope: Global
 * * SUMMARY:
 * Choice architecture is the organizing of the context in which people make 
 * decisions. It includes the use of defaults, framing, and the physical 
 * arrangement of options to "nudge" behavior in a predictable way.
 * * KEY AGENTS:
 * - The Choice Architect: The designer (e.g., UI designer, policymaker) who 
 * structures the environment.
 * - The Chooser: The individual making decisions within that environment.
 * - The Goal: The intended outcome (e.g., health, savings, or profit).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(choice_architecture_design, 0, 10).
narrative_ontology:constraint_claim(choice_architecture_design, behavioral_nudge).

% Base extractiveness score (0.2)
% Low; when used ethically, it provides value by reducing cognitive load. 
% When used as a "Dark Pattern," it becomes extractive (0.8+).
domain_priors:base_extractiveness(choice_architecture_design, 0.2).

% Suppression score (0.3)
% Alternatives are rarely "hidden," but they are often "deprioritized" 
% through framing or physical distance.
domain_priors:suppression_score(choice_architecture_design, 0.3).

domain_priors:emerges_naturally(choice_architecture_design).

% Metrics
narrative_ontology:constraint_metric(choice_architecture_design, extractiveness, 0.2).
narrative_ontology:constraint_metric(choice_architecture_design, suppression_requirement, 0.3).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE OVERWHELMED USER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (high cognitive load)
   WHEN: immediate
   WHERE: mobile
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a person facing "Choice Overload," good architecture is a "Rope." 
   By highlighting "Recommended Options" or using sensible defaults, 
   the architect provides the coordination needed to reach a decision 
   without mental exhaustion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    choice_architecture_design,
    rope,
    context(agent_power(individual_moderate), time_horizon(immediate), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MANIPULATED CONSUMER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped (in a deceptive environment)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   When choice architecture is used to hide costs or shame "No" answers 
   (Confirmshaming), it is a "Noose." The environment strangles the 
   chooser's autonomy, forcing a "Match" that benefits the architect 
   at the expense of the chooser.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    choice_architecture_design,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(global))
) :-
    domain_priors:base_extractiveness(choice_architecture_design, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE COGNITIVE SCIENTIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, choice architecture is a "Mountain" of evolutionary 
   psychology. Humans are not "Econs" (perfectly rational); they are 
   "Humans" with predictable biases. You cannot build a world without 
   architecture; you can only decide what kind of architect you will be.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    choice_architecture_design,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :- !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
