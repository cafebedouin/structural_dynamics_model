% ============================================================================
% CONSTRAINT STORY: rope_design
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Thaler, R. H., & Sunstein, C. R. (2008). Nudge.
% ============================================================================

:- module(constraint_rope, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rope_design
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

narrative_ontology:interval(rope_design, 0, 10).
narrative_ontology:constraint_claim(rope_design, rope).

% Base extractiveness score (0.2)
% Low; when used ethically, it provides value by reducing cognitive load. 
% When used as a "Dark Pattern," it becomes extractive (0.8+).
domain_priors:base_extractiveness(rope_design, 0.2).

% Suppression score (0.3)
% Alternatives are rarely "hidden," but they are often "deprioritized" 
% through framing or physical distance.
domain_priors:suppression_score(rope_design, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rope_design, extractiveness, 0.2).
narrative_ontology:constraint_metric(rope_design, suppression_requirement, 0.3).

domain_priors:emerges_naturally(rope_design).

% Metrics
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
    rope_design,
    rope,
    context(agent_power(individual_moderate), time_horizon(immediate), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MANIPULATED CONSUMER - Snare
   --------------------------------------------------------------------------
   WHO: powerless
   WHEN: immediate
   WHERE: trapped (in a deceptive environment)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   When choice architecture is used to hide costs or shame "No" answers 
   (Confirmshaming), it is a "Snare." The environment strangles the 
   chooser's autonomy, forcing a "Match" that benefits the architect 
   at the expense of the chooser.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rope_design,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(global))
) :-
    domain_priors:base_extractiveness(rope_design, E),
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
    rope_design,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :- !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(choice_architecture_design, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(choice_architecture_design, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(choice_architecture_design, snare, agent_power(powerless)).

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
% Coordination mechanism in psychological domain — moderate institutional framing
domain_priors:theater_ratio(rope_design, 0.14).
narrative_ontology:constraint_metric(rope_design, theater_ratio, 0.14).

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (choice_architecture_design)
% ============================================================================
narrative_ontology:constraint_beneficiary(rope_design, choosers_with_good_defaults).
narrative_ontology:constraint_victim(rope_design, manipulated_consumers).

omega_variable(
    omega_dark_pattern_drift,
    "Will the proliferation of 'Dark Patterns' cause regulatory frameworks to reclassify choice architecture from Rope to Snare?",
    "Longitudinal analysis of EU Digital Services Act enforcement and FTC dark pattern litigation outcomes.",
    "If regulated: Remains a Rope with guardrails. If unchecked: Drifts toward a systemic Snare.",
    confidence_without_resolution(medium)
).
