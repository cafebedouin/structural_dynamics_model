% ============================================================================
% CONSTRAINT STORY: automatic_enrollment_defaults
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Thaler & Sunstein (2008). Nudge. / Pension Protection Act (2006).
% ============================================================================

:- module(constraint_automatic_enrollment, []).

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
 * * constraint_id: automatic_enrollment_defaults
 * human_readable: Automatic Enrollment (The Power of Defaults)
 * domain: economic/social/political
 * temporal_scope: Biographical (decades-long impact)
 * spatial_scope: National
 * * SUMMARY:
 * Automatic enrollment is a choice architecture where individuals are 
 * placed into a program (like a 401k) by default. It utilizes "Status Quo 
 * Bias" to ensure high participation rates.
 * * KEY AGENTS:
 * - The Employee: The agent whose inertia is being used to build their wealth.
 * - The Employer: The institutional designer setting the default.
 * - The Choice Architect: The strategist who decides whether the default 
 * is "participation" or "non-participation."
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(automatic_enrollment_defaults, 0, 10).
narrative_ontology:constraint_claim(automatic_enrollment_defaults, rope).

% Base extractiveness score (0.05)
% Extremely low; it is designed to retain value for the individual 
% by overcoming their own cognitive friction.
domain_priors:base_extractiveness(automatic_enrollment_defaults, 0.05).

% Suppression score (0.1)
% Low; while the default is "hidden" in the sense of being passive, 
% the opt-out mechanism is usually simple and transparent.
domain_priors:suppression_score(automatic_enrollment_defaults, 0.1).

domain_priors:emerges_naturally(automatic_enrollment_defaults).

% Metrics
narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, 0.05).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, 0.1).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RETIREE - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical (long-term)
   WHERE: mobile
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the worker, automatic enrollment is a "Rope." It coordinates 
   their present behavior with their future needs. It pulls them 
   toward a Global Optimum (wealth in old age) that their present-day 
   inertia would otherwise prevent them from reaching.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    automatic_enrollment_defaults,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(national))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LIBERTARIAN CRITIC - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful (intellectual influence)
   WHEN: immediate
   WHERE: trapped (in a system that assumes their consent)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To a critic of "Paternalism," the default is a "Noose." They see it as 
   an infringement on autonomy—a soft form of coercion that "strangles" 
   active decision-making by assuming consent where none was explicitly given.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    automatic_enrollment_defaults,
    noose,
    context(agent_power(individual_powerful), time_horizon(immediate), exit_options(trapped), spatial_scope(national))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE POLICY MAKER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The observer sees the "Default Effect" as a "Mountain" of behavioral 
   psychology. Humans are hard-wired to prefer the status quo. Policy 
   must be built on this mountain; there is no such thing as a "neutral" 
   default.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    automatic_enrollment_defaults,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :- !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
