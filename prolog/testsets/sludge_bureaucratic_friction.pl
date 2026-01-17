% ============================================================================
% CONSTRAINT STORY: sludge_bureaucratic_friction
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Sunstein, C. R. (2021). Sludge. / Thaler, R. H. (2018).
% ============================================================================

:- module(constraint_sludge, []).

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
 * * constraint_id: sludge_bureaucratic_friction
 * human_readable: Sludge (Intentional Friction)
 * domain: political/economic/social
 * temporal_scope: Immediate to Biographical
 * spatial_scope: National
 * * SUMMARY:
 * Sludge consists of administrative burdens—excessive paperwork, long wait 
 * times, and complex requirements—that act as a barrier to entry or exit. 
 * It is often used by institutions to "throttle" access to rights or resources.
 * * KEY AGENTS:
 * - The Applicant: The agent attempting to navigate the friction to access a benefit.
 * - The Bureaucrat: The administrator who maintains or enforces the sludge layer.
 * - The Policy Designer: The agent who uses sludge as a "hidden" budget control.
 * * NARRATIVE ARC:
 * Sludge functions as a "Viscous Noose." It does not provide a hard "No," 
 * but it makes the "Yes" so expensive in terms of time and cognitive effort 
 * that agents give up. It is the primary tool for creating a "thin" 
 * matching market in social services.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(sludge_bureaucratic_friction, 0, 10).
narrative_ontology:constraint_claim(sludge_bureaucratic_friction, administrative_burden).

% Base extractiveness score (0.7)
% High; it extracts time and psychological well-being from the most 
% vulnerable populations to save institutional costs.
domain_priors:base_extractiveness(sludge_bureaucratic_friction, 0.7).

% Suppression score (0.6)
% Moderate-High; while the rules are public, the "hidden costs" and 
% complexity suppress the viability of the benefit.
domain_priors:suppression_score(sludge_bureaucratic_friction, 0.6).

% Enforcement: Requires active enforcement (Institutional inertia/design).
domain_priors:requires_active_enforcement(sludge_bureaucratic_friction).

% Metrics
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, extractiveness, 0.7).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(sludge_bureaucratic_friction, budget_conscious_governments).
constraint_beneficiary(sludge_bureaucratic_friction, predatory_subscription_models).
constraint_victim(sludge_bureaucratic_friction, low_income_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ELIGIBLE APPLICANT - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate (crisis window)
   WHERE: trapped (no alternative source for the benefit)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For someone in need of emergency aid, sludge is a "Noose." 
   The 50-page form and the requirement to mail physical documents 
   strangles their access to survival. Every hour spent in the "waiting 
   room" is an extraction of their limited life-resources.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sludge_bureaucratic_friction,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMPLIANCE OFFICER - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: biographical
   WHERE: mobile
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To the institution, sludge is a "Rope." It is a coordination tool 
   used to "vet" applicants and prevent fraud. They see the friction 
   as a necessary filter to ensure that only the "truly deserving" 
   (those persistent enough to climb the rope) get through.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sludge_bureaucratic_friction,
    rope,
    context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(national))
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SOCIOLOGIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The observer sees sludge as a "Mountain" of institutional entropy. 
   Complexity naturally grows in any large system unless it is 
   actively fought. It is a structural feature of human organization 
   that dictates the "flow rate" of social justice.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sludge_bureaucratic_friction,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :-
    domain_priors:requires_active_enforcement(sludge_bureaucratic_friction),
    !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
