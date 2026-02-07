% ============================================================================
% CONSTRAINT STORY: overton_window
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Joseph P. Overton / Mackinac Center for Public Policy
% ============================================================================

:- module(constraint_overton_window, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: overton_window
 * human_readable: The Overton Window (Window of Discourse)
 * domain: political/social
 * temporal_scope: Modern Era (Post-1990s formalization)
 * spatial_scope: National/Global
 * * SUMMARY:
 * The Overton Window represents the range of policies or ideas acceptable to the 
 * mainstream population at a given time. It constrains politicians to a narrow 
 * "window" of viable options; moving outside this window is perceived as 
 * political suicide. The window shifts not through political mandate, but 
 * through social and cultural pressure.
 * * KEY AGENTS:
 * - The Politician: Bound by the window to remain "electable."
 * - The Think Tank/Activist: Working to shift the window by promoting "unthinkable" ideas.
 * - The Median Voter: The unconscious anchor of the current window position.
 * * NARRATIVE ARC:
 * Initially, the window acts as a "Mountain" of social consensus. Activists use 
 * "Rope" strategies (pushing extreme positions to make moderate ones look viable). 
 * For those whose lives depend on "unthinkable" policies, the window functions 
 * as a "Snare," strangling the possibility of relief through the democratic process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(overton_interval, 0, 10).
narrative_ontology:constraint_claim(overton_window, mountain).

% Base extractiveness: 0.4 (Moderate)
% Rationale: It extracts political "agency" from the population. By narrowing 
% what is "thinkable," it funnels social energy into a limited set of 
% institutionalized outcomes that often benefit the status quo.
domain_priors:base_extractiveness(overton_window, 0.4).

% Suppression score: 0.8 (High)
% Rationale: Ideas outside the window are not just ignored; they are 
% actively punished through social shaming, "cancelation," or labels like 
% "extremist," making alternatives effectively invisible to the mainstream.
domain_priors:suppression_score(overton_window, 0.8).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(overton_window, extractiveness, 0.4).
narrative_ontology:constraint_metric(overton_window, suppression_requirement, 0.8).

% Enforcement: Emerges naturally from social mimetics but is actively 
% manipulated by media and think tanks.
domain_priors:requires_active_enforcement(overton_window).

% Metrics for Section 1 of the Executive Summary
% Beneficiaries: Institutional incumbents and "centrist" political parties.
constraint_beneficiary(overton_window, political_incumbents).

% Victims: Radical reformers and marginalized groups whose needs are "unthinkable."
constraint_victim(overton_window, radical_reformers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CAREER POLITICIAN - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: institutional (An agent whose power depends on the window).
   WHEN: immediate (The current election cycle).
   WHERE: trapped (Cannot move outside the window without losing office).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the politician, the window is a Mountain. It is the fixed landscape 
   they must navigate. They do not see themselves as having the power to 
   change it; they can only stand on the "ground" that the public provides. 
   To them, the window is a law of political physics.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    overton_window,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE IDEOLOGICAL STRATEGIST - ROPE
   --------------------------------------------------------------------------
   
   WHO: powerful (A think-tank director or media mogul).
   WHEN: generational (Willing to wait decades to shift the center).
   WHERE: mobile (Can fund multiple narratives or "flank" the window).
   SCOPE: global/national.
   
   WHY THIS CLASSIFICATION:
   For the strategist, the window is a Rope. It is a coordination mechanism 
   they can pull. By strategically introducing "Radical" or "Unthinkable" 
   ideas into the discourse, they pull the window in their direction, 
   eventually making their preferred policies "Sensible."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    overton_window,
    rope,
    context(
        agent_power(powerful),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE URGENT REFORMER - SNARE
   --------------------------------------------------------------------------
   
   WHO: powerless (An activist for a non-mainstream cause).
   WHEN: immediate (The crisis is happening now).
   WHERE: constrained (Must use the existing political system).
   SCOPE: local/national.
   
   WHY THIS CLASSIFICATION:
   For the reformer whose survival depends on a policy currently outside 
   the window, the constraint is a Snare. They see the "sensible" center 
   ignoring a catastrophe because the solution is "politically impossible." 
   The window's narrowness strangles their ability to even speak the 
   necessary truth in a way that is heard.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    overton_window,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(overton_window_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(overton_window, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(overton_window, Type2, context(agent_power(powerful), _, _, _)),
    constraint_indexing:constraint_classification(overton_window, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(high_suppression_metric) :-
    % Overton window relies on suppressing "extremist" alternatives.
    domain_priors:suppression_score(overton_window, S),
    S >= 0.7.

test(time_horizon_shift) :-
    % Generational views see it as a tool (Rope), Immediate views as a wall (Mountain).
    constraint_indexing:constraint_classification(overton_window, rope, context(_, generational, _, _)).

:- end_tests(overton_window_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION (0.8): I set this very high because the Overton Window 
 * defines itself by what it excludes. Its primary function is the 
 * "shaming" of ideas outside its bounds.
 * 2. SNARE CLASSIFICATION: Essential for capturing the frustration of 
 * marginalized groups. If the "Sensible" window excludes your right to 
 * exist or eat, it is an active mechanism of harm.
 * 3. ROPE CLASSIFICATION: Reflects the reality that the window is 
 * plastic—it can be moved if one has enough time and media leverage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    discursive_fracture_impact,
    "In a hyper-polarized digital era, does a single 'Overton Window' still exist, 
    or has it fractured into multiple, incompatible 'micro-windows'?",
    resolution_mechanism("Network analysis of cross-partisan idea sharing and sentiment echoes"),
    impact("If fractured: The 'Mountain' of social consensus collapses into tribal Ropes."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Radical Pluralism (All ideas debated on merit)
 * Viability: Low (Human cognitive limits prefer heuristics/mainstreams).
 * Suppression: High (Mainstream media frames this as "platforming hate").
 * * ALTERNATIVE 2: Direct Democracy (Bypassing the window via referenda)
 * Viability: Moderate.
 * * CONCLUSION:
 * The window is a Snare because it forces complex social problems into 
 * a binary or narrow "sensible" range, suppressing radical but 
 * effective alternatives.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_overton_window].
 * 2. Report: ?- multi_index_report(overton_window).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
