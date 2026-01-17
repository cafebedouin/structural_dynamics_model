% ============================================================================
% CONSTRAINT STORY: permissive_software_licensing
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: MIT License / Apache License 2.0 / BSD Licenses
% ============================================================================

:- module(constraint_permissive_licensing, []).

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
 * * constraint_id: permissive_software_licensing
 * human_readable: Permissive Licenses (Minimal Friction)
 * domain: technological/legal/economic
 * temporal_scope: Biographical to Historical (1980s - Present)
 * spatial_scope: Global
 * * SUMMARY:
 * Permissive licenses (like MIT or Apache) are the "low-friction" alternative 
 * to Copyleft. They allow users to do almost anything with the code—copy, 
 * modify, and redistribute it—even as part of proprietary, closed-source 
 * software. The only common requirement is to include the original copyright 
 * and license notice.
 * * KEY AGENTS:
 * - The Corporate Developer: Seeks to incorporate open-source code into 
 * secret products without "viral" risk.
 * - The Open Source Contributor: Seeks maximum adoption and utility for 
 * their work, regardless of how it is used.
 * - The Ecosystem Architect: (e.g., Apple with Swift, Google with Go) who 
 * uses permissive licenses to build industry-wide standards.
 * * NARRATIVE ARC:
 * Permissive licenses function as a "Greased Slide." They remove the 
 * "Noose" of reciprocal obligations (Copyleft) to encourage maximum 
 * "Matching Market" thickness. By making the code "free as in beer" for 
 * commercial use, they turn a private asset into a ubiquitous, shared 
 * "Rope" for the entire industry.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(permissive_software_licensing, 0, 10).
narrative_ontology:constraint_claim(permissive_software_licensing, voluntary_coordination).

% Base extractiveness score (0.2)
% Slightly higher than Copyleft because it allows private entities to 
% extract profit from the work without sharing their modifications back.
domain_priors:base_extractiveness(permissive_software_licensing, 0.2).

% Suppression score (0.1)
% Extremely low; the licenses are famously short and transparent.
domain_priors:suppression_score(permissive_software_licensing, 0.1).

% Enforcement: Requires minimal enforcement (Notice requirement only).
domain_priors:requires_active_enforcement(permissive_software_licensing).

% Metrics
narrative_ontology:constraint_metric(permissive_software_licensing, extractiveness, 0.2).
narrative_ontology:constraint_metric(permissive_software_licensing, suppression_requirement, 0.1).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(permissive_software_licensing, commercial_software_vendors).
constraint_beneficiary(permissive_software_licensing, proprietary_platforms).
constraint_victim(permissive_software_licensing, none). % Voluntary gift economy

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STARTUP FOUNDER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For a founder, permissive licenses are a "Rope." They allow the startup to 
   build on top of world-class infrastructure (like React or Flutter) with 
   zero legal friction. It coordinates their growth without the "Noose" 
   fear of being forced to reveal their own secret sauce.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    permissive_software_licensing,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(permissive_software_licensing, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COPYLEFT IDEOLOGUE - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful (intellectual influence)
   WHEN: generational
   WHERE: trapped (watched the 'commons' become privatized)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To an advocate for free software, permissive licenses are a "Noose" for 
   the community. They see it as a mechanism that allows corporations to 
   "strip-mine" the commons—taking the community's work, closing it off, 
   and selling it back to users, effectively strangling long-term freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    permissive_software_licensing,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CLOUD PROVIDER - Mountain
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For a hyper-scaler (like AWS), permissive code is a "Mountain." It is a 
   raw natural resource. They treat it as an immutable feature of the 
   landscape that they can tap into to build their massive services. 
   It is the bedrock of their "infrastructure-as-a-service" model.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    permissive_software_licensing,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(permissive_software_licensing),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(permissive_tests).

test(frictionless_adoption) :-
    % Testing that permissive licenses act as a Rope for adoption.
    constraint_indexing:constraint_classification(permissive_software_licensing, rope, context(_, _, mobile, _)).

test(ideological_conflict) :-
    % Testing the Noose perception from the perspective of long-term freedom advocates.
    constraint_indexing:constraint_classification(permissive_software_licensing, noose, context(individual_powerful, _, _, _)).

:- end_tests(permissive_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Permissive licensing is the "Business-Friendly" Open Source. I set 
 * extractiveness to 0.2 because it allows the "capture" of value into 
 * proprietary products. This is the primary "Alternative Analysis" to 
 * Copyleft; it swaps reciprocal freedom (Rope) for frictionless 
 * utility (Greased Slide).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    re_licensing_trend,
    "Will more 'permissive' projects move to 'Business Source' licenses (BSL) as cloud providers extract more value?",
    resolution_mechanism("Tracking license changes in major databases and infra projects (e.g., Redis, Terraform)"),
    impact("If Yes: The permissive Mountain collapses into a Noose of commercial gatekeeping."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
