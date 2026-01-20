% ============================================================================
% CONSTRAINT STORY: copyright_protection
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: U.S. Copyright Act of 1976 / Berne Convention
% ============================================================================

:- module(constraint_copyright, []).

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
 * * constraint_id: copyright_protection
 * human_readable: Copyright (Creative Monopoly)
 * domain: social/economic/technological
 * temporal_scope: Civilizational (Life + 70 years)
 * spatial_scope: Global
 * * SUMMARY:
 * Copyright is a legal framework that grants creators exclusive rights to their 
 * original works of authorship (literature, music, art, software). It separates 
 * the "Expression" (protected) from the "Idea" (free).
 * * KEY AGENTS:
 * - The Creator: Holds the monopoly; seeks to monetize or control the work.
 * - The User/Remixer: Seeks to consume, adapt, or build upon existing work.
 * - The Platform: Acts as the gatekeeper and enforcement engine (e.g., YouTube Content ID).
 * * NARRATIVE ARC:
 * Copyright functions as a "Proprietary Horizon." It creates a temporary (though 
 * increasingly long) artificial scarcity. For creators, it is a foundation for 
 * a career; for the public, it is a fence around the cultural commons.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(copyright_protection, 0, 10).
narrative_ontology:constraint_claim(copyright_protection, rope).

% Base extractiveness score (0.4)
% While it allows creators to earn, it extracts "freedom of use" from the 
% public, and often large corporations extract the copyright from creators.
domain_priors:base_extractiveness(copyright_protection, 0.4).

% Suppression score (0.5)
% Fair Use provides an "exit," but automated enforcement systems often 
% suppress even legal uses (chilling effect).
domain_priors:suppression_score(copyright_protection, 0.5).

% Enforcement requirements
domain_priors:requires_active_enforcement(copyright_protection).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(copyright_protection, extractiveness, 0.4).
narrative_ontology:constraint_metric(copyright_protection, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(copyright_protection, intellectual_property_holders).
constraint_victim(copyright_protection, the_public_domain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDEPENDENT ARTIST - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the small creator, copyright is a "Rope." It is the only tool they 
   have to prevent their work from being captured and monetized by 
   large entities without compensation. It coordinates their ability to 
   participate in the marketplace.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyright_protection,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(copyright_protection, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AI TRAINING LAB - Noose
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: immediate
   WHERE: trapped (cannot easily "un-train" on copyrighted data)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For an entity attempting to build generative AI, copyright is a "Noose." 
   The massive scale of data required makes individual licensing 
   impossible, creating a legal bottleneck that threatens the viability 
   of the entire technological enterprise.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyright_protection,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(copyright_protection, S),
    S > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LEGAL SCHOLAR - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The scholar sees copyright as a "Mountain"—a fixed feature of the 
   global legal geography since the 18th century. While the "peaks" 
   (duration) may change, the fundamental concept of protecting 
   expression is an immutable part of modern civilization.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyright_protection,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    true.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(copyright_tests).

test(proportional_utility) :-
    % Testing that creators see the Rope while institutions (labs) see the Noose.
    constraint_indexing:constraint_classification(copyright_protection, rope, context(individual_moderate, _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, noose, context(institutional, _, trapped, _)).

test(historical_permanence) :-
    % Testing that analytical views recognize the structural Mountain.
    constraint_indexing:constraint_classification(copyright_protection, mountain, context(analytical, _, _, _)).

:- end_tests(copyright_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Copyright is unique because it is a "Monopoly on Expression." I set 
 * suppression to 0.5 because "Fair Use" acts as a pressure valve, but 
 * algorithmic enforcement (Content ID) often makes that valve stick.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    ai_fair_use_precedent,
    "Will training AI on copyrighted material be legally classified as 'Transformative Fair Use'?",
    resolution_mechanism("Supreme Court rulings on AI training datasets (e.g., NYT v. OpenAI)"),
    impact("If Yes: The Noose for AI labs vanishes. If No: Copyright becomes an insurmountable Mountain for AI."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
