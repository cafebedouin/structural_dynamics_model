% ============================================================================
% CONSTRAINT STORY: copyright_protection
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: U.S. Copyright Act of 1976 / Berne Convention
% ============================================================================

:- module(constraint_copyright_protection, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
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
 * 
 * constraint_id: copyright_protection
 * human_readable: Copyright (Creative Monopoly)
 * domain: social/economic/technological/legal
 * temporal_scope: Civilizational (Life + 70 years)
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Copyright is a legal framework granting creators exclusive rights to their 
 * original works. It separates "Expression" (protected) from "Idea" (free).
 * For creators, it's a foundation for a career; for the public, it's a fence
 * around the cultural commons, creating an artificial scarcity.
 * 
 * KEY AGENTS:
 * - The Casual Internet User (Individual Powerless): Consumes content without understanding legal nuances.
 * - The Independent Artist (Individual Moderate): Seeks to monetize or control their work.
 * - The AI Training Lab (Institutional): Seeks to use vast amounts of data, regardless of copyright.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(copyright_protection, 0, 10).
narrative_ontology:constraint_claim(copyright_protection, tangled_rope).

% Base extractiveness: 0.4.
% While it allows creators to earn, it extracts "freedom of use" from the 
% public, and often large corporations extract the copyright from creators.
domain_priors:base_extractiveness(copyright_protection, 0.4).

% Suppression: 0.5.
% Fair Use provides an "exit," but automated enforcement systems often 
% suppress even legal uses (chilling effect).
domain_priors:suppression_score(copyright_protection, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(copyright_protection, extractiveness, 0.4).
narrative_ontology:constraint_metric(copyright_protection, suppression_requirement, 0.5).

% Enforcement: Requires active enforcement (legal system, Content ID).
domain_priors:requires_active_enforcement(copyright_protection).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(copyright_protection, creators).
narrative_ontology:constraint_victim(copyright_protection, cultural_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CASUAL INTERNET USER - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Consumes content without legal understanding)
   WHEN: immediate (Sees a video, wants to share it)
   WHERE: trapped (Cannot discern fair use from infringement)
   
   WHY THIS CLASSIFICATION:
   For the casual internet user, copyright is an immutable 'Mountain' of legal
   restrictions. They simply consume content as presented, without considering
   fair use or licensing, because the perceived barrier to usage is too high.
   It's a fixed part of the digital landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyright_protection,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INDEPENDENT ARTIST - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Seeks to monetize or control their work)
   WHEN: biographical (Building a career over time)
   WHERE: mobile (Can choose different licensing options or enforcement levels)
   
   WHY THIS CLASSIFICATION:
   For the small creator, copyright is a 'Rope'. It is the only tool they 
   have to prevent their work from being captured and monetized by 
   large entities without compensation. It coordinates their ability to 
   participate in the marketplace and protects their livelihood.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE AI TRAINING LAB (INSTITUTIONAL) - Snare
   --------------------------------------------------------------------------
   WHO: institutional (Seeking to build generative AI systems)
   WHEN: immediate (Needing vast datasets for training)
   WHERE: trapped (Cannot easily "un-train" on copyrighted data)
   
   WHY THIS CLASSIFICATION:
   For an entity attempting to build generative AI, copyright is a 'Snare.' 
   The massive scale of data required makes individual licensing 
   impossible, creating a legal bottleneck that threatens the viability 
   of the entire technological enterprise. It strangles innovation at scale.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyright_protection,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(copyright_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(copyright_protection, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(copyright_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. PERSPECTIVE SELECTION: Added the 'Casual Internet User' as the
 *    'powerless' agent, highlighting their experience of copyright
 *    as an unyielding 'Mountain' of legal complexity.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - User (Mountain): Perceived as an immutable law due to complexity.
 *    - Artist (Rope): A tool for monetization and control.
 *    - AI Lab (Snare): A barrier to innovation due to data scale.
 * 
 * 3. TANGLED ROPE: Copyright is inherently a 'Tangled Rope'. It's a 'Rope' for
 *    creators to coordinate income, but it's 'Tangled' because it extracts
 *    freedom of use from the public and can strangle new forms of innovation
 *    (like AI training) if not carefully balanced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is how copyright law will adapt (or fail to adapt) to new technologies.
 */

omega_variable(
    ai_fair_use_precedent,
    "Will training AI on copyrighted material be legally classified as 'Transformative Fair Use', or will it require new licensing models?",
    resolution_mechanism("Supreme Court rulings on AI training datasets (e.g., NYT v. OpenAI), and the development of new, industry-standard AI licensing frameworks."),
    impact("If Yes (Fair Use): The 'Snare' for AI labs vanishes. If No: Copyright becomes an insurmountable 'Mountain' for AI, requiring fundamental shifts in data acquisition."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Public Domain
 *    Viability: Historically, works eventually enter the public domain.
 *    Suppression: Term extensions (e.g., Sonny Bono Act) have significantly
 *    delayed works entering the public domain, suppressing this alternative for
 *    contemporary works.
 *
 * ALTERNATIVE 2: Creative Commons Licensing
 *    Viability: A flexible framework allowing creators to choose terms.
 *    Suppression: Often perceived as a complex alternative to traditional copyright
 *    by the public, and by institutions as a threat to maximal control.
 *
 * CONCLUSION:
 * Copyright is a dynamic 'Tangled Rope' that balances creator rights with public
 * access. Its constant evolution (e.g., term extensions, digital rights management)
 * is an ongoing attempt to manage the tension between individual ownership and
 * the cultural commons.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/copyright_protection].
 * 2. Multi-perspective: ?- multi_index_report(copyright_protection).
 * 3. Run tests: ?- run_tests(copyright_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */