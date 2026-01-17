% ============================================================================
% CONSTRAINT STORY: public_domain_commons
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Copyright Law / Creative Commons / Public Domain Status
% ============================================================================

:- module(constraint_public_domain, []).

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
 * * constraint_id: public_domain_commons
 * human_readable: The Public Domain (Cultural Commons)
 * domain: social/economic/technological
 * temporal_scope: Civilizational (Permanent / Post-Copyright)
 * spatial_scope: Global
 * * SUMMARY:
 * The Public Domain consists of all creative work to which no exclusive 
 * intellectual property rights apply. These works are free for anyone to use, 
 * remix, and monetize without permission. It is the "default" state of 
 * information before or after the artificial constraint of Copyright.
 * * KEY AGENTS:
 * - The Archivist: Preserves and digitizes the commons for future generations.
 * - The Remix Artist: Uses the commons as raw material for new "Ropes."
 * - The Corporate Lobbyist: Attempts to shrink the commons via term extensions.
 * * NARRATIVE ARC:
 * The Public Domain is the "Basement of Culture." It is a Mountain of 
 * free resources that provides the necessary "Exit Option" for those 
 * trapped by the Copyright Noose. However, for 20 years (1998-2018), this 
 * Mountain was frozen by the Sonny Bono Copyright Term Extension Act.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(public_domain_commons, 0, 10).
narrative_ontology:constraint_claim(public_domain_commons, fundamental_commons).

% Base extractiveness score (0.0 = no extraction)
% By definition, the public domain is non-extractive.
domain_priors:base_extractiveness(public_domain_commons, 0.0).

% Suppression score (0.3)
% While the works are free, they are often suppressed by "copyfraud" 
% (false copyright claims by legacy estates).
domain_priors:suppression_score(public_domain_commons, 0.3).

% Enforcement requirements
domain_priors:emerges_naturally(public_domain_commons).

% Metrics
narrative_ontology:constraint_metric(public_domain_commons, extractiveness, 0.0).
narrative_ontology:constraint_metric(public_domain_commons, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(public_domain_commons, all_humanity).
constraint_victim(public_domain_commons, none). % Non-extractive

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIE DEVELOPER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For a small creator, the Public Domain is a "Rope." They can use the 
   characters of Shakespeare or the music of Beethoven as a coordination 
   tool to build their own products without the "Noose" of licensing fees.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    public_domain_commons,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(public_domain_commons, 0.0),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DISNEY LAWYER - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional
   WHEN: generational
   WHERE: trapped (business model relies on exclusivity)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From the perspective of a legacy media giant, the Public Domain is a 
   "Noose" for their profit margins. It represents the "death" of their 
   monopoly. As works like Steamboat Willie enter the commons, the 
   exclusive control that protected their revenue is "strangled."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    public_domain_commons,
    noose,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CIVILIZATION - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical
   WHEN: civilizational
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the historian, the Public Domain is the "Mountain." It is the 
   permanent bedrock of human culture. While laws may temporarily 
   fence it off, the gravitational pull of information toward the 
   commons is an immutable feature of time.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    public_domain_commons,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(public_domain_commons),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(public_domain_tests).

test(non_extractive_nature) :-
    domain_priors:base_extractiveness(public_domain_commons, E),
    E == 0.0.

test(term_extension_noose) :-
    % Testing that institutions see the commons as a Noose to their monopoly.
    constraint_indexing:constraint_classification(public_domain_commons, noose, context(institutional, _, _, _)).

:- end_tests(public_domain_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * The Public Domain is the only constraint story with an 
 * extractiveness score of 0.0. It is a "Zero-G" environment for 
 * matching markets. I classified it as a Noose for institutions to 
 * highlight the corporate motivation behind "Copyright Term Extensions."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    sovereign_ai_datasets,
    "Will AI trained exclusively on Public Domain data outperform those on copyrighted data?",
    resolution_mechanism("Benchmarking open-weights models against closed-copyright models"),
    impact("If Yes: The 'Mountain' of the commons becomes the primary 'Rope' for tech development."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
