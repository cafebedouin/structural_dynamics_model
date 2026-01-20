% ============================================================================
% CONSTRAINT STORY: creative_commons_licensing
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Creative Commons Legal Code / Lessig, L. (2004). Free Culture.
% ============================================================================

:- module(constraint_creative_commons, []).

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
 * * constraint_id: creative_commons_licensing
 * human_readable: Creative Commons (Modular Permissions)
 * domain: social/technological/legal
 * temporal_scope: Biographical to Historical (2001 - Present)
 * spatial_scope: Global
 * * SUMMARY:
 * Creative Commons is a licensing framework that allows creators to move away 
 * from the binary of "Noose" (Full Copyright) or "Mountain" (Public Domain). 
 * By using modular terms (Attribution, Non-Commercial, etc.), creators build 
 * a custom "Rope" that defines exactly how their work can be shared and remixed.
 * * KEY AGENTS:
 * - The Licensor (Creator): Voluntarily reduces their monopoly to gain reach.
 * - The Licensee (User): Gains legal certainty for reuse and remixing.
 * - The Commons Architect: (e.g., Lawrence Lessig) who designed the legal 
 * interoperability of the licenses.
 * * NARRATIVE ARC:
 * CC transforms copyright from a wall into a "User Manual." It solves the 
 * problem of high transaction costs; instead of negotiating every use, the 
 * license acts as a pre-signed permission slip that coordinates global 
 * creative collaboration.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(creative_commons_licensing, 0, 10).
narrative_ontology:constraint_claim(creative_commons_licensing, rope).

% Base extractiveness score (0.1)
% Low extraction; the creator intentionally gives up "extraction rights" 
% (licensing fees) to facilitate broader cultural flow.
domain_priors:base_extractiveness(creative_commons_licensing, 0.1).

% Suppression score (0.1)
% Extremely low; the system is designed for maximum transparency and 
% machine-readability.
domain_priors:suppression_score(creative_commons_licensing, 0.1).

% Enforcement: Requires active enforcement (though usually self-regulating 
% through community norms).
domain_priors:requires_active_enforcement(creative_commons_licensing).

% Metrics
narrative_ontology:constraint_metric(creative_commons_licensing, extractiveness, 0.1).
narrative_ontology:constraint_metric(creative_commons_licensing, suppression_requirement, 0.1).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(creative_commons_licensing, open_educational_resources).
constraint_beneficiary(creative_commons_licensing, remix_culture).
constraint_victim(creative_commons_licensing, none).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE WIKIPEDIAN - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For a contributor to an open project like Wikipedia, CC is the ultimate 
   "Rope." It provides the coordination infrastructure needed for thousands 
   of strangers to build a single "Mountain" of knowledge without a 
   single "Noose" of copyright litigation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    creative_commons_licensing,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(creative_commons_licensing, E),
    E < 0.2,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRADITIONAL PUBLISHER - Noose
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: generational
   WHERE: trapped (business model relies on copyright monopolies)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To a legacy academic publisher, CC is a "Noose." It strangles their 
   ability to charge paywall fees and maintain an "Exclusive Match" 
   with authors. The rise of "Open Access" (driven by CC) is a slow-motion 
   collapse of their extractive power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    creative_commons_licensing,
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
   PERSPECTIVE 3: THE INTERNET ARCHITECT - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The architect sees CC as a "Mountain"—a permanent layer of the internet's 
   legal stack. Much like the HTTP protocol coordinates data, CC 
   coordinates "Meaning." It is a structural feature that makes the modern 
   web possible.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    creative_commons_licensing,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(creative_commons_licensing),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(cc_tests).

test(coordination_efficiency) :-
    % Testing that CC functions as a non-extractive Rope for mobile agents.
    constraint_indexing:constraint_classification(creative_commons_licensing, rope, context(_, _, mobile, _)).

test(legacy_disruption) :-
    % Testing that CC acts as a Noose for institutions reliant on monopolies.
    constraint_indexing:constraint_classification(creative_commons_licensing, noose, context(institutional, _, _, _)).

:- end_tests(cc_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * CC is a "meta-constraint." It uses the rules of Copyright to create a 
 * non-copyright-like environment. I classified it as a Noose for 
 * publishers because it is the primary "Exit Option" that breaks their 
 * extractive business models.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    cc_license_enforcement_vulnerability,
    "Can CC licenses survive a targeted attack by AI companies claiming 'Fair Use' bypasses the license terms?",
    resolution_mechanism("Case law involving 'Share-Alike' clauses in AI training sets"),
    impact("If Yes: The Rope holds. If No: The Mountain collapses back into a Noose of raw data extraction."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
