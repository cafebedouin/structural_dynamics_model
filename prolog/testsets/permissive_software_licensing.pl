% ============================================================================
% CONSTRAINT STORY: permissive_software_licensing
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: MIT License / Apache License 2.0 / BSD Licenses
% ============================================================================

:- module(constraint_permissive_software_licensing, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: permissive_software_licensing
 * human_readable: Permissive Software Licenses (e.g., MIT, Apache)
 * domain: technological/legal/economic
 * temporal_scope: 1980s - Present
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Permissive licenses (like MIT or Apache) allow users to do almost anything with 
 * source code—copy, modify, and redistribute it—even as part of proprietary, 
 * closed-source software. The only common requirement is to include the original 
 * copyright and license notice.
 * 
 * KEY AGENTS:
 * - The Corporate Developer (Institutional): Seeks to incorporate open-source code into proprietary products.
 * - The Copyleft Ideologue (Individual Powerful): Believes software freedom requires sharing modifications.
 * - The Hobbyist Developer (Individual Powerless): Uses free code for personal projects without legal burden.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(permissive_software_licensing, 0, 10).
narrative_ontology:constraint_claim(permissive_software_licensing, rope).
narrative_ontology:human_readable(permissive_software_licensing, "Permissive Software Licenses (e.g., MIT, Apache)").

% Base extractiveness score (0.2)
% Slightly higher than Copyleft because it allows private entities to 
% extract profit from the work without sharing their modifications back.
domain_priors:base_extractiveness(permissive_software_licensing, 0.2).

% Suppression score (0.1)
% Extremely low; the licenses are famously short and transparent.
domain_priors:suppression_score(permissive_software_licensing, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(permissive_software_licensing, extractiveness, 0.2).
narrative_ontology:constraint_metric(permissive_software_licensing, suppression_requirement, 0.1).

% Enforcement: Requires minimal enforcement (Notice requirement only).
domain_priors:requires_active_enforcement(permissive_software_licensing).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(permissive_software_licensing, commercial_software_vendors).
narrative_ontology:constraint_victim(permissive_software_licensing, reciprocal_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HOBBYIST DEVELOPER - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (No legal team, limited resources)
   WHEN: immediate (Needs a library for a personal project)
   WHERE: trapped (Cannot afford paid software, avoids complex licenses)
   
   WHY THIS CLASSIFICATION:
   For a hobbyist, permissive licenses make the software landscape a 'Mountain'
   of free, usable resources. The code is just 'there' to be used, like a rock
   or a tree, without any complex thought about legal obligations.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    permissive_software_licensing,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STARTUP FOUNDER / CORPORATE DEVELOPER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Building a proprietary product)
   WHEN: biographical (Product lifecycle)
   WHERE: mobile (Can choose from thousands of libraries)
   
   WHY THIS CLASSIFICATION:
   For a business, permissive licenses are a 'Rope.' They allow the company to 
   build on world-class infrastructure (React, Go, etc.) with zero legal friction,
   accelerating development without the "viral" risk of copyleft forcing them
   to open-source their own proprietary code.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    permissive_software_licensing,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE COPYLEFT IDEOLOGUE - Snare
   --------------------------------------------------------------------------
   WHO: powerful (e.g., Richard Stallman; intellectual influence)
   WHEN: generational (Concerned with the long-term freedom of the commons)
   WHERE: constrained (Must compete with the ease of permissive licenses)
   
   WHY THIS CLASSIFICATION:
   To a free software advocate, permissive licenses are a 'Snare' for the community.
   They allow corporations to "strip-mine" the commons—taking community work, 
   closing it off in proprietary products, and selling it back, strangling the
   virtuous cycle of shared improvements.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    permissive_software_licensing,
    snare,
    context(
        agent_power(powerful),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(permissive_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(permissive_software_licensing, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(permissive_software_licensing, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(permissive_software_licensing, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(permissive_tests).

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
 * 1. PERSPECTIVE SELECTION: Added the 'Hobbyist Developer' as the 'powerless'
 *    agent. This shows how for a non-commercial actor, the license's permissiveness
 *    makes it feel like a simple, unchangeable feature of the landscape ('Mountain').
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Hobbyist (Mountain): A free, unchanging resource to be used without thought.
 *    - Corporation (Rope): A tool for rapid, low-risk product development.
 *    - Ideologue (Snare): A tool for privatizing the commons.
 *
 * 3. EXTRACTIVENESS (0.2): This reflects the value capture by commercial entities
 *    that is not reciprocated back to the commons, which is the core of the
 *    ideological conflict with copyleft.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether this model of licensing is sustainable as major
 * corporations become the primary beneficiaries of unpaid community labor.
 */

omega_variable(
    re_licensing_trend,
    "Will more permissive projects move to copyleft or 'Business Source' licenses (BSL) as cloud providers extract more value from them?",
    resolution_mechanism("Tracking license changes in major open source databases and infrastructure projects (e.g., Redis, Terraform) over the next 5-10 years."),
    impact("If Yes: The permissive 'Mountain' for hobbyists and 'Rope' for startups collapses into a 'Snare' of commercial gatekeeping."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Copyleft Licenses (e.g., GPL)
 *    Viability: The primary alternative, used by foundational projects like Linux.
 *    Suppression: Not suppressed, but its "viral" nature (requiring derivative works to also be open)
 *    is actively avoided by corporations, which has led to the dominance of permissive licenses
 *    in corporate-backed open source.
 *
 * CONCLUSION:
 * Permissive licensing became the dominant model specifically because it is a better 'Rope'
 * for commercial interests. It prioritizes frictionless adoption over the reciprocal freedom
 * enforced by the copyleft 'Snare' (from a corporate perspective).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/permissive_software_licensing].
 * 2. Multi-perspective: ?- multi_index_report(permissive_software_licensing).
 * 3. Run tests: ?- run_tests(permissive_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
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
% Functional coordination mechanism — primarily substantive
domain_priors:theater_ratio(permissive_software_licensing, 0.1).
narrative_ontology:constraint_metric(permissive_software_licensing, theater_ratio, 0.1).
