% ============================================================================
% CONSTRAINT STORY: creative_commons_licensing
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Creative Commons Legal Code / Lessig, L. (2004). Free Culture.
% ============================================================================

:- module(constraint_creative_commons_licensing, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: creative_commons_licensing
 * human_readable: Creative Commons Licensing Framework
 * domain: legal/social/technological
 * temporal_scope: 2001 - Present
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Creative Commons (CC) is a licensing framework that allows creators to move away 
 * from the binary of "Full Copyright" (a Snare) or "Public Domain" (a Mountain). 
 * By using modular terms (Attribution, Non-Commercial, etc.), creators build 
 * a custom "Rope" that defines exactly how their work can be shared and remixed.
 * 
 * KEY AGENTS:
 * - The Student (Individual Powerless): Uses a CC image for a school project.
 * - The Wikipedian (Individual Moderate): Uses CC licenses to build a global commons.
 * - The Traditional Publisher (Institutional): Views CC as a threat to their business model.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(creative_commons_licensing, 0, 10).
narrative_ontology:constraint_claim(creative_commons_licensing, rope).

% Base extractiveness score (0.1)
% Low; the creator intentionally gives up "extraction rights" (licensing fees)
% to facilitate broader cultural flow.
domain_priors:base_extractiveness(creative_commons_licensing, 0.1).

% Suppression score (0.1)
% Extremely low; the system is designed for maximum transparency and machine-readability.
domain_priors:suppression_score(creative_commons_licensing, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(creative_commons_licensing, extractiveness, 0.1).
narrative_ontology:constraint_metric(creative_commons_licensing, suppression_requirement, 0.1).

% Enforcement: Requires active enforcement (though often self-regulating).
domain_priors:requires_active_enforcement(creative_commons_licensing).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(creative_commons_licensing, remix_culture).
constraint_victim(creative_commons_licensing, legacy_publishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STUDENT - Rope
   --------------------------------------------------------------------------
   WHO: powerless (Needs an image for a presentation)
   WHEN: immediate (Project deadline)
   WHERE: trapped (Cannot afford to license a stock photo)
   
   WHY THIS CLASSIFICATION:
   For a student, finding a CC-licensed image is a simple 'Rope'. It allows them
   to complete their school project without worrying about copyright law or
   expensive licensing fees. It's a simple, helpful tool.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    creative_commons_licensing,
    rope,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE WIKIPEDIAN / COLLABORATOR - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Building a shared resource)
   WHEN: biographical (Contributing over a lifetime)
   WHERE: mobile (Can choose which works to contribute or use)
   
   WHY THIS CLASSIFICATION:
   For a contributor to an open project like Wikipedia, CC is the ultimate 
   'Rope.' It provides the coordination infrastructure needed for thousands of 
   strangers to build a single 'Mountain' of knowledge without a single 
   'Snare' of copyright litigation.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TRADITIONAL PUBLISHER - Snare
   --------------------------------------------------------------------------
   WHO: institutional (Business model relies on copyright monopolies)
   WHEN: generational (The slow decline of a business model)
   WHERE: constrained (Cannot compete with "free")
   
   WHY THIS CLASSIFICATION:
   To a legacy academic publisher, CC is a 'Snare.' It strangles their 
   ability to charge paywall fees and maintain an "Exclusive Match" 
   with authors. The rise of "Open Access" (driven by CC) is a slow-motion 
   collapse of their extractive power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    creative_commons_licensing,
    snare,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(cc_tests).

test(multi_perspective_variance) :-
    % Note: both powerless and moderate see a Rope, but for different reasons.
    % The test only requires 2+ distinct types.
    constraint_indexing:constraint_classification(creative_commons_licensing, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creative_commons_licensing, Type2, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2.

:- end_tests(cc_tests).

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
 * 1. PERSPECTIVE SELECTION: Added the 'Student' as the 'powerless'
 *    agent. Their use case is simpler than the Wikipedian's; it's about
 *    immediate, frictionless access, which is a core value proposition of CC.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Student/Wikipedian (Rope): A tool for creation and collaboration.
 *    - Publisher (Snare): A threat to an exclusive business model.
 * 
 * 3. CORE INSIGHT: Creative Commons is a "meta-constraint." It uses the rules
 *    of Copyright (a potential 'Snare') to build a collaborative commons (a 'Rope').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the legal framework can withstand pressure
 * from new technologies that challenge the definition of "use."
 */
omega_variable(
    cc_license_enforcement_vulnerability,
    "Can CC licenses survive a targeted attack by AI companies claiming that training on data constitutes 'Fair Use' and bypasses the license terms (e.g., Share-Alike or Non-Commercial)?",
    resolution_mechanism("Establishment of new case law specifically addressing the use of licensed material in large-scale AI training sets."),
    impact("If Fair Use wins: The CC 'Rope' is broken for AI, and the licenses become a weaker 'Mountain' of ethical guidelines. If CC terms hold: The Rope remains a strong coordination tool for creators."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: All Rights Reserved Copyright
 *    Viability: The default legal state for creative works.
 *    Suppression: CC was created specifically to provide a more flexible alternative to this "all or nothing" model.
 *
 * ALTERNATIVE 2: Public Domain
 *    Viability: The state of works after copyright expires.
 *    Suppression: Creators often want some control (like attribution) that the public domain does not provide, making CC a more attractive choice.
 *
 * CONCLUSION:
 * Creative Commons acts as a crucial middle-ground, creating a 'Rope' that is
 * more flexible than the 'Snare' of full copyright but provides more control than
 * the 'Mountain' of the public domain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/creative_commons_licensing].
 * 2. Multi-perspective: ?- multi_index_report(creative_commons_licensing).
 * 3. Run tests: ?- run_tests(cc_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */