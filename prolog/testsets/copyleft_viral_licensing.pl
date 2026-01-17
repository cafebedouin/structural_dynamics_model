% ============================================================================
% CONSTRAINT STORY: copyleft_viral_licensing
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: GNU General Public License (GPL) / Free Software Foundation
% ============================================================================

:- module(constraint_copyleft, []).

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
 * * constraint_id: copyleft_viral_licensing
 * human_readable: Copyleft (Reciprocal Freedom)
 * domain: technological/legal/social
 * temporal_scope: Biographical to Historical (1985 - Present)
 * spatial_scope: Global
 * * SUMMARY:
 * Copyleft is a legal mechanism that uses copyright law to ensure that a work 
 * and its derivatives remain free. Unlike "permissive" licenses, Copyleft 
 * requires that any modified versions of the software must also be licensed 
 * under the same Copyleft terms—a "Share-Alike" requirement on steroids.
 * * KEY AGENTS:
 * - The Free Software Advocate: (e.g., Richard Stallman) who views software 
 * freedom as a fundamental right.
 * - The Proprietary Developer: Views Copyleft as a "Virus" that threatens 
 * their ability to close and monetize source code.
 * - The Community Contributor: Gains security knowing their work cannot be 
 * "captured" by a single corporation.
 * * NARRATIVE ARC:
 * Copyleft functions as a "Recursive Rope." It builds a self-expanding 
 * commons. By using the "Noose" of copyright law (the power to exclude), 
 * it enforces a "Rope" of inclusion. It creates a matching market where 
 * the "price" of entry is the promise to keep the exit open for others.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(copyleft_viral_licensing, 0, 10).
narrative_ontology:constraint_claim(copyleft_viral_licensing, recursive_commons).

% Base extractiveness score (0.05)
% Extremely low extraction; it is designed to explicitly prevent the 
% extraction of private profit from a shared public resource.
domain_priors:base_extractiveness(copyleft_viral_licensing, 0.05).

% Suppression score (0.2)
% Low suppression; the terms are notoriously explicit, though corporations 
% may suppress the use of copylefted code internally to avoid "contamination."
domain_priors:suppression_score(copyleft_viral_licensing, 0.2).

% Enforcement: Requires active enforcement (Legal standing of the GPL).
domain_priors:requires_active_enforcement(copyleft_viral_licensing).

% Metrics
narrative_ontology:constraint_metric(copyleft_viral_licensing, extractiveness, 0.05).
narrative_ontology:constraint_metric(copyleft_viral_licensing, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(copyleft_viral_licensing, the_user_community).
constraint_beneficiary(copyleft_viral_licensing, open_source_ecosystems).
constraint_victim(copyleft_viral_licensing, proprietary_software_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE OPEN SOURCE DEVELOPER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the developer, Copyleft is a "Rope." It provides the guarantee that 
   if they contribute to a project (like the Linux kernel), their work will 
   always remain accessible to them and the community. It coordinates 
   mass collaboration by removing the fear of "Proprietary Capture."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyleft_viral_licensing,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(copyleft_viral_licensing, E),
    E < 0.1,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CORPORATE COUNSEL - Noose
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: generational
   WHERE: trapped (once code is 'contaminated', it cannot be easily extracted)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To a company trying to build a secret commercial product, the GPL is a 
   "Noose." They view it as a "viral" license that, if accidentally 
   linked to their proprietary code, forces them to open-source their 
   entire product. It is a legal bottleneck they spend millions to avoid.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyleft_viral_licensing,
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
   PERSPECTIVE 3: THE SOFTWARE HISTORIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The historian sees Copyleft as a "Mountain"—the foundational bedrock 
   of the modern internet. Without the GPL (Copyleft), the "Mountain" of 
   Linux and the web servers that run the world would have likely been 
   fragmented and privatized decades ago.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyleft_viral_licensing,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(copyleft_viral_licensing),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(copyleft_tests).

test(viral_noose_perception) :-
    % Testing that institutions see Copyleft as a Noose due to its reciprocal 
    % requirements.
    constraint_indexing:constraint_classification(copyleft_viral_licensing, noose, context(institutional, _, _, _)).

test(community_rope_utility) :-
    % Testing that mobile developers see the Rope of protected collaboration.
    constraint_indexing:constraint_classification(copyleft_viral_licensing, rope, context(_, _, mobile, _)).

:- end_tests(copyleft_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Copyleft is a "force multiplier" for freedom. I set extractiveness 
 * to near zero (0.05) because its only goal is to prevent the 
 * "Noose" of proprietary software from forming around shared code. 
 * Its "viral" nature is its most powerful and feared feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    saas_loophole_resolution,
    "Will the 'SaaS Loophole' (AGPL) eventually become the new legal Mountain for cloud-based software?",
    resolution_mechanism("Tracking adoption rates of AGPL vs GPL in hyperscale cloud providers"),
    impact("If Yes: The 'Noose' for cloud providers tightens; they must share back server-side code."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
