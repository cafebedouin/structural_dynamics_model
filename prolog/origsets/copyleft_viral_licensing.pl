% ============================================================================
% CONSTRAINT STORY: copyleft_viral_licensing
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: GNU General Public License (GPL) / Free Software Foundation
% ============================================================================

:- module(constraint_copyleft_viral_licensing, []).

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
 * commons. By using the "Snare" of copyright law (the power to exclude), 
 * it enforces a "Rope" of inclusion. It creates a matching market where 
 * the "price" of entry is the promise to keep the exit open for others.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(copyleft_viral_licensing, 0, 10).
narrative_ontology:constraint_claim(copyleft_viral_licensing, rope).

% Base extractiveness score (0.05)
% Extremely low extraction; it is designed to explicitly prevent the 
% extraction of private profit from a shared public resource.
domain_priors:base_extractiveness(copyleft_viral_licensing, 0.05).

% Suppression score (0.2)
% Low suppression; the terms are notoriously explicit, though corporations 
% may suppress the use of copylefted code internally to avoid "contamination."
domain_priors:suppression_score(copyleft_viral_licensing, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(copyleft_viral_licensing, extractiveness, 0.05).
narrative_ontology:constraint_metric(copyleft_viral_licensing, suppression_requirement, 0.2).

% Enforcement: Requires active enforcement (Legal standing of the GPL).
domain_priors:requires_active_enforcement(copyleft_viral_licensing).



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

    E < 0.1.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CORPORATE COUNSEL - Snare
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: generational
   WHERE: trapped (once code is 'contaminated', it cannot be easily extracted)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To a company trying to build a secret commercial product, the GPL is a 
   "Snare." They view it as a "viral" license that, if accidentally 
   linked to their proprietary code, forces them to open-source their 
   entire product. It is a legal bottleneck they spend millions to avoid.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyleft_viral_licensing,
    snare,
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
    domain_priors:requires_active_enforcement(copyleft_viral_licensing).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE END USER - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (A non-developer using the software)
   WHEN: immediate (When using the software)
   WHERE: trapped (Cannot change the license)
   SCOPE: local (Their own use of the software)
   
   WHY THIS CLASSIFICATION:
   For a regular end user, the copyleft license is a "Mountain." It is an
   invisible, unchangeable legal foundation that allows them to use powerful
   software for free. They do not engage with its "viral" nature but benefit
   from the commons it creates.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    copyleft_viral_licensing,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(copyleft_viral_licensing_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that copyleft is viewed differently across agents.
 */
test(multi_perspective_variance) :-
    % Open Source Developer (Rope)
    constraint_indexing:constraint_classification(
        copyleft_viral_licensing,
        Type1,
        context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(global))
    ),
    % Corporate Counsel (Snare)
    constraint_indexing:constraint_classification(
        copyleft_viral_licensing,
        Type2,
        context(agent_power(institutional), time_horizon(generational), exit_options(trapped), spatial_scope(global))
    ),
    % Software Historian (Mountain)
    constraint_indexing:constraint_classification(
        copyleft_viral_licensing,
        Type3,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
    ),
    % End User (Mountain)
    constraint_indexing:constraint_classification(
        copyleft_viral_licensing,
        Type4,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3. % Rope, Snare, Mountain are different

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that institutional agents (corporate counsel) experience higher extraction
 * in terms of legal risk and forced transparency, compared to individual developers.
 */
test(power_extractiveness_scaling) :-
    ContextIndividual = context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(global)),
    ContextInstitutional = context(agent_power(institutional), time_horizon(generational), exit_options(trapped), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(copyleft_viral_licensing, ContextIndividual, Score1),
    constraint_indexing:extractiveness_for_agent(copyleft_viral_licensing, ContextInstitutional, Score2),
    Score1 < Score2.  % The "powerful" institution experiences more extraction in this case.

/**
 * TEST 3: Domain-specific insight - The "Viral" Nature
 * Demonstrates that the "snare" for corporations is the very "rope" that protects the commons.
 */
test(viral_nature_insight) :-
    constraint_indexing:constraint_classification(copyleft_viral_licensing, ClassificationCorporate, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(copyleft_viral_licensing, ClassificationDeveloper, context(agent_power(individual_moderate), _, _, _)),
    ClassificationCorporate = snare,
    ClassificationDeveloper = rope.

:- end_tests(copyleft_viral_licensing_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. EXTRACTIVENESS (0.05): Copyleft is designed to prevent extraction, so the base score is near zero. The only "extraction" is the requirement to share back, which is a feature, not a bug, of the system.
 * 
 * 2. SUPPRESSION (0.2): Low suppression because the license's terms are explicit. Any "suppression" comes from corporate policies that avoid copyleft code, not from the license itself.
 * 
 * 3. PERSPECTIVE SELECTION:
 *    Selected Open Source Developer (Rope), Corporate Counsel (Snare), Software Historian (Mountain), and End User (Mountain) to show the full range of interactions with copyleft, from active participation to legal opposition to passive benefit.
 * 
 * 4. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        saas_loophole_resolution,
 *        "Will the 'SaaS Loophole' (AGPL) eventually become the new legal Mountain for cloud-based software?",
 *        resolution_mechanism("Tracking adoption rates of AGPL vs GPL in hyperscale cloud providers and related court cases."),
 *        impact("If Yes: The 'Snare' for cloud providers tightens, forcing them to share back server-side code. If No: The commons may be 'extracted' via SaaS models."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Permissive Licensing
 * 
 * ALTERNATIVE 1: Permissive Licenses (e.g., MIT, Apache)
 *    Viability: High. These licenses are extremely popular in corporate environments because they allow for proprietary derivatives.
 *    Suppression: Copyleft is not suppressed, but rather presents a different philosophical and legal choice. The two systems coexist.
 *    Evidence: The ongoing "license wars" and debates within the open-source community.
 * 
 * CONCLUSION:
 * The choice between copyleft and permissive licenses represents a fundamental fork in the road for open-source projects. Copyleft prioritizes the freedom of the commons (Rope for community), while permissive licenses prioritize the freedom of the individual developer/corporation (Rope for individual). The "Snare" of copyleft is the price paid for a guaranteed-free commons.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/copyleft_viral_licensing].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(copyleft_viral_licensing).
 * 
 * 3. Run tests:
 *    ?- run_tests(copyleft_viral_licensing_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(copyleft_viral_licensing).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(copyleft_viral_licensing, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
