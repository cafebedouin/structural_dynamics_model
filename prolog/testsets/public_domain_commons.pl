% ============================================================================
% CONSTRAINT STORY: public_domain_commons
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Copyright Law / Creative Commons / Public Domain Status
% ============================================================================

:- module(constraint_public_domain_commons, []).

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
 * constraint_id: public_domain_commons
 * human_readable: The Public Domain (Cultural Commons)
 * domain: social/economic/legal
 * temporal_scope: Civilizational (Permanent / Post-Copyright)
 * spatial_scope: Global
 * 
 * SUMMARY:
 * The Public Domain consists of all creative work to which no exclusive 
 * intellectual property rights apply. These works are free for anyone to use, 
 * remix, and build upon. It is the "default" state of information before or 
 * after the artificial constraint of Copyright is applied.
 * 
 * KEY AGENTS:
 * - The Student (Individual Powerless): Accesses public domain works for education.
 * - The Remix Artist (Individual Moderate): Uses the commons as raw material for new creations.
 * - The Corporate Rights-Holder (Institutional): Views the public domain as a threat to their business model.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(public_domain_commons, 0, 10).
narrative_ontology:constraint_claim(public_domain_commons, mountain).

% Base extractiveness score (0.0)
% By definition, the public domain is non-extractive.
domain_priors:base_extractiveness(public_domain_commons, 0.0).

% Suppression score (0.3)
% While the works are legally free, they are often suppressed by "copyfraud" 
% (false copyright claims by legacy estates) or are difficult to access.
domain_priors:suppression_score(public_domain_commons, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(public_domain_commons, extractiveness, 0.0).
narrative_ontology:constraint_metric(public_domain_commons, suppression_requirement, 0.3).

% Enforcement: Emerges naturally once copyright expires.
domain_priors:emerges_naturally(public_domain_commons).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(public_domain_commons, general_public).
constraint_victim(public_domain_commons, none).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STUDENT - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Limited funds, needs access to knowledge)
   WHEN: immediate (Needs to read a classic for a class)
   WHERE: trapped (Cannot afford to buy every book)
   
   WHY THIS CLASSIFICATION:
   For a student, the public domain is a 'Mountain' of free knowledge. Classic
   literature, historical documents, and scientific papers are simply 'there'
   to be accessed and used, like a natural feature of the educational landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    public_domain_commons,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INDIE DEVELOPER / REMIX ARTIST - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (A creator with agency)
   WHEN: biographical (Building a career or product)
   WHERE: mobile (Can choose which public domain works to use)
   
   WHY THIS CLASSIFICATION:
   For a small creator, the Public Domain is a 'Rope.' They can use the 
   characters of Shakespeare, the music of Beethoven, or historical images as a 
   coordination tool to build their own products without the 'Snare' of licensing fees.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISNEY LAWYER (INSTITUTIONAL) - Snare
   --------------------------------------------------------------------------
   WHO: institutional (Represents a legacy media giant)
   WHEN: generational (Business model relies on IP exclusivity)
   WHERE: constrained (Cannot stop copyright from expiring eventually)
   
   WHY THIS CLASSIFICATION:
   From the perspective of a rights-holding corporation, the Public Domain is a 
   'Snare' for their profit margins. It represents the "death" of their 
   monopoly. As works like Steamboat Willie enter the commons, the 
   exclusive control that protected their revenue is "strangled."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    public_domain_commons,
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

:- begin_tests(public_domain_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(public_domain_commons, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_domain_commons, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(public_domain_commons, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

test(non_extractive_nature) :-
    domain_priors:base_extractiveness(public_domain_commons, E),

    E < 0.1.

:- end_tests(public_domain_tests).

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
 *    agent. This captures the idea of the public domain as a foundational,
 *    unquestioned resource for those entering a field of knowledge.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Student (Mountain): An immutable source of free knowledge.
 *    - Artist (Rope): A tool for creating new works.
 *    - Corporation (Snare): A threat to a monopoly business model.
 * 
 * 3. EXTRACTIVENESS (0.0): The Public Domain is the conceptual baseline for a
 *    non-extractive information system. The conflict arises from others trying
 *    to re-impose extractive constraints (copyright) upon it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether AI will respect or erase the public domain.
 */

omega_variable(
    sovereign_ai_datasets,
    "Will AI models trained on public domain data be able to compete with those trained on vast copyrighted datasets, or will the latter create an insurmountable advantage?",
    resolution_mechanism("Benchmarking performance of open-weights models trained on public data vs. closed-weights models trained on proprietary data over time."),
    impact("If public data is sufficient: The 'Mountain' of the commons becomes the primary 'Rope' for future tech development. If not: The commons becomes a quaint but irrelevant starting point."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Perpetual Copyright
 *    Viability: Not currently legal, but consistently lobbied for by large rights-holders.
 *    Suppression: Suppressed by a centuries-long legal and philosophical tradition that sees
 *    copyright as a temporary, limited monopoly to encourage creativity, not a permanent property right.
 *
 * CONCLUSION:
 * The public domain is the default state. The constraint is 'copyright', which acts
 * as a temporary 'Snare' on the public's right to use information. The constant
 * battle over copyright term extension is an attempt by institutional agents to
 * prevent their 'Rope' (a marketable character) from dissolving back into the
 * 'Mountain' of the commons.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/public_domain_commons].
 * 2. Multi-perspective: ?- multi_index_report(public_domain_commons).
 * 3. Run tests: ?- run_tests(public_domain_tests).
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
% Structural constraint in social domain — low theater, high substance
domain_priors:theater_ratio(public_domain_commons, 0.08).
narrative_ontology:constraint_metric(public_domain_commons, theater_ratio, 0.08).

% --- Analytical perspective classification (missing) ---
% chi = 0.0 * 1.15 (analytical) * 1.2 (global) = 0.000
% Classification: scaffold
constraint_indexing:constraint_classification(public_domain_commons, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
