% ============================================================================
% CONSTRAINT STORY: click_chemistry_paradigm_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "A revolution in how we do chemistry" by Joshua Howgego (Jan 2026)
% ============================================================================

:- module(constraint_click_chemistry_paradigm_2026, []).

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
 * constraint_id: click_chemistry_paradigm_2026
 * human_readable: Click Chemistry Paradigm
 * domain: scientific/technological
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Laboratory/Molecular scale)
 * 
 * SUMMARY:
 * Click chemistry represents a shift from "messy, sluggish" traditional synthesis 
 * to a modular system of "spring-loaded" reactions that snap molecules together 
 * consistently. It prioritizes carbon-heteroatom bonds over the previous 
 * carbon-carbon bond orthodoxy to build complex molecules like Lego bricks.
 * 
 * KEY AGENTS:
 * - The Drug Developer (Institutional): Utilizes the new paradigm for rapid discovery.
 * - The Legacy Chemist (Individual Powerful): Expertise is in older, slower methods.
 * - The Graduate Student (Individual Powerless): Follows the new, established laboratory procedures.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(click_chemistry_paradigm_2026, 0, 10).
narrative_ontology:constraint_claim(click_chemistry_paradigm_2026, rope).

% Base extractiveness score (0.1 = Low)
% Rationale: The technology simplifies labor and reduces waste ("cleanly") 
% rather than extracting resources from subjects.
domain_priors:base_extractiveness(click_chemistry_paradigm_2026, 0.1).

% Suppression score (0.3 = Low)
% Rationale: While it breaks the "orthodoxy," traditional methods still exist; 
% click chemistry is a superior tool rather than an enforced prohibition.
domain_priors:suppression_score(click_chemistry_paradigm_2026, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, 0.1).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, 0.3).

% Enforcement: Emerges naturally through competitive efficiency.
domain_priors:emerges_naturally(click_chemistry_paradigm_2026).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, pharmaceutical_researchers).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, traditional_chemists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DRUG DEVELOPER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-shaping in pharmaceutical industry)
   WHEN: biographical (Seeking rapid drug candidates)
   WHERE: mobile (Can choose different molecular "bricks")
   
   WHY THIS CLASSIFICATION:
   For the researcher, click chemistry is a 'Rope'—a powerful, functional 
   coordination mechanism that allows them to "snap molecules together almost 
   like Lego bricks" to create medicines efficiently.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    click_chemistry_paradigm_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GRADUATE STUDENT - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Following lab protocols)
   WHEN: immediate (Daily lab work)
   WHERE: trapped (Must use the methods dictated by their Principal Investigator)
   
   WHY THIS CLASSIFICATION:
   For the student, click chemistry is a 'Mountain'. It is presented as the 
   correct, default way to do synthesis. They follow the protocol without question,
   making it an immutable reality of their work.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    click_chemistry_paradigm_2026,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LEGACY ORGANIC CHEMIST - Snare
   --------------------------------------------------------------------------
   WHO: powerful (Established experts in traditional synthesis)
   WHEN: biographical (Risk of skill obsolescence)
   WHERE: constrained (Tied to the "orthodoxy" of carbon-carbon bonds)
   
   WHY THIS CLASSIFICATION:
   For those trained in the "sluggish business" of painstaking C-C bond formation, 
   this new paradigm is a 'Snare.' It devalues their hard-won, complex skills 
   in favor of a simpler, modular approach, threatening their professional status.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    click_chemistry_paradigm_2026,
    snare,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(click_chemistry_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(click_chemistry_tests).

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
 * 1. PERSPECTIVE SELECTION: Added the 'Graduate Student' as the 'powerless'
 *    agent. This is a classic case where a new, efficient paradigm becomes an 
 *    immutable 'Mountain' for newcomers who are taught it as the only way.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Developer (Rope): It's a modular tool for creation.
 *    - Student (Mountain): It's the unquestioned, default procedure.
 *    - Traditionalist (Snare): Their complex craft is devalued by a simpler method.
 * 
 * 3. EXTRACTIVENESS SCORE (0.1): Kept low because the primary effect is displacing
 *    an old method with a more efficient one, not extracting value from a vulnerable population.
 *    The 'Snare' for the traditionalist is about status and skill relevance, not material extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Key uncertainties about the long-term viability and impact of the paradigm shift.
 */

omega_variable(
    heteroatom_scaffold_limit,
    "Does the reliance on carbon-heteroatom bonds limit the ultimate topological diversity of synthetic molecules compared to C-C bonds?",
    resolution_mechanism("Comparative analysis of click-derived libraries vs. natural products over a 50-year timeframe."),
    impact("If limited: Click chemistry is a partial Rope. If not: It becomes a universal Mountain of synthesis."),
    confidence_without_resolution(medium)
).

omega_variable(
    click_bio_toxicity,
    "Are all 'click' motifs, such as triazoles, truly inert in long-term human biological systems?",
    resolution_mechanism("Long-term, post-market surveillance of drugs like rufinamide that are built with click chemistry."),
    impact("If toxic: The drug discovery 'Rope' becomes a hidden biological 'Snare' for patients."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Traditional C-C Bond Synthesis (The "Orthodoxy")
 *    Viability: Successfully produced most industrial chemicals for a century. It remains viable for many applications.
 *    Suppression: Being labeled "messy," "sluggish," and inefficient in the new paradigm, leading to its de-prioritization in new research and teaching.
 * 
 * CONCLUSION:
 * Click chemistry doesn't eliminate the old methods, but it suppresses their perceived value and relevance, shifting the focus of an entire field. This re-valuation is a hallmark of a successful 'Rope' displacing a prior one.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/click_chemistry_paradigm_2026].
 * 2. Multi-perspective: ?- multi_index_report(click_chemistry_paradigm_2026).
 * 3. Run tests: ?- run_tests(click_chemistry_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */