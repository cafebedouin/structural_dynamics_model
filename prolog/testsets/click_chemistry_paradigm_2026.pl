% ============================================================================
% CONSTRAINT STORY: click_chemistry_paradigm_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "A revolution in how we do chemistry" by Joshua Howgego (Jan 2026)
% ============================================================================

:- module(click_chemistry_paradigm_2026, []).

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
 * * constraint_id: click_chemistry_paradigm_2026
 * human_readable: Click Chemistry Paradigm
 * domain: technological/scientific
 * temporal_scope: 2001-2026 (The "Spring-Loaded" Era)
 * spatial_scope: Global (Laboratory/Molecular scale)
 * * SUMMARY:
 * Click chemistry represents a shift from "messy, sluggish" traditional synthesis 
 * to a modular system of "spring-loaded" reactions that snap molecules together 
 * consistently. It prioritizes carbon-heteroatom bonds (oxygen, nitrogen) 
 * over the previous carbon-carbon bond orthodoxy to build complex molecules 
 * like Lego bricks.
 * * KEY AGENTS:
 * - The Pioneer (Sharpless/Meldal): Proponents of reactions that work "quickly, 
 * cleanly and consistently".
 * - The Drug Developer: Utilizes the triazole ring motif to bind to proteins 
 * and create medications like rufinamide.
 * - The Traditionalist Chemist: Bound by the "orthodoxy" of forming carbon-carbon 
 * bonds in round-bottomed flasks.
 * * NARRATIVE ARC:
 * Outlined in 2001, click chemistry broke the mould of traditional organic 
 * synthesis by shying away from difficult C-C bonds. This "simple idea" 
 * revolutionized drug discovery by allowing scientists to "quickly build a huge 
 * range of new and useful molecules".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(click_chemistry_paradigm_2026, 0, 10).
narrative_ontology:constraint_claim([click_chemistry_paradigm_2026], [scientific_revolution]).

% Base extractiveness score (0.1 = Low)
% Rationale: The technology simplifies labor and reduces waste ("cleanly") 
% rather than extracting resources from subjects.
domain_priors:base_extractiveness(click_chemistry_paradigm_2026, 0.1).

% Suppression score (0.3 = Low)
% Rationale: While it breaks the "orthodoxy," traditional methods still exist; 
% click chemistry is a superior tool rather than an enforced prohibition.
domain_priors:suppression_score(click_chemistry_paradigm_2026, 0.3).

% Enforcement: Emerges naturally 
% Rationale: The "brilliance" of the idea leads to its adoption through 
% competitive efficiency.
domain_priors:emerges_naturally(click_chemistry_paradigm_2026).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, 0.1).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(click_chemistry_paradigm_2026, pharmaceutical_researchers).
constraint_victim(click_chemistry_paradigm_2026, legacy_sluggish_methodologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DRUG DEVELOPER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping in pharmaceutical industry)
   WHEN: biographical (Seeking rapid drug candidates)
   WHERE: mobile (Can choose different molecular "bricks")
   SCOPE: global (Applied to antibiotics and anticonvulsants)
   
   WHY THIS CLASSIFICATION:
   For the researcher, click chemistry is a "Rope"—a functional coordination 
   mechanism that allows them to "snap molecules together almost like Lego 
   bricks" to create medicines.
   
   NARRATIVE EVIDENCE:
   "Build a huge range of new and useful molecules... medicines that Sharpless 
   mostly had in mind".
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LEGACY ORGANIC CHEMIST - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (Established experts in traditional synthesis)
   WHEN: biographical (Risk of skill obsolescence)
   WHERE: constrained (Tied to the "orthodoxy" of carbon-carbon bonds)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For those trained in the "sluggish business" of painstaking separations, 
   this paradigm is a "Noose." It devalues the expertise required for 
   carbon-carbon bond formation in favor of simple, "spring-loaded" clicks.
   
   NARRATIVE EVIDENCE:
   "Frequently involving cocktails of chemicals... that must later be 
   painstakingly separated".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    click_chemistry_paradigm_2026,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MOLECULAR ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of thermodynamic constants)
   WHEN: historical (From 1839/1950s foundations to 2001 discovery)
   WHERE: analytical (Universal chemical laws)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The "spring-loaded" nature of these reactions is a "Mountain"—a fixed 
   physical reality of chemical reactivity that simply required discovery 
   to become useful.
   
   NARRATIVE EVIDENCE:
   "The unifying thought... was that they shied away from forming carbon-carbon 
   bonds... and instead formed bonds between carbon and... heteroatoms".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    click_chemistry_paradigm_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(click_chemistry_tests).

test(multi_perspective_efficiency) :-
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, noose, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, mountain, context(agent_power(analytical), _, _, _)).

test(chemical_complexity_scaling) :-
    % Demonstrates that low extractiveness (0.1) facilitates building 
    % a "huge range" of new molecules.
    domain_priors:base_extractiveness(click_chemistry_paradigm_2026, E),
    E < 0.2.

:- end_tests(click_chemistry_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.1):
 * Reasoning: Click chemistry is described as highly efficient and clean. 
 * It reduces the "sluggish" labor of purification.
 * * 2. CLASSIFICATION RATIONALE:
 * - Developer (Rope): It's a modular tool for creation.
 * - Traditionalist (Noose): Their complex, difficult craft is "overhauled" 
 * by a simpler method.
 * * 3. AMBIGUITIES:
 * The text focuses on "heteroatoms" like oxygen and nitrogen. 
 * The extent to which this *permanently* replaces C-C bonds is an Omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heteroatom_scaffold_limit,
    "Does the reliance on carbon-heteroatom bonds limit the ultimate 
     topological diversity of synthetic molecules compared to C-C bonds?",
    resolution_mechanism("Comparative analysis of click-derived libraries vs. natural products over 50 years"),
    impact("If limited: Click chemistry is a partial Rope. If not: It's a universal Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    click_bio_toxicity,
    "Are all 'click' motifs, such as triazoles, truly inert in long-term human biological systems?",
    resolution_mechanism("Post-market surveillance of drugs like rufinamide"),
    impact("If toxic: The 'Rope' becomes a biological 'Noose'."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Traditional C-C Bond Synthesis (The "Orthodoxy")
 * Viability: Successfully produced most industrial chemicals for a century.
 * Suppression: Being labeled "messy" and "sluggish" in the new paradigm.
 * * ALTERNATIVE 2: Bio-Catalytic Synthesis (Enzymatic)
 * Viability: Highly specific but often limited to natural conditions.
 * Suppression: Click chemistry offers more "modular" industrial control.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system: ?- [click_chemistry_paradigm_2026].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
