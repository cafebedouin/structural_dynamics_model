% ============================================================================
% CONSTRAINT STORY: crispr_genomic_rewrite_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "We can rewrite our genetic code" by Michael Le Page (Jan 2026)
% ============================================================================

:- module(constraint_crispr_genomic_rewrite_2026, []).

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
 * constraint_id: crispr_genomic_rewrite_2026
 * human_readable: CRISPR Genomic Programmability
 * domain: technological/biological
 * temporal_scope: 2012-2026 (The CRISPR Era)
 * spatial_scope: Global (Biological/Agricultural)
 * 
 * SUMMARY:
 * CRISPR is a revolutionary tool enabling specific, targeted changes to DNA,
 * effectively "rewriting" the genetic code. It has transitioned genetic diseases
 * from "impossible to correct" errors into treatable conditions through precise
 * "spelling mistake" correction, expanding into preventative medicine and farming.
 * 
 * KEY AGENTS:
 * - The Cured Patient (Individual Powerless): Subject to severe genetic conditions like sickle cell disease.
 * - The Scientific Institution (Institutional): Developers of CRISPR therapies and agricultural transformations.
 * - The Biological Realist (Analytical): Observer of the genetic "code" and its new programmability.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(crispr_genomic_rewrite_2026, 0, 10).
narrative_ontology:constraint_claim(crispr_genomic_rewrite_2026, rope).

% Base extractiveness: 0.4
% Rationale: While curative, the potential for "better versions of ourselves"
% and asymmetric access to genetic enhancement introduces a moderate risk
% of social/biological extraction.
domain_priors:base_extractiveness(crispr_genomic_rewrite_2026, 0.4).

% Suppression: 0.2
% Rationale: Older genetic methods are not being actively suppressed by force,
% but are becoming obsolete through the superior efficacy of CRISPR.
domain_priors:suppression_score(crispr_genomic_rewrite_2026, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(crispr_genomic_rewrite_2026, extractiveness, 0.4).
narrative_ontology:constraint_metric(crispr_genomic_rewrite_2026, suppression_requirement, 0.2).

% Enforcement: CRISPR does not occur naturally as a therapy; it requires precise
% targeting and a secondary "change" mechanism.
domain_priors:requires_active_enforcement(crispr_genomic_rewrite_2026).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(crispr_genomic_rewrite_2026, genetic_disease_patients).
narrative_ontology:constraint_victim(crispr_genomic_rewrite_2026, congenital_disease_legacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CURED PATIENT (Victoria Gray) - Rope
   --------------------------------------------------------------------------
   WHO: powerless (A patient subject to severe disease pain)
   WHEN: biographical (A total shift in life quality: "everything is different")
   WHERE: trapped (Previously bound by sickle cell "freight train" pain)
   
   WHY THIS CLASSIFICATION:
   For the patient, CRISPR is a 'Rope'—a functional tool that allows them
   to pull themselves out of a "trapped" state of chronic disease. It is a
   beneficial coordination mechanism for their biology.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    crispr_genomic_rewrite_2026,
    rope,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SCIENTIFIC INSTITUTION - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Conducting hundreds of clinical trials)
   WHEN: generational (Planning for future routine child genome editing)
   WHERE: mobile (Moving the "cursor" to different places in the genome)
   
   WHY THIS CLASSIFICATION:
   For institutions, CRISPR is a highly flexible 'Rope'—a tool for
   optimizing human and agricultural systems. It is the cursor and
   editor of the biological world, enabling broad societal impact.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    crispr_genomic_rewrite_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGICAL REALIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of the genetic "code" and its new programmability)
   WHEN: civilizational (Permanent genetic changes)
   WHERE: analytical (Universal application of the technique)
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the ability to rewrite DNA has become
   a 'Mountain'—a fixed reality of the 21st century. The biological
   programmability of life is no longer a choice but a discoverable law of nature.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    crispr_genomic_rewrite_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(crispr_genomic_rewrite_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(crispr_genomic_rewrite_2026, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crispr_genomic_rewrite_2026, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(crispr_genomic_rewrite_2026, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type3, % Patient (Rope) vs Realist (Mountain)
    Type2 \= Type3. % Institution (Rope) vs Realist (Mountain)

:- end_tests(crispr_genomic_rewrite_2026_tests).

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
 * 1. EXTRACTIVENESS SCORE (0.4): Chosen because while the direct health benefits
 *    are high, the potential for "making better versions of ourselves"
 *    introduces competitive/extractive asymmetries in the social domain.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Patient (Rope): Escaped a biological Snare through a technological Rope.
 *    - Institution (Rope): A highly flexible tool for optimizing biological systems.
 *    - Biological Realist (Mountain): The fact that DNA is now editable is an immutable feature.
 * 
 * 3. CORE INSIGHT: CRISPR transforms "genetic destiny" from a 'Mountain' into
 *    a programmable 'Rope', but this power creates new ethical considerations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Core uncertainties around long-term safety and ethical implications.
 */

omega_variable(
    germline_editing_safety_threshold,
    'When will CRISPR be considered safe enough for routine germline editing in children (i.e., making permanent, heritable changes)?',
    resolution_mechanism('Long-term safety data from current somatic clinical trials, and broad regulatory and societal consensus.'),
    impact('If soon: CRISPR is a universal \'Rope\' (prevents common disease). If never: It remains limited to somatic treatment (a weaker \'Rope\').'),
    confidence_without_resolution(medium)
).

omega_variable(
    long_term_metabolic_impacts,
    'What are the unforeseen long-term metabolic impacts of ''permanently lowering'' cholesterol via CRISPR (as mentioned in the source)?',
    resolution_mechanism('30-year longitudinal follow-ups on the ''single dose'' recipients mentioned in the text.'),
    impact('If safe: Universal ''Rope''. If dangerous: Biological ''Snare'' for the treated.'),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Conventional Genetic Counseling and Symptomatic Treatment
 *    Viability: Historically the standard approach for managing genetic diseases.
 *    Suppression: CRISPR is actively replacing this by offering "correction" of errors,
 *    rendering the alternative less effective and desirable for many conditions.
 *
 * CONCLUSION:
 * CRISPR represents a fundamental paradigm shift that makes previous alternatives
 * less relevant. It transforms "genetic destiny" from a 'Mountain' into a
 * manageable 'Rope'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/crispr_genomic_rewrite_2026].
 * 2. Multi-perspective: ?- multi_index_report(crispr_genomic_rewrite_2026).
 * 3. Run tests: ?- run_tests(crispr_genomic_rewrite_2026_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
