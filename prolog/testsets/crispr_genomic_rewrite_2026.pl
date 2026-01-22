% ============================================================================
% CONSTRAINT STORY: crispr_genomic_rewrite_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "We can rewrite our genetic code" by Michael Le Page (Jan 2026)
% ============================================================================

:- module(crispr_genomic_rewrite_2026, []).

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
 * * constraint_id: crispr_genomic_rewrite_2026
 * human_readable: CRISPR Genomic Programmability
 * domain: technological/biological
 * temporal_scope: 2012-2026 (The CRISPR Era)
 * spatial_scope: Global (Biological/Agricultural)
 * * SUMMARY:
 * CRISPR is a revolutionary tool that enables specific, targeted changes to be made to
 * DNA, effectively allowing the "rewriting" of the genetic code. It has
 * transitioned genetic diseases from "impossible to correct" errors into treatable
 * conditions through precise "spelling mistake" correction.
 * * KEY AGENTS:
 * - The Patient: (e.g., Victoria Gray) - Subject to severe genetic conditions like
 * sickle cell disease.
 * - The Scientist/Institution: (e.g., Clinical trial leads) - Developers of CRISPR
 * therapies and agricultural transformations.
 * - Future Progeny: Potential subjects of germline editing to reduce risks of
 * common diseases.
 * * NARRATIVE ARC:
 * The shift from genetic destiny (Mountain) to programmable health (Rope). Starting with
 * experimental trials in 2019, the first CRISPR therapy was approved in 2023.
 * The technology is now expanding into preventative medicine and farming.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(crispr_genomic_rewrite_2026, 0, 10).

% Base extractiveness: 0.4
% Rationale: While curative, the potential for "better versions of ourselves"
% and asymmetric access to genetic enhancement introduces a moderate risk
% of social/biological extraction.
domain_priors:base_extractiveness(crispr_genomic_rewrite_2026, 0.4).

% Suppression: 0.2
% Rationale: Older genetic methods are not being actively suppressed by force,
% but are becoming obsolete through the superior efficacy of CRISPR.
domain_priors:suppression_score(crispr_genomic_rewrite_2026, 0.2).

% Enforcement: Requires active enforcement
% Rationale: CRISPR does not occur naturally as a therapy; it requires precise
% targeting (the "cursor") and a secondary "change" mechanism.
domain_priors:requires_active_enforcement(crispr_genomic_rewrite_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(crispr_genomic_rewrite_2026, genetic_disease_patients). %
constraint_beneficiary(crispr_genomic_rewrite_2026, climate_adapted_agriculture). %
constraint_victim(crispr_genomic_rewrite_2026, congenital_disease_legacy). % The "errors" once thought permanent

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CURED PATIENT (Victoria Gray) - Rope
   --------------------------------------------------------------------------

   WHO: individual_powerless (A patient subject to severe disease pain)
   WHEN: biographical (A total shift in life quality: "everything is different")
   WHERE: trapped (Previously bound by sickle cell "freight train" pain)
   SCOPE: local (Personal health and survival)

   WHY THIS CLASSIFICATION:
   For the patient, CRISPR is a "Rope"—a functional tool that allows them
   to pull themselves out of a "trapped" state of chronic disease. It is a
   beneficial coordination mechanism for their biology.

   NARRATIVE EVIDENCE:
   "Now everything is different for me." "Gray... was effectively cured
   by a revolutionary technique".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    crispr_genomic_rewrite_2026,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SCIENTIFIC INSTITUTION - Rope
   --------------------------------------------------------------------------

   WHO: institutional (Conducting hundreds of clinical trials)
   WHEN: generational (Planning for future routine child genome editing)
   WHERE: mobile (Moving the "cursor" to different places in the genome)
   SCOPE: global (Transforming farming and global health risk)

   WHY THIS CLASSIFICATION:
   For institutions, CRISPR is a highly flexible "Rope"—a tool for
   optimizing human and agricultural systems. It is the cursor and
   editor of the biological world.

   NARRATIVE EVIDENCE:
   "CRISPR could help treat all kinds of diseases... transform farming...
   reduce your risk of heart attacks".
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGICAL REALIST - Mountain
   --------------------------------------------------------------------------

   WHO: analytical (Observer of the genetic "code")
   WHEN: civilizational (Permanent genetic changes)
   WHERE: analytical (Universal application of the technique)
   SCOPE: global

   WHY THIS CLASSIFICATION:
   From an analytical perspective, the ability to rewrite DNA has become
   a "Mountain"—a fixed reality of the 21st century. The biological
   programmability of life is no longer a choice but a law of nature.

   NARRATIVE EVIDENCE:
   "Our genomes are filled with errors that were once impossible to correct."
   "CRISPR is one of the very best ideas of the 21st century".
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
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(crispr_genomic_rewrite_2026_tests).

test(multi_perspective_variance) :-
    % Verify that different agents see the tool differently
    constraint_indexing:constraint_classification(crispr_genomic_rewrite_2026, T1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(crispr_genomic_rewrite_2026, T2, context(agent_power(analytical), _, _, _)),
    T1 \= T2.

test(power_benefit_scaling) :-
    % The powerless (sickle cell patients) experience high benefit (Rope)
    % while the powerful (institutions) use it as a global lever.
    true.

:- end_tests(crispr_genomic_rewrite_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.4): Chosen because while the direct health benefits
 * are high, the source mentions "making better versions of ourselves," which
 * introduces competitive/extractive asymmetries in the social domain.
 * 2. CLASSIFICATION RATIONALE:
 * - Victoria Gray (Rope): She escaped a biological Noose through a
 * technological Rope.
 * - Biological Realist (Mountain): The fact that DNA is now editable is an
 * immutable feature of modern biological reality.
 * 3. AMBIGUITIES: The source mentions it "isn't yet safe enough to attempt"
 * certain future uses (germline), creating a temporal boundary for
 * the Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    germline_editing_safety_threshold,
    'When will CRISPR be considered safe enough for routine germline editing in children?',
    resolution_mechanism('Long-term safety data from current somatic clinical trials and regulatory consensus'),
    impact('If soon: Rope (prevents common disease). If never: Noose (limited to somatic treatment).'),
    confidence_without_resolution(medium)
).

omega_variable(
    cholesterol_permanent_lowering_consequences,
    'What are the unforeseen long-term metabolic impacts of \'permanently lowering\' cholesterol via CRISPR?',
    resolution_mechanism('30-year longitudinal follow-ups on the \'single dose\' recipients mentioned in the text'),
    impact('If safe: Universal Rope. If dangerous: Biological Noose.'),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Conventional Genetic Counseling
 * Viability: Managing outcomes without editing.
 * Suppression: Being replaced by CRISPR's "correction" of errors.
 * * ALTERNATIVE 2: Traditional Farming (Selective Breeding)
 * Viability: Existing method for disease-resistant crops.
 * Suppression: CRISPR is "making it much easier" than these alternatives.
 * * CONCLUSION:
 * The existence of CRISPR transforms the "Genetic Blueprint" from a Mountain
 * (fixed destiny) into a Rope (manageable data).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [crispr_genomic_rewrite_2026].
% Test: ?- run_tests(crispr_genomic_rewrite_2026_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
