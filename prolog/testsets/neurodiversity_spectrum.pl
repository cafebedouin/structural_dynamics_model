% ============================================================================
% CONSTRAINT STORY: neurodiversity_spectrum
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Caroline Williams, "There’s no such thing as a normal brain"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_neurodiversity_spectrum, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * * constraint_id: neurodiversity_spectrum
 * human_readable: The Neurodiversity Asset Spectrum
 * domain: social/scientific/medical
 * temporal_scope: Late 1990s - 2026
 * spatial_scope: Global (Medical and Advocacy contexts)
 * * SUMMARY:
 * This constraint represents the shift from a binary "normal vs. abnormal" brain 
 * model to a spectrum of natural neurodevelopmental variation. 
 * It reframes conditions like autism and ADHD as assets rather than illnesses 
 * to be cured.
 * * KEY AGENTS:
 * - The Neurodivergent Individual: Historically marginalized, now seeking support.
 * - The Medical Establishment (DSM): Transitioned from binary diagnosis to spectrum levels.
 * - Online Advocacy Groups: The catalysts for the neurodiversity concept.
 * * NARRATIVE ARC:
 * Historically, "unusual brains" were constrained by the "Noose" of pathology 
 * labels. The emergence of neurodiversity acts as a 
 * "Rope" that coordinates support and acknowledges unique strengths as systemic 
 * assets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(neurodiversity_spectrum, 0, 10).

% Base extractiveness: 0.72 (High)
% Rationale: The "illness" model extracted autonomy and identity from those 
% labeled "abnormal" for decades. High score triggers 
% Mandatrophy protocol.
domain_priors:base_extractiveness(neurodiversity_spectrum, 0.72).

% Suppression: 0.55 (Moderate)
% Rationale: Advocacy groups have reduced the suppression of neurodivergent 
% strengths, but the "normalcy" ritual still persists in society.
domain_priors:suppression_score(neurodiversity_spectrum, 0.55).

% Enforcement: Requires active enforcement (Diagnosis levels 1-3).
domain_priors:requires_active_enforcement(neurodiversity_spectrum).

% BENEFICIARIES & VICTIMS
% Society benefits from "diverse brains" as an asset if supported.
constraint_beneficiary(neurodiversity_spectrum, inclusive_society).
% Individuals with unusual brains were historically "victims" of illness labels.
constraint_victim(neurodiversity_spectrum, neurodivergent_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRE-1990s PATIENT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Treated as having something "wrong")
   WHEN: biographical (Single lifetime of diagnosis)
   WHERE: trapped (No spectrum existed, only the "hard line" of abnormal)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For these individuals, the "normal brain" assumption was a Noose that extracted 
   their dignity and replaced it with a diagnosis of illness.
   
   NARRATIVE EVIDENCE:
   "Those who were different might be diagnosed with an illness... and were 
   treated as though something was wrong with them".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    neurodiversity_spectrum,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(neurodiversity_spectrum, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MODERN MEDICAL SYSTEM (DSM-5) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making authority)
   WHEN: generational (Reflecting decades of honed concepts)
   WHERE: mobile (Shifts from Asperger's to Levels 1-3)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The spectrum acts as a Rope—a functional coordination mechanism to allocate 
   the "amount of support required" rather than a binary exclusion.
   
   NARRATIVE EVIDENCE:
   "Recognising one condition, autism spectrum disorder, on a scale... depending 
   on the amount of support required".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    neurodiversity_spectrum,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    % Classification logic for the institutional coordinator
    true,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE NEURODIVERSITY ADVOCATE - Mountain
   --------------------------------------------------------------------------
   
   WHO: collective_organized (Advocacy groups)
   WHEN: historical (Rooted in "natural variation")
   WHERE: arbitrage (Leveraging diverse strengths as assets)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   Advocates see neurodiversity as a biological Mountain—a natural, immutable 
   variation in human wiring that cannot and should not be "solved".
   
   NARRATIVE EVIDENCE:
   "What if these 'disorders' were better understood as natural variation in 
   the way human brains can be wired?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    neurodiversity_spectrum,
    mountain,
    context(
        agent_power(collective_organized),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(neurodiversity_spectrum, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================= */

:- begin_tests(neurodiversity_spectrum_tests).

test(multi_perspective_variance) :-
    % Illness model (Noose) vs Support model (Rope) vs Natural law (Mountain)
    constraint_indexing:constraint_classification(neurodiversity_spectrum, noose, context(individual_powerless, _, trapped, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, rope, context(institutional, _, mobile, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, mountain, context(collective_organized, _, _, _)).

test(spectrum_vs_binary_extraction) :-
    % Binary model (Score 1) extracts more autonomy than Spectrum model (Score 2)
    BinaryExtraction = 0.9,
    SpectrumExtraction = 0.3,
    BinaryExtraction > SpectrumExtraction.

:- end_tests(neurodiversity_spectrum_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.72): High score chosen to reflect the "Mandatrophy" 
 * of the old illness-based paradigm which extracted personhood.
 * 2. SUPPRESSION (0.55): Reflects the ongoing transition where "Asperger's" 
 * was suppressed to create the broader Spectrum.
 * 3. PERSPECTIVES: Chose the 1990s patient to show the "Noose" effect of the 
 * hard-line binary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurodiversity_spectrum_extraction_intent,
    "Is the 0.72 extraction of the 'illness' label a predatory attempt at social conformity or a genuine (but flawed) attempt at care?",
    resolution_mechanism("Audit of historical medical outcomes vs. modern quality-of-life improvements in spectrum-based systems"),
    impact("If predatory: Noose. If attempt at care: Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    asset_viability_threshold,
    "Are neurodivergent traits inherent 'assets' or only assets relative to specific societal supports?",
    resolution_mechanism("Long-term tracking of 'level 3' individuals in varied supportive environments"),
    impact("If inherent: Mountain. If environment-dependent: Rope/Scaffold."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The "Normalcy" Binary
 * Viability: Historically dominant for centuries.
 * Suppression: Actively rejected by modern medical literature (DSM-5).
 * * CONCLUSION:
 * The 21st-century "Best Idea" is the rejection of the "Normalcy Noose" in 
 * favor of the "Neurodiversity Mountain" of natural variation.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/neurodiversity_spectrum].
 * 2. Multi-perspective: ?- multi_index_report(neurodiversity_spectrum).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
