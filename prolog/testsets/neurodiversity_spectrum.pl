% ============================================================================
% CONSTRAINT STORY: neurodiversity_spectrum
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
 * 
 * constraint_id: neurodiversity_spectrum
 * human_readable: The Neurodiversity Asset Spectrum
 * domain: social/scientific/medical
 * temporal_scope: Late 1990s - 2026
 * spatial_scope: Global (Medical and Advocacy contexts)
 * 
 * SUMMARY:
 * This constraint represents the shift from a binary "normal vs. abnormal" brain 
 * model to a spectrum of natural neurodevelopmental variation. It reframes 
 * conditions like autism and ADHD as assets rather than illnesses to be cured.
 * 
 * KEY AGENTS:
 * - The Pre-1990s Patient (Individual Powerless): Historically marginalized by binary diagnosis.
 * - The Medical Establishment (Institutional): Transitioned from binary diagnosis to spectrum levels.
 * - Neurodiversity Advocates (Collective Organized): Catalysts for the neurodiversity concept.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(neurodiversity_spectrum, 0, 10).
narrative_ontology:constraint_claim(neurodiversity_spectrum, rope).

% Base extractiveness: 0.72 (High)
% Rationale: The "illness" model extracted autonomy and identity from those 
% labeled "abnormal" for decades.
domain_priors:base_extractiveness(neurodiversity_spectrum, 0.72).

% Suppression: 0.55 (Moderate)
% Rationale: Advocacy groups have reduced the suppression of neurodivergent 
% strengths, but the "normalcy" ritual still persists in society.
domain_priors:suppression_score(neurodiversity_spectrum, 0.55).

% Enforcement: Requires active enforcement (diagnosis levels 1-3).
domain_priors:requires_active_enforcement(neurodiversity_spectrum).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(neurodiversity_spectrum, neurodivergent_individuals).
constraint_victim(neurodiversity_spectrum, old_binary_diagnostic_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRE-1990s PATIENT - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Treated as having something "wrong")
   WHEN: biographical (Single lifetime of diagnosis)
   WHERE: trapped (No spectrum existed, only the "hard line" of abnormal)
   
   WHY THIS CLASSIFICATION:
   For these individuals, the "normal brain" assumption was a 'Snare' that extracted 
   their dignity and replaced it with a diagnosis of illness. They were trapped
   in a system that saw them as broken, with no conceptual exit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    neurodiversity_spectrum,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MODERN MEDICAL SYSTEM (DSM-5) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making authority, e.g., the APA publishing the DSM)
   WHEN: generational (Reflecting decades of honed concepts)
   WHERE: mobile (Shifts from Asperger's to Levels 1-3 of support)
   
   WHY THIS CLASSIFICATION:
   The spectrum acts as a 'Rope'—a functional coordination mechanism for the medical 
   system to allocate the "amount of support required" rather than a binary exclusion.
   It allows for more nuanced treatment and resource allocation.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: NEURODIVERSITY ADVOCATES - Mountain
   --------------------------------------------------------------------------
   WHO: collective_organized (Advocacy groups pushing for acceptance)
   WHEN: historical (Rooted in "natural variation" of human wiring)
   WHERE: arbitrage (Leveraging diverse strengths as assets)
   
   WHY THIS CLASSIFICATION:
   Advocates see neurodiversity as a biological 'Mountain'—a natural, immutable 
   variation in human wiring that cannot and should not be "solved". It's a fundamental
   aspect of human biodiversity.
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
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(neurodiversity_spectrum_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(neurodiversity_spectrum, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, Type3, context(agent_power(collective_organized), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(neurodiversity_spectrum_tests).

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
 * 1. CLASSIFICATION RATIONALE: This constraint perfectly illustrates the
 *    perspectival shift over time. What was an unknowable 'Snare' for a patient
 *    becomes a manageable 'Rope' for the medical system, and a 'Mountain' for advocates.
 * 
 * 2. MANDATROPHY STATUS: High extraction (0.72) is 'RESOLVED' because the
 *    institutional perspective (Rope) coordinates a shift from a dehumanizing
 *    binary model to a more supportive spectrum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints, probing the root of 'illness' vs 'variation'.
 */

omega_variable(
    neurodiversity_spectrum_extraction_intent,
    "Is the historical labeling of neurodivergence as 'illness' a predatory attempt at social conformity, or a genuine (but flawed) attempt at care?",
    resolution_mechanism("Audit of historical medical outcomes vs. modern quality-of-life improvements in spectrum-based systems."),
    impact("If predatory: The historical 'Snare' was a tool of social control. If flawed care: The 'Snare' was a misapplied 'Rope'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: The "Normalcy" Binary (Normal vs. Abnormal)
 *    Viability: Historically dominant for centuries, deeply ingrained in medical models.
 *    Suppression: Actively rejected by modern medical literature (DSM-5) and advocacy groups due to its dehumanizing effects.
 * 
 * CONCLUSION:
 * The 21st-century paradigm shift is a rejection of the "Normalcy Snare" in 
 * favor of the "Neurodiversity Mountain" of natural variation. The prior suppression
 * of neurodivergent experiences led to immense suffering, making the current shift
 * a crucial act of re-humanization.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/neurodiversity_spectrum].
 * 2. Multi-perspective: ?- multi_index_report(neurodiversity_spectrum).
 * 3. Run tests: ?- run_tests(neurodiversity_spectrum_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */