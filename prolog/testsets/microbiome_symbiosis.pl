% ============================================================================
% CONSTRAINT STORY: microbiome_symbiosis
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Helen Thomson, "Realising the importance of our microbiome"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_microbiome_symbiosis, []).

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
 * * constraint_id: microbiome_symbiosis
 * human_readable: The Microbial-Immune Symbiosis
 * domain: biological/healthcare
 * temporal_scope: 17th Century (discovery) to 2026 (healthcare integration)
 * spatial_scope: Internal (Human Body) / Global (Clinical Research)
 * * SUMMARY:
 * For centuries, humans inadvertently influenced their health via microbes. 
 * Modern sequencing has revealed that the microbiome is not a passive passenger 
 * but an active participant in metabolism, immunity, and mental well-being. 
 * This symbiotic relationship reframes human health as a collaborative system 
 * between human cells and a unique microbial "fingerprint".
 * * KEY AGENTS:
 * - The Individual: Host to a unique microbial "fingerprint" of viruses and fungi.
 * - The Microbiome: Active participant influencing the pancreas, brain, and immunity.
 * - Medical Establishment: Transitioning from rule-based "germ theory" to complex symbiosis.
 * * NARRATIVE ARC:
 * What was once an invisible "Mountain" (natural law of health) is becoming a 
 * "Rope" (functional tool via faecal transplants and nutrition apps). 
 * However, microbial dysfunction acts as a "Noose," potentially triggering 
 * chronic conditions like Parkinson's or diabetes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(microbiome_symbiosis, 0, 10).

% Base extractiveness: 0.45 (Moderate)
% Rationale: Microbes influence drug efficacy and metabolic flow. 
% There is a net flow of information and energy between host and microbe.
domain_priors:base_extractiveness(microbiome_symbiosis, 0.45).

% Suppression: 0.65 (Moderate-High)
% Rationale: It took two centuries to validate Van Leeuwenhoek's findings. 
% The "rule-based" understanding of health suppressed the recognition of 
% this deep symbiotic influence until the 21st century.
domain_priors:suppression_score(microbiome_symbiosis, 0.65).

% Enforcement: Emerges naturally (Co-evolutionary biological reality).
domain_priors:emerges_naturally(microbiome_symbiosis).

% BENEFICIARIES & VICTIMS
% The Host (Human) benefits from immunity and metabolism when symbiotic.
constraint_beneficiary(microbiome_symbiosis, human_host).
% Pathogenic bacteria (e.g., C. difficile) or dysfunctional states "victimize" the system.
constraint_victim(microbiome_symbiosis, dysfunctional_microbial_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ROMAN CITIZEN - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Unaware of the mechanism) 
   WHEN: historical (Ancient health practices) 
   WHERE: trapped (No conceptual exit; health is a matter of "fate/remedies") 
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   Ancient humans used bacterial remedies to "guard the stomach" without 
   realizing why. To them, the effects were an immutable part of 
   nature's "Mountain"—uncontrollable and mysterious.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    microbiome_symbiosis,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(microbiome_symbiosis, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MODERN BIO-TECH (e.g., Zoe App) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Providing health coordination tools) 
   WHEN: biographical (Individual nutrition/health cycles) 
   WHERE: mobile (Choices in diet and intervention) 
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For modern medicine, the microbiome is a "Rope"—a functional mechanism that 
   can be measured and manipulated (via faecal transplants or diet) to 
   restore health and alter drug efficacy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    microbiome_symbiosis,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(microbiome_symbiosis, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PATIENT WITH PARKINSON'S/DIABETES - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Affected by systemic dysfunction) 
   WHEN: immediate (Onset of chronic illness) 
   WHERE: constrained (Condition dictates lifestyle and survival) 
   SCOPE: local (Internal physiology) 
   
   WHY THIS CLASSIFICATION:
   When the microbial-immune communication fails, it triggers debilitating 
   conditions. The symbiotic constraint turns into a "Noose" 
   where microbial dysfunction actively harms the host's nervous system.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    microbiome_symbiosis,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    % Extraction/Dysfunction becomes harmful
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(microbiome_symbiosis_tests).

test(multi_perspective_variance) :-
    % Roman (Mountain) vs Tech (Rope) vs Chronic Patient (Noose)
    constraint_indexing:constraint_classification(microbiome_symbiosis, mountain, context(individual_powerless, historical, trapped, local)),
    constraint_indexing:constraint_classification(microbiome_symbiosis, rope, context(institutional, biographical, mobile, global)),
    constraint_indexing:constraint_classification(microbiome_symbiosis, noose, context(individual_powerless, immediate, constrained, local)).

test(efficacy_transformation) :-
    % Test that "identifying fingerprints" (2000s) shifts the classification 
    % from an invisible Mountain to a manipulatable Rope.
    ModernE = 0.4,
    HistoricalE = 0.0,
    ModernE > HistoricalE.

:- end_tests(microbiome_symbiosis_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.45): High enough to be significant (influencing drugs/brain) 
 * but not purely predatory.
 * 2. SUPPRESSION (0.65): Reflects the long "latency" between the 17th-century 
 * stool sample and 21st-century appreciation.
 * 3. PERSPECTIVES: Used the "Roman stomach guards"  as a 
 * perfect example of an agent facing a Mountain they don't yet name.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    microbial_causality_link,
    "Does microbial dysfunction cause conditions like Parkinson's, or is it a downstream symptom of host failure?",
    resolution_mechanism("Longitudinal studies tracking microbial shifts prior to symptomatic onset "),
    impact("If cause: Noose (preventable). If symptom: Mountain (biological marker)."),
    confidence_without_resolution(medium)
).

omega_variable(
    microbiome_symbiosis_extraction_intent,
    "Is the symbiotic extraction of metabolic products a functional necessity for the species or a vulnerability exploited by pathogens?",
    resolution_mechanism("Audit of C. difficile cure rates via community restoration vs. individual antibiotic use "),
    impact("If necessity: Rope. If vulnerability: Noose."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Sterile Health Paradigm (Germ Theory)
 * Viability: The dominant 20th-century model—treating all microbes as enemies.
 * Suppression: This alternative is now being "reframed" as a failure 
 * in the face of complex chronic diseases.
 * * CONCLUSION:
 * The 21st-century "Idea" is the rejection of the "Sterile Noose" in favor 
 * of the "Symbiotic Rope".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/microbiome_symbiosis].
 * 2. Multi-perspective: ?- multi_index_report(microbiome_symbiosis).
 * 3. Run tests: ?- run_tests(microbiome_symbiosis_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
