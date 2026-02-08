% ============================================================================
% CONSTRAINT STORY: microbiome_symbiosis
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Helen Thomson, "Realising the importance of our microbiome"
% ============================================================================

:- module(constraint_microbiome_symbiosis, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * constraint_id: microbiome_symbiosis
 * human_readable: The Microbial-Immune Symbiosis
 * domain: biological/healthcare
 * temporal_scope: 17th Century to 2026
 * spatial_scope: Internal (Human Body) / Global (Clinical Research)
 * 
 * SUMMARY:
 * Modern sequencing has revealed that the microbiome is not a passive passenger 
 * but an active participant in metabolism, immunity, and mental well-being. 
 * This symbiotic relationship reframes human health as a collaborative system 
 * between human cells and a unique microbial "fingerprint".
 * 
 * KEY AGENTS:
 * - The Ancient Human (Individual Powerless): Unaware of the underlying microbial mechanism.
 * - The Modern Bio-Tech Company (Institutional): Manipulates the microbiome as a health tool.
 * - The Chronic Patient (Individual Powerless): Suffers from a dysfunctional symbiotic relationship.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(microbiome_symbiosis, 0, 10).
narrative_ontology:constraint_claim(microbiome_symbiosis, tangled_rope).

% Base extractiveness: 0.45 (Moderate)
% Rationale: Microbes influence drug efficacy and metabolic flow. There is a 
% net flow of information and energy between host and microbe, which can be
% beneficial (symbiosis) or harmful (dysbiosis).
domain_priors:base_extractiveness(microbiome_symbiosis, 0.45).

% Suppression: 0.65 (Moderate-High)
% Rationale: It took two centuries to validate Van Leeuwenhoek's findings. 
% The "germ theory" model of health suppressed the recognition of this deep 
% symbiotic influence until the 21st century.
domain_priors:suppression_score(microbiome_symbiosis, 0.65).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(microbiome_symbiosis, extractiveness, 0.45).
narrative_ontology:constraint_metric(microbiome_symbiosis, suppression_requirement, 0.65).

% Enforcement: Emerges naturally from co-evolution.
domain_priors:emerges_naturally(microbiome_symbiosis).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(microbiome_symbiosis, human_host). % When in symbiosis
constraint_victim(microbiome_symbiosis, human_host). % When in dysbiosis

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ANCIENT ROMAN - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Unaware of the mechanism) 
   WHEN: historical (Ancient health practices) 
   WHERE: trapped (No conceptual exit; health is a matter of "fate") 
   
   WHY THIS CLASSIFICATION:
   Ancient humans used bacterial remedies to "guard the stomach" without 
   realizing why. To them, the effects were an immutable part of nature's 
   'Mountain'—uncontrollable, mysterious, and a matter of divine or natural law.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    microbiome_symbiosis,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MODERN BIO-TECH (e.g., Zoe App) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Providing health coordination tools) 
   WHEN: biographical (Individual nutrition/health cycles) 
   WHERE: mobile (Choices in diet and intervention like transplants) 
   
   WHY THIS CLASSIFICATION:
   For modern medicine, the microbiome is a 'Rope'—a functional mechanism that 
   can be measured and manipulated (via faecal transplants or diet) to 
   restore health and alter drug efficacy. It's a powerful tool.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PATIENT WITH PARKINSON'S/DIABETES - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Affected by systemic dysfunction) 
   WHEN: immediate (Onset of chronic illness) 
   WHERE: trapped (Condition dictates lifestyle and survival) 
   
   WHY THIS CLASSIFICATION:
   When the microbial-immune communication fails, it can trigger debilitating 
   conditions. The symbiotic constraint turns into a 'Snare' where the host's 
   own internal ecosystem actively harms them, creating a trap of chronic illness.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    microbiome_symbiosis,
    snare,
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

:- begin_tests(microbiome_symbiosis_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(microbiome_symbiosis, Type1, context(agent_power(powerless), historical, _, _)),
    constraint_indexing:constraint_classification(microbiome_symbiosis, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(microbiome_symbiosis, Type3, context(agent_power(powerless), immediate, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

:- end_tests(microbiome_symbiosis_tests).

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
 *    perspectival shifts over time and with knowledge. What was an unknowable
 *    'Mountain' to an ancient becomes a manipulable 'Rope' for modern science.
 *    When the system breaks, it becomes an internal 'Snare' for the patient.
 * 
 * 2. TANGLED ROPE: The overall constraint is a 'Tangled Rope'. The microbiome is
 *    a biological system we must coordinate with (Rope), but its dysfunction
 *    can be a source of severe, life-altering extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the direction of causality in chronic illness.
 */

omega_variable(
    microbial_causality_link,
    "Does microbial dysfunction cause conditions like Parkinson's, or is it a downstream symptom of host failure?",
    resolution_mechanism("Longitudinal studies tracking microbial shifts prior to symptomatic onset in at-risk populations."),
    impact("If cause: The 'Snare' is a primary driver of the illness. If symptom: It's merely a biomarker of an underlying 'Mountain' of biological destiny."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Sterile Health Paradigm (Germ Theory)
 *    Viability: Was the dominant 20th-century model—treating all microbes as enemies to be eliminated.
 *    Suppression: This model is being actively reframed and partially rejected as it fails to explain
 *    the rise of chronic autoimmune and metabolic diseases.
 * 
 * CONCLUSION:
 * The 21st-century paradigm shift is a rejection of the "Sterile Snare" (kill all germs)
 * in favor of cultivating a "Symbiotic Rope" (manage the ecosystem).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/microbiome_symbiosis].
 * 2. Multi-perspective: ?- multi_index_report(microbiome_symbiosis).
 * 3. Run tests: ?- run_tests(microbiome_symbiosis_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */