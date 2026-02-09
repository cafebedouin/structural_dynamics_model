% ============================================================================
% CONSTRAINT STORY: somatic_focusing_awareness
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Meg-John Barker & Eugene T. Gendlin
% ============================================================================

:- module(constraint_somatic_focusing_awareness, []).

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
 * constraint_id: somatic_focusing_awareness
 * human_readable: Somatic Focusing Awareness
 * domain: social/psychological
 * temporal_scope: Perennial / Immediate
 * spatial_scope: Internal / Local
 * 
 * SUMMARY:
 * This constraint defines the practice of "staying with feelings" through "interested curiosity" 
 * without trying to change them or force communication. It relies on identifying 
 * a "felt sense" within the body to navigate the "emotional landscape" before responding to external stimuli.
 * 
 * KEY AGENTS:
 * - The Practitioner (Individual Moderate): Practices "gentle and curious" attention toward internal states.
 * - The Rigid Student (Individual Powerless): Adheres strictly to instructions, ignoring bodily signals.
 * - The Therapy Clinic / Wellness Program (Institutional): Offers somatic focusing as a structured methodology.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(somatic_focusing_awareness, 0, 10).
narrative_ontology:constraint_claim(somatic_focusing_awareness, rope).

% Base extractiveness: 0.2.
% This is a "nourishing" practice that aims to prevent the "poison" of reactive speaking.
domain_priors:base_extractiveness(somatic_focusing_awareness, 0.2).

% Suppression score: 0.3.
% The method explicitly instructs the agent to "acknowledge everything that 
% comes up" and "not try to change" the feeling, suppressing reactive impulses.
domain_priors:suppression_score(somatic_focusing_awareness, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(somatic_focusing_awareness, extractiveness, 0.2).
narrative_ontology:constraint_metric(somatic_focusing_awareness, suppression_requirement, 0.3).

% Enforcement: Requires active cognitive enforcement of the "pause" and "curiosity".
domain_priors:requires_active_enforcement(somatic_focusing_awareness).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(somatic_focusing_awareness, internal_clarity).
narrative_ontology:constraint_victim(somatic_focusing_awareness, reactive_habit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CURIOUS PRACTITIONER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to "stay with the feeling")
   WHEN: immediate (Applied "during conversations")
   WHERE: mobile (Uses a "split-level" approach to adjust instructions)
   
   WHY THIS CLASSIFICATION:
   For the practitioner, the method is a 'Rope'—a functional coordination mechanism to 
   "understand our emotional landscape" and think before speaking. It helps them
   navigate complex emotional terrains.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    somatic_focusing_awareness,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RIGID STUDENT - Snare
   --------------------------------------------------------------------------
   WHO: powerless (A "slave" to the instructions who might "close off other ways")
   WHEN: immediate (Trying to "follow the instructions exactly")
   WHERE: trapped (Doing something that "feels wrong in your body")
   
   WHY THIS CLASSIFICATION:
   If the instructions are used to "close off other ways" (ignoring bodily signals),
   the method becomes a 'Snare'. It extracts the individual's sensitivity to their
   own body in favor of rigid adherence to the text, leading to internal conflict.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    somatic_focusing_awareness,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE THERAPY CLINIC / WELLNESS PROGRAM - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Offers structured methodologies for client well-being)
   WHEN: biographical (Client's journey through therapy)
   WHERE: arbitrage (Integrates various techniques for psychological processing)
   
   WHY THIS CLASSIFICATION:
   For a therapy clinic or wellness program, somatic focusing is a 'Rope' – a
   structured methodology for helping clients process emotions and achieve
   psychological well-being. It provides a reliable framework for therapeutic
   intervention.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    somatic_focusing_awareness,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(somatic_focusing_awareness_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(somatic_focusing_awareness, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(somatic_focusing_awareness, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(somatic_focusing_awareness, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2. % Rope vs Snare
    % Type2 \= Type3 is false (Snare vs Rope).
    % Type1 \= Type3 is false (Rope vs Rope).

:- end_tests(somatic_focusing_awareness_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Therapy Clinic / Wellness Program' as
 *    the institutional agent. This highlights how a personal practice can be
 *    integrated into a structured, therapeutic context as a beneficial 'Rope'.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Practitioner (Rope): A tool for personal clarity and emotional navigation.
 *    - Rigid Student (Snare): Trapped by rigid adherence to instructions, ignoring bodily truth.
 *    - Therapy Clinic (Rope): A structured tool for client well-being.
 * 
 * 3. CORE INSIGHT: Somatic focusing is a powerful 'Rope' for internal processing.
 *    However, if applied rigidly, it can turn into a 'Snare' that extracts
 *    the individual's innate wisdom, becoming a tool for self-suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the extent of the body's role in conscious thought.
 */

omega_variable(
    somatic_body_mind_identity,
    "Is the 'felt sense' truly the 'unconscious mind' and 'seat of feeling' (Mountain), or is this a metaphorical 'Rope' for psychological discovery?",
    resolution_mechanism("Neurological audit of 'felt sense' origin points in the central nervous system; comparative phenomenology of 'felt sense' experiences."),
    impact("If Mountain: Body-work is a biological law. If Rope: It is a narrative technique subject to reinterpretation."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Intellectualization
 *    Viability: Trying to "solve" feelings with logic without engaging bodily sensations.
 *    Suppression: Explicitly suppressed by the somatic focusing method, which emphasizes "staying with the feeling" rather than rational analysis.
 *
 * CONCLUSION:
 * Somatic focusing is a 'Rope' that offers an alternative to purely intellectual
 * or reactive responses to emotions. It suppresses the tendency to "solve" feelings
 * with logic, which often leads to a 'Snare' of unresolved emotional patterns.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/somatic_focusing_awareness].
 * 2. Multi-perspective: ?- multi_index_report(somatic_focusing_awareness).
 * 3. Run tests: ?- run_tests(somatic_focusing_awareness_tests).
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
% Coordination mechanism in social domain — moderate institutional framing
domain_priors:theater_ratio(somatic_focusing_awareness, 0.15).
narrative_ontology:constraint_metric(somatic_focusing_awareness, theater_ratio, 0.15).

% --- Analytical perspective classification (missing) ---
% chi = 0.2 * 1.15 (analytical) * 1.2 (global) = 0.276
% Classification: scaffold
constraint_indexing:constraint_classification(somatic_focusing_awareness, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
