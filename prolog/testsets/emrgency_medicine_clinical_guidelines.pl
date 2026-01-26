% ============================================================================
% CONSTRAINT STORY: emergency_medicine_clinical_guidelines
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Indexical Relativity in Clinical Medicine
% ============================================================================

:- module(constraint_em_clinical_guidelines, []).

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
 * * constraint_id: em_clinical_guidelines
 * human_readable: Emergency Medicine Clinical Guidelines
 * domain: medical/legal/institutional
 * temporal_scope: Modern EBM (Evidence-Based Medicine) Era (2000-2026)
 * spatial_scope: National (Hospital-specific implementations)
 * * SUMMARY:
 * Clinical guidelines (e.g., PECARN for pediatric head trauma or Ottawa Ankle Rules) 
 * serve as decision-support tools. However, their ontological status shifts 
 * depending on the agent's exposure to litigation and systemic throughput pressure.
 * * KEY AGENTS:
 * - ER Physician: The frontline agent balancing diagnostic speed and liability.
 * - Medical Director/CMS: The institutional agent monitoring "compliance" metrics.
 * - Legal Defense/Plaintiff: Analytical observers who use guidelines as "Standard of Care" weapons.
 * * NARRATIVE ARC:
 * Guidelines are introduced as Ropes to reduce unnecessary testing. Under the pressure 
 * of a crowded ER, they harden into Mountains (perceived as natural law) or Nooses 
 * (asymmetric liability traps if deviated from).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(em_practice_v2, 0, 10).
narrative_ontology:constraint_claim(em_clinical_guidelines, rope).

% Base extractiveness score (0.6: Extraction of cognitive autonomy and time)
% Rationale: Compliance auditing extracts clinician time for data entry.
domain_priors:base_extractiveness(em_clinical_guidelines, 0.6).

% Suppression score (0.5: Moderate)
% Rationale: Clinical intuition is often devalued in favor of "algorithmic" adherence.
domain_priors:suppression_score(em_clinical_guidelines, 0.5).

domain_priors:requires_active_enforcement(em_clinical_guidelines).

% Metrics for DR-Audit
narrative_ontology:constraint_metric(em_clinical_guidelines, extractiveness, 0.6).
narrative_ontology:constraint_metric(em_clinical_guidelines, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(em_clinical_guidelines, [hospital_billing, insurance_payers, legal_teams]).
constraint_victim(em_clinical_guidelines, [frontline_physicians, edge_case_patients]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ER Attending Physician - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (within the hospital hierarchy)
   WHEN: immediate (the current shift)
   WHERE: trapped (must document within the EMR framework)
   SCOPE: local (the specific patient encounter)
   
   WHY THIS CLASSIFICATION:
   In a high-acuity environment, guidelines function as a Snare. They create 
   asymmetric liability; following them provides minimal "reward," but 
   deviating from them—even for sound clinical reasons—creates a permanent 
   legal vulnerability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    em_clinical_guidelines,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(em_clinical_guidelines, E),
    E > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Hospital Quality Committee - Rope
   --------------------------------------------------------------------------
   WHO: institutional (rule-making body)
   WHEN: generational (long-term outcomes)
   WHERE: mobile (can adopt/reject different guidelines)
   SCOPE: national (benchmarking against other hospitals)
   
   WHY THIS CLASSIFICATION:
   The committee sees a Rope. Guidelines are coordination mechanisms used to 
   standardize care, reduce variance, and lower costs across a population. 
   The functional benefit to the system justifies the constraint.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    em_clinical_guidelines,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Expert Witness (Legal) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (observer/evaluator)
   WHEN: historical (analyzing a past event)
   WHERE: analytical (not subject to ER pressure)
   SCOPE: regional (state-level legal standards)
   
   WHY THIS CLASSIFICATION:
   In the courtroom, guidelines are presented as Mountains. They are treated 
   as immutable "Standards of Care" that represent the only valid path, 
   ignoring the real-time fluidity of the clinical environment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    em_clinical_guidelines,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(regional)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(em_clinical_guidelines_tests).

test(multi_perspective_em) :-
    % Physician (Snare) vs Committee (Rope) vs Legal (Mountain)
    constraint_indexing:constraint_classification(em_clinical_guidelines, Snare, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(em_clinical_guidelines, Rope, context(institutional, generational, mobile, national)),
    constraint_indexing:constraint_classification(em_clinical_guidelines, Mountain, context(analytical, historical, analytical, regional)),
    Snare \= Rope,
    Rope \= Mountain.

test(throughput_pressure_scaling) :-
    % As time horizon shortens (immediate shift), extractiveness of documentation feels higher.
    ContextImmediate = context(individual_powerless, immediate, trapped, local),
    constraint_indexing:extractiveness_for_agent(em_clinical_guidelines, ContextImmediate, Score),
    Score > 0.5.

:- end_tests(em_clinical_guidelines_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SELECTION: Chose the "Expert Witness" as the analytical 
 * Mountain-maker to show how hindsight bias freezes fluid Ropes into rigid Mountains.
 * * 2. OMEGAS:
 * omega_variable(guideline_drift,
 * "Do guidelines track biological truth or legal safety?",
 * resolution_mechanism("Analysis of guideline changes following major malpractice settlements"),
 * impact("If legal: the system is a pure Snare disguised as medicine."),
 * confidence_without_resolution(medium)
 * ).
 * * 3. AMBIGUITIES: The "Standard of Care" is a legal fiction that lacks a 
 * precise medical definition, which I resolved by assigning it to the 
 * 'analytical' power level during retrospective review.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Bayesian Clinical Decision Support
 * Viability: Systems that provide % risk rather than "Do X/Don't do X".
 * Suppression: High. "Black Box" AI or complex math is often rejected by 
 * legal systems that demand binary "Reasonable Physician" standards.
 * * CONCLUSION:
 * The legal requirement for a binary "standard" forces the Rope (the 
 * guideline) to become a Snare for the clinician who sees the nuance 
 * the guideline ignores.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * ?- [constraint_em_clinical_guidelines].
 * ?- constraint_indexing:multi_index_report(em_clinical_guidelines).
 * ?- run_tests(em_clinical_guidelines_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
