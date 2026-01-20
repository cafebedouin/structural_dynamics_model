% ============================================================================
% CONSTRAINT STORY: burden_of_proof_legal_criminal
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: General Jurisprudence / Common Law Criminal Procedure
% ============================================================================

:- module(constraint_legal_burden_of_proof, []).

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
 * * constraint_id: burden_of_proof_legal_criminal
 * human_readable: Beyond a Reasonable Doubt (Criminal Legal Burden)
 * domain: political/social
 * temporal_scope: Modern Era (Common Law Jurisdictions)
 * spatial_scope: National
 * * SUMMARY:
 * In criminal law, the burden of proof rests entirely on the prosecution to 
 * prove every element of a crime "beyond a reasonable doubt." This is a 
 * foundational constraint designed to protect individuals from the 
 * overwhelming power of the state, codifying the principle that it is better 
 * for ten guilty persons to escape than for one innocent to suffer.
 * * KEY AGENTS:
 * - The Defendant: Individual powerless; subject to the state's accusation and potential loss of liberty.
 * - The Prosecutor: Institutional; represents the state and carries the weight of proving the charge.
 * - The Juror: Analytical; the immediate arbiter of whether the burden has been met.
 * * NARRATIVE ARC:
 * Unlike the engineering variety which extracts safety to meet schedules, 
 * the legal burden acts as a Rope (coordination) for the state but a 
 * Mountain (unchangeable protection) for the accused. It defines the 
 * "rules of the game" for civil society to ensure the state's power is 
 * not used as a Noose against its citizens.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(criminal_trial_sequence, 0, 10).
narrative_ontology:constraint_claim(burden_of_proof_legal_criminal, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.1. Low extraction; the burden is designed to *prevent* % extraction (wrongful imprisonment) rather than facilitate it.
domain_priors:base_extractiveness(burden_of_proof_legal_criminal, 0.1).

% Suppression score (0.0-1.0)
% Rationale: 0.2. Low suppression; the legal system (ideally) allows for 
% all relevant evidence and alternative theories of the crime to be presented.
domain_priors:suppression_score(burden_of_proof_legal_criminal, 0.2).

% Enforcement requirements
% Requires active enforcement: Judges must instruct juries, and appellate 
% courts must review if the burden was properly applied.
domain_priors:requires_active_enforcement(burden_of_proof_legal_criminal).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, extractiveness, 0.1).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(burden_of_proof_legal_criminal, civil_liberties).
constraint_beneficiary(burden_of_proof_legal_criminal, the_accused).
constraint_victim(burden_of_proof_legal_criminal, prosecutorial_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DEFENDANT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Facing the state's resources; cannot change the law.
   WHEN: immediate - The duration of the trial and potential sentencing.
   WHERE: trapped - Bound by the court's jurisdiction and the finality of the verdict.
   SCOPE: local - Impact on their specific life and liberty.
   
   WHY THIS CLASSIFICATION:
   For the defendant, the high burden of proof is an unchangeable Mountain 
   of protection. They do not have to "do" anything for it to exist; it is 
   a pre-existing feature of the legal landscape that the state must climb 
   over to reach them.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_legal_criminal,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(burden_of_proof_legal_criminal),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROSECUTOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making and rule-applying power representing the state.
   WHEN: biographical - Careers built on the successful navigation of trials.
   WHERE: mobile - Can choose which cases to bring based on evidence strength.
   SCOPE: national - Operating within a country's legal framework.
   
   WHY THIS CLASSIFICATION:
   For the prosecutor, the burden of proof is a Rope—a coordination mechanism. 
   It tells them how much evidence they must gather and how to structure 
   their narrative to gain a conviction. It is a functional tool that 
   standardizes the state's exercise of power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_legal_criminal,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(burden_of_proof_legal_criminal, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE JUROR - Analytical
   --------------------------------------------------------------------------
   
   WHO: analytical - Temporary citizen-judge tasked with weighing the evidence.
   WHEN: immediate - Limited to the evidence presented during the trial.
   WHERE: analytical - Not personally trapped by the outcome, but morally bound.
   SCOPE: local - A single specific case and defendant.
   
   WHY THIS CLASSIFICATION:
   The juror views the burden as a Rope (standard) they must use to measure 
   the truth. It is the objective scale upon which they place the 
   prosecution's case.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_legal_criminal,
    rope,
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(analytical),
        spatial_scope(local)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DEFENDANT (ACCUSED) - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - Subject to the overwhelming power of the state.
   WHEN: immediate - The duration of the trial and pre-trial detention.
   WHERE: trapped - Bound by the legal process with no meaningful exit.
   SCOPE: local - The individual's life, liberty, and financial solvency.
   
   WHY THIS CLASSIFICATION:
   The "Noose" is the process of prosecution. While "Beyond a Reasonable Doubt"
   is a protection, the individual must still survive the state's extraction of 
   their resources, time, and reputation. For the defendant, the high burden 
   is not a "tool" they use, but a narrow opening in a trap that otherwise 
   seeks their total suppression within the penal system.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_legal_criminal,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(burden_of_proof_legal_criminal, S),
    S > 0.3.

% Explicit priors reflecting the high stakes and resource cost for the defendant.
domain_priors:base_extractiveness(burden_of_proof_legal_criminal, 0.4).
domain_priors:suppression_score(burden_of_proof_legal_criminal, 0.5).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(legal_burden_of_proof_tests).

test(multi_perspective_variance) :-
    % Defendant (Mountain) vs Prosecutor (Rope)
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, T2, context(institutional, biographical, mobile, national)),
    T1 \= T2.

test(anti_extraction_nature) :-
    % High extractiveness would imply a Noose; legal burden should be low.
    domain_priors:base_extractiveness(burden_of_proof_legal_criminal, E),
    E < 0.2.

:- end_tests(legal_burden_of_proof_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. DOMAIN ISOLATION: This is a **legal/social** burden of proof. It differs 
 * from the **engineering** variety because it is designed to increase 
 * friction against state action (protecting the subject), whereas the 
 * Challenger-style burden reversal decreased friction against risky 
 * activity (extracting safety).
 * * 2. CLASSIFICATION: Primarily labeled as 'Rope' because it is the 
 * quintessential coordination tool for a fair society.
 * * 3. EXTRACTIVENESS: Set to 0.1 because its primary function is protective.
 */

omega_variable(
    subjective_certainty_threshold,
    "Is 'Beyond a Reasonable Doubt' an objective mathematical threshold or 
     an irreducible psychological state of the juror?",
    resolution_mechanism("Comparative neuro-legal studies on juror brain states during verdict deliberation"),
    impact("If Objective: The burden is a Rope. If Subjective: The burden 
            is a persistent psychological Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Preponderance of the Evidence
 * Viability: Used in civil law (51% certainty). Highly viable but 
 * rejected in criminal law because the stakes (liberty) are too high.
 * Suppression: Suppressed by Constitutional protections and centuries 
 * of common law tradition.
 * * CONCLUSION:
 * The existence of lower burdens (preponderance, clear and convincing) 
 * demonstrates that the high criminal burden is an intentional 
 * institutional choice (Rope) designed to safeguard the individual.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [burden_of_proof_legal_criminal].
% 2. Analyze: ?- multi_index_report(burden_of_proof_legal_criminal).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
