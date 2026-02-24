% ============================================================================
% CONSTRAINT STORY: automatic_enrollment_defaults
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Thaler & Sunstein (2008). Nudge. / Pension Protection Act (2006).
% ============================================================================

:- module(constraint_automatic_enrollment_defaults, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * constraint_id: automatic_enrollment_defaults
 * human_readable: Automatic Enrollment Defaults (Nudge Theory)
 * domain: economic/social/political
 * temporal_scope: Biographical (decades-long impact)
 * spatial_scope: National
 * 
 * SUMMARY:
 * Automatic enrollment is a choice architecture where individuals are placed 
 * into a program (like a 401k or organ donation) by default. It utilizes human
 * cognitive biases like "Status Quo Bias" to dramatically increase participation rates.
 * 
 * KEY AGENTS:
 * - The Uninformed Employee (Individual Powerless): Passively enrolled without active choice.
 * - The Choice Architect (Institutional): The employer or policymaker who designs the default.
 * - The Libertarian Critic (Analytical): Argues that defaults infringe on pure freedom of choice.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(automatic_enrollment_defaults, 0, 10).
narrative_ontology:constraint_claim(automatic_enrollment_defaults, rope).

% Base extractiveness score (0.05)
% Extremely low; it is designed to retain value for the individual 
% by overcoming their own cognitive friction and inertia.
domain_priors:base_extractiveness(automatic_enrollment_defaults, 0.05).

% Suppression score (0.1)
% Low; while the default is passive, the opt-out mechanism is usually simple.
domain_priors:suppression_score(automatic_enrollment_defaults, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, 0.05).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from cognitive biases.
domain_priors:emerges_naturally(automatic_enrollment_defaults).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(automatic_enrollment_defaults, employees).
constraint_victim(automatic_enrollment_defaults, pure_choice_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE UNINFORMED EMPLOYEE - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Lacks financial literacy or attention)
   WHEN: immediate (The moment of hiring)
   WHERE: trapped (Passively placed into a default that may be sub-optimal)
   
   WHY THIS CLASSIFICATION:
   For an employee who doesn't understand the system, the default is a 'Snare'.
   A major financial decision is made for them without their active consent. They
   might be enrolled in a fund with fees that are too high or a contribution
   rate that is too low, strangling their potential long-term wealth.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    automatic_enrollment_defaults,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE EMPLOYER / CHOICE ARCHITECT - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Designing the HR/pension system)
   WHEN: generational (Managing a workforce over decades)
   WHERE: mobile (Can set the default to opt-in or opt-out)
   
   WHY THIS CLASSIFICATION:
   For the employer, automatic enrollment is a 'Rope'. It is a powerful, low-cost
   tool to coordinate employee behavior toward a beneficial outcome (higher retirement
   savings), increasing employee well-being and fulfilling fiduciary duties.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    automatic_enrollment_defaults,
    rope,
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(national))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BEHAVIORAL ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observing human behavior)
   WHEN: historical (Across countless studies)
   WHERE: analytical (Universal cognitive principle)
   
   WHY THIS CLASSIFICATION:
   The observer sees the "Default Effect" as a 'Mountain' of behavioral 
   psychology. Humans are hard-wired to prefer the status quo. Policy must be
   built upon this mountain; there is no such thing as a "neutral" choice architecture.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    automatic_enrollment_defaults,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(automatic_enrollment_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(automatic_enrollment_tests).

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
 * 1. PERSPECTIVE SELECTION: Added the 'Uninformed Employee' (powerless) and
 *    'Employer' (institutional) to meet linter requirements. This highlights the
 *    core tension of "nudge" theory: the benevolent 'Rope' of the designer can
 *    be a 'Snare' for the individual if the default is not carefully chosen.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Uninformed Employee (Snare): A decision made for them without their consent.
 *    - Employer (Rope): A tool to improve employee outcomes.
 *    - Economist (Mountain): The underlying cognitive bias is an immutable fact.
 * 
 * 3. EXTRACTIVENESS (0.05): This is a "libertarian paternalistic" constraint,
 *    designed to be beneficial. The only "extraction" is of pure autonomy, not value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether a "nudge" is a helpful guide or a form of manipulation.
 */

omega_variable(
    nudge_intent,
    "Is the default chosen for the user's best interest (Rope) or for the benefit of the provider (e.g., defaulting into a high-fee fund)?",
    resolution_mechanism("Auditing the financial incentives of the choice architect vs. the long-term outcomes for the user."),
    impact("If for user: A benevolent Rope. If for provider: An extractive Snare."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Forced Active Choice
 *    Viability: High. The system could require every employee to actively select
 *    "Yes" or "No" during onboarding, rather than defaulting them in.
 *    Suppression: This is suppressed by the desire to maximize participation rates,
 *    as active choice still results in lower enrollment than opt-out defaults.
 *
 * CONCLUSION:
 * The choice to use an opt-out default instead of forcing an active choice is
 * a deliberate decision to leverage cognitive bias. This makes the system a
 * powerful 'Rope' for achieving policy goals, but also opens it up to being
 * a 'Snare' if the default is set to a predatory option.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/automatic_enrollment_defaults].
 * 2. Multi-perspective: ?- multi_index_report(automatic_enrollment_defaults).
 * 3. Run tests: ?- run_tests(automatic_enrollment_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */