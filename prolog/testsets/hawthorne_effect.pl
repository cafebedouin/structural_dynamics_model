% ============================================================================
% CONSTRAINT STORY: hawthorne_effect
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Psychometrics / Industrial Psychology / Hawthorne Works Study (1924-1932)
% ============================================================================

:- module(constraint_hawthorne_effect, []).

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
 * * constraint_id: hawthorne_effect
 * human_readable: The Hawthorne Effect (Observer Effect)
 * domain: social
 * temporal_scope: Modern Era (1924 - Present)
 * spatial_scope: Global (Institutional/Organizational)
 * * SUMMARY:
 * The Hawthorne Effect is a type of reactivity in which individuals modify an aspect 
 * of their behavior in response to their awareness of being observed. Originally 
 * discovered during productivity studies at the Western Electric Hawthorne Works, 
 * it represents a fundamental limit on the ability to gather "neutral" data on 
 * human social systems.
 * * KEY AGENTS:
 * - The Researcher: Analytical observer attempting to measure "natural" productivity.
 * - The Factory Manager: Institutional agent using the "gaze" to drive output.
 * - The Monitored Worker: Individual powerless agent whose behavior is warped by the observer.
 * * NARRATIVE ARC:
 * An organization introduces an observer to "improve" a system. The observer 
 * acts as a "Rope" for management to pull more effort from the workers. However, 
 * for the worker, the gaze becomes a "Noose" of performance anxiety and 
 * inauthenticity. Ultimately, the effect is revealed as a "Mountain"—an 
 * inescapable psychological law where the act of measurement inevitably 
 * changes the thing being measured.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(hawthorne_interval, 0, 10).
narrative_ontology:constraint_claim(hawthorne_effect, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The effect extracts "authentic behavior" and 
% additional labor/effort through the psychological pressure of the gaze.
domain_priors:base_extractiveness(hawthorne_effect, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). It suppresses the "Natural State." Once an 
% observer is present, the alternative—how the worker behaves when alone—is 
% rendered conceptually and empirically invisible.
domain_priors:suppression_score(hawthorne_effect, 0.5).

% Enforcement: Emerges naturally from human social cognition.
domain_priors:emerges_naturally(hawthorne_effect).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(hawthorne_effect, extractiveness, 0.4).
narrative_ontology:constraint_metric(hawthorne_effect, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hawthorne_effect, [management, study_leads]).
constraint_victim(hawthorne_effect, [monitored_subjects, experimental_integrity]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RESEARCHER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of social phenomena.
   WHEN: civilizational - Viewing human psychology as a fixed substrate.
   WHERE: analytical - Not personally constrained by the productivity quota.
   SCOPE: global - Applies to all human observation.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the effect is a Mountain. It is an unchangeable 
   epistemological limit. No matter how much you "standardize" the lab, 
   the presence of the scientist creates a fixed peak of bias that 
   cannot be leveled, only accounted for.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hawthorne_effect,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CORPORATE SUPERVISOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power within the firm.
   WHEN: biographical - Managing a plant's output over a career.
   WHERE: arbitrage - Can decide when to introduce or withdraw observation.
   SCOPE: regional - Within the specific factory or department.
   
   WHY THIS CLASSIFICATION:
   For management, the effect is a Rope. It is a coordination mechanism 
   used to "nudge" behavior. By simply placing a supervisor on the floor 
   or announcing a "study," they pull the workforce toward higher 
   productivity without needing to change wages or hardware.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hawthorne_effect,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE FLOOR WORKER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the gaze and the quota.
   WHEN: immediate - The 8-hour shift under the clipboard.
   WHERE: trapped - Cannot leave the workstation or hide from the gaze.
   SCOPE: local - The immediate workbench.
   
   WHY THIS CLASSIFICATION:
   For the worker, the effect is a Noose. The awareness of being 
   watched creates an involuntary "tightening" of performance. They 
   cannot relax into their natural rhythm; the gaze extracts extra 
   vitality and focus, strangling their autonomy and mental peace 
   until the observer leaves.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hawthorne_effect,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hawthorne_effect_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(hawthorne_effect, mountain, context(analytical, civilizational, analytical, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(hawthorne_effect, rope, context(institutional, biographical, arbitrage, regional)),
    % Powerless sees Noose
    constraint_indexing:constraint_classification(hawthorne_effect, noose, context(individual_powerless, immediate, trapped, local)).

test(power_extractiveness_gaze) :-
    % Powerless individuals feel the extraction of their "authentic self" (Noose).
    % Institutions use the gaze to manage throughput (Rope).
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, regional),
    constraint_indexing:extractiveness_for_agent(hawthorne_effect, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(hawthorne_effect, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_psychology) :-
    % Over biographical time, the worker is trapped (Noose).
    % Over civilizational time, it is a Law of Nature (Mountain).
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(hawthorne_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: It is a "soft" extraction. It doesn't steal money directly, 
 * but it extracts psychological energy and authentic agency.
 * * 2. PERSPECTIVE SELECTION:
 * Analyzed from the Scientist (Hardware limit), Manager (Strategic tool), 
 * and Worker (Psychological trap) to show the indexical range.
 * * 3. CLASSIFICATION RATIONALE:
 * Analytical → Mountain: It is a law of physics for social systems.
 * Institutional → Rope: It is a low-cost coordination tool.
 * Powerless → Noose: It is a restrictive performance trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observation_latency,
    "How long does it take for a subject to 'habituate' and return to 
    natural behavior, effectively untying the Noose (Rope)?",
    resolution_mechanism("Longitudinal study of 24/7 surveillance vs. intermittent observation"),
    impact("If Habituation is fast: The effect is a weak Rope. If slow: It is a permanent Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Covert Observation (Surreptitious Monitoring)
 * Viability: Theoretically allows for the gathering of neutral data.
 * Suppression: High. Actively suppressed by modern ethics boards (IRB) 
 * and privacy laws.
 * Evidence: The shift from "secret" studies to "informed consent" requirements.
 * * CONCLUSION:
 * The existence of a "Natural State" that is only accessible through 
 * suppressed/unethical means confirms that for the modern subject, the 
 * Hawthorne Effect is a Noose—you are denied the exit into unobserved 
 * reality by the rules of the system.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_hawthorne_effect].
 * 2. Multi-perspective: ?- multi_index_report(hawthorne_effect).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
