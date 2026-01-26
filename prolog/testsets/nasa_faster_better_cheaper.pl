% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: nasa_faster_better_cheaper
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: NASA Mishap Investigation Board (MCO) / Daniel Goldin Era Policy
% ============================================================================

:- module(constraint_fbc_philosophy, []).

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
 * * constraint_id: nasa_faster_better_cheaper
 * human_readable: The "Faster, Better, Cheaper" (FBC) Paradigm
 * domain: political/economic/technological
 * temporal_scope: 1992-1999 (Daniel Goldin Era)
 * spatial_scope: National (NASA / Aerospace Sector)
 * * SUMMARY:
 * Faster, Better, Cheaper (FBC) was a NASA management philosophy designed to 
 * increase mission frequency and reduce costs by accepting higher technical 
 * risk. It constrained engineering behavior by imposing strict, unyielding 
 * budget caps and compressed schedules, effectively attempting to optimize 
 * all three corners of the traditional management triangle simultaneously.
 * * KEY AGENTS:
 * - Mission Engineer: Individual powerless; forced to cut redundant systems 
 * and testing to stay under budget caps.
 * - NASA Administrator (Goldin): Institutional; the architect of the policy 
 * aiming for high-volume scientific output.
 * - Mishap Investigation Board: Analytical; post-mortem body identifying 
 * FBC as the cultural root of the Mars Climate Orbiter failure.
 * * NARRATIVE ARC:
 * FBC began as a Rope (coordinating a post-Cold War NASA toward efficiency). 
 * For engineers on the ground, the budget caps were a Mountain. In practice, 
 * the lack of "Margin" turned FBC into a Snare that extracted technical 
 * integrity and mission safety to feed institutional productivity metrics, 
 * resulting in the loss of multiple planetary missions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(fbc_era_operations, 0, 10).
narrative_ontology:constraint_claim(nasa_faster_better_cheaper, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.8. High extraction; FBC "extracted" the safety margins and 
% redundant systems (capital) from missions to produce a higher *number* of 
% launches, even if those launches failed at a higher rate.
domain_priors:base_extractiveness(nasa_faster_better_cheaper, 0.8).

% Suppression score (0.0-1.0)
% Rationale: 0.7. The philosophy actively suppressed traditional "safety-first" 
% engineering alternatives by labeling them as "obsolete" or "inefficient."
domain_priors:suppression_score(nasa_faster_better_cheaper, 0.7).

% Enforcement requirements
% Requires active enforcement (strict budgetary audits and schedule-driven 
% performance reviews).
domain_priors:requires_active_enforcement(nasa_faster_better_cheaper).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, extractiveness, 0.8).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(nasa_faster_better_cheaper, high_launch_tempo_metrics).
constraint_victim(nasa_faster_better_cheaper, mission_success_probability).
constraint_victim(nasa_faster_better_cheaper, planetary_science_data_retention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: MISSION SYSTEMS ENGINEER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the "Faster" and "Cheaper" caps; 
         cannot increase budget to fix known risks.
   WHEN: immediate - The duration of the development and flight phases.
   WHERE: trapped - Locked into the FBC culture of the 1990s JPL environment.
   SCOPE: local - Specific mission subsystems (e.g., MCO or Mars Polar Lander).
   
   WHY THIS CLASSIFICATION:
   For the engineer, the budget cap is a Mountain. It is an unchangeable law 
   of the project. If a test is required but not budgeted, it simply does 
   not happen. The constraint is treated as an immovable physical limit 
   on what can be built.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    nasa_faster_better_cheaper,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(nasa_faster_better_cheaper),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NASA HEADQUARTERS - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to set the paradigm and distribute funds.
   WHEN: biographical - Careers tied to "reforming" the agency's speed and cost.
   WHERE: mobile - Authority to set the "Better" standard for the whole agency.
   SCOPE: national - Managing the premiere US space exploration program.
   
   WHY THIS CLASSIFICATION:
   Leadership viewed FBC as a Rope—a functional coordination tool to maximize 
   scientific return on investment. They believed they were "weaving" a more 
   efficient agency by stripping away "excess" bureaucracy and caution.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    nasa_faster_better_cheaper,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(nasa_faster_better_cheaper, E),
    E > 0.6, % Managers don't see the extraction; they see "optimization."
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: POST-FAILURE INVESTIGATOR - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Outside observer identifying the root cause of loss.
   WHEN: historical - Evaluating the FBC era in the wake of the MCO/MPL crashes.
   WHERE: analytical - Free from the "Faster, Better, Cheaper" cultural pressure.
   SCOPE: global - A cautionary lesson for complex systems engineering.
   
   WHY THIS CLASSIFICATION:
   The investigator sees the Snare. The constraint of "Cheaper" extracted 
   the safety margin until the mission "choked" on the first unanticipated 
   error (like the metric/English unit mismatch). The system guaranteed 
   failure by removing the friction required to catch errors.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    nasa_faster_better_cheaper,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(nasa_faster_better_cheaper, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(fbc_philosophy_tests).

test(multi_perspective_paradox) :-
    % HQ sees efficiency (Rope), Engineer sees impossibility (Mountain)
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, T1, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, T2, context(individual_powerless, immediate, trapped, local)),
    T1 \= T2.

test(extraction_threshold_breach) :-
    % FBC era failures indicate high extraction of mission value
    domain_priors:base_extractiveness(nasa_faster_better_cheaper, E),
    E > 0.7.

:- end_tests(fbc_philosophy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.8): I chose a high score because FBC fundamentally 
 * trades the *survival* of a system for the *tempo* of a system. 
 * 2. CLASSIFICATION: I primary-labeled this as a 'Snare' from an analytical 
 * perspective. While it claimed to be a Rope (efficiency), it actively 
 * suppressed the "Margin" (the space where safety exists), leading to 
 * catastrophic extraction.
 * 3. OMEGA: The uncertainty lies in whether "Better" was ever a real 
 * variable or just a marketing label for the Rope.
 */

omega_variable(
    better_variable_reality,
    "Was 'Better' ever a measurable metric in FBC, or was it a rhetorical 
     'Rope' used to justify the extraction of the 'margin'?",
    resolution_mechanism("Comparison of scientific data per dollar in FBC vs. traditional eras"),
    impact("If data/dollar is higher: FBC was a flawed Rope. If lower: 
            FBC was a pure Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Conservative Risk-Averse Engineering
 * Viability: The Apollo-era standard. High success, high cost.
 * Suppression: Suppressed by Congressional budget cuts and the 
 * "Faster, Better, Cheaper" management mandate.
 * * CONCLUSION:
 * The fact that a known successful (but expensive) alternative was 
 * suppressed in favor of FBC is the primary indicator of the Snare.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [nasa_faster_better_cheaper].
% 2. Analyze: ?- multi_index_report(nasa_faster_better_cheaper).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
