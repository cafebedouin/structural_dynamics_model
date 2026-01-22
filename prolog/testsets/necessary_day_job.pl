% ============================================================================
% CONSTRAINT STORY: necessary_day_job
% ============================================================================
% Generated: January 20, 2026
% Model: Gemini 2.0 Flash
% Source: "The Work Behind the Writing: On Writers and Their Day Jobs" by Ed Simon
% ============================================================================

:- module(constraint_necessary_day_job, []).

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
* * constraint_id: necessary_day_job


* human_readable: The Economic Subsistence Labor Constraint
* domain: economic
* temporal_scope: 19th Century to 2026
* spatial_scope: National (USA)
* * SUMMARY:


* The "necessary day job" represents the economic constraint where creative
* vocation is decoupled from subsistence labor. Writers must perform
* unrelated, often "boring" or "sterile" work to fund their literary passion
*. This constraint fluctuates between a functional necessity
* and a soul-crushing mechanism of extraction.
* * KEY AGENTS:


* * Herman Melville: A U.S. Customs Inspector working 6 days a week for $4/day.


* * Ed Simon: A communications editor for the USPS experiencing "existential crisis".


* * The Manager: A "jackass" figure enforcing the "Panopticon" of office work.


* * NARRATIVE ARC:


* From Melville’s 19-year "torment" at the docks to Simon’s 6-month stint in a
* "sterile cubicle," the constraint dictates how time is partitioned between
* survival and creation.
*/

/* ==========================================================================
2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(literary_labor_history, 0, 10).
narrative_ontology:constraint_claim(necessary_day_job, noose).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High extraction in the case of adjuncting ("abysmal pay") or
% Melville's stagnant wages.
domain_priors:base_extractiveness(necessary_day_job, 0.65).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Economic alternatives (living solely off royalties) are rare
% and suppressed by market indifference.
domain_priors:suppression_score(necessary_day_job, 0.7).

% Enforcement requirements
domain_priors:requires_active_enforcement(necessary_day_job).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(necessary_day_job, extractiveness, 0.65).
narrative_ontology:constraint_metric(necessary_day_job, suppression_requirement, 0.7).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (Perspectival Truth)
========================================================================== */

/* --------------------------------------------------------------------------
PERSPECTIVE 1: HERMAN MELVILLE - Mountain

WHO: individual_powerless - A man devastated by criticism, working without raises.
WHEN: biographical - Nineteen years of service until retirement.
WHERE: trapped - "Everlasting itch for things remote" while stuck at a desk.
SCOPE: local - The Merchant’s Exchange Building at 55 Wall Street.

WHY THIS CLASSIFICATION:
For Melville, the Customs House was an unchangeable reality. He could not
alter his pay or the market's rejection of his work; he could only
endure it as a natural law of his middle age.

NARRATIVE EVIDENCE:
"Melville worked for six days a week... and was paid $4 a day, never
receiving a raise in two decades".
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
necessary_day_job,
mountain,
context(
agent_power(individual_powerless),
time_horizon(biographical),
exit_options(trapped),
constraint_beneficiary(necessary_day_job, us_government),
constraint_victim(necessary_day_job, herman_melville),
spatial_scope(local)
)
) :-
domain_priors:suppression_score(necessary_day_job, S),
S > 0.6,
!.

/* --------------------------------------------------------------------------
PERSPECTIVE 2: ED SIMON (Adjunct Era) - Noose

WHO: individual_powerless - Facing "ample disrespect" and "nonexistent benefits".
WHEN: immediate - The struggle to "pay the light bill".
WHERE: constrained - Forced into federal work by economic necessity.
SCOPE: regional - University adjuncting circuits.

WHY THIS CLASSIFICATION:
Adjuncting is seen as a Noose: an asymmetric, extractive system where the
labor provided far outweighs the compensation and professional respect
received.

NARRATIVE EVIDENCE:
"The ample disrespect, nonexistent benefits, and abysmal pay of university
adjuncting".
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
necessary_day_job,
noose,
context(
agent_power(individual_powerless),
time_horizon(immediate),
exit_options(constrained),
constraint_beneficiary(necessary_day_job, university_system),
constraint_victim(necessary_day_job, ed_simon),
spatial_scope(regional)
)
) :-
domain_priors:base_extractiveness(necessary_day_job, E),
E > 0.5,
!.

/* --------------------------------------------------------------------------
PERSPECTIVE 3: WILLIAM CARLOS WILLIAMS / AGATHA CHRISTIE - Rope

WHO: individual_powerful (or moderate) - Professionals with specialized skills.
WHEN: biographical - Career-long integration of job and art.
WHERE: mobile - Jobs that provide material (poisons, prescriptions) for art.
SCOPE: local - The physician's pad or the pharmacy.

WHY THIS CLASSIFICATION:
For these writers, the day job is a Rope: a functional coordination mechanism
that provides both economic stability and the raw data/inspiration
required for their "real" work.

NARRATIVE EVIDENCE:
"Agatha Christie developed her deep knowledge of poison while earning her
living as a pharmacist".
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
necessary_day_job,
rope,
context(
agent_power(individual_moderate),
time_horizon(biographical),
exit_options(mobile),
constraint_beneficiary(necessary_day_job, society),
constraint_victim(necessary_day_job, none),
spatial_scope(local)
)
) :-
domain_priors:base_extractiveness(necessary_day_job, E),
E < 0.4,
!.

/* ==========================================================================
4. TESTS (What We Learn About Constraints)
========================================================================== */

:- begin_tests(day_job_tests).

/**

* TEST 1: The "Melville-Simon Variance"
* Demonstrates how time horizon and exit options shift classification.
*/
test(perspective_shift) :-
% Melville (Mountain)
constraint_indexing:constraint_classification(
necessary_day_job,
mountain,
context(individual_powerless, biographical, trapped, _, _, _)
),
% Simon (Noose/Exitable)
constraint_indexing:constraint_classification(
necessary_day_job,
noose,
context(individual_powerless, immediate, constrained, _, _, _)
).

/**

* TEST 2: Extraction Scaling
* Powerless agents (adjuncts) experience higher extraction than specialized professionals (physicians).
*/
test(power_extraction_scaling) :-
ContextAdjunct = context(individual_powerless, immediate, constrained, _, _, _),
ContextDoctor = context(individual_moderate, biographical, mobile, _, _, _),
constraint_indexing:extractiveness_for_agent(necessary_day_job, ContextAdjunct, Score1),
constraint_indexing:extractiveness_for_agent(necessary_day_job, ContextDoctor, Score2),
Score1 > Score2.

:- end_tests(day_job_tests).

/* ==========================================================================
5. MODEL INTERPRETATION (Commentary)
========================================================================== */

/**

* LLM GENERATION NOTES
* * Model: Gemini 2.0 Flash


* * KEY DECISIONS:


* 1. BASE EXTRACTIVENESS (0.65): High, due to the recurring theme of "abysmal


* pay" and "boring office work" that provides little benefit to the writer's
* vocation.
* 2. PERSPECTIVE SELECTION: Focused on Melville (Mountain/Trapped), Simon


* (Noose/Adjunct), and the "functional" writers like Christie (Rope).
* * OMEGAS:


* omega_variable(vocational_ecstasy_v_labor,
* "Can the 'ecstasy' of a job (Rope) coexist with its 'soul-crushing' anonymity (Noose)?",
* resolution_mechanism("Qualitative analysis of 'transcendence' reports in labor diaries"),
* impact("If Rope dominates: Writing is enriched. If Noose dominates: Writing is killed."),
* confidence_without_resolution(low)
* ).
*/

/* ==========================================================================
6. ALTERNATIVE ANALYSIS
========================================================================== */

/**

* VIABLE ALTERNATIVES
* * ALTERNATIVE 1: Independent Wealth / Trust Funds


* Viability: Historically rare (e.g., Burroughs had a trust fund but still worked).
* Suppression: Limited by socioeconomic birth.
* * ALTERNATIVE 2: Tenure / Academic Protections


* Viability: The post-WWII "halcyon days" where universities were sights of independence.
* Suppression: Currently eroded by the rise of adjuncting.
*/

/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================== */

/**

* TO USE THIS FILE:
* ?- [necessary_day_job].
* ?- run_tests(day_job_tests).
*/

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
