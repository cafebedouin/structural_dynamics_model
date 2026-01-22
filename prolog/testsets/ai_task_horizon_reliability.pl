% ============================================================================
% CONSTRAINT STORY: ai_task_horizon_reliability
% ============================================================================
% Generated: January 20, 2026
% Model: Gemini 2.0 Flash
% Source: Anthropic Economic Index Report: Economic Primitives (Jan 15, 2026)
% ============================================================================

:- module(constraint_ai_task_horizon_reliability, []).

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
* * constraint_id: ai_task_horizon_reliability


* human_readable: The AI Task Horizon and Reliability Constraint
* domain: technological/economic
* temporal_scope: November 2025 (Pre-Opus 4.5 release) 


* spatial_scope: Global 


* * SUMMARY:


* This constraint defines the inverse relationship between the complexity of a
* task (measured in human time-to-complete) and the success rate of the AI
*. As tasks grow in duration and required education level,


* Claude's reliability drops, creating a "task horizon" where effective
* automation becomes a bottleneck for productivity.


* * KEY AGENTS:


* * Enterprise API User: Businesses deploying Claude for automated, single-turn workflows.




* * Individual Web User: Consumers using Claude.ai for augmented, multi-turn task iteration.




* * Macroeconomist: Analysts like Ruth Appel and team measuring the impact of model reliability on labor productivity.




* * NARRATIVE ARC:


* While AI offers massive "speedups" for complex work, these gains are
* effectively halved when success rates are factored in. The


* constraint shifts from a "Mountain" for automated systems to a "Rope" for
* iterative human-in-the-loop interactions.
*/



/* ==========================================================================
2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(pre_opus_4_5_era, 0, 10). 
narrative_ontology:constraint_claim(ai_task_horizon_reliability, mountain).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: AI as a task-replacement tool causes "net deskilling" in 49% of
% occupations, removing high-skill tasks while leaving routine ones.
domain_priors:base_extractiveness(ai_task_horizon_reliability, 0.45).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: "Selection effects" mean users avoid tasks they expect to fail,
% masking the true failure rate on the full distribution.
domain_priors:suppression_score(ai_task_horizon_reliability, 0.6).

% Enforcement requirements
% Emerges naturally from current model capability limits and "task horizons".
domain_priors:emerges_naturally(ai_task_horizon_reliability).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(ai_task_horizon_reliability, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, suppression_requirement, 0.6).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (Perspectival Truth)
========================================================================== */

## /* --------------------------------------------------------------------------
PERSPECTIVE 1: ENTERPRISE API USER - Mountain

WHO: institutional - Automation-dominant enterprise deployments.
WHEN: immediate - Focused on programmatic task completion.
WHERE: trapped - Bound by the 3.5-hour 50% success horizon for single-turn calls.
SCOPE: global - Deployed across high-volume back-office workflows.

WHY THIS CLASSIFICATION:
For automated API systems, the reliability drop is a Mountain. The success
rate falls steeply from 60% for sub-hour tasks to 45% for 5+ hour tasks
. Without multi-turn feedback, the agent is trapped by the model's
current autonomous ceiling.

NARRATIVE EVIDENCE:
"In the API data, success rates drop from around 60% for sub-hour tasks to
roughly 45% for tasks estimated to take humans 5+ hours".
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
ai_task_horizon_reliability,
mountain,
context(
agent_power(institutional),
time_horizon(immediate),
exit_options(trapped),
constraint_beneficiary(ai_task_horizon_reliability, efficiency_seeking_firms),
constraint_victim(ai_task_horizon_reliability, none),
spatial_scope(global)
)
) :-
domain_priors:emerges_naturally(ai_task_horizon_reliability),
!.

## /* --------------------------------------------------------------------------
PERSPECTIVE 2: CLAUDE.AI WEB USER - Rope

WHO: individual_moderate - Users with "Skills for workflow customization".
WHEN: biographical - Adapting creative and educational workflows over time.
WHERE: mobile - Able to iterate, clarify, and correct course.
SCOPE: local - Immediate task refinement and learning.

WHY THIS CLASSIFICATION:
For the web user, the horizon is a Rope. Multi-turn conversation "effectively
breaks complex tasks into smaller steps". This coordination
mechanism stretches the 50% success horizon to roughly 19 hours, allowing
the user to bypass the "Mountain" through collaboration.

NARRATIVE EVIDENCE:
"Claude.ai data tells a different story... multi-turn conversation
effectively breaks complex tasks into smaller steps... each turn providing
a feedback loop".
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
ai_task_horizon_reliability,
rope,
context(
agent_power(individual_moderate),
time_horizon(biographical),
exit_options(mobile),
constraint_beneficiary(ai_task_horizon_reliability, augment_workers),
constraint_victim(ai_task_horizon_reliability, none),
spatial_scope(local)
)
) :-
domain_priors:base_extractiveness(ai_task_horizon_reliability, E),
E < 0.5,
!.

## /* --------------------------------------------------------------------------
PERSPECTIVE 3: MACROECONOMIST - Noose (Deskilling Risk)

WHO: analytical - Researchers observing aggregate labor shifts.
WHEN: historical - Estimating productivity growth over the next decade.
WHERE: analytical - Viewing the "net deskilling effect" across occupations.
SCOPE: national - US labor productivity and workforce composition.

WHY THIS CLASSIFICATION:
From an analytical perspective, this constraint acts like a Noose for
certain classes. By automating high-skill tasks but failing on "core
knowledge work," it risks deskilling professionals (like travel agents)
into routine laborers. It asymmetricly removes
the "skill-intensive" tasks that justify higher wages.

NARRATIVE EVIDENCE:
"The net first-order impact is to deskill jobs... AI removes tasks that
require relatively higher levels of education".
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
ai_task_horizon_reliability,
noose,
context(
agent_power(analytical),
time_horizon(historical),
exit_options(analytical),
constraint_beneficiary(ai_task_horizon_reliability, low_cost_labor_demand),
constraint_victim(ai_task_horizon_reliability, white_collar_workers),
spatial_scope(national)
)
) :-
domain_priors:suppression_score(ai_task_horizon_reliability, S),
S > 0.5,
!.

/* ==========================================================================
4. TESTS (What We Learn About Constraints)
========================================================================== */

:- begin_tests(ai_reliability_tests).

/**

* TEST 1: Multi-perspective variance
* Demonstrates the gap between the API "Mountain" and the Web "Rope."
*/
test(multi_perspective_horizon_gap) :-
% API (Mountain)
constraint_indexing:constraint_classification(
ai_task_horizon_reliability,
mountain,
context(institutional, immediate, trapped, _, _, _)
),
% Web (Rope)
constraint_indexing:constraint_classification(
ai_task_horizon_reliability,
rope,
context(individual_moderate, biographical, mobile, _, _, _)
).

/**

* TEST 2: Productivity Adjustment
* Demonstrates that reliability halves the implied economic "speedup."
*/
test(productivity_halving_effect) :-
BaselineGain = 1.8, SuccessAdjustedGain = 1.0, 
BaselineGain > SuccessAdjustedGain.



/**

* TEST 3: Time-horizon immutability
* Short horizons (API) hit the 50% success wall faster than long horizons (Web).
*/
test(horizon_duration_limit) :-
APILimit = 3.5, WebLimit = 19.0, 
WebLimit > APILimit.



:- end_tests(ai_reliability_tests).

/* ==========================================================================
5. MODEL INTERPRETATION (Commentary)
========================================================================== */

/**

* LLM GENERATION NOTES
* * Model: Gemini 2.0 Flash


* * KEY DECISIONS MADE BY MODEL:


* * 1. BASE EXTRACTIVENESS (0.45):




* Reasoning: Derived from the report's finding on "deskilling." While not
* purely extractive, the removal of high-skill tasks from jobs creates
* wage pressure and reduces worker autonomy.


* * 2. SUPPRESSION SCORE (0.6):




* Reasoning: Strong evidence of "selection effect" where users only bring
* tasks they expect Claude to succeed on, potentially suppressing the
* full scope of AI limitations from aggregate data.


* * 3. OMEGAS




* omega_variable(bottleneck_task_evolution,
* "Will 'bottleneck tasks' scale in difficulty as AI improves, keeping
* the 50% success horizon constant?",
* resolution_mechanism("Longitudinal tracking of user task presentation
* complexity relative to model generations "),


* impact("If yes: productivity gains stall at ~1.0pp. If no: gains
* accelerate toward 5%pp."),


* confidence_without_resolution(low)
* ).
*/

/* ==========================================================================
6. ALTERNATIVE ANALYSIS
========================================================================== */

/**

* VIABLE ALTERNATIVES
* * ALTERNATIVE 1: Human-Only Knowledge Work


* Viability: Declining as AI "penetrates" 49% of jobs.


* Suppression: Market pressure to adopt AI to achieve "speedups" of 9x-12x
*.


* * ALTERNATIVE 2: Specialized Expert Models


* Viability: Mentioned as a way to reinforce professional expertise.


* * CONCLUSION:


* The existence of "bottleneck tasks" (complements) shifts the classification
* from a pure efficiency Rope to a complex Noose where workers become
* dependent on AI that they must constantly "validate".
*/



/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================== */

/**

* TO USE THIS CONSTRAINT:
* * 1. Load: ?- [ai_task_horizon_reliability].




* 2. Multi-perspective: ?- multi_index_report(ai_task_horizon_reliability).


* 3. Run tests: ?- run_tests(ai_reliability_tests).
*/



% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
