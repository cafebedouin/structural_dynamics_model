% ============================================================================
% CONSTRAINT STORY: goodharts_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Charles Goodhart (1975) / Marilyn Strathern (1997) / Economics
% ============================================================================

:- module(constraint_goodharts_law, []).

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
 * * constraint_id: goodharts_law
 * human_readable: Goodhart's Law
 * domain: economic/social/technological
 * temporal_scope: Modern Bureaucratic/Algorithmic Era
 * spatial_scope: Global (Institutional/Digital Systems)
 * * SUMMARY:
 * Goodhart's Law states that "When a measure becomes a target, it ceases to be 
 * a good measure." It describes the process where agents optimize for a 
 * proxy metric rather than the intended outcome, leading to systemic 
 * distortion, gaming, and the collapse of the metric's informational value.
 * * KEY AGENTS:
 * - The Administrator (Institutional): Sets the metric to manage at scale.
 * - The Optimizer (Individual Moderate): Strategically games the metric to 
 * maximize personal or departmental rewards.
 * - The Analytical Observer: Recognizes the divergence between the proxy 
 * and the reality it was meant to represent.
 * * NARRATIVE ARC:
 * To manage complexity, an institution creates a "Rope" (a metric) to pull 
 * behavior toward a goal. However, agents begin to treat the Rope as the 
 * goal itself. As optimization for the metric increases, the connection to 
 * reality snaps, turning the metric into a "Noose" that strangles actual 
 * productivity and truth, while appearing as a "Mountain" of inevitable 
 * bureaucratic logic to those trapped within it.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for ID extraction
narrative_ontology:interval(goodharts_law_interval, 0, 10).
narrative_ontology:constraint_claim(goodharts_law, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). It extracts "systemic truth" and "genuine utility." 
% It benefits those who can "game" the system while extracting effort and 
% resources from those attempting to fulfill the original, unmeasured goal.
domain_priors:base_extractiveness(goodharts_law, 0.5).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). The metric actively suppresses qualitative 
% alternatives. Once a target is set, work that does not contribute to 
% the metric becomes "invisible" or "valueless" to the institution.
domain_priors:suppression_score(goodharts_law, 0.7).

% Enforcement requirements
% Emerges naturally from the interaction between rational agents and 
% simplified feedback loops.
domain_priors:emerges_naturally(goodharts_law).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(goodharts_law, [metric_gamers, bureaucratic_optimizers]).
constraint_victim(goodharts_law, [systemic_integrity, qualitative_performers]).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(goodharts_law, extractiveness, 0.5).
narrative_ontology:constraint_metric(goodharts_law, suppression_requirement, 0.7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ECONOMIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the laws of incentives and information.
   WHEN: civilizational - Viewing human response to incentives as a constant.
   WHERE: analytical - Not a participant in the specific reward loop.
   SCOPE: global - Universal to all formal incentive systems.
   
   WHY THIS CLASSIFICATION:
   To the economist, Goodhart's Law is a Mountain. It is an unchangeable 
   feature of the informational landscape. You cannot "fix" it through 
   better metrics; any proxy will eventually succumb to the same 
   optimizing pressure. It is a fixed peak in the topography of social 
   science.
   
   NARRATIVE EVIDENCE:
   "Any observed statistical regularity will tend to collapse once 
   pressure is placed upon it for control purposes."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    goodharts_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, analytical, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DATA ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design and implement metrics/dashboards.
   WHEN: biographical - Managing systems over a career.
   WHERE: arbitrage - Can pivot between different metrics to minimize gaming.
   SCOPE: national - Infrastructure for a large organization or state.
   
   WHY THIS CLASSIFICATION:
   For the designer, metrics are a Rope. They are coordination mechanisms. 
   While the law is a risk, the metric is the only tool available to 
   pull a massive, distributed organization toward a unified goal. They 
   use the constraint as a tether to manage complexity, periodically 
   "re-tying" the rope as it begins to fray.
   
   NARRATIVE EVIDENCE:
   "We need a KPI to track progress; without it, we have no way to 
   coordinate thousands of employees toward the objective."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    goodharts_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(goodharts_law, E),
    E < 0.7, % It remains a tool if handled with institutional power
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE OVER-MANAGED WORKER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "performance review" of the metric.
   WHEN: immediate - Today's quota or deadline.
   WHERE: constrained - High cost to ignoring the metric (firing/demotion).
   SCOPE: local - Immediate workspace and task list.
   
   WHY THIS CLASSIFICATION:
   For the worker, the law is a Noose. They are forced to perform 
   meaningless tasks just to "hit the number," even when they know it 
   harms the actual goal. The extraction of their professional integrity 
   strangles their sense of purpose, while the metric itself tightens 
   as management "optimizes" the targets.
   
   NARRATIVE EVIDENCE:
   "I spent all day filling out forms to meet the 'compliance' quota 
   instead of actually helping the client; the system is a trap."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    goodharts_law,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(goodharts_law, E),
    E > 0.4, % Extraction of agency and job utility
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(goodharts_law_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(goodharts_law, mountain, context(analytical, civilizational, analytical, global)),
    constraint_indexing:constraint_classification(goodharts_law, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(goodharts_law, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_gaming) :-
    % Powerless workers feel the total extraction of their utility (Noose).
    % Institutional leaders leverage metrics for coordination (Rope).
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(goodharts_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(goodharts_law, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_incentives) :-
    % Civilizational scale view treats incentive failure as a Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(goodharts_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * Reasoning: Goodhart's Law extracts "truth" and "agency." It forces 
 * agents to act as puppets for a proxy metric, effectively 
 * extracting their qualitative value and redirecting it into 
 * measurable, but often useless, quantitative outputs.
 * * 2. SUPPRESSION SCORE (0.7):
 * Reasoning: High. Metrics are inherently exclusionary. Any 
 * alternative reality that cannot be quantified is effectively 
 * suppressed from the institutional view.
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Analyst (Law), the Designer (Tool), and the 
 * Worker (Victim) to highlight the indexical shift from "Math" 
 * to "Policy" to "Suffering."
 * * 4. CLASSIFICATION RATIONALE:
 * The "Noose" for the powerless is critical; it shows how a 
 * management "Rope" becomes a trap when there is no exit 
 * option or feedback loop.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_corruption_threshold,
    "At what degree of 'optimization pressure' does a metric's utility 
    cross the threshold from Rope to Noose (The Phase Transition)?",
    resolution_mechanism("Longitudinal study of KPI health vs. actual 
    outcomes in high-pressure bureaucratic environments"),
    impact("If Low: Almost all metrics are Nooses. If High: Metrics can 
    be maintained as Ropes for longer periods."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Qualitative/Holistic Assessment (Expert Judgment)
 * Viability: High in small systems. Relies on human trust and nuance.
 * Suppression: High in large systems. It is "unscalable" and 
 * viewed as "biased" or "non-transparent" by modern bureaucracy.
 * * ALTERNATIVE 2: Multi-Metric Frameworks (Anti-Gaming Metrics)
 * Viability: Attempting to balance one metric with another to prevent 
 * narrow optimization.
 * Suppression: Low. This is the "Rope" institutional architects use, 
 * though it eventually succumbs to the same law.
 * * CONCLUSION:
 * The existence of suppressed qualitative alternatives confirms that 
 * for the individual, Goodhart's Law is a Noose created by the 
 * institutional choice of "Scalability" over "Nuance."
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_goodharts_law].
 * 2. Multi-perspective: ?- multi_index_report(goodharts_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
