% ============================================================================
% CONSTRAINT STORY: asshole_filter_2015
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini 2.0 Flash
% Source: "The Asshole Filter" by siderea (2015)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(asshole_filter_2015, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: asshole_filter_2015
 * human_readable: The Asshole Filter
 * domain: psychological/social
 * temporal_scope: 2015-present
 * spatial_scope: Organizational and Social environments
 * * SUMMARY:
 * An "asshole filter" is a systemic phenomenon where established norms or boundaries 
 * unintentionally repel non-transgressive people while attracting and rewarding 
 * transgressive ones. This occurs when a rule is publicly stated but not 
 * enforced; those who break it are rewarded with faster or better service, while 
 * those who obey are penalized with delays or neglect.
 * * KEY AGENTS:
 * - The Weak Enforcer (Fred): The rule-setter who fails to defend his own boundaries 
 * out of a desire to be "nice".
 * - The Rule-Follower (Non-Asshole): Respects the stated boundary and is 
 * subsequently penalized by the system.
 * - The Transgressor (Asshole): Ignores the boundary and is rewarded, 
 * reinforcing their transgressive behavior.
 * * NARRATIVE ARC:
 * A boundary is set (e.g., "don't email my personal account"). Respectful people 
 * obey and wait in a slow queue. Transgressive people ignore the rule, 
 * email directly, and receive immediate attention. The "filter" drains 
 * away polite people, leaving the enforcer surrounded only by "assholes".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(asshole_filter_2015, 0, 10).
narrative_ontology:constraint_claim(asshole_filter_2015, snare).
narrative_ontology:human_readable(asshole_filter_2015, "The Asshole Filter").
narrative_ontology:topic_domain(asshole_filter_2015, "psychological/social").

% Base Properties
domain_priors:base_extractiveness(asshole_filter_2015, 0.75).
domain_priors:suppression_score(asshole_filter_2015, 0.60).
domain_priors:theater_ratio(asshole_filter_2015, 0.55). % Reflects performative "agreeableness" masking enforcement failure.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(asshole_filter_2015, extractiveness, 0.75).
narrative_ontology:constraint_metric(asshole_filter_2015, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(asshole_filter_2015, theater_ratio, 0.55).

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(asshole_filter_2015).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(asshole_filter_2015, transgressive_agents).
narrative_ontology:constraint_victim(asshole_filter_2015, rule_following_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RULE-FOLLOWER - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Respects rules but lacks power to enforce them.
   WHEN: immediate - Experiencing current delays and systemic unfairness.
   WHERE: trapped - Committed to social decency and norms.
   
   WHY THIS CLASSIFICATION:
   For the decent person, the norm is a Snare. They follow the rule 
   and are penalized ("screwed") while watching transgressors get rewarded. 
   The rule constrains only those willing to be constrained.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(asshole_filter_2015, snare, 
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))) :-
    domain_priors:base_extractiveness(asshole_filter_2015, E), E > 0.7, !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRANSGRESSOR (ASSHOLE) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has the agency and willingness to ignore rules.
   WHEN: immediate - Getting immediate needs met.
   WHERE: mobile - Will readily find "illicit" paths or shortcuts.
   
   WHY THIS CLASSIFICATION:
   The transgressor sees the rule as a Rope—a coordination mechanism used as 
   a shortcut. It clears "suckers" out of the way, providing easier 
   direct access to the target.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(asshole_filter_2015, rope, 
    context(agent_power(individual_moderate), time_horizon(immediate), exit_options(mobile), spatial_scope(local))).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE WEAK ENFORCER (FRED) - Snare
   --------------------------------------------------------------------------
   WHO: institutional - In charge of the process/boundaries.
   WHEN: biographical - Enduring a career of escalating harassment.
   WHERE: constrained - Trapped by a desire to be "nice" and lack of judgment.
   
   WHY THIS CLASSIFICATION:
   Fred is in a Snare of his own making. By failing to enforce 
   boundaries, he drains away pleasant interactions and is strangled by 
   concentrated entitlement and disrespect.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(asshole_filter_2015, snare, 
    context(agent_power(institutional), time_horizon(biographical), exit_options(constrained), spatial_scope(national))).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of social and behavioral patterns.
   WHEN: civilizational - Viewing fundamental human behavioral mechanisms.
   WHERE: analytical - Objective observer stance.
   
   WHY THIS CLASSIFICATION:
   Analytically, this is a Mountain—a predictable law of social dynamics 
   where the absence of enforcement inevitably selects for transgressive 
   populations.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(asshole_filter_2015, mountain, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(asshole_filter_2015_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(asshole_filter_2015, Type1, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(asshole_filter_2015, Type2, context(individual_moderate, immediate, mobile, local)),
    Type1 \= Type2.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextTransgressor = context(individual_moderate, immediate, mobile, local),
    constraint_indexing:extractiveness_for_agent(asshole_filter_2015, ContextPowerless, S1),
    constraint_indexing:extractiveness_for_agent(asshole_filter_2015, ContextTransgressor, S2),
    S1 > S2.

test(enforcer_snare_logic) :-
    % Institutional power combined with biographical horizon + constrained exit = Snare
    constraint_indexing:constraint_classification(asshole_filter_2015, snare, context(institutional, biographical, constrained, national)).

:- end_tests(asshole_filter_2015_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.75): Set to high because the system essentially "taxes" 
 * decency to subsidize rudeness.
 * 2. PERSPECTIVES: Contrast between the Rule-Follower (Snare) and Transgressor 
 * (Rope) highlights the "Filter" effect. The inclusion of the "Fred" perspective 
 * shows the feedback loop on the enforcer.
 * 3. MANDATROPHY RESOLUTION: The "predatory" nature is evident for rule-followers 
 * but vanishes for transgressors who view the lack of enforcement as a 
 * functional advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asshole_filter_extraction_intent,
    "Is the extraction of time from rule-followers an unintentional side-effect of agreeableness or a predatory selection for 'tough' customers?",
    resolution_mechanism("Audit of queue throughput: do transgressors increase total efficiency or merely shift costs to the polite?"),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    agreeableness_mutability,
    "Can 'agreeableness' be recalibrated through boundary training (Rope), or is it a fixed personality trait (Mountain)?",
    resolution_mechanism("Longitudinal behavioral study of rule-setters under mandatory enforcement protocols."),
    impact("If Mountain: The enforcer is trapped. If Rope: The system can be reformed."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic/Solicitous Enforcement
 * Viability: The enforcer can politely hold the boundary (e.g., "I'll get to 
 * this once it's in the official queue").
 * Suppression: Often suppressed by the enforcer's own fear of "not being nice".
 * * ALTERNATIVE 2: Explicit/Transparent Exceptions
 * Viability: Formalizing urgent paths so rule-followers don't have to transgress 
 * to get attention.
 * * CONCLUSION:
 * The presence of these alternatives shifts the system from a "natural" Mountain 
 * toward a Snare when the enforcer prioritizes their own comfort over 
 * systemic health.
 */

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as the enforcer's "agreeableness" (0.15) shifts from 
% functional politeness to performative "niceness" (0.55) that masks rule failure.
narrative_ontology:measurement(filter_tr_t0, asshole_filter_2015, theater_ratio, 0, 0.15).
narrative_ontology:measurement(filter_tr_t5, asshole_filter_2015, theater_ratio, 5, 0.35).
narrative_ontology:measurement(filter_tr_t10, asshole_filter_2015, theater_ratio, 10, 0.55).

% Extraction: Progressive liquidation of rule-follower patience as 
% transgressors capture an increasing share of systemic rewards.
narrative_ontology:measurement(filter_ex_t0, asshole_filter_2015, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(filter_ex_t5, asshole_filter_2015, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(filter_ex_t10, asshole_filter_2015, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [asshole_filter_2015].
 * 2. Multi-perspective: ?- multi_index_report(asshole_filter_2015).
 * 3. Run tests: ?- run_tests(asshole_filter_2015_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
