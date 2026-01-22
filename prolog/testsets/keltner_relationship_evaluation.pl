% ============================================================================
% CONSTRAINT STORY: keltner_relationship_evaluation
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "A Keltner List for Relationships" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_keltner_relationship_evaluation, []).

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
 * * constraint_id: keltner_relationship_evaluation
 * human_readable: The Keltner Relationship List
 * domain: social/psychological
 * temporal_scope: Contemporary / Ongoing
 * spatial_scope: Interpersonal / The Dyad
 * * SUMMARY:
 * The Keltner List is a 15-question diagnostic framework designed to evaluate the 
 * psychological and social health of a romantic relationship. It sets a 
 * "Gold Standard" for mutual growth, trust, shared power, and emotional stability 
 * to predict if a relationship will "stand the test of time".
 * * KEY AGENTS:
 * - The Evaluator (Agent): The individual applying the list to their partner and self.
 * - The Partner (Subject): The person being measured against the 15 criteria.
 * - The Social Circle (Observers): Friends whose opinions are indexed as a metric for 
 * relationship longevity.
 * * NARRATIVE ARC:
 * The list functions as a psychological "Mountain" of ideal traits (trust, compatibility, 
 * stability). For a thriving couple, it acts as a "Rope" for coordination and 
 * growth. However, if used as a rigid, binary "Yes/No" tool in an imbalanced 
 * relationship, it can become a "Noose" that extracts identity or justifies 
 * termination based on "nascent" imperfections.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(keltner_relationship_interval, 0, 10).
narrative_ontology:constraint_claim([keltner_relationship_evaluation], [social_governance]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Moderate (0.35). While intended as a gift of clarity, the list 
% extracts the "complexity" of a relationship, reducing idiosyncratic 
% bonds to a simple binary metric.
domain_priors:base_extractiveness(keltner_relationship_evaluation, 0.35).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.45). It suppresses the visibility of alternative 
% relationship models (e.g., highly independent or conflict-heavy but 
% resilient bonds) in favor of the "Best Friend" and "We-focused" dogma.
domain_priors:suppression_score(keltner_relationship_evaluation, 0.45).

% Enforcement: Requires active maintenance (Truthful answering).
domain_priors:requires_active_enforcement(keltner_relationship_evaluation).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(keltner_relationship_evaluation, extractiveness, 0.35).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, suppression_requirement, 0.45).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(keltner_relationship_evaluation, relationship_clarity). 
constraint_victim(keltner_relationship_evaluation, non_standard_dyads). % Those who don't fit the "agreeable/stable" personality mold.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMMITTED PARTNER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Seeking mutual growth and "bettering" each other)
   WHEN: biographical (Long-term relationship success)
   WHERE: mobile (Can choose to work on specific "No" answers)
   SCOPE: local (The dyad)
   
   WHY THIS CLASSIFICATION:
   For a healthy partner, the list is a "Rope." It is a functional coordination 
   mechanism to identify areas for "sacrifice" and "betterment," turning 
   potential conflicts into shared decisions.
   
   NARRATIVE EVIDENCE:
   "Does your partner make you a better person... do you and your partner share 
   decision-making, power and influence?".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    keltner_relationship_evaluation,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MEASURED SUBJECT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Being judged by a partner with a "binary" checklist)
   WHEN: immediate (A moment of "answering truthfully" that may end the bond)
   WHERE: trapped (Subject to the partner's "rough determination" of compatibility)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a person being measured against these 15 points by a "controlling" or 
   "negativity-focused" partner, the list is a "Noose." Any "No" answer 
   is extracted as a "red flag" or justification for "liquidation" of the 
   relationship.
   
   NARRATIVE EVIDENCE:
   "answer truthfully with a simple yes or no response... Is your relationship 
   free of red flags like... controlling behavior?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    keltner_relationship_evaluation,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(keltner_relationship_evaluation, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE RELATIONSHIP COUNSELOR - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "test of time" across many couples)
   WHEN: historical (The perennial requirements of human pair-bonding)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the counselor, these 15 questions represent "Mountains"—fixed psychological 
   laws. Stability, compatibility, and respect are not "stories we tell" 
   but unchangeable requirements for a relationship that isn't a "Black Iron Prison".
   
   NARRATIVE EVIDENCE:
   "Do your close friends... think you have a great relationship that will 
   stand the test of time?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    keltner_relationship_evaluation,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(keltner_relationship_evaluation_tests).

test(multi_perspective_evaluation) :-
    % Partner sees Rope
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope, context(individual_moderate, biographical, mobile, local)),
    % Subject sees Noose
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, noose, context(individual_powerless, immediate, trapped, local)),
    % Counselor sees Mountain
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, mountain, context(analytical, historical, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_evaluation) :-
    % The powerless subject feels more extraction (judgment) than the moderate partner.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(keltner_relationship_evaluation, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(keltner_relationship_evaluation, ContextModerate, Score2),
    Score1 > Score2.

test(binary_suppression_insight) :-
    % Demonstrates that high suppression (0.45) forces a "binary" view of relationship health.
    domain_priors:suppression_score(keltner_relationship_evaluation, S),
    S > 0.4.

:- end_tests(keltner_relationship_evaluation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.35):
 * Reasoning: I chose a moderate score because while the list is helpful, it 
 * extracts the "nuance" and "ordinary repetition" of relationship life, 
 * replacing them with 15 "extraordinary" metrics. 
 * 2. PERSPECTIVE SELECTION:
 * Chose the Partner (Rope), the Subject (Noose), and the Counselor (Mountain) 
 * to show how a "gift of wisdom" can be experienced as a "tax of judgment" 
 * depending on power.
 * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The constraint is a "Rope" for the 
 * individual seeking growth, but it becomes a "Noose" for the partner 
 * who is "trapped" by an unyielding checklist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    keltner_checklist_accuracy,
    "Do these 15 questions accurately predict the 'Mountain' of long-term survival, or are they a 'Rope' of cultural preference?",
    resolution_mechanism("Longitudinal study of couples who pass the list vs. those who fail but stay together"),
    impact("If accurate: It is a true Mountain. If cultural: It is a local Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    personality_stability_adult,
    "Is 'agreeable and emotionally stable' an unchangeable Mountain, or can microdosing/therapy create a Rope for change?",
    resolution_mechanism("Audit of adult personality shifts following intervention vs. 'Keltner' scores"),
    impact("If unchangeable: Keltner's Mountain is valid. If changeable: The list is a Noose for the potentially transformed."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Idiosyncratic Negotiation
 * Viability: Relationships that don't share "passwords" or "best friend" status 
 * but thrive on privacy and independence.
 * Suppression: Suppressed by questions 8 ("trust with passwords") and 6 ("best friend").
 * * ALTERNATIVE 2: The "Magic Ratio" (5:1)
 * Viability: Using a purely observational metric of interaction quality rather 
 * than a cognitive checklist of 15 values.
 * Suppression: The Keltner List replaces the "Rope" of interaction volume 
 * with a "Mountain" of categorical alignment.
 * * CONCLUSION:
 * The existence of successful, private relationships makes the Keltner List 
 * a "Noose" when applied to non-standard dyads.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_keltner_relationship_evaluation].
% Multi-perspective: ?- constraint_indexing:multi_index_report(keltner_relationship_evaluation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
