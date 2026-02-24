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
    narrative_ontology:constraint_metric/3,
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
 * relationship, it can become a "Snare" that extracts identity or justifies 
 * termination based on "nascent" imperfections.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(keltner_relationship_evaluation, 0, 10).
narrative_ontology:constraint_claim(keltner_relationship_evaluation, [social_governance]).

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

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(keltner_relationship_evaluation, extractiveness, 0.35).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, suppression_requirement, 0.45).

% Enforcement: Requires active maintenance (Truthful answering).
domain_priors:requires_active_enforcement(keltner_relationship_evaluation).



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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MEASURED SUBJECT - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Being judged by a partner with a "binary" checklist)
   WHEN: immediate (A moment of "answering truthfully" that may end the bond)
   WHERE: trapped (Subject to the partner's "rough determination" of compatibility)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a person being measured against these 15 points by a "controlling" or 
   "negativity-focused" partner, the list is a "Snare." Any "No" answer 
   is extracted as a "red flag" or justification for "liquidation" of the 
   relationship.
   
   NARRATIVE EVIDENCE:
   "answer truthfully with a simple yes or no response... Is your relationship 
   free of red flags like... controlling behavior?".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    keltner_relationship_evaluation,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(keltner_relationship_evaluation, E),

    E > 0.3.

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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE PUBLISHER / MEDIA OUTLET - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Promotes and disseminates the Keltner List)
   WHEN: biographical (Content cycles, trends)
   WHERE: arbitrage (Can choose which frameworks to promote for audience engagement)
   SCOPE: national (The audience reach)
   
   WHY THIS CLASSIFICATION:
   For a media outlet, the Keltner List is a "Rope" for generating engaging
   content. It's a tool to coordinate a large-scale conversation about
   relationships, attracting an audience by offering a simple, actionable
   framework. The list's "shareability" makes it a valuable asset for
   programmatic content.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    keltner_relationship_evaluation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(keltner_relationship_evaluation_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that belief vs. detachment transforms a Snare into a Rope.
 */
test(multi_perspective_evaluation) :-
    % Partner sees Rope
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope, context(agent_power(individual_moderate), _, _, _)),
    % Subject sees Snare
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, snare, context(agent_power(powerless), _, _, _)),
    % Counselor sees Mountain
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, mountain, context(agent_power(analytical), _, _, _)),
    % Publisher sees Rope
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope, context(agent_power(institutional), _, _, _)).

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that the powerless subject feels more extraction (judgment) than the moderate partner.
 */
test(power_extractiveness_evaluation) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(keltner_relationship_evaluation, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(keltner_relationship_evaluation, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(keltner_relationship_evaluation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. BASE EXTRACTIVENESS (0.35):
 *    Reasoning: Chose moderate-low because while the list is helpful, it 
 *    extracts the "nuance" of a relationship, replacing it with a set of metrics.
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Chose Partner (Rope), Subject (Snare), Counselor (Mountain), and Publisher (Rope) 
 *    to show how a tool can be used for growth, control, analysis, or content.
 * 
 * 3. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        keltner_checklist_accuracy,
 *        "Do these 15 questions accurately predict the 'Mountain' of long-term survival, or are they a 'Rope' of cultural preference?",
 *        resolution_mechanism("Longitudinal study of couples who pass the list vs. those who fail but stay together"),
 *        impact("If accurate: It is a true Mountain. If cultural: It is a local Rope."),
 *        confidence_without_resolution(medium)
 *    ).
 * 
 *    omega_variable(
 *        personality_stability_adult,
 *        "Is 'agreeable and emotionally stable' an unchangeable Mountain, or can microdosing/therapy create a Rope for change?",
 *        resolution_mechanism("Audit of adult personality shifts following intervention vs. 'Keltner' scores"),
 *        impact("If unchangeable: Keltner's Mountain is valid. If changeable: The list is a Snare for the potentially transformed."),
 *        confidence_without_resolution(low)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
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
 * a "Snare" when applied to non-standard dyads.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/keltner_relationship_evaluation].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(keltner_relationship_evaluation).
 * 
 * 3. Run tests:
 *    ?- run_tests(keltner_relationship_evaluation_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(keltner_relationship_evaluation).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(keltner_relationship_evaluation, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
