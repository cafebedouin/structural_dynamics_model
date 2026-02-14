% ============================================================================
% CONSTRAINT STORY: availability_heuristic
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini 2.0 Flash
% Source: Tversky, A., & Kahneman, D. (1973) / Behavioral Economics
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_availability_heuristic, []).

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
    narrative_ontology:constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: availability_heuristic
 * human_readable: Availability Heuristic
 * domain: social/cognitive/economic
 * temporal_scope: Permanent (Evolved Biological Property)
 * spatial_scope: Global (Human Decision Systems)
 * * SUMMARY:
 * The availability heuristic is a mental shortcut relying on immediate, vivid 
 * examples that come to mind when evaluating a topic. It operates 
 * on the premise that if something can be recalled easily, it must be important 
 * or more frequent than alternative solutions.
 * * KEY AGENTS:
 * - The Media Architect: Shapes "availability" through selective, vivid, and 
 * frequent reporting.
 * - The Anxious Citizen: Subject to the information environment, experiencing 
 * distorted risk perception.
 * - The Risk Analyst: Attempts to use base-rate data to override the 
 * heuristic's biases.
 * * NARRATIVE ARC:
 * Originally an evolutionary "Rope" for rapid decision-making, in modern 
 * information-dense societies it becomes a "Snare" as vivid but rare events 
 * (e.g., plane crashes) strangle rational assessment of common but dull 
 * threats.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(availability_heuristic, 0, 10).
narrative_ontology:constraint_claim(availability_heuristic, tangled_rope).
domain_priors:requires_active_enforcement(availability_heuristic).

% Base extractiveness: 0.40 (Moderate)
% Rationale: Extracts "rational accuracy" and "long-term utility" from subjects 
% . Benefits entities leveraging "vividness" to influence behavior.
domain_priors:base_extractiveness(availability_heuristic, 0.40).

% Suppression: 0.70 (High)
% Rationale: Suppresses "statistical base rates" and "dull facts". 
% Ease of recall acts as a block against data-driven reasoning.
domain_priors:suppression_score(availability_heuristic, 0.70).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(availability_heuristic, extractiveness, 0.4).
narrative_ontology:constraint_metric(availability_heuristic, suppression_requirement, 0.7).

% Enforcement: Emerges naturally as a biological feature.
domain_priors:emerges_naturally(availability_heuristic).

% Mandatory Asymmetry Hooks (Required for any score > 0.3)
narrative_ontology:constraint_beneficiary(availability_heuristic, news_media).
narrative_ontology:constraint_beneficiary(availability_heuristic, advertising_executives).
narrative_ontology:constraint_victim(availability_heuristic, anxious_citizen).
narrative_ontology:constraint_victim(availability_heuristic, public_health_policy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ANXIOUS CITIZEN - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to the prevailing information environment.
   WHEN: immediate - Current state of fear or decision-making.
   WHERE: constrained - Difficulty escaping the "vividness" of modern media.
   SCOPE: local - Immediate personal environment and risks.
   
   WHY THIS CLASSIFICATION:
   For the citizen, the heuristic is a Snare. Vivid images of rare 
   disasters strangle their ability to make sane risk assessments (e.g., 
   flying vs. driving), extracting peace of mind through salience.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(availability_heuristic, snare, 
    context(agent_power(powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ADVERTISING EXECUTIVE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-shaping power in the attention economy.
   WHEN: biographical - Achieving commercial targets over a career.
   WHERE: arbitrage - Can shift focus between different campaigns/channels.
   SCOPE: national - Broad market management.
   
   WHY THIS CLASSIFICATION:
   For the executive, it is a Rope. It serves as a coordination 
   mechanism to "top-load" a brand into consumer minds through repetition 
   and vivid imagery, pulling the consumer toward a purchase.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(availability_heuristic, rope, 
    context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national))).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE COGNITIVE PSYCHOLOGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Objective observer of biological traits.
   WHEN: civilizational - Long-term evolutionary perspective.
   WHERE: trapped - Bound by the fixed neural architecture of the species.
   SCOPE: global - Universal human property.
   
   WHY THIS CLASSIFICATION:
   To the observer, it is a Mountain. It is an unchangeable 
   feature of "System 1" thinking—a physical limit on how information is 
   retrieved and weighted in the human brain.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(availability_heuristic, tangled_rope, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(trapped), spatial_scope(global))).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(availability_heuristic_tests).

/**
 * TEST 1: Multi-perspective variance
 */
test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(availability_heuristic, Type1, context(powerless, immediate, constrained, local)),
    constraint_indexing:constraint_classification(availability_heuristic, Type2, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(availability_heuristic, Type3, context(analytical, civilizational, trapped, global)),
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(powerless, immediate, constrained, local),
    ContextInstitutional = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(availability_heuristic, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(availability_heuristic, ContextInstitutional, Score2),
    Score1 > Score2.

/**
 * TEST 3: Time-horizon immutability
 */
test(evolutionary_immutability) :-
    % Civilizational horizon sees the trait as unchangeable.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(availability_heuristic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. SUPPRESSION SCORE (0.70):
 * High score reflects the biological priority given to vivid memories over 
 * statistical data.
 * * 2. MANDATROPHY RESOLUTION:
 * While base extractiveness is 0.40, the status [RESOLVED MANDATROPHY] is 
 * applied because the extraction of rational peace of mind from the citizen 
 * is systemic. Perspective 1 (Snare) vs Perspective 2 (Rope) validates 
 * the perspectival gap.
 * * 3. CLASSIFICATION RATIONALE:
 * - Powerless -> Snare: They are the subjects of the manipulated salience.
 * - Institutional -> Rope: They leverage the heuristic as a tool for influence.
 * - Analytical -> Mountain: Physics and biology of the brain are immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salience_threshold_shift,
    "Does the 'Vividness' threshold required to trigger the heuristic increase in a hyper-visual digital society?",
    resolution_mechanism("Longitudinal study of desensitization to disaster imagery across digital generations"),
    impact("If Yes: The Snare is temporary. If No: The Snare tightens as information volume grows."),
    confidence_without_resolution(medium)
).

omega_variable(
    availability_heuristic_extraction_intent,
    "Is the extraction of rational accuracy a biological necessity for survival or a predatory systemic exploit?",
    resolution_mechanism("Audit of modern media incentive structures vs. base-rate accuracy thresholds"),
    impact("If necessity: Mountain. If predatory exploit: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Base-Rate Reasoning (System 2 Thinking)
 * Viability: High, but cognitively expensive.
 * Suppression: Naturally suppressed by the brain's preference for low-energy 
 * heuristics and intentionally suppressed by vivid media reporting.
 * * CONCLUSION:
 * The presence of suppressed alternatives (statistical reasoning) shifts the 
 * experience from a natural "Mountain" to a "Snare" when media institutions 
 * actively exploit the bias.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_availability_heuristic].
 * 2. Multi-perspective: ?- multi_index_report(availability_heuristic).
 * 3. Run tests: ?- run_tests(availability_heuristic_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
