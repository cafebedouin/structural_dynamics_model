% ============================================================================
% CONSTRAINT STORY: evolutionary_knowledge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_knowledge, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: evolutionary_knowledge
 *   human_readable: The Primordial Pain-Epistemic Constraint
 *   domain: biological/philosophy/social
 *
 * SUMMARY:
 *   Knowledge is an evolved biological adaptation rooted in the sensation of
 *   pain. Organisms learn to avoid stimuli that cause harm, leading to
 *   increased survival rates. This constraint presents a complex interplay
 *   between subjective experience, objective understanding, and evolutionary
 *   advantage. While pain creates extraction on an individual level it
 *   simultaneously allows benefits to arise on a larger level.
 *
 * KEY AGENTS:
 *   - Organisms Experiencing Pain: Primary target (powerless/trapped) - Suffer direct negative experience.
 *   - Organisms Avoiding Immediate Threats: Primary beneficiary (institutional/arbitrage) - Species benefits from survival.
 *   - Objective Understanding of Reality: Victim (Moderate/Constrained) - Subject to distorted perception.
 *   - Analytical Observer: Civilizational observer (analytical/analytical) - Recognizes the complex interplay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_knowledge, 0.6).
domain_priors:suppression_score(evolutionary_knowledge, 0.7).
domain_priors:theater_ratio(evolutionary_knowledge, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_knowledge, extractiveness, 0.6).
narrative_ontology:constraint_metric(evolutionary_knowledge, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(evolutionary_knowledge, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_knowledge, tangled_rope).
narrative_ontology:human_readable(evolutionary_knowledge, "The Primordial Pain-Epistemic Constraint").
narrative_ontology:topic_domain(evolutionary_knowledge, "biological/philosophy/social").

domain_priors:requires_active_enforcement(evolutionary_knowledge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_knowledge, organisms_avoiding_immediate_threats).
narrative_ontology:constraint_victim(evolutionary_knowledge, organisms_experiencing_pain).
narrative_ontology:constraint_victim(evolutionary_knowledge, objective_understanding_of_reality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANISM EXPERIENCING PAIN (SNARE) - Trapped in immediate suffering, focus is solely on escaping the pain, limiting higher-level cognitive functions and objective understanding.
constraint_indexing:constraint_classification(evolutionary_knowledge, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANISM LEARNING FROM PAIN (TANGLED ROPE) - Constrained by past painful experiences, which shapes future behaviour and decision-making. Also benefits by adapting for future survival. Experiencing both positive and negative impact, so effective extraction is mid-range.
constraint_indexing:constraint_classification(evolutionary_knowledge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SPECIES BENEFITING FROM PAIN (ROPE) - Benefits from avoiding the immediate threat that triggered the pain. Extraction runs towards the species, thus negative extraction experienced.
constraint_indexing:constraint_classification(evolutionary_knowledge, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - From a civilizational/universal perspective, the observer recognizes pain as a mechanism intertwined with the evolutionary development of intelligence and knowledge, producing a tangled rope effect. While suffering constrains objective perception, it simultaneously drives survival adaptations. Significant extraction from individual experience, but necessary for collective survival and future knowledge.
constraint_indexing:constraint_classification(evolutionary_knowledge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_knowledge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_knowledge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_knowledge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evolutionary_knowledge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(evolutionary_knowledge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Moderate - The pain acts as an extraction process for the organism. Suppression: High - The pain acts as an extraction process for the organism. High suppression of possible alternative behaviours due to strong stimuli. Theater ratio: Low - Minimal theater involved.
 *
 * PERSPECTIVAL GAP:
 *   The organism experiencing pain sees pure extraction, while the species benefits from this extraction process. Objective understanding gets warped but the species benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position - their power level, exit options, and relationship to the extraction flow. The pipeline computes d from these context parameters and applies the sigmoid f(d) to produce experienced extractiveness chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The presence of victims (organisms) and beneficiaries (species) ensures that the constraint is tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjectivity_objectivity_tradeoff,
    'To what extent does the inherent subjectivity of pain distort or enhance our objective understanding of reality?',
    'Philosophical analysis and neuroscientific investigation into the relationship between subjective experience and objective measurement.',
    'Understanding the tradeoff will refine our assessment of the epistemic value of pain and its influence on knowledge acquisition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjectivity_objectivity_tradeoff, conceptual, 'Assess the inherent subjectivity of pain on our objective understanding of reality.').

omega_variable(
    long_term_epistemic_consequences,
    'What are the long-term epistemic consequences of grounding knowledge in the sensation of pain, as opposed to other possible foundations?',
    'Historical and comparative analysis of different knowledge systems and their relationship to emotional and sensory experiences.',
    'Revealing the long-term consequences will inform discussions about alternative foundations for knowledge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_epistemic_consequences, empirical, 'Determine what the epistemic consequences will be grounding knowledge in pain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_knowledge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evol_tr_t0, evolutionary_knowledge, theater_ratio, 0, 0.1).
narrative_ontology:measurement(evol_tr_t5, evolutionary_knowledge, theater_ratio, 5, 0.2).
narrative_ontology:measurement(evol_tr_t10, evolutionary_knowledge, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(evol_be_t0, evolutionary_knowledge, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(evol_be_t5, evolutionary_knowledge, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(evol_be_t10, evolutionary_knowledge, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_knowledge, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
