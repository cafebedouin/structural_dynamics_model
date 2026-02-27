% ============================================================================
% CONSTRAINT STORY: meta_model_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_model_lock_in, []).

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
 *   constraint_id: meta_model_lock_in
 *   human_readable: The Ontological Cage
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   A foundational AI model or classification framework (the Meta-Model)
 *   becomes so deeply integrated into social and technical infrastructure
 *   that alternative ways of perceiving or organizing reality are suppressed.
 *   This creates an "Ontological Cage" where innovative thought and diverse
 *   perspectives are stifled. Widespread adoption leads to extraction from
 *   cognitive diversity, as novel viewpoints become harder to express and
 *   explore.
 *
 * KEY AGENTS:
 *   - Meta-Model Developers: Institutional actor benefiting from widespread adoption.
 *   - Early Adopters of Meta-Model: Benefit from network effects and compatibility.
 *   - Cognitive Diversity: Represents alternative worldviews, targeted by the suppression.
 *   - Independent Model Developers: Constrained by the dominance of the Meta-Model.
 *   - Critical Thinking: Increasingly aligned to meta-model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_model_lock_in, 0.6).
domain_priors:suppression_score(meta_model_lock_in, 0.7).
domain_priors:theater_ratio(meta_model_lock_in, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_model_lock_in, extractiveness, 0.6).
narrative_ontology:constraint_metric(meta_model_lock_in, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(meta_model_lock_in, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_model_lock_in, tangled_rope).
narrative_ontology:human_readable(meta_model_lock_in, "The Ontological Cage").
narrative_ontology:topic_domain(meta_model_lock_in, "technological/cognitive").

domain_priors:requires_active_enforcement(meta_model_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_model_lock_in, meta_model_developers).
narrative_ontology:constraint_beneficiary(meta_model_lock_in, early_adopters_of_meta_model).
narrative_ontology:constraint_victim(meta_model_lock_in, cognitive_diversity).
narrative_ontology:constraint_victim(meta_model_lock_in, independent_model_developers).
narrative_ontology:constraint_victim(meta_model_lock_in, critical_thinking).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Cognitive diversity, representing alternative worldviews, is trapped and suppressed by the dominant meta-model, leading to a snare classification.
constraint_indexing:constraint_classification(meta_model_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Independent model developers are constrained by the need to conform to or compete with the meta-model, but also benefit from its existence as a common reference point, resulting in a tangled rope classification.
constraint_indexing:constraint_classification(meta_model_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Meta-model developers benefit from the widespread adoption of their model, which establishes their framework as the standard, leading to a rope classification.
constraint_indexing:constraint_classification(meta_model_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Critical thinking risks becoming performative in light of the dominant meta-model. Theater_ratio increases as critical thinking becomes about alignment, not about truth-seeking.
constraint_indexing:constraint_classification(meta_model_lock_in, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_model_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_model_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_model_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_model_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_model_lock_in, TR),
    TR >= 0.70.

:- end_tests(meta_model_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Significant extraction occurs as the meta-model becomes entrenched. Suppression (0.7): The meta-model actively or passively suppresses alternative viewpoints, making cognitive diversity difficult to maintain. Theater Ratio (0.3): Low amount of pure theater; genuine usage and adoption are the primary drivers.
 *
 * PERSPECTIVAL GAP:
 *   The gap arises from the different positions within the ecosystem. The meta-model developers see a rope, while those who are suppressed and challenged by the meta-model see a snare or tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's relationship to the meta-model. Developers benefit and have arbitrage options, while those representing alternative worldviews are trapped and bear the costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_malleability,
    'To what extent can the meta-model be reshaped and adapted to accommodate diverse perspectives?',
    'Assess the architectural flexibility and extensibility of the model.',
    'If highly malleable: shifts towards a rope classification. If rigid: solidifies the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_malleability, empirical, 'Assess the malleability of the meta-model.').

omega_variable(
    alternative_framework_viability,
    'How viable are alternative cognitive frameworks in challenging or complementing the meta-model?',
    'Evaluate the resources, adoption rates, and performance of alternative models.',
    'If viable alternatives emerge: moves to scaffold. If no alternatives: snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_viability, empirical, 'Assess the viability of competing cognitive frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_model_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_tr_t0, meta_model_lock_in, theater_ratio, 0, 0.1).
narrative_ontology:measurement(meta_tr_t5, meta_model_lock_in, theater_ratio, 5, 0.2).
narrative_ontology:measurement(meta_tr_t10, meta_model_lock_in, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(meta_be_t0, meta_model_lock_in, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(meta_be_t5, meta_model_lock_in, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(meta_be_t10, meta_model_lock_in, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_model_lock_in, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
