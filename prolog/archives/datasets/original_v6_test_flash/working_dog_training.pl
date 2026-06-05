% ============================================================================
% CONSTRAINT STORY: working_dog_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_working_dog_training, []).

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
 *   constraint_id: working_dog_training
 *   human_readable: Training of Working Dogs for Specific Tasks
 *   domain: social
 *
 * SUMMARY:
 *   This constraint describes the training regime imposed on working dogs,
 *   such as those used for bomb detection or search and rescue. The training
 *   process extracts effort and obedience from the dog, benefiting the task
 *   sponsors and trainers. The dog's perspective, however, is often one of
 *   limited choice and potentially high stress.
 *
 * KEY AGENTS:
 *   - Working Dogs: Primary target (powerless/trapped)
 *   - Dog Trainers: Primary beneficiary (moderate/mobile)
 *   - Task Sponsors: Secondary beneficiary (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(working_dog_training, 0.5).
domain_priors:suppression_score(working_dog_training, 0.4).
domain_priors:theater_ratio(working_dog_training, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(working_dog_training, extractiveness, 0.5).
narrative_ontology:constraint_metric(working_dog_training, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(working_dog_training, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(working_dog_training, tangled_rope).
narrative_ontology:human_readable(working_dog_training, "Training of Working Dogs for Specific Tasks").
narrative_ontology:topic_domain(working_dog_training, "social").

domain_priors:requires_active_enforcement(working_dog_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(working_dog_training, task_sponsors).
narrative_ontology:constraint_beneficiary(working_dog_training, dog_trainers).
narrative_ontology:constraint_victim(working_dog_training, working_dogs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The dog is subject to the training regime, often with limited agency or understanding of the broader purpose.
constraint_indexing:constraint_classification(working_dog_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The trainer coordinates the dog's activities and benefits from the dog's skills, but is also constrained by the requirements of task sponsors.
constraint_indexing:constraint_classification(working_dog_training, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Organizations (police, military, etc) sponsoring the dog's training benefit from the dog's capabilities and can choose which trainers to use.
constraint_indexing:constraint_classification(working_dog_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Training creates utility but imposes involuntary constraints on a non-human actor.
constraint_indexing:constraint_classification(working_dog_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(working_dog_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(working_dog_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(working_dog_training, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(working_dog_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(working_dog_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. The training regime extracts effort and obedience from the dog, but also provides the dog with stimulation and purpose. Suppression (0.40): Moderate. The dog's freedom of movement and choice is limited during training. Theater ratio (0.20): Low. The activities are primarily functional and involve little theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   The dog experiences the training as a constraint on its natural behaviors, while the trainers and sponsors see it as a beneficial process. The analytical observer acknowledges the ethical tension.
 *
 * DIRECTIONALITY LOGIC:
 *   The working dog is the primary victim of the training regime, experiencing trapped exit options and therefore high directionality. Trainers and task sponsors are beneficiaries, with mobile and arbitrage exit options, resulting in lower (or even negative) directionality values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dog_welfare,
    'How well does the training regime align with the dog''s physical and psychological welfare?',
    'Veterinary evaluation of dogs post-training; behavioral analysis of dog stress levels',
    'If alignment is low, ethical questions are raised. High extractiveness classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dog_welfare, empirical, 'How ethical are the training methods used?').

omega_variable(
    alternative_method_efficacy,
    'Are there alternative, less coercive training methods that would achieve similar efficacy?',
    'Comparative studies of dogs trained with different methods; long-term efficacy analysis',
    'If alternative methods are effective, the constraint could shift from tangled_rope to rope for the dog.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_method_efficacy, empirical, 'Do other methods achieve similar results with less coercion?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(working_dog_training, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(work_tr_t0, working_dog_training, theater_ratio, 0, 0.1).
narrative_ontology:measurement(work_tr_t5, working_dog_training, theater_ratio, 5, 0.2).
narrative_ontology:measurement(work_tr_t10, working_dog_training, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(work_be_t0, working_dog_training, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(work_be_t5, working_dog_training, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(work_be_t10, working_dog_training, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(working_dog_training, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
