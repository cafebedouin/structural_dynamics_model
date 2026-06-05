% ============================================================================
% CONSTRAINT STORY: ai_training_data_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_training_data_dependency, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: ai_training_data_dependency
 *   human_readable: AI Training Data Dependency
 *   domain: technological
 *
 * SUMMARY:
 *   AI models, particularly in specialized fields like semiconductor design,
 *   increasingly rely on vast amounts of training data. This dependency
 *   creates a complex ecosystem with both coordination and extraction
 *   aspects. AI model developers benefit from access to data, while
 *   organizations lacking such access are disadvantaged. This can lead to
 *   monopolies. Society may suffer from biased outcomes if the training data
 *   reflects societal biases.
 *
 * KEY AGENTS:
 *   - AI Model Developers: Primary beneficiaries (institutional/arbitrage) - benefit from training data to develop and improve AI models.
 *   - Data Owners: Secondary beneficiaries (organized/constrained) - benefit from selling data, but are increasingly subject to pressure.
 *   - Organizations without access to data: Primary victims (powerless/trapped) - cannot compete in AI development.
 *   - Society at Large: Potential victims (powerless/trapped) - may suffer from biased outcomes if AI is trained on biased datasets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_training_data_dependency, 0.55).
domain_priors:suppression_score(ai_training_data_dependency, 0.65).
domain_priors:theater_ratio(ai_training_data_dependency, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_training_data_dependency, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_training_data_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_training_data_dependency, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_training_data_dependency, tangled_rope).
narrative_ontology:human_readable(ai_training_data_dependency, "AI Training Data Dependency").
narrative_ontology:topic_domain(ai_training_data_dependency, "technological").

domain_priors:requires_active_enforcement(ai_training_data_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_training_data_dependency, ai_model_developers).
narrative_ontology:constraint_beneficiary(ai_training_data_dependency, data_owners).
narrative_ontology:constraint_victim(ai_training_data_dependency, organizations_without_access_to_data).
narrative_ontology:constraint_victim(ai_training_data_dependency, society_at_large).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Organizations without access to sufficient training data are trapped, unable to compete in AI development. They bear the cost of being excluded from advancements.
constraint_indexing:constraint_classification(ai_training_data_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% AI Model Developers benefit from the availability of training data, enabling them to create and improve AI models. They have arbitrage exit options by switching to different datasets or model architectures.
constraint_indexing:constraint_classification(ai_training_data_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees the dependency as a Tangled Rope: AI development relies on data, creating a system with both coordination and extraction aspects.
constraint_indexing:constraint_classification(ai_training_data_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Data owners have some influence, but are constrained by ethical considerations, regulations, and competition. They benefit from data access agreements, but are increasingly being pressured to make data accessible
constraint_indexing:constraint_classification(ai_training_data_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Society at large may become victims if AI is trained on biased datasets, leading to unfair or discriminatory outcomes
constraint_indexing:constraint_classification(ai_training_data_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_training_data_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_training_data_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_training_data_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_training_data_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_training_data_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The AI model developers derive significant value from the training data, and the organizations that do not have access suffer as a result. Suppression (0.65): High. The organizations lacking data are highly suppressed in their ability to build AI applications. Theater ratio (0.30): Relatively low. The theater ratio is somewhat low because there is still useful functional activity, and the AI development process benefits from the collected data.
 *
 * PERSPECTIVAL GAP:
 *   Organizations without data view it as a snare, while AI model developers view it as coordination (rope). Analytical observers view it as a tangled rope. The data owners' perspective is a bit tangled_rope as they benefit from access to their proprietary data, but also are pressured.
 *
 * DIRECTIONALITY LOGIC:
 *   AI model developers are beneficiaries; organizations without access are victims; analytical observers see both coordination and extraction. The directionality follows from those relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This is Tangled Rope because there is a genuine coordination aspect of providing useful data for AI model development, but the dependence creates extraction because some organizations are excluded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_quality_vs_quantity,
    'What is the relative importance of data quality versus quantity in achieving reliable AI performance?',
    'Empirical studies comparing AI models trained on different datasets with varying quality and quantity.',
    'If quality is paramount, efforts should focus on data curation. If quantity is key, then data aggregation is more important.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_quality_vs_quantity, empirical, 'Relative importance of data quality versus quantity.').

omega_variable(
    bias_mitigation_effectiveness,
    'How effective are current techniques for mitigating bias in AI training data?',
    'Benchmarking and comparing the performance of AI models trained with and without bias mitigation techniques.',
    'If bias mitigation techniques are ineffective, AI outcomes may remain unfair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_mitigation_effectiveness, empirical, 'Effectiveness of current bias mitigation techniques.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_training_data_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_t_tr_t0, ai_training_data_dependency, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_t_tr_t5, ai_training_data_dependency, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ai_t_tr_t10, ai_training_data_dependency, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_t_be_t0, ai_training_data_dependency, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_t_be_t5, ai_training_data_dependency, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_t_be_t10, ai_training_data_dependency, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_training_data_dependency, resource_allocation).
narrative_ontology:affects_constraint(ai_training_data_dependency, algorithm_bias).
narrative_ontology:affects_constraint(ai_training_data_dependency, data_privacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
