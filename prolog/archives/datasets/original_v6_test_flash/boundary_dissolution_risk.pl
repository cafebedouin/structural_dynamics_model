% ============================================================================
% CONSTRAINT STORY: boundary_dissolution_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_dissolution_risk, []).

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
 *   constraint_id: boundary_dissolution_risk
 *   human_readable: The Infinite Porosity Trap
 *   domain: technological/labor
 *
 * SUMMARY:
 *   The Infinite Porosity Trap describes the dissolution of boundaries
 *   between work and personal life due to pervasive monitoring and
 *   connectivity. This blurring increases the amount of data available for
 *   algorithmic management systems to extract from workers, reducing their
 *   autonomy and potentially harming labor collectives. Beneficiaries include
 *   surveillance platform operators and algorithmic management systems, while
 *   victims are individual workers and labor collectives.
 *
 * KEY AGENTS:
 *   - Individual Worker: Powerless/Trapped - Suffers from constant monitoring and lack of privacy.
 *   - Labor Collectives: Moderate/Constrained - Face legal and economic restrictions.
 *   - Surveillance Platform Operators: Institutional/Arbitrage - Benefit from increased data collection.
 *   - Algorithmic Management Systems: Institutional/Mobile - Control the monitoring process and benefit from efficient labor management.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_dissolution_risk, 0.6).
domain_priors:suppression_score(boundary_dissolution_risk, 0.7).
domain_priors:theater_ratio(boundary_dissolution_risk, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_dissolution_risk, extractiveness, 0.6).
narrative_ontology:constraint_metric(boundary_dissolution_risk, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(boundary_dissolution_risk, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_dissolution_risk, tangled_rope).
narrative_ontology:human_readable(boundary_dissolution_risk, "The Infinite Porosity Trap").
narrative_ontology:topic_domain(boundary_dissolution_risk, "technological/labor").

domain_priors:requires_active_enforcement(boundary_dissolution_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_dissolution_risk, surveillance_platform_operators).
narrative_ontology:constraint_beneficiary(boundary_dissolution_risk, algorithmic_management_systems).
narrative_ontology:constraint_victim(boundary_dissolution_risk, individual_worker_autonomy).
narrative_ontology:constraint_victim(boundary_dissolution_risk, labor_collectives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of an individual worker with few outside options (powerless, trapped, immediate, local). Feels trapped by pervasive monitoring and algorithmic management with no escape, resulting in a snare.
constraint_indexing:constraint_classification(boundary_dissolution_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of a regional labor collective (moderate, constrained, biographical, regional). It benefits from some organizational capacity but is constrained by legal restrictions and economic pressures, and experiences both extraction and coordination benefits, resulting in a tangled rope.
constraint_indexing:constraint_classification(boundary_dissolution_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of a surveillance platform operator (institutional, arbitrage, generational, global). It benefits from enhanced data collection and extraction, resulting in a rope.
constraint_indexing:constraint_classification(boundary_dissolution_risk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical perspective over a civilizational timescale (analytical, analytical, civilizational, global) recognizes both the benefits and extraction mechanisms in the system and classifies it as a tangled rope.
constraint_indexing:constraint_classification(boundary_dissolution_risk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_dissolution_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_dissolution_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_dissolution_risk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boundary_dissolution_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(boundary_dissolution_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.60) because workers' data and time are constantly extracted. Suppression is also high (0.70) as workers are pressured to comply with monitoring. The theater ratio is relatively low (0.30) due to the functional nature of the monitoring, which directly informs management decisions.
 *
 * PERSPECTIVAL GAP:
 *   Workers experience this as a snare because they are constantly monitored with little recourse. Platform operators see it as a rope because it enables better resource allocation. The analytical observer recognizes that it is a tangled rope, balancing the benefits of efficient management with the costs to worker autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual workers are victims and therefore have a high 'd' value, leading to high extraction. Surveillance platform operators benefit from the arrangement and have a low 'd' value. The labor collective has a moderate 'd' value as they are both affected by and can benefit from this arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The system could be misclassified as a rope from the perspective of the surveillance platform operators who benefit from increased data extraction. However, the victim perspective reveals a clear snare dynamic for individual workers who are trapped and monitored constantly. Therefore, recognizing the power asymmetry is critical for accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_reversal_threshold,
    'What conditions would cause a reversal in the trend toward automation and monitoring?',
    'Historical analysis of similar transitions; modeling of economic and social feedback loops',
    'If likely: worker power may increase. If unlikely: snare conditions may worsen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_reversal_threshold, empirical, 'Conditions for automation reversal').

omega_variable(
    data_ownership_clarity,
    'How can individual data ownership be better defined and enforced?',
    'Legal scholarship; technical mechanisms for data sovereignty; economic modeling of data markets',
    'If clarified: individual worker autonomy increases. If not: platform operators maintain extraction advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_ownership_clarity, conceptual, 'Clarity of individual data ownership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_dissolution_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boun_tr_t0, boundary_dissolution_risk, theater_ratio, 0, 0.1).
narrative_ontology:measurement(boun_tr_t5, boundary_dissolution_risk, theater_ratio, 5, 0.2).
narrative_ontology:measurement(boun_tr_t10, boundary_dissolution_risk, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(boun_be_t0, boundary_dissolution_risk, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(boun_be_t5, boundary_dissolution_risk, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(boun_be_t10, boundary_dissolution_risk, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_dissolution_risk, resource_allocation).
narrative_ontology:affects_constraint(boundary_dissolution_risk, algorithmic_bias_amplification).
narrative_ontology:affects_constraint(boundary_dissolution_risk, digital_presenteeism_pressure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
