% ============================================================================
% CONSTRAINT STORY: heglig_oil_field_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_heglig_oil_field_control, []).

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
 *   constraint_id: heglig_oil_field_control
 *   human_readable: Control of Heglig Oil Field as a Strategic War Asset
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   In the context of the Sudanese civil war, the paramilitary Rapid Support
 *   Forces (RSF) have captured Heglig, the country's largest oil field. This
 *   control acts as a strategic war asset, providing the RSF with a
 *   significant revenue stream while depriving the Sudanese state and local
 *   populations of crucial economic resources. The international community's
 *   efforts to resolve the conflict and restore stability are facing
 *   substantial challenges.
 *
 * KEY AGENTS:
 *   - RSF: Primary beneficiary (powerful/arbitrage) - Benefits from oil revenue to sustain war efforts.
 *   - Sudanese State: Primary victim (moderate/constrained) - Loses critical revenue, hindering governance and stability.
 *   - Local Populations: Secondary victim (powerless/trapped) - Experience displacement, disruption, and economic hardship.
 *   - International Community: Observer (institutional/constrained) - Attempts mediation and peace efforts with limited success.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heglig_oil_field_control, 0.75).
domain_priors:suppression_score(heglig_oil_field_control, 0.8).
domain_priors:theater_ratio(heglig_oil_field_control, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heglig_oil_field_control, extractiveness, 0.75).
narrative_ontology:constraint_metric(heglig_oil_field_control, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(heglig_oil_field_control, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(heglig_oil_field_control, tangled_rope).
narrative_ontology:human_readable(heglig_oil_field_control, "Control of Heglig Oil Field as a Strategic War Asset").
narrative_ontology:topic_domain(heglig_oil_field_control, "geopolitical/economic").

domain_priors:requires_active_enforcement(heglig_oil_field_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(heglig_oil_field_control, rsf).
narrative_ontology:constraint_victim(heglig_oil_field_control, sudanese_state).
narrative_ontology:constraint_victim(heglig_oil_field_control, international_community).
narrative_ontology:constraint_victim(heglig_oil_field_control, local_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Local Populations (Snare). Trapped within the conflict zone, they experience the control of the oil field as pure extraction, with limited to no benefits and significant disruption to their lives and livelihoods. No exit options.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Sudanese State (Snare). The loss of the oil field represents a major economic blow, depriving the state of crucial revenue. Constrained in its ability to regain control due to ongoing conflict and RSF entrenchment. Significant, ongoing extraction.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Rapid Support Forces (RSF) (Tangled Rope). Beneficiaries of the oil field's revenue, which fuels their war efforts. However, they also face the costs of maintaining control against resistance and international pressure. They must actively enforce their control (requires_active_enforcement = true) but benefit from the resource flow, making it a Tangled Rope. High coordination costs in enforcement.
constraint_indexing:constraint_classification(heglig_oil_field_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Perspective 4: International Community (Piton). International efforts to mediate and restore stability are largely performative due to lack of enforcement mechanisms and continued conflict, leading to a Piton classification. High theater, low effectiveness in regaining control.
constraint_indexing:constraint_classification(heglig_oil_field_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Analytical Observer (Tangled Rope). From a global perspective, the control of Heglig represents a complex mix of extraction and coordination. RSF extracts resources to fund conflict, but also establishes a degree of control that coordinates resource allocation (albeit coercively). International actors are affected, but also observe and analyze without direct intervention. Enforcement costs drive categorization toward tangled rope. Requires active enforcement.
constraint_indexing:constraint_classification(heglig_oil_field_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heglig_oil_field_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(heglig_oil_field_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(heglig_oil_field_control, TR),
    TR >= 0.70.

:- end_tests(heglig_oil_field_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The RSF's control of the oil field allows them to extract significant economic resources, which are diverted from the Sudanese state and local populations. Suppression (0.80): High. The RSF actively suppresses any resistance to their control of the oil field, limiting the Sudanese state and local populations' ability to regain control or benefit from the resources. Theater Ratio (0.75): High. The international community's efforts to mediate and restore stability are largely performative due to lack of enforcement mechanisms and continued conflict.
 *
 * PERSPECTIVAL GAP:
 *   The local populations and Sudanese state experience the control of the oil field as a snare, deprived of essential revenue and resources. The RSF views it as a tangled rope, gaining benefits from the revenue but incurring costs through the ongoing conflict and international pressure. The international community's peace efforts are a piton, being largely performative with little functional impact. The analytical observer recognizes the overall situation as a tangled rope, involving active enforcement and a combination of extraction and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The RSF benefits directly from the oil revenue, while the Sudanese state and local populations bear the costs of the loss of this revenue. The RSF's power and ability to arbitrage give them a low d value, whereas the Sudanese state and local populations have high d values due to their lack of power and limited exit options. The international community's d value is in the middle, reflecting their constrained ability to influence the situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a tangled rope due to the high degree of extraction and suppression, combined with active enforcement by the RSF. While the RSF benefits, they also incur costs through conflict and international pressure, making it a mixed coordination-extraction scenario. The high extractiveness is balanced by the active enforcement required to maintain control. The international community's role is largely performative, contributing to the piton classification from their perspective. The RSF's enforcement activity is related to maintaining the extraction, not contributing to genuine coordination for the benefit of all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rsf_control_duration,
    'How long will the RSF maintain control of the oil field?',
    'Monitoring military and political developments in Sudan',
    'Longer control increases extraction and suppression; shorter control reduces the long-term impact on the Sudanese economy',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rsf_control_duration, empirical, 'Duration of RSF control over Heglig').

omega_variable(
    international_intervention_threshold,
    'What level of humanitarian crisis or regional instability would trigger effective international intervention?',
    'Analyzing geopolitical dynamics and international security considerations',
    'Higher threshold implies more prolonged conflict and extraction; lower threshold could lead to restoration of Sudanese state control',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_intervention_threshold, preference, 'Threshold for international intervention').

omega_variable(
    legitimacy_of_rsf_control,
    'To what extent is the RSF''s control of the oil field seen as legitimate by local populations and international actors?',
    'Analyzing public opinion surveys, media coverage, and diplomatic statements',
    'Higher legitimacy could lead to reduced resistance and increased stability; lower legitimacy could lead to increased conflict and instability',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_rsf_control, conceptual, 'Legitimacy of RSF control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(heglig_oil_field_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hegl_tr_t0, heglig_oil_field_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hegl_tr_t5, heglig_oil_field_control, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hegl_tr_t10, heglig_oil_field_control, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(hegl_be_t0, heglig_oil_field_control, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(hegl_be_t5, heglig_oil_field_control, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(hegl_be_t10, heglig_oil_field_control, base_extractiveness, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(heglig_oil_field_control, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
