% ============================================================================
% CONSTRAINT STORY: institutional_mutation_without_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_mutation_without_selection, []).

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
 *   constraint_id: institutional_mutation_without_selection
 *   human_readable: The Zombie Bureaucracy Drift
 *   domain: organizational/political
 *
 * SUMMARY:
 *   This scenario describes how an institution's internal rules and goals
 *   mutate over time, while being shielded from any external 'selection' or
 *   accountability mechanism (e.g., market failure, democratic removal). Over
 *   time, the institution's purpose drifts from its original mission, and it
 *   becomes primarily focused on self-preservation and internal power
 *   struggles.
 *
 * KEY AGENTS:
 *   - External Stakeholders: Primary target (powerless/trapped) - Bear the cost of the institution's inefficiency and misaligned goals.
 *   - Internal Bureaucratic Actors: Primary beneficiary (moderate/mobile) - Benefit from the stability and predictability of the institution, but may also be constrained by it.
 *   - Organizational Mission: Secondary victim (powerless/trapped) - The original purpose of the institution is lost over time.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_mutation_without_selection, 0.6).
domain_priors:suppression_score(institutional_mutation_without_selection, 0.7).
domain_priors:theater_ratio(institutional_mutation_without_selection, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_mutation_without_selection, extractiveness, 0.6).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_mutation_without_selection, piton).
narrative_ontology:human_readable(institutional_mutation_without_selection, "The Zombie Bureaucracy Drift").
narrative_ontology:topic_domain(institutional_mutation_without_selection, "organizational/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_mutation_without_selection, internal_bureaucratic_actors).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, external_stakeholders).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, organizational_mission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Stakeholders are trapped due to lack of alternatives or political power, bearing the costs of bureaucratic drift.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The institution persists in a degraded state, with theatrical performance masking the loss of original function.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Internal actors benefit from the stability and predictability of the institution, but are also constrained by its inertia and misaligned goals. Some may exit, but others remain.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Views the long-term degradation of the institution, the increasing theater, and the loss of its original mission as a piton-like structure.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_without_selection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_mutation_without_selection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_mutation_without_selection, TR),
    TR >= 0.70.

:- end_tests(institutional_mutation_without_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The institution extracts resources from its environment and stakeholders without delivering proportional value. Suppression (0.70): External actors are suppressed from changing the institution's behavior due to lack of power or access. Theater Ratio (0.80): The institution focuses more on appearances and internal processes than on achieving its original goals.
 *
 * PERSPECTIVAL GAP:
 *   External stakeholders see the institution as a snare, extracting resources without delivering value. Internal actors see it as a tangled rope, providing stability but also imposing constraints. The analytical observer sees the overall degradation as a piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Internal actors benefit from the stability and predictability of the institution, giving them a lower directionality value. External stakeholders bear the costs of the institution's inefficiency and misaligned goals, giving them a higher directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by considering the benefits that internal actors receive, even as external stakeholders are harmed. This ensures that the constraint is not simply a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_pressure_strength,
    'How strong are the external selection pressures (market competition, democratic accountability) on the institution?',
    'Quantitative analysis of market share, public opinion polls, or election results.',
    'Strong selection pressure implies less drift. Weak selection pressure implies more drift and a higher likelihood of piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_pressure_strength, empirical, 'Strength of external selection pressure').

omega_variable(
    internal_accountability_effectiveness,
    'How effective are the internal accountability mechanisms within the institution?',
    'Qualitative assessment of internal audit reports, whistleblower protections, and employee surveys.',
    'Effective internal accountability can mitigate drift. Ineffective internal accountability exacerbates drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_accountability_effectiveness, empirical, 'Effectiveness of internal accountability mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mutation_without_selection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_mutation_without_selection, theater_ratio, 0, 0.3).
narrative_ontology:measurement(inst_tr_t5, institutional_mutation_without_selection, theater_ratio, 5, 0.55).
narrative_ontology:measurement(inst_tr_t10, institutional_mutation_without_selection, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_mutation_without_selection, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(inst_be_t5, institutional_mutation_without_selection, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(inst_be_t10, institutional_mutation_without_selection, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_mutation_without_selection, resource_allocation).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, regulatory_capture).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, mission_creep).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
