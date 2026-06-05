% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_boundaries, []).

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
 *   constraint_id: planetary_boundaries
 *   human_readable: Planetary Boundaries Framework
 *   domain: environmental/economic
 *
 * SUMMARY:
 *   The Planetary Boundaries framework aims to define a safe operating space
 *   for humanity by identifying critical Earth system processes and their
 *   thresholds. While intended as a coordination mechanism for global
 *   sustainability, its implementation involves inherent extraction and
 *   suppression, leading to varied perspectives. Developing nations may
 *   perceive it as a snare, while the scientific community views it as a
 *   rope. The framework's success hinges on addressing equity concerns and
 *   ensuring economically feasible transitions.
 *
 * KEY AGENTS:
 *   - Developing Nations: Primary target (powerless/trapped) - Bear disproportionate burden, face constrained development.
 *   - Fossil Fuel Industry: Constrained actor (moderate/constrained) - Faces transition challenges, potential opportunities.
 *   - Scientific Community: Beneficiary (institutional/analytical) - Benefits from structured risk assessment.
 *   - Environmental Organizations: Organized actor (organized/mobile) - Advocacy based on scientific evidence.
 *   - Analytical Observer: Comprehensive view (analytical/analytical) - Acknowledges coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_boundaries, 0.6).
domain_priors:suppression_score(planetary_boundaries, 0.7).
domain_priors:theater_ratio(planetary_boundaries, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_boundaries, extractiveness, 0.6).
narrative_ontology:constraint_metric(planetary_boundaries, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(planetary_boundaries, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_boundaries, tangled_rope).
narrative_ontology:human_readable(planetary_boundaries, "Planetary Boundaries Framework").
narrative_ontology:topic_domain(planetary_boundaries, "environmental/economic").

domain_priors:requires_active_enforcement(planetary_boundaries).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_boundaries, scientific_community).
narrative_ontology:constraint_beneficiary(planetary_boundaries, environmental_organizations).
narrative_ontology:constraint_victim(planetary_boundaries, developing_nations).
narrative_ontology:constraint_victim(planetary_boundaries, fossil_fuel_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Developing nations are often trapped, bearing a disproportionate burden of environmental degradation and facing constrained development options due to the framework's limits. They have limited exit options and are highly vulnerable to the extraction caused by restricted resource use.
constraint_indexing:constraint_classification(planetary_boundaries, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The fossil fuel industry faces constrained exit options as it transitions away from its core business. It experiences significant extraction due to the framework's emphasis on reducing carbon emissions, but also benefits from potential opportunities in renewable energy and carbon capture technologies.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The scientific community benefits from the framework as it provides a structured approach for assessing and communicating environmental risks. This perspective highlights the coordination aspect of the framework in guiding research and policy efforts.
constraint_indexing:constraint_classification(planetary_boundaries, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Environmental organizations benefit from the framework as it provides a scientific basis for advocacy and policy interventions. However, they also face the challenge of navigating the complexities and uncertainties associated with the framework, experiencing a mixed coordination and extraction dynamic.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the framework as a tangled rope, acknowledging both its coordination benefits in guiding environmental policy and its extractive aspects in limiting development options for certain actors. The high extractiveness stems from the framework's potential to impose significant constraints on resource use and economic activities.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_boundaries_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_boundaries, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planetary_boundaries, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(planetary_boundaries_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-high. The framework restricts resource use and economic activities, leading to significant extraction from certain actors. Suppression (0.70): High. The framework's emphasis on reducing carbon emissions and preserving biodiversity limits development options and suppresses alternative approaches. Theater Ratio (0.30): Low. The framework's primary function is to guide policy and research, with limited performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the framework's uneven distribution of costs and benefits. Developing nations, facing constrained development options, perceive it as a snare. The scientific community views it as a rope, guiding research and policy. Environmental organizations benefit from the framework's scientific basis but also navigate its complexities. The analytical observer sees both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural relationships between agents and the framework. Developing nations, as primary targets, experience high directionality. The scientific community, as beneficiaries, experiences low directionality. Environmental organizations have moderate directionality due to their mixed role. The overall tangled rope classification arises from the framework's combination of coordination and asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Planetary Boundaries framework is classified as a Tangled Rope due to its combination of coordination and asymmetric extraction. It is not purely a Snare because it provides a coordination function by defining a safe operating space. It is not purely a Rope because it involves significant extraction and suppression, particularly for developing nations. Mandatrophy is resolved by acknowledging both the coordination benefits and the extractive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_threshold_uncertainty,
    'How accurately can the thresholds for each planetary boundary be determined?',
    'Improved Earth system modeling, data collection, and interdisciplinary research.',
    'If thresholds are too conservative, economic development may be unnecessarily constrained. If thresholds are too lenient, irreversible environmental damage may occur.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_threshold_uncertainty, empirical, 'Uncertainty in the precise location of planetary boundary thresholds.').

omega_variable(
    equity_distribution_effects,
    'How are the costs and benefits of adhering to planetary boundaries distributed across different nations and communities?',
    'Economic modeling, social impact assessments, and participatory decision-making processes.',
    'If the framework disproportionately burdens developing nations, it may face resistance and undermine its effectiveness. Fair distribution enhances cooperation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_distribution_effects, preference, 'Equity considerations in the implementation of the Planetary Boundaries framework.').

omega_variable(
    economic_feasibility_transitions,
    'What is the economic feasibility of transitioning to a sustainable economy within planetary boundaries?',
    'Technological innovation, policy reforms, and investment in sustainable infrastructure.',
    'If the transition is economically unviable, it may face political opposition and hinder progress towards sustainability. Viable transitions foster adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_feasibility_transitions, empirical, 'Economic viability of transitioning to a sustainable economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_boundaries, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plan_tr_t0, planetary_boundaries, theater_ratio, 0, 0.1).
narrative_ontology:measurement(plan_tr_t5, planetary_boundaries, theater_ratio, 5, 0.2).
narrative_ontology:measurement(plan_tr_t10, planetary_boundaries, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(plan_be_t0, planetary_boundaries, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(plan_be_t5, planetary_boundaries, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(plan_be_t10, planetary_boundaries, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_boundaries, information_standard).
narrative_ontology:affects_constraint(planetary_boundaries, sustainable_development_goals).
narrative_ontology:affects_constraint(planetary_boundaries, paris_agreement).

% DUAL FORMULATION NOTE:
% The planetary boundaries framework provides a scientific basis for the Sustainable Development Goals and the Paris Agreement, but it also involves distinct extractive and suppressive elements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
