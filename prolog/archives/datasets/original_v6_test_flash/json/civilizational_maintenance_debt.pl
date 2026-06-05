% ============================================================================
% CONSTRAINT STORY: civilizational_maintenance_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilizational_maintenance_debt, []).

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
 *   constraint_id: civilizational_maintenance_debt
 *   human_readable: The Crumbling Foundation: Civilizational Maintenance Debt
 *   domain: technological
 *
 * SUMMARY:
 *   The civilizational maintenance debt refers to the growing gap between the
 *   infrastructure required to sustain modern living standards and the actual
 *   resources allocated to maintain them. This manifests as deferred
 *   maintenance, underinvestment in upgrades, and a general decline in the
 *   quality and reliability of essential services. This constraint creates a
 *   tension between short-term economic gains and long-term civilizational
 *   sustainability, with significant consequences for future generations.
 *
 * KEY AGENTS:
 *   - Short-Term Economic Actors: Primary beneficiary (institutional/arbitrage) - Benefit from deferring maintenance to free up resources for short-term gains.
 *   - Future Generations: Primary victim (powerless/trapped) - Inherit the decaying infrastructure and bear the consequences.
 *   - Critical Infrastructure: Primary victim (powerless/trapped) - Experiences neglect and decay.
 *   - Critical Infrastructure Operators: Moderate actors (moderate/constrained) - Constrained by limited budgets but benefit from subsidies.
 *   - Incumbent Industries: Degraded institutions (institutional/constrained) - Resist upgrades to existing infrastructure in order to maintain market dominance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilizational_maintenance_debt, 0.65).
domain_priors:suppression_score(civilizational_maintenance_debt, 0.7).
domain_priors:theater_ratio(civilizational_maintenance_debt, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_maintenance_debt, extractiveness, 0.65).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilizational_maintenance_debt, tangled_rope).
narrative_ontology:human_readable(civilizational_maintenance_debt, "The Crumbling Foundation: Civilizational Maintenance Debt").
narrative_ontology:topic_domain(civilizational_maintenance_debt, "technological").

domain_priors:requires_active_enforcement(civilizational_maintenance_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, short_term_economic_actors).
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, incumbent_industries).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, future_generations).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, critical_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations inherit the decaying infrastructure and bear the consequences of deferred maintenance, with limited ability to influence present decisions.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Operators are constrained by limited budgets and regulatory frameworks, but also benefit from government contracts and subsidies. They experience both the coordination and extraction aspects of the maintenance debt.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% These actors (e.g., politicians focused on election cycles, corporations focused on quarterly profits) benefit from deferring maintenance as it frees up resources for more immediate gains. They can 'arbitrage' the system by prioritizing short-term gains over long-term sustainability.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Incumbent industries (e.g., those profiting from existing infrastructure) may resist innovation and maintenance upgrades to protect their market share, but are also constrained by existing regulations and customer expectations. Their power has atrophied, as nimbler competitors enter the space. The performative aspect is that they make symbolic gestures towards maintenance without actually addressing the underlying issues.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the analytical observer sees the tension between short-term gains and long-term sustainability as a tangled rope: some coordination is needed to maintain basic functionality, but significant extraction occurs from future generations.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_maintenance_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civilizational_maintenance_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilizational_maintenance_debt, TR),
    TR >= 0.70.

:- end_tests(civilizational_maintenance_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The extraction comes from future generations who bear the burden of decaying infrastructure. Short-term economic actors extract value by deferring maintenance. Suppression (0.70): High. The lack of effective political mechanisms for prioritizing long-term infrastructure needs suppresses alternatives. Theater Ratio (0.75): High. Some efforts are made to address maintenance needs, but these are often insufficient to prevent further decline. Incumbent industries make symbolic gestures towards maintenance without actually addressing the underlying issues.
 *
 * PERSPECTIVAL GAP:
 *   Future generations experience this as a snare, as they are trapped with the decaying infrastructure. Short-term economic actors see it as a rope, a way to free up resources for immediate gains. Critical Infrastructure Operators experience a tangled rope, constrained by budgets but benefiting from contracts. Incumbent industries see a piton as the rules around maintenance are vestiges of a system in decay, with symbolic gestures being made to address the issue.
 *
 * DIRECTIONALITY LOGIC:
 *   Short term economic actors are beneficiaries because they gain by deferring maintenance. Future generations are the victims because they must bear the cost of deferred maintenance. Incumbent industries, although benefitting from the current system, also have constraints in existing structure making them constrained. Critical Infrastructure Operators are constrained as it becomes difficult to perform maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling as the benefits are realized by a small group in the present, while the costs are spread across a large group over a prolonged period of time. The focus on extraction onto future generations prevents misclassification. The high theater ratio and extractiveness necessitate the mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate_justification,
    'What is the appropriate discount rate for valuing future infrastructure needs relative to present economic benefits?',
    'Economic modeling that incorporates environmental and social costs of infrastructure failure; intergenerational equity frameworks',
    'High discount rate justifies deferring maintenance, leading to snare classification. Low discount rate prioritizes long-term sustainability, enabling a scaffold or rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_justification, preference, 'Appropriate discount rate for valuing future infrastructure needs').

omega_variable(
    technical_substitution_viability,
    'To what extent can new technologies substitute for existing infrastructure, mitigating the impact of maintenance debt?',
    'Technological forecasting and engineering assessments of the potential for disruptive innovations (e.g., decentralized energy, autonomous vehicles) to reduce reliance on aging infrastructure.',
    'High substitution viability reduces extractiveness, potentially shifting classification to scaffold. Low viability exacerbates the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_substitution_viability, empirical, 'Potential for new technologies to substitute for existing infrastructure').

omega_variable(
    political_horizon_threshold,
    'What event or trigger is needed to shorten the political horizon beyond short-term economic considerations?',
    'Historical analysis of transformative changes that are correlated to infrastructure investment.',
    'A shift in the political horizon from election cycle to long-term planning would prioritize funding maintenance, pushing this constraint towards a rope or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_horizon_threshold, conceptual, 'Required changes to shorten the political horizon').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_maintenance_debt, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t0, civilizational_maintenance_debt, theater_ratio, 0, 0.2).
narrative_ontology:measurement(civi_tr_t10, civilizational_maintenance_debt, theater_ratio, 10, 0.5).
narrative_ontology:measurement(civi_tr_t20, civilizational_maintenance_debt, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(civi_be_t0, civilizational_maintenance_debt, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(civi_be_t10, civilizational_maintenance_debt, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(civi_be_t20, civilizational_maintenance_debt, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_maintenance_debt, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
