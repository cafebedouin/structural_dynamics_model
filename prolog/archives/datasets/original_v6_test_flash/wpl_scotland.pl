% ============================================================================
% CONSTRAINT STORY: wpl_scotland
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wpl_scotland, []).

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
 *   constraint_id: wpl_scotland
 *   human_readable: Scotland's Workplace Parking Levy (WPL)
 *   domain: economic
 *
 * SUMMARY:
 *   Scottish councils have been granted the power to implement a Workplace
 *   Parking Levy (WPL), a charge on employers for providing parking spaces.
 *   This levy aims to reduce congestion and promote the use of public
 *   transport. The effectiveness and fairness of the WPL depend on various
 *   factors, including the availability of alternative transport options and
 *   the ability of employers and employees to absorb the cost.
 *
 * KEY AGENTS:
 *   - Scottish Councils: Beneficiary (institutional/arbitrage) - Receives revenue from the levy for transport improvements.
 *   - Public Transport Operators: Beneficiary (institutional/arbitrage) - Benefits from increased ridership.
 *   - Employers: Victim (moderate/constrained) - Face additional costs for providing parking.
 *   - Employees: Victim (powerless/trapped) - May bear the cost of the levy if employers pass it on.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wpl_scotland, 0.55).
domain_priors:suppression_score(wpl_scotland, 0.4).
domain_priors:theater_ratio(wpl_scotland, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wpl_scotland, extractiveness, 0.55).
narrative_ontology:constraint_metric(wpl_scotland, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(wpl_scotland, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wpl_scotland, tangled_rope).
narrative_ontology:human_readable(wpl_scotland, "Scotland's Workplace Parking Levy (WPL)").
narrative_ontology:topic_domain(wpl_scotland, "economic").

domain_priors:requires_active_enforcement(wpl_scotland).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wpl_scotland, scottish_councils).
narrative_ontology:constraint_beneficiary(wpl_scotland, public_transport_operators).
narrative_ontology:constraint_victim(wpl_scotland, employers).
narrative_ontology:constraint_victim(wpl_scotland, employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Employees who have no viable alternative to driving to work and cannot afford the additional cost of parking. They are trapped and bear the brunt of the levy.
constraint_indexing:constraint_classification(wpl_scotland, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Employers are constrained as they need to provide parking for some employees but face the cost of the levy. They benefit indirectly from improved public transport and reduced congestion.
constraint_indexing:constraint_classification(wpl_scotland, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Councils benefit from the revenue generated by the levy, which can be used to fund transport improvements. They can adjust the levy to optimize revenue.
constraint_indexing:constraint_classification(wpl_scotland, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Public transport operators benefit from increased ridership as a result of the WPL.
constraint_indexing:constraint_classification(wpl_scotland, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% The analytical observer sees the WPL as a mechanism to reduce congestion and promote public transport, but also recognizes the potential for unintended consequences and distributional effects.
constraint_indexing:constraint_classification(wpl_scotland, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wpl_scotland_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wpl_scotland, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wpl_scotland, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wpl_scotland, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wpl_scotland_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The WPL extracts resources from employers and potentially employees to fund transport improvements. The level of extraction is dependent on the specific levy rate and the availability of alternatives. Suppression (0.40): Moderate. The WPL suppresses the availability of workplace parking, encouraging the use of alternative transport. However, it does not completely eliminate the option of driving to work. Theater ratio (0.20): Low. The WPL has a primarily functional role in reducing congestion and generating revenue. The performative aspect is relatively low.
 *
 * PERSPECTIVAL GAP:
 *   Employees with limited transport alternatives experience the WPL as a snare, while employers see it as a tangled rope due to the cost and indirect benefits. Councils and transport operators view it as a rope facilitating transport improvements. The analytical observer recognizes the complexity of the system, acknowledging both positive and negative consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Councils, Transport Operators) have low directionality due to the benefits they receive. Victims (Employers, Employees) have high directionality because they bear the cost. The exit options influence the magnitude of the directionality. Trapped employees experience higher extraction than employers with arbitrage options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elasticity_of_demand_for_parking,
    'How sensitive is the demand for workplace parking to the levy? Will people switch to public transport, carpool, or find other alternatives?',
    'Empirical study of changes in parking demand and mode of transport after WPL implementation.',
    'High elasticity: WPL is effective in reducing congestion. Low elasticity: WPL primarily generates revenue with little impact on congestion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elasticity_of_demand_for_parking, empirical, 'Sensitivity of parking demand to the WPL.').

omega_variable(
    administrative_burden,
    'What is the administrative cost of implementing and enforcing the WPL?',
    'Cost-benefit analysis of WPL implementation.',
    'High administrative burden: WPL is inefficient and costly. Low administrative burden: WPL is a relatively efficient revenue source.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_burden, empirical, 'Administrative cost of WPL implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wpl_scotland, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpl__tr_t0, wpl_scotland, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wpl__tr_t5, wpl_scotland, theater_ratio, 5, 0.15).
narrative_ontology:measurement(wpl__tr_t10, wpl_scotland, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(wpl__be_t0, wpl_scotland, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(wpl__be_t5, wpl_scotland, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(wpl__be_t10, wpl_scotland, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wpl_scotland, resource_allocation).
narrative_ontology:affects_constraint(wpl_scotland, congestion_pricing).
narrative_ontology:affects_constraint(wpl_scotland, public_transport_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
