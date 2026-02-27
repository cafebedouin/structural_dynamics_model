% ============================================================================
% CONSTRAINT STORY: automatic_enrollment_defaults
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_automatic_enrollment_defaults, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: automatic_enrollment_defaults
 *   human_readable: Automatic Enrollment Defaults in Retirement Plans
 *   domain: economic/social
 *
 * SUMMARY:
 *   Automatic enrollment leverages behavioral economics to increase
 *   participation in retirement savings plans. By making enrollment the
 *   default, individuals are more likely to participate, addressing inertia
 *   and procrastination. This design choice has implications for retirement
 *   plan providers, employees, and employers.
 *
 * KEY AGENTS:
 *   - Retirement Plan Providers: Primary beneficiaries (institutional/arbitrage) — increased participation and asset management fees
 *   - Employees' Future Selves: Secondary beneficiaries (moderate/mobile) — increased retirement savings
 *   - Employers: Facilitators (powerful/mobile) — simplified plan administration
 *   - Disengaged Employees: constrained (powerless/constrained) — may have suboptimal allocations due to inertia
 *   - Behavioral Economists: Analytical observer (analytical/analytical) - advocate for the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(automatic_enrollment_defaults, 0.35).
domain_priors:suppression_score(automatic_enrollment_defaults, 0.2).
domain_priors:theater_ratio(automatic_enrollment_defaults, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, 0.35).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(automatic_enrollment_defaults, piton).
narrative_ontology:human_readable(automatic_enrollment_defaults, "Automatic Enrollment Defaults in Retirement Plans").
narrative_ontology:topic_domain(automatic_enrollment_defaults, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, retirement_plan_providers).
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, employees_future_selves).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Benefits from increased participation and asset management fees.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Benefits from increased retirement savings, even if they are initially unaware or resistant.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Benefit from simplified plan administration and potential tax advantages. They have some exit options as they can choose which plans to offer.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% See this as a scaffold - a way to encourage retirement savings until individuals become more engaged and make active decisions.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% Employees who are automatically enrolled but remain disengaged may end up with suboptimal investment allocations or contribution rates. While they may not be actively harmed, the system is not actively benefitting them, making it function as a Piton - an inert structure.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, piton,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(automatic_enrollment_defaults_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(automatic_enrollment_defaults, TR),
    TR >= 0.70.

:- end_tests(automatic_enrollment_defaults_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low to moderate. There is some extraction in the form of management fees, but the primary goal is to encourage savings. Suppression (0.20): Low. Individuals can easily opt-out, so suppression is minimal. Theater Ratio (0.70): High. The system is primarily functional, but there is a performative aspect to the initial enrollment process, as it creates the illusion of active engagement even when individuals remain passive.
 *
 * PERSPECTIVAL GAP:
 *   Retirement plan providers benefit directly from increased participation. Employees benefit through long-term savings, even if they initially perceive it as a minor inconvenience or don't fully understand the implications. Employers benefit from simpler plan administration and potential tax advantages. The piton perspective accounts for employees who, while enrolled, do not actively manage their account.
 *
 * DIRECTIONALITY LOGIC:
 *   Plan providers are structural beneficiaries with high arbitrage; employee's future selves benefit over a long time horizon with high mobility. Employers are powerful, but do not derive the primary benefit, thus d is moderate and exit options are good. Disengaged employees are structurally powerless/constrained as their plans stagnate and do not change.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(automatic_enrollment_defaults, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(automatic_enrollment_defaults, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
