% ============================================================================
% CONSTRAINT STORY: isa_education_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_isa_education_scaffold, []).

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
 *   constraint_id: isa_education_scaffold
 *   human_readable: Income Share Agreement (ISA) Funding for Education
 *   domain: economic/educational
 *
 * SUMMARY:
 *   An Income Share Agreement (ISA) allows students to access education with
 *   zero upfront cost in exchange for a fixed percentage of future earnings
 *   over a set term. It acts as a scaffold by providing temporary support for
 *   education funding, with a sunset clause as the income sharing term ends.
 *   It can coordinate access to education without requiring upfront capital.
 *
 * KEY AGENTS:
 *   - Students: Primary beneficiaries (moderate/mobile) - access education without upfront cost.
 *   - Educational Institutions: Beneficiaries (institutional/arbitrage) - increased enrollment and revenue.
 *   - Taxpayers: Potential victims (powerless/trapped) - bear the risk of subsidized ISA programs.
 *   - ISA Providers: Intermediaries (powerful/constrained) - manage the agreements and bear some risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(isa_education_scaffold, 0.35).
domain_priors:suppression_score(isa_education_scaffold, 0.25).
domain_priors:theater_ratio(isa_education_scaffold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(isa_education_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(isa_education_scaffold, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(isa_education_scaffold, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(isa_education_scaffold, scaffold).
narrative_ontology:human_readable(isa_education_scaffold, "Income Share Agreement (ISA) Funding for Education").
narrative_ontology:topic_domain(isa_education_scaffold, "economic/educational").

domain_priors:requires_active_enforcement(isa_education_scaffold).
narrative_ontology:has_sunset_clause(isa_education_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(isa_education_scaffold, students).
narrative_ontology:constraint_beneficiary(isa_education_scaffold, educational_institutions).
narrative_ontology:constraint_victim(isa_education_scaffold, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Educational institutions benefit from increased enrollment and revenue without the risk of student loan defaults.
constraint_indexing:constraint_classification(isa_education_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Students can access education without upfront costs, but bear the risk of income sharing. They have mobility (can seek employment elsewhere) and over time the ISA ends so it acts as a scaffold.
constraint_indexing:constraint_classification(isa_education_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% From an analytical perspective, ISAs represent a tangled rope: they coordinate education funding and student access but also extract a portion of future income. The sunset clause is key to it being a scaffold.
constraint_indexing:constraint_classification(isa_education_scaffold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% Taxpayers may bear the risk of subsidized ISA programs if the returns are lower than expected, and they don't get a direct benefit. They are trapped by the political system.
constraint_indexing:constraint_classification(isa_education_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(isa_education_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(isa_education_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(isa_education_scaffold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(isa_education_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, reflecting the income sharing aspect. Suppression (0.25): Moderate, reflecting some limitations on student choice of career due to the ISA obligations. Theater Ratio (0.15): Low, indicating a functional focus on education funding rather than performative compliance measures.
 *
 * PERSPECTIVAL GAP:
 *   Students see ISAs as a way to access education they might not otherwise afford. Educational institutions see it as a way to increase enrollment and revenue. Taxpayers may bear the cost if the programs are subsidized. The analytical observer sees a mixed system with both coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Students benefit from access to education, resulting in a lower d value and rope/scaffold classification. Taxpayers are at risk, so the d value is higher, resulting in a snare classification. Educational institutions benefit directly, with a d value close to zero. The analytical observer, weighing the pros and cons, sees a tangled rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_predictability,
    'How predictable are future income streams for ISA participants?',
    'Longitudinal data analysis of income trajectories for different fields of study.',
    'High predictability reduces risk for both students and ISA providers. Low predictability increases risk and may require higher income sharing percentages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_predictability, empirical, 'Predictability of income streams.').

omega_variable(
    default_risk_allocation,
    'Who bears the risk of default in ISA programs?',
    'Legal and contractual analysis of ISA agreements.',
    'If students bear the risk, ISAs may become predatory loans. If providers bear the risk, access to funding may be restricted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_risk_allocation, conceptual, 'Allocation of default risk in ISAs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(isa_education_scaffold, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isa__tr_t0, isa_education_scaffold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(isa__tr_t10, isa_education_scaffold, theater_ratio, 10, 0.15).
narrative_ontology:measurement(isa__tr_t20, isa_education_scaffold, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(isa__be_t0, isa_education_scaffold, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(isa__be_t10, isa_education_scaffold, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(isa__be_t20, isa_education_scaffold, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(isa_education_scaffold, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
