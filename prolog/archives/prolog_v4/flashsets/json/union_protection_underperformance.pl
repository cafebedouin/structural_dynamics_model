% ============================================================================
% CONSTRAINT STORY: union_protection_underperformance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_union_protection_underperformance, []).

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
 *   constraint_id: union_protection_underperformance
 *   human_readable: "Just Cause" Protection for Underperforming Union Employees
 *   domain: economic/labor_relations
 *
 * SUMMARY:
 *   This constraint models the "Just Cause" and due process provisions in
 *   collective bargaining agreements that make it difficult to terminate
 *   employees for sub-par performance without exhaustive documentation and
 *   remediation attempts. The constraint leads to a variety of consequences,
 *   including reduced employer productivity and potential protection of
 *   underperforming employees at the expense of more productive ones. Union
 *   leadership benefits from increased power, while employers face increased
 *   costs and administrative burdens.
 *
 * KEY AGENTS:
 *   - Union Employees: Beneficiaries of job security (moderate/constrained)
 *   - Union Leadership: Institutional beneficiary with increased power (institutional/arbitrage)
 *   - Employer Productivity: Victim of reduced flexibility (powerless/trapped)
 *   - Non-Union Employees: Indirect victims potentially facing increased workload (moderate/constrained)
 *   - Customers: Indirect victims if product quality or service declines (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(union_protection_underperformance, 0.6).
domain_priors:suppression_score(union_protection_underperformance, 0.7).
domain_priors:theater_ratio(union_protection_underperformance, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(union_protection_underperformance, extractiveness, 0.6).
narrative_ontology:constraint_metric(union_protection_underperformance, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(union_protection_underperformance, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(union_protection_underperformance, tangled_rope).
narrative_ontology:human_readable(union_protection_underperformance, "\"Just Cause\" Protection for Underperforming Union Employees").
narrative_ontology:topic_domain(union_protection_underperformance, "economic/labor_relations").

domain_priors:requires_active_enforcement(union_protection_underperformance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(union_protection_underperformance, union_employees).
narrative_ontology:constraint_beneficiary(union_protection_underperformance, union_leadership).
narrative_ontology:constraint_victim(union_protection_underperformance, employer_productivity).
narrative_ontology:constraint_victim(union_protection_underperformance, non_union_employees).
narrative_ontology:constraint_victim(union_protection_underperformance, customers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The employer experiences this as a snare, as they are trapped by the union contract and cannot easily remove underperforming employees, leading to decreased productivity and potential losses.
constraint_indexing:constraint_classification(union_protection_underperformance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Union leadership benefits from the protection, as it solidifies their power and bargaining position. They can leverage this protection to negotiate better terms for all employees. They have arbitrage options due to their institutional power and ability to influence negotiations.
constraint_indexing:constraint_classification(union_protection_underperformance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Union employees benefit from job security, but can also be constrained by the presence of underperforming colleagues, leading to increased workloads and potential resentment. Their exit options are constrained as they are part of the union.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% The 'Just Cause' doctrine was initially intended to protect workers from arbitrary dismissal, but now it often creates more legal wrangling than it solves, with both parties expending large amounts of resources to fight. The legal system finds itself locked into an increasingly bureaucratic and unhelpful process, though some lawyers benefit from increased billing hours.
constraint_indexing:constraint_classification(union_protection_underperformance, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, this constraint represents a tangled rope, as it provides a degree of job security for employees (coordination), but also hinders employer flexibility and potentially lowers overall productivity (extraction). The effectiveness of this protection depends on the specific industry, economic conditions, and the strength of the union. The constraint has both coordination and extraction components, making it a tangled rope.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_protection_underperformance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(union_protection_underperformance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_protection_underperformance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(union_protection_underperformance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(union_protection_underperformance, TR),
    TR >= 0.70.

:- end_tests(union_protection_underperformance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is rated at 0.60 because the constraint significantly reduces employer flexibility and increases costs associated with managing underperforming employees. Suppression is rated at 0.70, because it actively suppresses the employer's ability to terminate employees even if they are not meeting expectations. The theater ratio is at 0.75 because there is a substantial legal and administrative burden associated with documenting and addressing performance issues, and the process is more than just performative. It does involve real resources and effort, and the theater has increased over time as documentation requirements have grown.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing positions of the stakeholders. Employers experience the constraint as a snare because they are trapped by the collective bargaining agreement. Union leadership views it as a rope, as it strengthens their position and allows them to advocate for their members. Employees experience it as a tangled rope, balancing the benefits of job security with the potential drawbacks of working alongside underperforming colleagues. The analytical perspective captures the complex interplay between coordination and extraction inherent in this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the flow of resources and control. Union leadership benefits from increased power and security, resulting in a low 'd' value. Employers bear the costs of reduced flexibility and productivity, leading to a high 'd' value. Employees experience both benefits and costs, resulting in a moderate 'd' value. The analytical observer assesses the overall impact, taking into account the various perspectives and trade-offs.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by acknowledging that this constraint contains both coordination and extraction elements. While the intention of 'Just Cause' provisions is to provide job security and protect employees from arbitrary dismissal (coordination), the practical implementation often results in reduced employer flexibility and potential protection of underperforming employees (extraction). The classification as a tangled rope reflects this mixed nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_measurement_accuracy,
    'How accurately can employee performance be measured and documented, particularly in roles with subjective performance criteria?',
    'Implementation of standardized performance metrics and rigorous documentation processes.',
    'If performance can be accurately measured, the extraction is reduced, and the constraint may shift towards a rope. If performance is difficult to measure, the extraction is higher, reinforcing the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_measurement_accuracy, empirical, 'Accuracy of performance measurement and documentation.').

omega_variable(
    union_bargaining_power,
    'How strong is the union''s bargaining power relative to the employer?',
    'Analysis of union membership density, strike history, and political influence.',
    'Stronger bargaining power leads to greater protection for employees, increasing the extraction from employers. Weaker bargaining power may result in a relaxation of just cause provisions, lessening the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(union_bargaining_power, empirical, 'Strength of union bargaining power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_protection_underperformance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unio_tr_t0, union_protection_underperformance, theater_ratio, 0, 0.5).
narrative_ontology:measurement(unio_tr_t5, union_protection_underperformance, theater_ratio, 5, 0.6).
narrative_ontology:measurement(unio_tr_t10, union_protection_underperformance, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(unio_be_t0, union_protection_underperformance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(unio_be_t5, union_protection_underperformance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(unio_be_t10, union_protection_underperformance, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(union_protection_underperformance, enforcement_mechanism).
narrative_ontology:affects_constraint(union_protection_underperformance, minimum_wage_laws).
narrative_ontology:affects_constraint(union_protection_underperformance, occupational_safety_regulations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
