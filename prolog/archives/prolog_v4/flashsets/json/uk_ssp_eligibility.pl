% ============================================================================
% CONSTRAINT STORY: uk_ssp_eligibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_ssp_eligibility, []).

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
 *   constraint_id: uk_ssp_eligibility
 *   human_readable: UK Statutory Sick Pay (SSP) Eligibility and Rate
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK Statutory Sick Pay (SSP) system provides a minimum level of sick
 *   pay for eligible employees. However, the eligibility criteria,
 *   particularly the Lower Earnings Limit (LEL), exclude a significant
 *   portion of low-wage workers, part-time workers, and those in the gig
 *   economy. This creates a system where those most vulnerable to income loss
 *   due to illness are often excluded from support.
 *
 * KEY AGENTS:
 *   - Low-Wage Workers: Primary target (powerless/trapped) - Excluded from SSP due to LEL.
 *   - Part-Time Workers: Target (powerless/trapped) - Often ineligible due to working hours/earnings.
 *   - Gig Economy Workers: Target (powerless/trapped) - Ineligible due to variable income and employment status.
 *   - UK Government: Beneficiary (institutional/arbitrage) - Limits expenditure on sick pay.
 *   - Large Employers: Beneficiary (institutional/constrained) - Cost savings from lower wage bill.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_ssp_eligibility, 0.6).
domain_priors:suppression_score(uk_ssp_eligibility, 0.7).
domain_priors:theater_ratio(uk_ssp_eligibility, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_ssp_eligibility, extractiveness, 0.6).
narrative_ontology:constraint_metric(uk_ssp_eligibility, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(uk_ssp_eligibility, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_ssp_eligibility, tangled_rope).
narrative_ontology:human_readable(uk_ssp_eligibility, "UK Statutory Sick Pay (SSP) Eligibility and Rate").
narrative_ontology:topic_domain(uk_ssp_eligibility, "economic/political").

domain_priors:requires_active_enforcement(uk_ssp_eligibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, uk_government).
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, large_employers).
narrative_ontology:constraint_victim(uk_ssp_eligibility, low_wage_workers).
narrative_ontology:constraint_victim(uk_ssp_eligibility, part_time_workers).
narrative_ontology:constraint_victim(uk_ssp_eligibility, gig_economy_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-wage workers, particularly those in part-time or gig economy roles, are often ineligible for SSP due to the Lower Earnings Limit. They have limited exit options and bear the full cost of illness without income support.
constraint_indexing:constraint_classification(uk_ssp_eligibility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% SMEs are constrained by the SSP requirements. Although they receive some compensation from the government, they still bear administrative and financial burdens. They benefit from having a healthier workforce, but are also extracting through lower wage costs overall from those not eligible. This makes them tangled_rope.
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The UK government benefits from the SSP system by limiting public expenditure on sick pay and potentially incentivizing workforce participation. They can adjust policy, rates, and eligibility.
constraint_indexing:constraint_classification(uk_ssp_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Large employers benefit from the SSP system through cost savings associated with a lower overall wage bill due to reduced sick pay expenses. They are somewhat constrained because they may want better benefits to attract more workers. The system is piton because large employers can offer their own sick pay on top of this, but it adds theater compared to its functional benefits, such as reporting requirements and compliance checks.
constraint_indexing:constraint_classification(uk_ssp_eligibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the SSP system represents a tangled rope. It coordinates a minimum level of sick pay provision but also extracts from vulnerable workers by excluding them from eligibility, which requires enforcement through employers only paying it for eligible workers.
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_ssp_eligibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_ssp_eligibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_ssp_eligibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_ssp_eligibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_ssp_eligibility, TR),
    TR >= 0.70.

:- end_tests(uk_ssp_eligibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The LEL and other eligibility criteria extract a significant portion of potential sick pay benefits from low-wage, part-time, and gig economy workers, creating a substantial wealth transfer to the government and indirectly to employers through lower labor costs. Suppression (0.70): High, as limited alternatives exist for low-wage workers, given their often precarious employment status and lack of access to private insurance or savings. Theater ratio (0.75): The SSP system has a performative aspect because large employers can offer their own sick pay on top of this, but it adds theater compared to its functional benefits, such as reporting requirements and compliance checks.
 *
 * PERSPECTIVAL GAP:
 *   The system creates a perspectival gap. Low-wage workers experience it as a snare, with no escape from the negative consequences of illness without income. The government views it as a rope, coordinating a basic level of sick pay provision while managing public finances. Large employers see it as a piton. The analytical observer sees a tangled rope: a system with both coordination and extraction components.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers, with limited exit options, experience high extraction (d=0.95). The UK government benefits from reduced expenditure and sees the system as coordination (d=0.05). Small employers are both beneficiary and victim, so their d is 0.5, a tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The SSP system is a tangled rope because it coordinates a minimum level of sick pay provision but also extracts from vulnerable workers by excluding them from eligibility. The high suppression score reflects the limited alternatives available to low-wage workers. The analytical observer sees a tangled rope: a system with both coordination and extraction components. The system is actively enforced through employers only paying it for eligible workers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_earnings_threshold,
    'What is the optimal minimum earnings threshold for SSP eligibility that balances worker protection with employer costs and government expenditure?',
    'Economic modeling and empirical analysis of the impact of different thresholds on worker health, labor supply, and government finances.',
    'Higher threshold improves worker protection but increases employer costs and government expenditure. Lower threshold reduces worker protection but decreases employer costs and government expenditure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_earnings_threshold, empirical, 'Optimal minimum earnings threshold for SSP eligibility.').

omega_variable(
    alternative_sick_pay_models,
    'How do alternative sick pay models (e.g., universal basic income, social insurance) compare to the SSP system in terms of coverage, adequacy, and efficiency?',
    'Comparative analysis of the design and outcomes of different sick pay models across countries.',
    'Alternative models may provide broader coverage and greater adequacy but may also be more costly or less efficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_sick_pay_models, conceptual, 'Comparison of alternative sick pay models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_ssp_eligibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_s_tr_t0, uk_ssp_eligibility, theater_ratio, 0, 0.7).
narrative_ontology:measurement(uk_s_tr_t5, uk_ssp_eligibility, theater_ratio, 5, 0.75).
narrative_ontology:measurement(uk_s_tr_t10, uk_ssp_eligibility, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(uk_s_be_t0, uk_ssp_eligibility, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uk_s_be_t5, uk_ssp_eligibility, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(uk_s_be_t10, uk_ssp_eligibility, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_ssp_eligibility, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
