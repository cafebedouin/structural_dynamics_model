% ============================================================================
% CONSTRAINT STORY: blackstone_carried_interest_taxation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_carried_interest_taxation, []).

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
 *   constraint_id: blackstone_carried_interest_taxation
 *   human_readable: Carried Interest Partnership Taxation
 *   domain: economic/political
 *
 * SUMMARY:
 *   The regulatory and tax framework in the United States that treats
 *   "carried interest" (performance fees) for partners in private equity and
 *   hedge funds as long-term capital gains rather than ordinary income. This
 *   creates a tax advantage for these individuals, allowing them to pay a
 *   lower tax rate than most wage earners. This is an ongoing debate in US
 *   economic policy.
 *
 * KEY AGENTS:
 *   - Private Equity Partners: Primary beneficiary (institutional/arbitrage) – benefits from lower tax rate.
 *   - Hedge Fund Partners: Primary beneficiary (institutional/arbitrage) – benefits from lower tax rate.
 *   - General Taxpayers: Primary victim (powerless/trapped) – bears the cost of the tax loophole.
 *   - Government Tax Revenue: Secondary victim (institutional/constrained) – loses potential revenue due to lower tax rate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_carried_interest_taxation, 0.65).
domain_priors:suppression_score(blackstone_carried_interest_taxation, 0.7).
domain_priors:theater_ratio(blackstone_carried_interest_taxation, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, extractiveness, 0.65).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_carried_interest_taxation, tangled_rope).
narrative_ontology:human_readable(blackstone_carried_interest_taxation, "Carried Interest Partnership Taxation").
narrative_ontology:topic_domain(blackstone_carried_interest_taxation, "economic/political").

domain_priors:requires_active_enforcement(blackstone_carried_interest_taxation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, private_equity_partners).
narrative_ontology:constraint_beneficiary(blackstone_carried_interest_taxation, hedge_fund_partners).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, general_taxpayers).
narrative_ontology:constraint_victim(blackstone_carried_interest_taxation, government_tax_revenue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% General taxpayers bear the burden of the tax loophole, effectively subsidizing the income of high-earning fund managers. They are trapped as they cannot directly influence or change the law.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Partners in private equity and hedge funds benefit significantly from this tax treatment, paying a lower tax rate on their carried interest income. They actively lobby to maintain this favorable treatment.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the carried interest taxation as a tangled rope: it facilitates capital allocation (coordination) but also allows for tax avoidance (extraction), requiring ongoing political effort to maintain the status quo.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% While the government theoretically benefits from increased investment activity, the revenue lost due to the lower tax rate could be allocated to other public services. The system continues due to political inertia, but the actual benefits may be less than claimed.
constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_carried_interest_taxation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_carried_interest_taxation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blackstone_carried_interest_taxation, TR),
    TR >= 0.70.

:- end_tests(blackstone_carried_interest_taxation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Significant extraction of tax revenue from the general public to the benefit of a small number of high-income individuals. Suppression (0.70): High. Strong lobbying efforts and political influence from the financial industry maintain this tax treatment despite public criticism. Theater Ratio (0.75): High. While there are justifications for incentivizing investment, the current system primarily benefits fund managers with minimal connection to genuine coordination. The performative aspect is that it is sold as incentivizing investment, but the primary effect is tax reduction for fund managers.
 *
 * PERSPECTIVAL GAP:
 *   General taxpayers see the carried interest as an unfair tax loophole (snare), while private equity partners view it as a necessary incentive for investment (rope). An analytical observer recognizes the mix of coordination and extraction (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Private equity and hedge fund partners benefit directly from the lower tax rate (beneficiary, d close to 0). General taxpayers bear the burden of the tax loophole (victim, d close to 1).
 *
 * MANDATROPHY ANALYSIS:
 *   The key issue is distinguishing legitimate capital formation incentives (which would classify as a rope or scaffold) from a pure tax extraction (snare). The significant suppression indicates active rent-seeking rather than a naturally-emerging or temporary coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_impact_assessment,
    'What is the true economic impact of carried interest taxation on investment, job creation, and overall economic growth?',
    'Comprehensive economic modeling and empirical studies analyzing the effects of changing the tax treatment of carried interest.',
    'If the economic benefits are substantial, maintaining the current tax treatment might be justified. If the benefits are minimal or negative, reform is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_assessment, empirical, 'True economic impact of carried interest taxation.').

omega_variable(
    political_feasibility_reform,
    'What is the political feasibility of reforming the tax treatment of carried interest, considering lobbying efforts and political polarization?',
    'Analysis of lobbying expenditures, campaign contributions, and voting records related to carried interest taxation.',
    'If reform is politically impossible, alternative approaches may be needed. If reform is feasible, the focus should be on designing effective and equitable legislation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_reform, preference, 'Political feasibility of reforming the tax treatment of carried interest.').

omega_variable(
    alternative_tax_structures,
    'Are there alternative tax structures that could achieve similar economic benefits while minimizing tax avoidance and promoting fairness?',
    'Comparative analysis of different tax structures used in other countries and their potential applicability to the United States.',
    'Identifying superior tax structures could pave the way for more effective and equitable tax reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_tax_structures, conceptual, 'Are there alternative tax structures?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_carried_interest_taxation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blac_tr_t0, blackstone_carried_interest_taxation, theater_ratio, 0, 0.65).
narrative_ontology:measurement(blac_tr_t10, blackstone_carried_interest_taxation, theater_ratio, 10, 0.7).
narrative_ontology:measurement(blac_tr_t20, blackstone_carried_interest_taxation, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(blac_be_t0, blackstone_carried_interest_taxation, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(blac_be_t10, blackstone_carried_interest_taxation, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(blac_be_t20, blackstone_carried_interest_taxation, base_extractiveness, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_carried_interest_taxation, resource_allocation).
narrative_ontology:affects_constraint(blackstone_carried_interest_taxation, capital_gains_taxation).
narrative_ontology:affects_constraint(blackstone_carried_interest_taxation, lobbying_regulations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
