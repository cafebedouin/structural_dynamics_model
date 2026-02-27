% ============================================================================
% CONSTRAINT STORY: usc_26_469_passive_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usc_26_469_passive_loss, []).

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
 *   constraint_id: usc_26_469_passive_loss
 *   human_readable: Passive Activity Loss (PAL) Rules
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Established by the Tax Reform Act of 1986, Section 469 prohibits
 *   taxpayers from using losses from 'passive activities' (rental properties
 *   or businesses without material participation) to offset active income
 *   (wages) or portfolio income. This rule aims to prevent tax shelters and
 *   ensure a more equitable distribution of the tax burden. The effectiveness
 *   and fairness of these rules have been debated for decades.
 *
 * KEY AGENTS:
 *   - Passive Investors: Primary target (powerless/trapped) - unable to utilize losses to offset income.
 *   - Rental Property Owners: Secondary target (moderate/constrained) - restricted in offsetting income with rental losses.
 *   - IRS: Primary beneficiary (institutional/arbitrage) - increased tax revenue, reduced tax avoidance.
 *   - Active Income Taxpayers: Secondary beneficiary (powerful/arbitrage) - more equitable tax burden.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usc_26_469_passive_loss, 0.55).
domain_priors:suppression_score(usc_26_469_passive_loss, 0.65).
domain_priors:theater_ratio(usc_26_469_passive_loss, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usc_26_469_passive_loss, extractiveness, 0.55).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usc_26_469_passive_loss, tangled_rope).
narrative_ontology:human_readable(usc_26_469_passive_loss, "Passive Activity Loss (PAL) Rules").
narrative_ontology:topic_domain(usc_26_469_passive_loss, "economic/legal").

domain_priors:requires_active_enforcement(usc_26_469_passive_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, irs).
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, active_income_taxpayers).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, passive_investors).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, rental_property_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The passive investor, often lacking the resources or knowledge to navigate the complex rules, finds their losses trapped and unusable to offset other income. They are largely powerless and trapped by the legislation.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Rental property owners are constrained because they can't easily exit the rental market without incurring losses. They are victims of the rule, but also benefit from the general tax structure that allows some deductions. They are constrained, not trapped, as they have some mobility, albeit limited.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The IRS benefits from the rule by increasing tax revenue and reducing opportunities for tax avoidance. They also need to expend resources to enforce the law and arbitrate disputes.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Those who primarily earn active income (wages) benefit because the playing field is considered more level as other taxpayers can't reduce their tax liability through passive losses.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the trade-offs inherent in this legislation. Some coordination (fairness) at the expense of asymmetric extraction (limits investor freedom).
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usc_26_469_passive_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(usc_26_469_passive_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usc_26_469_passive_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because, while it restricts the use of passive losses, it doesn't completely eliminate all tax benefits for passive activities. The suppression is moderate-high because it significantly limits the options available to taxpayers for managing their tax liability related to passive investments. The theater ratio is low because the rules are primarily functional with limited performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The passive investor sees a snare because they are unable to use losses. The IRS sees a rope as it helps to collect tax revenue. The analytical observer, depending on their assessment of the rule's economic impact, may see either a tangled rope or a rope. Active income taxpayers are powerful because they benefit from the curtailing of tax shelters that passive income earners used to use.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by whether the entity benefits from the policy or is harmed by the policy. The IRS and Active Income Taxpayers are beneficiaries, while Passive Investors and Rental Property Owners are victims. The level of power also influences directionality. Powerless agents have high directionality values, and institutional agents have low directionality values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_impact,
    'What is the overall economic impact of the PAL rules on investment in passive activities?',
    'Econometric studies analyzing investment behavior before and after the implementation of Section 469.',
    'If negative: the rule stifles productive investment. If neutral or positive: the rule primarily curtails tax avoidance without harming the economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact, empirical, 'The economic impact of PAL rules.').

omega_variable(
    complexity_cost,
    'How much does the complexity of the PAL rules cost in terms of compliance and enforcement?',
    'Studies analyzing tax preparation fees, IRS enforcement costs, and litigation expenses related to Section 469.',
    'If high: the rule''s benefits may be outweighed by its costs. If low: the rule is relatively efficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_cost, empirical, 'The complexity cost of PAL rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usc_26_469_passive_loss, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usc__tr_t0, usc_26_469_passive_loss, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usc__tr_t15, usc_26_469_passive_loss, theater_ratio, 15, 0.2).
narrative_ontology:measurement(usc__tr_t30, usc_26_469_passive_loss, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(usc__be_t0, usc_26_469_passive_loss, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usc__be_t15, usc_26_469_passive_loss, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(usc__be_t30, usc_26_469_passive_loss, base_extractiveness, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usc_26_469_passive_loss, enforcement_mechanism).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, usc_26_tax_code).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
