% ============================================================================
% CONSTRAINT STORY: us_debt_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_debt_ceiling, []).

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
 *   constraint_id: us_debt_ceiling
 *   human_readable: US Debt Ceiling
 *   domain: political
 *
 * SUMMARY:
 *   The US debt ceiling is a legal limit on the total amount of money the
 *   United States federal government is authorized to borrow to meet its
 *   existing legal obligations. It has become a recurring source of political
 *   conflict and economic uncertainty. The constraint creates a situation
 *   where the government can potentially default on its obligations, leading
 *   to economic instability and reputational damage.
 *
 * KEY AGENTS:
 *   - US Taxpayers: Primary victim (powerless/trapped) - bear the costs of political brinkmanship and potential economic instability.
 *   - Social Security Recipients: Secondary victim (moderate/constrained) - benefits are subject to political negotiation and potential cuts.
 *   - Incumbent Political Parties: Primary beneficiary (institutional/arbitrage) - use the debt ceiling as leverage to negotiate policy changes.
 *   - Federal Reserve: Secondary beneficiary (powerful/constrained) - role as lender of last resort is reinforced, but constrained by the need to maintain financial stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_debt_ceiling, 0.6).
domain_priors:suppression_score(us_debt_ceiling, 0.7).
domain_priors:theater_ratio(us_debt_ceiling, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_debt_ceiling, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_debt_ceiling, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_debt_ceiling, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_debt_ceiling, tangled_rope).
narrative_ontology:human_readable(us_debt_ceiling, "US Debt Ceiling").
narrative_ontology:topic_domain(us_debt_ceiling, "political").

domain_priors:requires_active_enforcement(us_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_debt_ceiling, incumbent_political_parties).
narrative_ontology:constraint_beneficiary(us_debt_ceiling, federal_reserve).
narrative_ontology:constraint_victim(us_debt_ceiling, us_taxpayers).
narrative_ontology:constraint_victim(us_debt_ceiling, social_security_recipients).
narrative_ontology:constraint_victim(us_debt_ceiling, federal_employees).
narrative_ontology:constraint_victim(us_debt_ceiling, us_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% US Taxpayers are ultimately trapped by the debt ceiling. They bear the long-term costs of political brinkmanship, increased interest rates, and potential economic instability. Their exit options are limited, and they have little direct influence over the process.
constraint_indexing:constraint_classification(us_debt_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Social Security recipients are constrained by the political uncertainty surrounding the debt ceiling. While they are not entirely trapped, their benefits are subject to political negotiation and potential cuts. They have some ability to advocate for their interests but are ultimately vulnerable to political decisions.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Federal Employees, during debt ceiling crises, often face furloughs and uncertainty, reflecting a dysfunctional system. Though the stated goal of the debt ceiling is fiscal responsibility, the recurrent brinkmanship demonstrates the mechanism's degradation and theatrical nature. It is mainly a symbolic constraint.
constraint_indexing:constraint_classification(us_debt_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Incumbent Political Parties benefit from the debt ceiling because it provides them with leverage to negotiate policy changes and score political points. They can use the threat of default to extract concessions from the opposing party. They can arbitrage the situation to their advantage.
constraint_indexing:constraint_classification(us_debt_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Federal Reserve benefits from the debt ceiling because it reinforces its role as the lender of last resort and the manager of the US economy. However, it is also constrained by the need to maintain financial stability and avoid a default. They have some influence but cannot fully control the situation.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the US debt ceiling is a tangled rope. It is intended to serve as a constraint on government spending, but it has become a tool for political brinkmanship and has the potential to destabilize the global economy. It's neither pure coordination nor pure extraction, but a hybrid.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_debt_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_debt_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_debt_ceiling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_debt_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_debt_ceiling, TR),
    TR >= 0.70.

:- end_tests(us_debt_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Moderate to High. The debt ceiling extracts economic value from taxpayers and the global economy through uncertainty, increased interest rates, and potential default. Suppression (0.7): High. The debt ceiling suppresses alternative policy options and forces political negotiation under duress. The theater ratio (0.8) is high, reflecting the symbolic nature of the constraint and the performative political posturing that accompanies it.
 *
 * PERSPECTIVAL GAP:
 *   US Taxpayers, as a powerless group, perceive the debt ceiling as a snare because they bear the cost when it is used for political leverage. Incumbent political parties, who have the power to make changes see the debt ceiling as a rope, as it gives them an upper hand in negotiations. An analytical observer would see it as a tangled rope: part extraction, part coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Taxpayers have no exit and bear the brunt, while those wielding the ceiling as leverage have arbitrage and benefit. The perspectives vary based on exit options and power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_polarization,
    'How will increasing political polarization affect the likelihood of future debt ceiling crises?',
    'Analysis of voting patterns and public discourse; modeling of political dynamics.',
    'Increased polarization could lead to more frequent and severe debt ceiling crises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_polarization, empirical, 'Impact of political polarization on debt ceiling crises.').

omega_variable(
    economic_impact,
    'What is the true economic cost of debt ceiling brinkmanship?',
    'Economic modeling and analysis of past debt ceiling episodes.',
    'More accurate cost estimates could influence political decision-making.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact, empirical, 'The economic cost of debt ceiling brinkmanship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_debt_ceiling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_d_tr_t0, us_debt_ceiling, theater_ratio, 0, 0.4).
narrative_ontology:measurement(us_d_tr_t5, us_debt_ceiling, theater_ratio, 5, 0.6).
narrative_ontology:measurement(us_d_tr_t10, us_debt_ceiling, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(us_d_be_t0, us_debt_ceiling, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_d_be_t5, us_debt_ceiling, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(us_d_be_t10, us_debt_ceiling, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_debt_ceiling, enforcement_mechanism).
narrative_ontology:affects_constraint(us_debt_ceiling, us_federal_budget).
narrative_ontology:affects_constraint(us_debt_ceiling, global_financial_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
