% ============================================================================
% CONSTRAINT STORY: bwb_adeg_rewesale_conditions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bwb_adeg_rewesale_conditions, []).

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
 *   constraint_id: bwb_adeg_rewesale_conditions
 *   human_readable: BWB Conditions on Rewe's Adeg Store Divestment
 *   domain: economic
 *
 * SUMMARY:
 *   The Austrian Federal Competition Authority (BWB) imposed conditions on
 *   the transfer of 75 Adeg grocery stores from the Rewe Group to independent
 *   merchants to ensure fair competition. This constraint story examines the
 *   different perspectives on these conditions.
 *
 * KEY AGENTS:
 *   - Independent Merchants: Beneficiaries (moderate/constrained)
 *   - Austrian Consumers: Beneficiaries (institutional/arbitrage)
 *   - Rewe Group Potential Acquirers: Victims (powerless/trapped)
 *   - Austrian Federal Competition Authority (BWB): Enforcer (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bwb_adeg_rewesale_conditions, 0.35).
domain_priors:suppression_score(bwb_adeg_rewesale_conditions, 0.45).
domain_priors:theater_ratio(bwb_adeg_rewesale_conditions, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, extractiveness, 0.35).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bwb_adeg_rewesale_conditions, tangled_rope).
narrative_ontology:human_readable(bwb_adeg_rewesale_conditions, "BWB Conditions on Rewe's Adeg Store Divestment").
narrative_ontology:topic_domain(bwb_adeg_rewesale_conditions, "economic").

domain_priors:requires_active_enforcement(bwb_adeg_rewesale_conditions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, independent_merchants).
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, austrian_consumers).
narrative_ontology:constraint_victim(bwb_adeg_rewesale_conditions, rewe_group_potential_acquirers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Potential acquirers who are prevented from acquiring the stores due to the conditions see it as a pure extraction.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The BWB views the conditions as a coordination mechanism to prevent anti-competitive behavior.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The independent merchants benefit from the conditions but are also constrained by them.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% An analytical observer would see the conditions as a tangled rope, balancing the need for competition with potential extraction.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bwb_adeg_rewesale_conditions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(bwb_adeg_rewesale_conditions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because while the conditions restrict Rewe's options, they also ensure a more competitive market. Suppression is also moderate, as the conditions limit the potential acquirers but allow independent merchants to thrive. The theater ratio is low, suggesting that the conditions are primarily functional.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions of the actors. The BWB sees the conditions as necessary for competition, while potential acquirers may view them as unfair restrictions. Independent merchants see both benefits and constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   The BWB benefits from the conditions as they fulfill their mandate. Independent merchants benefit from increased market access. Potential acquirers bear the cost of restricted opportunities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    condition_effectiveness,
    'How effective are the conditions in preventing anti-competitive behavior?',
    'Market analysis and monitoring of market share and pricing.',
    'If ineffective, the constraint is a piton. If highly effective, it is a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(condition_effectiveness, empirical, 'Effectiveness of conditions in preventing anti-competitive behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bwb_adeg_rewesale_conditions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwb__tr_t0, bwb_adeg_rewesale_conditions, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bwb__tr_t5, bwb_adeg_rewesale_conditions, theater_ratio, 5, 0.2).
narrative_ontology:measurement(bwb__tr_t10, bwb_adeg_rewesale_conditions, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(bwb__be_t0, bwb_adeg_rewesale_conditions, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bwb__be_t5, bwb_adeg_rewesale_conditions, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(bwb__be_t10, bwb_adeg_rewesale_conditions, base_extractiveness, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bwb_adeg_rewesale_conditions, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
