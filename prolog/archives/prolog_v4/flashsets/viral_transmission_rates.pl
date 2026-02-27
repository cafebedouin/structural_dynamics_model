% ============================================================================
% CONSTRAINT STORY: viral_transmission_rates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viral_transmission_rates, []).

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
 *   constraint_id: viral_transmission_rates
 *   human_readable: Socio-Political Response to Viral Transmission
 *   domain: political/technological
 *
 * SUMMARY:
 *   This constraint models the socio-political system of mandates, lockdowns,
 *   and behavioral controls enacted in response to a biological reality
 *   (viral transmission). The perspective varies based on power, exit
 *   options, and scope.
 *
 * KEY AGENTS:
 *   - Public Health Institutions: Primary beneficiary (institutional/arbitrage)
 *   - Pharmaceutical Companies: Secondary beneficiary (institutional/arbitrage)
 *   - Individual Liberties: Primary victim (powerless/trapped)
 *   - Small Businesses: Secondary victim (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viral_transmission_rates, 0.55).
domain_priors:suppression_score(viral_transmission_rates, 0.7).
domain_priors:theater_ratio(viral_transmission_rates, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_transmission_rates, extractiveness, 0.55).
narrative_ontology:constraint_metric(viral_transmission_rates, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(viral_transmission_rates, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viral_transmission_rates, tangled_rope).
narrative_ontology:human_readable(viral_transmission_rates, "Socio-Political Response to Viral Transmission").
narrative_ontology:topic_domain(viral_transmission_rates, "political/technological").

domain_priors:requires_active_enforcement(viral_transmission_rates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viral_transmission_rates, public_health_institutions).
narrative_ontology:constraint_beneficiary(viral_transmission_rates, pharmaceutical_companies).
narrative_ontology:constraint_victim(viral_transmission_rates, individual_liberties).
narrative_ontology:constraint_victim(viral_transmission_rates, small_businesses).
narrative_ontology:constraint_victim(viral_transmission_rates, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual liberties are significantly curtailed by mandates and restrictions, with limited ability to exit the system.
constraint_indexing:constraint_classification(viral_transmission_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Small businesses are constrained by lockdowns and reduced consumer activity, but may also benefit from government assistance and reduced competition.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Public health institutions benefit from increased funding and authority, and view the restrictions as necessary for public safety.
constraint_indexing:constraint_classification(viral_transmission_rates, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Pharmaceutical companies benefit from vaccine development and distribution contracts.
constraint_indexing:constraint_classification(viral_transmission_rates, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical perspective observing the overall structure.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_transmission_rates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viral_transmission_rates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viral_transmission_rates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(viral_transmission_rates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The system extracts from individual liberties and economic activity to benefit public health institutions and pharmaceutical companies. The extractiveness is due to restrictions on movement, assembly, and economic activity. Suppression reflects limited exit options from the system.
 *
 * PERSPECTIVAL GAP:
 *   Individuals see a snare as their freedoms are curtailed. Small businesses see a tangled rope as they are both helped and hindered. Public health sees a rope as they are empowered to save lives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries have low d values, and victims have high d values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_non_pharmaceutical_interventions,
    'How effective are non-pharmaceutical interventions (NPIs) in reducing viral transmission?',
    'Meta-analysis of studies on NPI effectiveness',
    'Determines the degree to which restrictions are justified and whether they are a net benefit or detriment to society.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_non_pharmaceutical_interventions, empirical, 'Effectiveness of NPIs').

omega_variable(
    tradeoffs_between_health_and_economy,
    'What are the acceptable tradeoffs between public health and economic activity?',
    'Cost-benefit analysis of different intervention strategies',
    'Informs policy decisions on the level and duration of restrictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tradeoffs_between_health_and_economy, preference, 'Tradeoffs between health and economy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_transmission_rates, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vira_tr_t0, viral_transmission_rates, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vira_tr_t12, viral_transmission_rates, theater_ratio, 12, 0.2).
narrative_ontology:measurement(vira_tr_t24, viral_transmission_rates, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(vira_be_t0, viral_transmission_rates, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vira_be_t12, viral_transmission_rates, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(vira_be_t24, viral_transmission_rates, base_extractiveness, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_transmission_rates, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
