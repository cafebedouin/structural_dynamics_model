% ============================================================================
% CONSTRAINT STORY: rent_seeking_equilibrium
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rent_seeking_equilibrium, []).

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
 *   constraint_id: rent_seeking_equilibrium
 *   human_readable: The Toll-Bridge Stagnation
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Toll-Bridge Stagnation describes a scenario where economic agents
 *   invest more resources in capturing existing wealth through political or
 *   legal influence (rent-seeking) than in creating new value. This can lead
 *   to reduced innovation, higher prices, and slower economic growth.
 *   Established firms often benefit from regulations that create barriers to
 *   entry, while the general public and innovative startups bear the costs.
 *
 * KEY AGENTS:
 *   - Consumers: Primary victim (powerless/trapped) - bears the costs of reduced innovation and higher prices.
 *   - Rent-Seeking Firms: Primary beneficiary (powerful/arbitrage) - benefits from preferential treatment and extracted rents.
 *   - Captured Regulators: Secondary beneficiary (institutional/constrained) - benefits from career advancement and revolving door opportunities.
 *   - Innovative Startups: Secondary victim (moderate/constrained) - face barriers to entry and lack resources to compete effectively.
 *   - Legal System: Degraded enforcer (institutional/constrained) - original function degraded by regulatory capture.
 *   - Analytical Observer: Neutral observer (analytical/analytical) - sees the structural dynamics of rent-seeking.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rent_seeking_equilibrium, 0.65).
domain_priors:suppression_score(rent_seeking_equilibrium, 0.7).
domain_priors:theater_ratio(rent_seeking_equilibrium, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rent_seeking_equilibrium, extractiveness, 0.65).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rent_seeking_equilibrium, tangled_rope).
narrative_ontology:human_readable(rent_seeking_equilibrium, "The Toll-Bridge Stagnation").
narrative_ontology:topic_domain(rent_seeking_equilibrium, "economic/political").

domain_priors:requires_active_enforcement(rent_seeking_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, rent_seeking_firms).
narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, captured_regulators).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, consumers).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, innovative_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The consumers are trapped within the system, bearing the costs of reduced innovation and higher prices due to rent-seeking. They lack the power and resources to effectively challenge the established rent-seeking equilibrium.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Captured regulators benefit from the system through career advancement, lobbying connections, and revolving door opportunities. However, they are also constrained by their official duties and the need to maintain a semblance of public interest. This perspective reflects a tangled rope, where there is a mix of benefit and constraint.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The legal system may be initially designed to protect property rights and promote competition. Over time, however, loopholes and regulatory capture lead to it becoming a tool for rent-seeking. The system persists, but its original function is degraded.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Rent-seeking firms actively engage in lobbying and regulatory capture to secure preferential treatment and extract rents. They are powerful actors with the ability to influence policy and regulations. They benefit from the system while also facing constraints due to competition from other rent-seekers.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Innovative startups face significant barriers to entry due to the rent-seeking equilibrium. They are constrained by regulations that favor incumbents and lack the resources to compete effectively. They are victims of the system.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees a tangled rope: some coordination is required to maintain the rules of the game, but there is asymmetric extraction by powerful firms at the expense of the general public and innovative startups.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rent_seeking_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rent_seeking_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rent_seeking_equilibrium, TR),
    TR >= 0.70.

:- end_tests(rent_seeking_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High, reflecting significant wealth transfer from the consumers and startups to rent-seeking firms. Suppression (0.70): High, indicating significant barriers to entry and limited competition. Theater Ratio (0.75): High, suggesting increased performative activity masking the underlying extraction. The system extracts real economic value and delivers it to the beneficiaries of the rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   The consumers experience a snare, bearing the costs of the system with limited power to change it. Rent-seeking firms see a tangled rope, benefiting from their activities while also facing competition from other rent-seekers. Captured regulators experience a tangled rope, benefiting from the system while constrained by their public duties. Innovative startups face a snare, encountering barriers to entry and limited opportunities to compete. The analytical observer sees the mixed coordination (establishing rules) and asymmetric extraction (benefit concentrated on a few) which is a defining characteristic of a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rent-seeking firms and captured regulators) gain advantages and capture resources. Victims (consumers and innovative startups) bear the cost of the system and are disadvantaged by it. The directionality flows towards those who invest in rent-seeking and away from those who contribute to value creation. The relatively high theater ratio indicates the system is actively enforced, but the enforcement is largely performative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_degree,
    'To what extent are regulators captured by the industries they regulate?',
    'Analysis of lobbying expenditures, revolving door appointments, and regulatory outcomes.',
    'If regulatory capture is high, the rent-seeking equilibrium is more entrenched. If low, there is greater potential for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree, empirical, 'Degree of regulatory capture').

omega_variable(
    political_influence_channels,
    'What are the primary channels through which rent-seekers exert political influence?',
    'Network analysis of political donations, lobbying contacts, and legislative voting patterns.',
    'Understanding the channels of influence allows for targeted interventions to reduce rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_influence_channels, empirical, 'Channels of political influence').

omega_variable(
    policy_reform_feasibility,
    'What is the feasibility of policy reforms to reduce rent-seeking?',
    'Political feasibility analysis, considering the interests of various stakeholders and potential opposition.',
    'Assesses the potential for breaking the rent-seeking equilibrium through policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_reform_feasibility, preference, 'Feasibility of policy reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rent_seeking_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rent_tr_t0, rent_seeking_equilibrium, theater_ratio, 0, 0.5).
narrative_ontology:measurement(rent_tr_t5, rent_seeking_equilibrium, theater_ratio, 5, 0.6).
narrative_ontology:measurement(rent_tr_t10, rent_seeking_equilibrium, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(rent_be_t0, rent_seeking_equilibrium, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(rent_be_t5, rent_seeking_equilibrium, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(rent_be_t10, rent_seeking_equilibrium, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rent_seeking_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, regulatory_capture).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, lobbying_influence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
