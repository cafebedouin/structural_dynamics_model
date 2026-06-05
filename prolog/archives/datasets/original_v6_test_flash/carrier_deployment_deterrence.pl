% ============================================================================
% CONSTRAINT STORY: carrier_deployment_deterrence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carrier_deployment_deterrence, []).

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
 *   constraint_id: carrier_deployment_deterrence
 *   human_readable: US Carrier Strike Group Deployment as Regional Deterrent
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   The deployment of US Carrier Strike Groups as a regional deterrent is a
 *   complex geopolitical constraint. It involves a combination of
 *   coordination (projecting power, deterring aggression) and extraction
 *   (costs to taxpayers and rival states, limitations on policy flexibility).
 *   The effectiveness of this constraint is constantly debated, as is the
 *   availability of alternative strategies.
 *
 * KEY AGENTS:
 *   - US Military Industrial Complex: Primary beneficiary (institutional/arbitrage) - benefits from long-term contracts and deployment
 *   - US Government: Secondary beneficiary (institutional/constrained) - benefits from power projection, but constrained by high costs and policy limitations
 *   - Regional Rival States: Primary victim (powerless/trapped) - constrained by carrier presence, extraction through defense spending
 *   - US Taxpayers: Secondary victim (powerless/trapped) - extraction through funding carrier deployment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrier_deployment_deterrence, 0.6).
domain_priors:suppression_score(carrier_deployment_deterrence, 0.7).
domain_priors:theater_ratio(carrier_deployment_deterrence, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrier_deployment_deterrence, extractiveness, 0.6).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carrier_deployment_deterrence, tangled_rope).
narrative_ontology:human_readable(carrier_deployment_deterrence, "US Carrier Strike Group Deployment as Regional Deterrent").
narrative_ontology:topic_domain(carrier_deployment_deterrence, "geopolitical/military").

domain_priors:requires_active_enforcement(carrier_deployment_deterrence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, us_government).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, regional_rival_states).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, us_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of regional rival states, the carrier deployment acts as a snare, limiting their options and extracting resources for defense. These states are often trapped by geography and existing power dynamics, making exit difficult.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% For the US military-industrial complex, carrier deployments are a rope, coordinating resource allocation, providing market stability and long-term contracts, and demonstrating capabilities to allies and adversaries. They benefit from the continued investment and deployment.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The US government experiences the deployment as a tangled rope. While it offers a degree of coordination by projecting power and deterring aggression, it comes at a high cost to US taxpayers and can limit flexibility in foreign policy.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From the perspective of US Taxpayers, the carrier deployments can act as a snare extracting wealth to maintain and deploy these costly assets, while offering questionable returns in long-term security.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% From an analytical observer's perspective, the carrier deployment is a tangled rope. It provides a coordination function in projecting power and deterring conflict, but it also leads to significant extraction from both US taxpayers and rival states, and creates a dependency cycle.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrier_deployment_deterrence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carrier_deployment_deterrence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(carrier_deployment_deterrence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness of 0.60 reflects the significant costs imposed on regional rivals (through the need to maintain a credible defense) and US taxpayers (through the high cost of deployment and maintenance). The suppression of 0.70 reflects the limitations imposed on the political and military options of regional rivals due to the overwhelming force projected by a Carrier Strike Group. The theater ratio of 0.40 accounts for the symbolic aspect of the deployment, separate from the functional military capacity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is wide. The US military-industrial complex sees the deployment as coordination, solidifying their financial health and demonstrating capabilities. Regional rival states perceive the constraint as a snare, restricting their strategic options. The US government sees it as a tangled rope due to budget impact and policy constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (US military-industrial complex) experience the deployment as coordination, giving them negative directionality. Victims (regional rivals and US taxpayers) experience it as extraction, giving them positive directionality. The US government is both a beneficiary (power projection) and a constrained actor (budgetary implications and policy constraints), resulting in a tangled rope assessment. The analytical observer sees both extraction and coordination, which is a tangled rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_effectiveness,
    'How effective are carrier deployments at actually deterring regional conflicts and maintaining stability?',
    'Historical analysis of conflict occurrences in regions with and without carrier presence, game-theoretic models of deterrence dynamics.',
    'If highly effective: justification for continued deployments. If ineffective: questioning the value of the constraint and exploration of alternative approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effectiveness, empirical, 'The actual effectiveness of carrier deployments in achieving deterrence.').

omega_variable(
    alternative_deterrence_strategies,
    'What are the costs and benefits of alternative deterrence strategies, such as cyber warfare, economic sanctions, or diplomatic engagement?',
    'Comparative analysis of different deterrence strategies, including cost-benefit analysis and assessment of unintended consequences.',
    'Identification of more cost-effective and less escalatory deterrence methods. Potential shift away from reliance on carrier deployments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_deterrence_strategies, conceptual, 'Availability and viability of alternative deterrence strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carrier_deployment_deterrence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carr_tr_t0, carrier_deployment_deterrence, theater_ratio, 0, 0.3).
narrative_ontology:measurement(carr_tr_t5, carrier_deployment_deterrence, theater_ratio, 5, 0.4).
narrative_ontology:measurement(carr_tr_t10, carrier_deployment_deterrence, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(carr_be_t0, carrier_deployment_deterrence, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(carr_be_t5, carrier_deployment_deterrence, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(carr_be_t10, carrier_deployment_deterrence, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carrier_deployment_deterrence, enforcement_mechanism).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, global_trade_routes).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, international_maritime_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
