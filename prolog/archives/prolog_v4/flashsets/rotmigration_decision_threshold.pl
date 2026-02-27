% ============================================================================
% CONSTRAINT STORY: rotmigration_decision_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotmigration_decision_threshold, []).

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
 *   constraint_id: rotmigration_decision_threshold
 *   human_readable: The Migration Decision Threshold (Cost-Benefit Equilibrium)
 *   domain: economic/social
 *
 * SUMMARY:
 *   The migration decision threshold represents the point at which an
 *   individual or family decides to migrate based on a perceived cost-benefit
 *   analysis. Factors include economic opportunities, political stability,
 *   social networks, and the costs and risks of migration. This threshold is
 *   not static, but influenced by policy interventions, information
 *   availability, and individual circumstances.
 *
 * KEY AGENTS:
 *   - Vulnerable Migrants: Primary target (powerless/trapped) - Face exploitation and limited exit options.
 *   - Destination Country Employers: Primary beneficiary (institutional/arbitrage) - Benefit from low-cost labor.
 *   - Average Migrant: Secondary actor (moderate/constrained) - A mix of benefits and constraints.
 *   - Remittance-Receiving Families: Beneficiary (powerless, mobile) - benefit from monetary support
 *   - Origin Country Economy: Victim (institutional, constrained) - suffers from brain drain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotmigration_decision_threshold, 0.5).
domain_priors:suppression_score(rotmigration_decision_threshold, 0.3).
domain_priors:theater_ratio(rotmigration_decision_threshold, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotmigration_decision_threshold, extractiveness, 0.5).
narrative_ontology:constraint_metric(rotmigration_decision_threshold, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rotmigration_decision_threshold, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotmigration_decision_threshold, tangled_rope).
narrative_ontology:human_readable(rotmigration_decision_threshold, "The Migration Decision Threshold (Cost-Benefit Equilibrium)").
narrative_ontology:topic_domain(rotmigration_decision_threshold, "economic/social").

domain_priors:requires_active_enforcement(rotmigration_decision_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, destination_country_employers).
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, remittance_receiving_families).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, origin_country_economy).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, vulnerable_migrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For vulnerable migrants with limited information and resources, the migration decision threshold can act as a snare, trapping them in exploitative situations in the destination country.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Employers in destination countries benefit from the labor supplied by migrants, especially in sectors with labor shortages. They arbitrage the difference in labor costs.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The average migrant experiences a tangled rope: benefits from higher wages and opportunities but is also constrained by integration challenges and potential exploitation.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% From an analytical perspective, the migration decision threshold is a tangled rope, involving both coordination (labor allocation) and extraction (exploitation of vulnerable migrants, brain drain from origin countries).
constraint_indexing:constraint_classification(rotmigration_decision_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The origin country economy can experience a piton effect as skilled workers emigrate (brain drain), and the benefits of remittances may not fully compensate for the loss of human capital. There is a high level of theatrical activity but the original coordinating function has atrophied.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotmigration_decision_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotmigration_decision_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotmigration_decision_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotmigration_decision_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotmigration_decision_threshold, TR),
    TR >= 0.70.

:- end_tests(rotmigration_decision_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.5): Moderate. Some exploitation and loss of labor from origin countries. Suppression (0.3): Moderate. Limited information, costs, and legal barriers suppress choices. Theater Ratio (0.1): Relatively low. Clear, direct function dominating.
 *
 * PERSPECTIVAL GAP:
 *   Migrants who are well-informed and have access to resources may view the migration decision as a beneficial rope (coordination), while vulnerable migrants may experience it as a snare (extraction). Destination country employers see it as a rope, while the origin country economy might see a piton effect (degraded coordination). The average migrant feels a Tangled Rope dynamic. The analytical observer sees the constraint as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Employers in destination countries gain access to labor, reducing costs. Remittance-receiving families get resources. Victims: Origin countries lose skilled labor, and vulnerable migrants are subject to exploitation. The vulnerable have no real exit and the source country is constrained by their decisions.
 *
 * MANDATROPHY ANALYSIS:
 *   The migration decision is not pure extraction or pure coordination, but a complex interplay of both. It is also not a pure mountain: The amount of suppression is subject to policy changes and other interventions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry,
    'How much does incomplete information about the destination country affect migration decisions and outcomes?',
    'Surveys of migrants'' expectations versus their actual experiences; analysis of information networks and access to reliable data.',
    'If high information asymmetry, migration decision becomes a snare. If low, can function as a rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry, empirical, 'Impact of information asymmetry on migration decisions').

omega_variable(
    integration_policies,
    'How effective are integration policies in destination countries at mitigating the negative impacts of migration and maximizing its benefits?',
    'Comparative studies of integration policies across different countries; analysis of migrants'' socio-economic outcomes under different policy regimes.',
    'Strong integration policies can shift from snare to scaffold or rope. Weak policies reinforce snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_policies, preference, 'Effectiveness of integration policies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotmigration_decision_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rotm_tr_t0, rotmigration_decision_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rotm_tr_t5, rotmigration_decision_threshold, theater_ratio, 5, 0.1).
narrative_ontology:measurement(rotm_tr_t10, rotmigration_decision_threshold, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(rotm_be_t0, rotmigration_decision_threshold, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rotm_be_t5, rotmigration_decision_threshold, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(rotm_be_t10, rotmigration_decision_threshold, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotmigration_decision_threshold, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
