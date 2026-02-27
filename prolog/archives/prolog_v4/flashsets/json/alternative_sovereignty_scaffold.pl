% ============================================================================
% CONSTRAINT STORY: alternative_sovereignty_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_sovereignty_scaffold, []).

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
 *   constraint_id: alternative_sovereignty_scaffold
 *   human_readable: The Decentralized Parallel
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Decentralized Parallel is a framework of decentralized protocols
 *   designed to offer coordination outside of traditional state structures.
 *   It aims to act as a temporary scaffold, enabling populations to migrate
 *   away from failing institutional systems. The constraint presents varying
 *   perspectives, from the trapped citizen under a failing state to the
 *   mobile early adopter seeking alternatives. Its success hinges on
 *   overcoming challenges related to scalability, social cohesion, and
 *   ultimately, whether it can offer a viable alternative to existing systems
 *   of governance.
 *
 * KEY AGENTS:
 *   - Citizen Under Failing State: Primary target (powerless/trapped) facing decaying institutions.
 *   - Early Adopter: Primary beneficiary (moderate/mobile) seeking alternative systems.
 *   - Existing State Institutions: Established powers (powerful/constrained) who may see the parallel as a threat.
 *   - Exit Communities: Collectives forming around alternative systems (organized/mobile).
 *   - Analytical Observer: Assesses the framework's effectiveness (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_sovereignty_scaffold, 0.35).
domain_priors:suppression_score(alternative_sovereignty_scaffold, 0.25).
domain_priors:theater_ratio(alternative_sovereignty_scaffold, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_sovereignty_scaffold, scaffold).
narrative_ontology:human_readable(alternative_sovereignty_scaffold, "The Decentralized Parallel").
narrative_ontology:topic_domain(alternative_sovereignty_scaffold, "technological/social").

domain_priors:requires_active_enforcement(alternative_sovereignty_scaffold).
narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, early_adopters).
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, exit_communities).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, citizens_under_failing_state).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Citizen under failing state. Trapped within a decaying institutional system. The alternative sovereignty scaffold offers a potential, but difficult exit.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Early adopters. Mobile and seeking alternatives. They see the decentralized parallel as a scaffold to a new way of life.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 3: Analytical Observer. Sees the decentralized parallel as a rope if it effectively coordinates new forms of governance and social interaction.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 4: Existing State Institutions. Initially sees the decentralized parallel as a piton, a degraded institution that no longer has significant influence.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_sovereignty_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(alternative_sovereignty_scaffold, TR),
    TR >= 0.70.

:- end_tests(alternative_sovereignty_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as the system extracts resources and mindshare from existing institutions but also coordinates new communities. Suppression is low (0.25) as the decentralized parallel aims to provide exit options, but faces challenges in scalability and accessibility. The theater ratio is moderate (0.75) because it focuses on functional alternatives, but also requires performative governance to gain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the trapped citizen and the early adopter. The citizen sees a snare as they struggle to navigate failing institutions. The early adopter views a scaffold that offers an exit, but requires initiative and risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position of each agent. Trapped citizens experience the highest extraction (d=0.95) due to the failing institutions. Early adopters experience lower extraction (d=0.15) as they benefit from the alternative system. Existing states will have their directionality influenced by their success and legitimacy. If they are functional then directionality will be near 0.5, if failing then directionality will be nearer 0.2.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_failure_threshold,
    'At what point does state failure become irreversible, necessitating alternative structures?',
    'Historical analysis of state collapse and recovery; metrics of institutional trust and effectiveness.',
    'Determines the urgency and necessity of the alternative sovereignty scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_failure_threshold, empirical, 'Threshold for state failure requiring alternative structures.').

omega_variable(
    protocol_scalability,
    'Can decentralized protocols scale to handle the complexity and demands of a large population?',
    'Stress tests and simulations of decentralized systems under high load; analysis of governance and conflict resolution mechanisms.',
    'Determines the viability of the decentralized parallel as a replacement for existing institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protocol_scalability, empirical, 'Scalability of decentralized protocols.').

omega_variable(
    exit_community_cohesion,
    'Can exit communities maintain social cohesion and stability without traditional state structures?',
    'Sociological studies of intentional communities and decentralized governance experiments; analysis of conflict resolution and social support mechanisms.',
    'Determines the long-term sustainability of the decentralized parallel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_community_cohesion, empirical, 'Social cohesion of exit communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_sovereignty_scaffold, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0, alternative_sovereignty_scaffold, theater_ratio, 0, 0.6).
narrative_ontology:measurement(theater_t10, alternative_sovereignty_scaffold, theater_ratio, 10, 0.7).
narrative_ontology:measurement(theater_t20, alternative_sovereignty_scaffold, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0, alternative_sovereignty_scaffold, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(extractiveness_t10, alternative_sovereignty_scaffold, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(extractiveness_t20, alternative_sovereignty_scaffold, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_sovereignty_scaffold, global_infrastructure).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, state_capacity).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, decentralized_governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
