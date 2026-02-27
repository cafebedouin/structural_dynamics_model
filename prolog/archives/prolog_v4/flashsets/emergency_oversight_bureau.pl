% ============================================================================
% CONSTRAINT STORY: emergency_oversight_bureau
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [DRAFT]
% ============================================================================

:- module(constraint_emergency_oversight_bureau, []).

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
 *   constraint_id: emergency_oversight_bureau
 *   human_readable: The Crisis Scaffold
 *   domain: political
 *
 * SUMMARY:
 *   A temporary administrative body created to manage a specific recovery
 *   period. The structure is designed to rapidly allocate resources and
 *   coordinate efforts, but is at risk of mission creep and becoming a
 *   permanent fixture, even after the crisis has passed.
 *
 * KEY AGENTS:
 *   - Crisis Victims: Primary beneficiaries (organized/constrained) - Receive aid but rely on the bureau's existence.
 *   - Taxpayers: Primary victims (powerless/trapped) - Fund the bureau with limited oversight.
 *   - Local Communities: Beneficiaries (moderate/constrained) - Receive resources and coordination.
 *   - Competing Interests: Victims (powerful/mobile) - Oppose the bureau's resource control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_oversight_bureau, 0.35).
domain_priors:suppression_score(emergency_oversight_bureau, 0.4).
domain_priors:theater_ratio(emergency_oversight_bureau, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_oversight_bureau, extractiveness, 0.35).
narrative_ontology:constraint_metric(emergency_oversight_bureau, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(emergency_oversight_bureau, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_oversight_bureau, scaffold).
narrative_ontology:human_readable(emergency_oversight_bureau, "The Crisis Scaffold").
narrative_ontology:topic_domain(emergency_oversight_bureau, "political").

domain_priors:requires_active_enforcement(emergency_oversight_bureau).
narrative_ontology:has_sunset_clause(emergency_oversight_bureau).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, crisis_victims).
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, local_communities).
narrative_ontology:constraint_victim(emergency_oversight_bureau, taxpayers).
narrative_ontology:constraint_victim(emergency_oversight_bureau, competing_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The bureau, designed for rapid response and resource allocation, ideally sees its function as a pure coordination mechanism. Its existence justifies its budget and influence.
constraint_indexing:constraint_classification(emergency_oversight_bureau, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Taxpayers bear the cost of the bureau's operations but have little direct control over its spending or effectiveness. They're structurally trapped unless a strong oversight system prevails.
constraint_indexing:constraint_classification(emergency_oversight_bureau, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% For crisis victims, the bureau provides temporary relief and support, acting as a scaffold to help them rebuild their lives. Access is constrained but possible.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Looking back, the historian sees the bureau as an institution that, while initially intended to be temporary, outlived its usefulness and became a source of patronage and rent-seeking. The sunset clause was not respected, and the theater ratio is high.
constraint_indexing:constraint_classification(emergency_oversight_bureau, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_oversight_bureau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_oversight_bureau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_oversight_bureau, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_oversight_bureau, TR),
    TR >= 0.70.

:- end_tests(emergency_oversight_bureau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.35) as resources are diverted from other uses to fund the bureau. Suppression is moderate (0.40) due to the reduced control over spending and limited exit options available. Theater is initially low (0.25) but could increase over time as the crisis recedes and the bureau engages in more performative activities.
 *
 * PERSPECTIVAL GAP:
 *   Victims see the agency as extractive. The oversight agency may see itself as an important coordination mechanism. An outside observer may see the agency as wasteful theater. The agency structure enables each position.
 *
 * DIRECTIONALITY LOGIC:
 *   The crisis victims benefit, but are also constrained by the structure of the bureaucracy. Competing interests are targeted by suppression, which limits exit. Taxpayers must subsidize the budget, regardless of performance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bureau_effectiveness,
    'How effective is the bureau at achieving its stated goals?',
    'Independent audits and performance evaluations.',
    'If highly effective, the classification trends towards rope. If ineffective, it drifts towards piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureau_effectiveness, empirical, 'The actual effectiveness of the bureau''s operations.').

omega_variable(
    sunset_clause_enforcement,
    'Will the sunset clause be enforced, or will the bureau become a permanent institution?',
    'Political will and legislative action.',
    'If enforced, the classification remains scaffold. If not enforced, it drifts towards piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_enforcement, preference, 'The political feasibility of enforcing the sunset clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_oversight_bureau, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emer_tr_t0, emergency_oversight_bureau, theater_ratio, 0, 0.1).
narrative_ontology:measurement(emer_tr_t2, emergency_oversight_bureau, theater_ratio, 2, 0.2).
narrative_ontology:measurement(emer_tr_t5, emergency_oversight_bureau, theater_ratio, 5, 0.25).

% Extraction over time
narrative_ontology:measurement(emer_be_t0, emergency_oversight_bureau, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(emer_be_t2, emergency_oversight_bureau, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(emer_be_t5, emergency_oversight_bureau, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_oversight_bureau, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
