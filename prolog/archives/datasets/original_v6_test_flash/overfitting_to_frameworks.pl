% ============================================================================
% CONSTRAINT STORY: overfitting_to_frameworks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overfitting_to_frameworks, []).

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
 *   constraint_id: overfitting_to_frameworks
 *   human_readable: The Rigidity of the Map
 *   domain: technological/cognitive/organizational
 *
 * SUMMARY:
 *   This constraint occurs when an organization or agent optimizes their
 *   behavior so tightly to a specific evaluative or technical framework that
 *   they lose the ability to perceive or respond to external reality. This
 *   can lead to a situation where the 'map' (the framework) becomes more
 *   important than the 'territory' (the real world), resulting in decreased
 *   adaptability and innovation.
 *
 * KEY AGENTS:
 *   - Framework Vendors: Primary beneficiary (institutional/arbitrage) - benefit from framework adoption.
 *   - Internal Framework Maintainers: Secondary beneficiary (powerful/constrained) - responsible for framework implementation.
 *   - Organizational Adaptability: Primary victim (moderate/constrained) - suffers from the rigidity imposed by the framework.
 *   - Novel Problem Solvers: Secondary victim (powerless/trapped) - unable to propose solutions outside the framework.
 *   - Analytical Observer: Sees both benefits and drawbacks (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overfitting_to_frameworks, 0.55).
domain_priors:suppression_score(overfitting_to_frameworks, 0.6).
domain_priors:theater_ratio(overfitting_to_frameworks, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overfitting_to_frameworks, extractiveness, 0.55).
narrative_ontology:constraint_metric(overfitting_to_frameworks, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(overfitting_to_frameworks, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overfitting_to_frameworks, tangled_rope).
narrative_ontology:human_readable(overfitting_to_frameworks, "The Rigidity of the Map").
narrative_ontology:topic_domain(overfitting_to_frameworks, "technological/cognitive/organizational").

domain_priors:requires_active_enforcement(overfitting_to_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, framework_vendors).
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, internal_framework_maintainers).
narrative_ontology:constraint_victim(overfitting_to_frameworks, organizational_adaptability).
narrative_ontology:constraint_victim(overfitting_to_frameworks, novel_problem_solvers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of individuals who see novel solutions but are blocked because those solutions don't fit the framework.
constraint_indexing:constraint_classification(overfitting_to_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the overall organization, which benefits from the structure but is also constrained by its rigidity. The organization cannot easily exit the use of the framework due to sunk costs and embedded processes.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the vendors who sell the frameworks. They benefit from the widespread adoption of their framework and can arbitrage between different clients.
constraint_indexing:constraint_classification(overfitting_to_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of those responsible for maintaining the framework within the organization. They benefit from the framework providing structure but are constrained in their ability to adapt it to changing circumstances. Over time, the function has atrophied, but the constraint remains.
constraint_indexing:constraint_classification(overfitting_to_frameworks, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the framework as a Tangled Rope, providing some coordination benefits but also extracting value from the system by limiting adaptability.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overfitting_to_frameworks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overfitting_to_frameworks, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overfitting_to_frameworks, TR),
    TR >= 0.70.

:- end_tests(overfitting_to_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The framework extracts from the organization by limiting adaptability and innovation. Suppression (0.60): Alternative approaches are suppressed because they don't fit within the framework. Theater Ratio (0.40): Some performative aspects, but framework also has practical benefits.
 *
 * PERSPECTIVAL GAP:
 *   The victims (Organizational Adaptability and Novel Problem Solvers) experience the framework as a Snare, while the beneficiaries (Framework Vendors) experience it as a Rope. The Analytical Observer and the organization itself (Organizational Adaptability) experience it as a Tangled Rope, recognizing both the benefits and drawbacks.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries are the framework vendors, who actively promote and sell the framework. Internal framework maintainers also benefit by having structure. The victims are Organizational Adaptability because the reliance on the framework hinders changes. Novel Problem Solvers are victims because the only acceptable solutions are those that conform to the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework seems like an information standard but often becomes an enforcement mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptability_vs_efficiency,
    'What is the optimal balance between short-term efficiency gains from using the framework and long-term adaptability?',
    'Cost-benefit analysis of framework usage vs. alternative approaches, considering both direct costs and opportunity costs.',
    'Determines whether the framework should be abandoned, modified, or continued as is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptability_vs_efficiency, empirical, 'Balance between adaptability and efficiency.').

omega_variable(
    framework_scope,
    'What is the appropriate scope of the framework, considering the diversity of problems that need to be solved?',
    'Evaluation of the framework''s performance across different problem domains.',
    'Determines whether the framework should be applied to all problems or only a subset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framework_scope, conceptual, 'The appropriate scope of the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overfitting_to_frameworks, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(over_tr_t0, overfitting_to_frameworks, theater_ratio, 0, 0.2).
narrative_ontology:measurement(over_tr_t5, overfitting_to_frameworks, theater_ratio, 5, 0.3).
narrative_ontology:measurement(over_tr_t10, overfitting_to_frameworks, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(over_be_t0, overfitting_to_frameworks, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(over_be_t5, overfitting_to_frameworks, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(over_be_t10, overfitting_to_frameworks, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overfitting_to_frameworks, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
