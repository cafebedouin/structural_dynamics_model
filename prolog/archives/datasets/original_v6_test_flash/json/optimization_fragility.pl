% ============================================================================
% CONSTRAINT STORY: optimization_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimization_fragility, []).

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
 *   constraint_id: optimization_fragility
 *   human_readable: The Efficiency-Resilience Tradeoff
 *   domain: economic/technological/infrastructural
 *
 * SUMMARY:
 *   The efficiency-resilience tradeoff describes the scenario where a system,
 *   optimized for maximum efficiency (e.g., just-in-time manufacturing, lean
 *   supply chains), becomes highly vulnerable to disruptions. By removing
 *   redundancies and buffers, the system minimizes costs under normal
 *   conditions but sacrifices its ability to withstand unexpected shocks or
 *   crises. This constraint highlights the tension between short-term gains
 *   and long-term stability, exhibiting characteristics of both coordination
 *   and extraction. A system designed to maximize efficiency also extracts
 *   the option for it to be resilient.
 *
 * KEY AGENTS:
 *   - System Operators: Primary beneficiaries (institutional/arbitrage) - benefit from cost savings and efficiency gains.
 *   - Consumers (normal times): Secondary beneficiaries (moderate/constrained) - benefit from lower prices and increased availability, constrained as consumers are exposed to the vulnerabilities of the optimized system.
 *   - Vulnerable Populations: Primary victims (powerless/trapped) - bear the brunt of system failure during crises.
 *   - System Stability: Primary victim (powerless/trapped) - the hyper-optimized system compromises global stability.
 *   - Government Regulators: Intermediary (institutional/constrained) - designed to guarantee public safety and system robustness, but constrained due to theater or capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimization_fragility, 0.55).
domain_priors:suppression_score(optimization_fragility, 0.6).
domain_priors:theater_ratio(optimization_fragility, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimization_fragility, extractiveness, 0.55).
narrative_ontology:constraint_metric(optimization_fragility, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(optimization_fragility, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimization_fragility, tangled_rope).
narrative_ontology:human_readable(optimization_fragility, "The Efficiency-Resilience Tradeoff").
narrative_ontology:topic_domain(optimization_fragility, "economic/technological/infrastructural").

domain_priors:requires_active_enforcement(optimization_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimization_fragility, system_operators).
narrative_ontology:constraint_beneficiary(optimization_fragility, consumers_normal_times).
narrative_ontology:constraint_victim(optimization_fragility, vulnerable_populations).
narrative_ontology:constraint_victim(optimization_fragility, system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Vulnerable Populations during crisis. Powerless, trapped. Bears the brunt of system failure. Limited options for exit. Experiences the full negative impact as a Snare. High dependence on the optimized system makes them vulnerable.
constraint_indexing:constraint_classification(optimization_fragility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: System Operators during normal times. Institutional power, benefit from cost savings, can arbitrage by shifting resources. Experience the efficiency gains as a Rope. They manage the system and benefit from its optimized performance.
constraint_indexing:constraint_classification(optimization_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 3: Consumers during normal times. Moderate power, constrained. Benefit from lower prices/increased availability, but are vulnerable to disruptions. Tangled Rope: benefit from efficiency, but bear the cost of fragility. Dependence is usually not a conscious choice but more a function of ease of use. Some exit options exist (switch brands, reduce consumption) but are difficult.
constraint_indexing:constraint_classification(optimization_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 4: Analytical Observer, Civilizational scope. Views the system holistically and recognizes the tradeoff. Tangled Rope classification reflects the inherent tension between efficiency and resilience, coordination/extraction function.
constraint_indexing:constraint_classification(optimization_fragility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 5: Government Regulators. Institutional power, constrained exit (career concerns, political pressures). Intended to ensure system stability and public safety, however become Piton due to regulatory capture or lack of resources. Over time, the regulators' original purpose is degraded, however theatrical compliance is still upheld. A focus on paperwork compliance over on-site testing or auditing.
constraint_indexing:constraint_classification(optimization_fragility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimization_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_fragility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimization_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(optimization_fragility, TR),
    TR >= 0.70.

:- end_tests(optimization_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The system extracts resilience from the vulnerable populations and system stability. The benefits are narrowly focused on efficiency. Suppression (0.60): Moderate-High. The optimization actively suppresses alternative, more resilient designs. High economic pressures and standardized technological stacks lead to lack of diversity, and increased lock-in effects. Theater Ratio (0.75): High. Over time, the regulators' original purpose is degraded, however theatrical compliance is still upheld. A focus on paperwork compliance over on-site testing or auditing.
 *
 * PERSPECTIVAL GAP:
 *   The Vulnerable Populations during a crisis see a Snare. The system provides no benefits and extracts heavily when it fails. System operators, on the other hand, see coordination (Rope) during normal operation and gain from reduced cost. Analytical Observer is the Tangled Rope as this agent recognizes the design decision and associated tradeoffs. Consumers, benefitting during uptime, see a Tangled Rope as well as they are somewhat constrained, however can sometimes leverage local exit options. The regulators' view is degraded, as they are no longer an active safeguard, but more an outdated compliance checkbox.
 *
 * DIRECTIONALITY LOGIC:
 *   The agent's structural position dictates the experienced directionality. System operators with institutional power and arbitrage options experience the constraint as a rope, while the vulnerable populations, powerless and trapped, experience a snare. Consumers have some exit options, so see a Tangled Rope. The Government regulators, designed as a safeguard, become Pitons due to capture or obsolescence.
 *
 * MANDATROPHY ANALYSIS:
 *   The efficiency-resilience tradeoff is a classic example of a potential mandatrophy problem, as pure extraction (Snare) can be mislabeled as coordination (Rope). The key is to properly account for the costs of potential failures and who bears those costs. The long-term stability of the overall system must be considered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    acceptable_failure_rate,
    'What is the acceptable failure rate for the optimized system, balancing cost savings and societal disruption?',
    'Cost-benefit analysis of different system configurations, accounting for potential crisis scenarios and their associated costs.',
    'Determines the optimal level of redundancy and buffering within the system. Affects the classification from Snare to Tangled Rope or even Rope if resilience measures are implemented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_failure_rate, preference, 'Threshold for acceptable failure rate').

omega_variable(
    black_swan_likelihood,
    'How accurately can we predict and prepare for black swan events that could cripple the hyper-optimized system?',
    'Improved risk modeling, stress testing, and scenario planning. Incorporating historical data and expert opinions to refine probability estimates.',
    'Affects the perceived risk and vulnerability of the system. Lower likelihood reduces the perceived extractiveness from some perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_swan_likelihood, empirical, 'Likelihood of unpredictable events').

omega_variable(
    alternative_system_availability,
    'How readily available are alternative systems or fallback options in case of a major disruption?',
    'Assessment of existing infrastructure, capacity of backup systems, and ease of switching between different modes of operation.',
    'Availability of alternatives reduces the ''trapped'' status of some agents and shifts the classification towards Tangled Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_availability, empirical, 'Availability of alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimization_fragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opti_tr_t0, optimization_fragility, theater_ratio, 0, 0.5).
narrative_ontology:measurement(opti_tr_t5, optimization_fragility, theater_ratio, 5, 0.6).
narrative_ontology:measurement(opti_tr_t10, optimization_fragility, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(opti_be_t0, optimization_fragility, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(opti_be_t5, optimization_fragility, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(opti_be_t10, optimization_fragility, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimization_fragility, resource_allocation).
narrative_ontology:affects_constraint(optimization_fragility, global_supply_chains).
narrative_ontology:affects_constraint(optimization_fragility, just_in_time_manufacturing).

% DUAL FORMULATION NOTE:
% The optimization_fragility is the broad constraint of hyper-efficiency leading to vulnerability. This constraint affects many other supply chains, however has its own unique signature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
