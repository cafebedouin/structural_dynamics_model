% ============================================================================
% CONSTRAINT STORY: maintenance_capacity_shortfall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maintenance_capacity_shortfall, []).

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
 *   constraint_id: maintenance_capacity_shortfall
 *   human_readable: The Entropic Debt Trap
 *   domain: infrastructure/logistical/technological
 *
 * SUMMARY:
 *   The entropic debt trap occurs when the complexity of a system scales
 *   beyond the available resources for its upkeep. This leads to a
 *   degradation of functionality and increased costs for downstream users and
 *   future generations. The system benefits initial adopters and vendors, who
 *   extract short-term gains without fully accounting for long-term
 *   maintenance requirements.
 *
 * KEY AGENTS:
 *   - System Vendors: Primary beneficiary (institutional/arbitrage) - benefits from initial deployment and upgrades.
 *   - Initial Adopters: Secondary beneficiary (moderate/mobile) - benefits from early adoption of the technology or system.
 *   - Downstream Users: Primary victim (moderate/constrained) - relies on system, but constrained by its complexity. 
 *   - Future Generations: Primary victim (powerless/trapped) - inherits decaying systems and increasing costs.
 *   - Open Source Maintainers: Mitigation agent (organized/mobile) - attempts to support and fix systems, relies on voluntary contributions.
 *   - Legacy Governance Body: Devolved actor (institutional/constrained) - diminished authority over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maintenance_capacity_shortfall, 0.6).
domain_priors:suppression_score(maintenance_capacity_shortfall, 0.7).
domain_priors:theater_ratio(maintenance_capacity_shortfall, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, extractiveness, 0.6).
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maintenance_capacity_shortfall, tangled_rope).
narrative_ontology:human_readable(maintenance_capacity_shortfall, "The Entropic Debt Trap").
narrative_ontology:topic_domain(maintenance_capacity_shortfall, "infrastructure/logistical/technological").

domain_priors:requires_active_enforcement(maintenance_capacity_shortfall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, system_vendors).
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, initial_adopters).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, downstream_users).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations inherit decaying infrastructure and technological debt, with limited capacity to influence initial design decisions. Trapped by legacy systems.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Downstream users rely on the system but are constrained by its complexity and limited maintenance capacity. They experience both benefit from functionality and extraction from system failures and costs of workarounds.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% System vendors benefit from initial deployment and upgrades, but may externalize maintenance costs or create planned obsolescence. Can arbitrage to newer contracts.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Open source maintainers attempt to provide ongoing support and fixes, but rely on voluntary contributions and are often overwhelmed by the system's complexity. They see the system as a temporary support structure. Mobile due to the freedom to fork projects.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The original governing body attempts to manage the system, but their ability diminishes over time. Their management capacity is degraded and their authority may have atrophied.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer identifies that the entropic debt trap constitutes a system where the increasing complexity and maintenance requirements lead to a decline in overall functionality. Coordination is attempted but extraction dominates.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maintenance_capacity_shortfall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maintenance_capacity_shortfall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maintenance_capacity_shortfall, TR),
    TR >= 0.70.

:- end_tests(maintenance_capacity_shortfall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Significant resources extracted from downstream users and future generations due to system failures, workarounds, and technological debt. Suppression (0.70): High. Alternatives are often suppressed due to lock-in effects, network externalities, or the complexity of migrating to new systems. Theater Ratio (0.40): Moderate. While there are governance and regulatory bodies, they often lack the resources or authority to effectively address the maintenance capacity shortfall.
 *
 * PERSPECTIVAL GAP:
 *   The entropic debt trap manifests differently depending on the observer's position. System vendors see a rope (coordination) because they initially benefit from the sale and implementation of the system. Downstream users experience a tangled rope (mixed coordination and extraction) as they both benefit from the system's functionality but suffer from its failures and maintenance costs. Future generations are trapped in a snare as they inherit the decaying infrastructure and lack the ability to influence initial design decisions. Open source maintainers view the system as a temporary scaffold aimed at ameliorating issues but often overwhelmed. Legacy Governance sees itself as a Piton, unable to fix the situation due to atrophy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position. System vendors are beneficiaries and can arbitrage, resulting in low d. Downstream users are constrained, resulting in higher d. Future generations are trapped, resulting in the highest d. Open source maintainers are organized and mobile, resulting in low d. The Legacy governance is constrained, leading to a high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the type depends on perspective. The vendors are acting in their best interest, so they see the system as a rope to coordinate deployment and upgrades. However, this view doesn't fully consider the long term consequences on future generations that face a snare. The analytical observer needs to consider the perspectives of each actor to accurately classify the constraint as a Tangled Rope. The coordination has an ulterior motive that results in extraction from vulnerable actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_metrics,
    'How do we effectively measure and quantify the complexity of infrastructure/logistical/technological systems?',
    'Development of new complexity metrics, analysis of system architectures, measurement of interdependencies',
    'Better complexity measures would allow for a more precise estimation of the maintenance burden and allow for proactive design of simpler systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_metrics, conceptual, 'Effective metrics to quantify systems complexity.').

omega_variable(
    discount_rate_selection,
    'What discount rate should be used when evaluating future maintenance costs versus initial deployment savings?',
    'Ethical and economical debate on how to value the well-being of future generations versus current benefits.',
    'A high discount rate would justify underinvestment in maintenance; a low discount rate would push for designs with lower long-term maintenance needs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discount_rate_selection, preference, 'Choice of discount rate when valuing future costs.').

omega_variable(
    emergence_robustness,
    'Can design approaches be found that are emergent (adaptable without central control) and yet are robust (avoid collapse into destructive regimes)?',
    'Exploration of modularity, local information processing, and distributed control.',
    'A positive resolution would suggest maintenance cost could be dramatically lowered. A negative resolution would suggest the inevitable cost of increased complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_robustness, empirical, 'Design patterns for both emergence and robustness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maintenance_capacity_shortfall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(main_tr_t0, maintenance_capacity_shortfall, theater_ratio, 0, 0.2).
narrative_ontology:measurement(main_tr_t5, maintenance_capacity_shortfall, theater_ratio, 5, 0.3).
narrative_ontology:measurement(main_tr_t10, maintenance_capacity_shortfall, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(main_be_t0, maintenance_capacity_shortfall, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(main_be_t5, maintenance_capacity_shortfall, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(main_be_t10, maintenance_capacity_shortfall, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maintenance_capacity_shortfall, global_infrastructure).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, software_obsolescence).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, technical_debt_accumulation).

% DUAL FORMULATION NOTE:
% The entropic debt trap is a higher-level constraint that encompasses specific instances like software obsolescence and technical debt. These downstream constraints are affected by the entropic debt trap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
