% ============================================================================
% CONSTRAINT STORY: autonomous_toolchain_sprawl
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomous_toolchain_sprawl, []).

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
 *   constraint_id: autonomous_toolchain_sprawl
 *   human_readable: The Recursive Maintenance Trap
 *   domain: technological
 *
 * SUMMARY:
 *   An organization deploys autonomous agents and CI/CD tools to manage
 *   infrastructure. Over time, the complexity of the toolchain grows,
 *   creating a recursive maintenance trap where engineers spend more time
 *   maintaining the automation than on the infrastructure itself. The
 *   increased complexity extracts from downstream engineering teams and
 *   long-term maintainability, while benefiting tool vendors and the initial
 *   automation teams.
 *
 * KEY AGENTS:
 *   - Tool Vendors: Primary beneficiary (institutional/arbitrage) - benefit from increased tool sales and lock-in.
 *   - Initial Automation Teams: Secondary beneficiary (moderate/constrained) - benefit from initial recognition, but become constrained by the growing complexity.
 *   - Downstream Engineering Teams: Primary victim (powerless/trapped) - reliant on complex tools without adequate training, facing high cognitive overhead.
 *   - Long-Term Maintainability: Victim (powerless/trapped) - abstract concept that suffers due to increased complexity and lack of maintainability focus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomous_toolchain_sprawl, 0.55).
domain_priors:suppression_score(autonomous_toolchain_sprawl, 0.6).
domain_priors:theater_ratio(autonomous_toolchain_sprawl, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomous_toolchain_sprawl, extractiveness, 0.55).
narrative_ontology:constraint_metric(autonomous_toolchain_sprawl, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(autonomous_toolchain_sprawl, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomous_toolchain_sprawl, tangled_rope).
narrative_ontology:human_readable(autonomous_toolchain_sprawl, "The Recursive Maintenance Trap").
narrative_ontology:topic_domain(autonomous_toolchain_sprawl, "technological").

domain_priors:requires_active_enforcement(autonomous_toolchain_sprawl).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomous_toolchain_sprawl, tool_vendors).
narrative_ontology:constraint_beneficiary(autonomous_toolchain_sprawl, initial_automation_teams).
narrative_ontology:constraint_victim(autonomous_toolchain_sprawl, downstream_engineering_teams).
narrative_ontology:constraint_victim(autonomous_toolchain_sprawl, long_term_maintainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Downstream engineering teams are increasingly reliant on complex automated toolchains, often without adequate training or understanding. They become trapped in maintaining systems they didn't build, facing high cognitive overhead and limited exit options.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Tool vendors benefit directly from the increasing complexity and sprawl of autonomous toolchains. They experience the constraint as a coordination mechanism, driving sales and creating vendor lock-in.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The initial automation teams benefit from the initial deployment and recognition, but become constrained by the growing complexity of their own creation. They have some exit options, but are also responsible for maintaining the systems they built.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% From an analytical perspective, the recursive maintenance trap is a tangled rope: it provides some coordination benefits through automation, but also creates significant extraction due to increased complexity and maintenance overhead.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomous_toolchain_sprawl_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autonomous_toolchain_sprawl, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(autonomous_toolchain_sprawl_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The downstream teams experience a significant extraction due to the added complexity of maintaining the system, even though the initial goal was to reduce workload. Suppression (0.60): High. The complexity of the automated toolchain suppresses the alternatives available to downstream teams, making them dependent on the system.
 *
 * PERSPECTIVAL GAP:
 *   The tool vendors experience a rope, because it increases their revenue. The engineers experience it as a snare, because it increases their workload. The analytical observer recognizes the tangled rope, and the initial automation team sees the short term benefit but the long term cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Tool vendors are the primary beneficiaries with low directionality and high exit options (arbitrage). Downstream teams are the primary victims with high directionality and low exit options (trapped).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_overhead_threshold,
    'What level of cognitive overhead is acceptable for engineers maintaining autonomous toolchains?',
    'Surveys and cognitive load measurements of engineers working with different types of autonomous systems.',
    'A lower threshold would classify this as a more severe Snare, while a higher threshold might indicate a manageable Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_overhead_threshold, empirical, 'Determines the acceptable level of cognitive overhead').

omega_variable(
    toolchain_coupling_degree,
    'How tightly coupled are the components of the autonomous toolchain?',
    'Network analysis of the toolchain dependencies and inter-tool communication patterns.',
    'Tightly coupled toolchains are more difficult to maintain and evolve, increasing the extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(toolchain_coupling_degree, empirical, 'Measures the degree of coupling within the toolchain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomous_toolchain_sprawl, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t0, autonomous_toolchain_sprawl, theater_ratio, 0, 0.1).
narrative_ontology:measurement(auto_tr_t5, autonomous_toolchain_sprawl, theater_ratio, 5, 0.3).
narrative_ontology:measurement(auto_tr_t10, autonomous_toolchain_sprawl, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(auto_be_t0, autonomous_toolchain_sprawl, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(auto_be_t5, autonomous_toolchain_sprawl, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(auto_be_t10, autonomous_toolchain_sprawl, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomous_toolchain_sprawl, resource_allocation).
narrative_ontology:affects_constraint(autonomous_toolchain_sprawl, technical_debt_accumulation).
narrative_ontology:affects_constraint(autonomous_toolchain_sprawl, vendor_lockin).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
