% ============================================================================
% CONSTRAINT STORY: complexity_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complexity_debt, []).

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
 *   constraint_id: complexity_debt
 *   human_readable: The Cumulative Fragility Surcharge
 *   domain: technological/organizational
 *
 * SUMMARY:
 *   The Cumulative Fragility Surcharge represents the accrued cost of
 *   'quick-fix' solutions and layered abstractions within a system. This
 *   leads to increased bugs, decreased performance, and overall instability.
 *   While individual quick fixes may seem beneficial in the short term, their
 *   cumulative effect creates a tangled web of dependencies that are
 *   difficult to manage and maintain, ultimately harming end users and
 *   burdening maintainers.
 *
 * KEY AGENTS:
 *   - Original Developers: Primary beneficiary (institutional/arbitrage) - benefits from quick delivery.
 *   - Short Term Managers: Primary beneficiary (powerful/constrained) - benefits from perceived short-term gains.
 *   - Long Term Maintainers: Primary victim (moderate/constrained) - bears the cost of refactoring and bug fixing.
 *   - End Users: Primary victim (powerless/trapped) - experiences increased bugs and decreased performance.
 *   - Analytical Observer: Analytical observer (analytical/analytical) - sees the cumulative effect creating a long-term problem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complexity_debt, 0.6).
domain_priors:suppression_score(complexity_debt, 0.7).
domain_priors:theater_ratio(complexity_debt, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complexity_debt, extractiveness, 0.6).
narrative_ontology:constraint_metric(complexity_debt, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(complexity_debt, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complexity_debt, tangled_rope).
narrative_ontology:human_readable(complexity_debt, "The Cumulative Fragility Surcharge").
narrative_ontology:topic_domain(complexity_debt, "technological/organizational").

domain_priors:requires_active_enforcement(complexity_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complexity_debt, original_developers).
narrative_ontology:constraint_beneficiary(complexity_debt, short_term_managers).
narrative_ontology:constraint_victim(complexity_debt, long_term_maintainers).
narrative_ontology:constraint_victim(complexity_debt, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% End users are trapped, experiencing increased bugs, decreased performance, and overall instability due to unchecked complexity.
constraint_indexing:constraint_classification(complexity_debt, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Maintainers are constrained by the existing system's architecture, bearing the cost of refactoring and bug fixing. However, they benefit from job security.
constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Original developers initially benefit from delivering features quickly. They can arbitrage their knowledge into new positions or projects.
constraint_indexing:constraint_classification(complexity_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Management benefits from perceived short-term gains and are now stuck with a system they do not understand. The system is a liability.
constraint_indexing:constraint_classification(complexity_debt, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The observer sees the cumulative effect of short-term decisions creating a long-term problem. The short-term solutions are not bad alone, but create a tangled rope when combined.
constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complexity_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complexity_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complexity_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(complexity_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(complexity_debt, TR),
    TR >= 0.70.

:- end_tests(complexity_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The cumulative effect of quick fixes extracts a significant cost in terms of increased maintenance, reduced performance, and user dissatisfaction. Suppression (0.7): High. The complexity of the system makes it difficult to refactor or replace, effectively suppressing alternative solutions. Theater Ratio (0.75): High. The focus is on delivering quick fixes rather than addressing the underlying architectural issues, creating a performative culture of addressing symptoms rather than root causes.
 *
 * PERSPECTIVAL GAP:
 *   End users see a snare because they are trapped in a system with increased bugs and decreased performance. Maintainers see a tangled rope because they are constrained by the existing architecture but also benefit from job security. Original developers see a rope because they benefit from quick delivery. Short term management see a piton because they are stuck with something that is a liability and not an asset. The analytical observer is able to understand the overall effects of compounding complexity and see a tangled rope. 
 *
 * DIRECTIONALITY LOGIC:
 *   Original developers and short term management are beneficiaries because they receive direct gains from the system. Long term maintainers and end users are victims because they bear the burden of the system's fragility. The analytical observer sees the full picture, weighing the short-term gains against the long-term costs.
 *
 * MANDATROPHY ANALYSIS:
 *   Without this classification, the initial quick fixes could be misconstrued as pure coordination, when in reality they are contributing to a long-term extraction mechanism. The tangled rope classification acknowledges the initial coordination benefits but also highlights the cumulative extraction that results from unchecked complexity. The differing perspectives illustrate how the same constraint can be perceived differently depending on one's position within the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refactoring_cost_threshold,
    'At what point does refactoring outweigh the cost of maintaining the fragile system?',
    'Cost-benefit analysis of refactoring vs. maintenance.',
    'Determines if the system transitions from tangled rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refactoring_cost_threshold, empirical, 'Threshold determining refactoring viability.').

omega_variable(
    system_replacement_viability,
    'Is a complete system replacement a viable option, or is the complexity so deeply embedded that it is impossible?',
    'Feasibility study evaluating replacement options.',
    'Determines if end users remain trapped or gain mobility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(system_replacement_viability, empirical, 'System replacement viability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complexity_debt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, complexity_debt, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t5, complexity_debt, theater_ratio, 5, 0.5).
narrative_ontology:measurement(comp_tr_t10, complexity_debt, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, complexity_debt, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t5, complexity_debt, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(comp_be_t10, complexity_debt, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complexity_debt, resource_allocation).
narrative_ontology:affects_constraint(complexity_debt, technical_debt).
narrative_ontology:affects_constraint(complexity_debt, organizational_inertia).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
