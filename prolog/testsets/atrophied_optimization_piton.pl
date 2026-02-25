% ============================================================================
% CONSTRAINT STORY: atrophied_optimization_piton
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_atrophied_optimization_piton, []).

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
 *   constraint_id: atrophied_optimization_piton
 *   human_readable: The Ghost in the Habit
 *   domain: technological/social
 *
 * SUMMARY:
 *   This constraint models a common form of organizational decay in the
 *   digital age: an AI or algorithmic optimization system is deactivated or
 *   becomes obsolete, but the human processes, KPIs, and behavioral habits it
 *   created persist through institutional inertia. Employees are forced to
 *   follow a 'ghost protocol' that no longer serves its original purpose. The
 *   function has atrophied, but the performance remains mandatory, creating a
 *   high-theater environment where compliance is detached from outcome.
 *
 * KEY AGENTS:
 *   - Frontline Employees: Primary victims (powerless/trapped) — forced to execute pointless tasks, bearing the cost of wasted time and cognitive load.
 *   - Legacy Process Enforcers: Primary beneficiaries (organized/constrained) — middle managers or compliance officers whose authority and roles are tied to the enforcement of the obsolete process.
 *   - Organizational Adaptability: Abstract victim (powerless/trapped) — the organization's ability to evolve is crippled by the rigid, nonsensical workflow.
 *   - The Turnaround Consultant: External agent (powerful/arbitrage) — sees the system as a temporary inefficiency to be eliminated, representing a potential sunset clause.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(atrophied_optimization_piton, 0.48).
domain_priors:suppression_score(atrophied_optimization_piton, 0.8).
domain_priors:theater_ratio(atrophied_optimization_piton, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(atrophied_optimization_piton, extractiveness, 0.48).
narrative_ontology:constraint_metric(atrophied_optimization_piton, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(atrophied_optimization_piton, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(atrophied_optimization_piton, piton).
narrative_ontology:human_readable(atrophied_optimization_piton, "The Ghost in the Habit").
narrative_ontology:topic_domain(atrophied_optimization_piton, "technological/social").

domain_priors:requires_active_enforcement(atrophied_optimization_piton).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(atrophied_optimization_piton, legacy_process_enforcers).
narrative_ontology:constraint_victim(atrophied_optimization_piton, frontline_employees).
narrative_ontology:constraint_victim(atrophied_optimization_piton, organizational_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE EMPLOYEE (TANGLED ROPE) — Trapped within a mandatory, nonsensical workflow. The process extracts time and effort for no discernible benefit, feeling like a coercive system. The high suppression and moderate base extraction create a Tangled Rope experience. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.54.
constraint_indexing:constraint_classification(atrophied_optimization_piton, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LEGACY PROCESS ENFORCER (ROPE) — A middle manager whose role is defined by enforcing the legacy workflow. From this view, the process is a valuable coordination tool that ensures stability and predictability. Their role is constrained by the system they uphold. d≈0.25 (beneficiary+constrained), f(d)≈0.14, σ=0.9 → χ≈0.06.
constraint_indexing:constraint_classification(atrophied_optimization_piton, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (PITON) — The system's original function has atrophied, but the ritual remains. The defining feature is the massive gap between performative action and functional outcome. The theater_ratio of 0.85 decisively classifies this as a Piton.
constraint_indexing:constraint_classification(atrophied_optimization_piton, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: TURNAROUND CONSULTANT (SCAFFOLD) — An external agent brought in to fix the inefficiency. They see the legacy process as a temporary obstacle to be dismantled and replaced. Their intervention represents a sunset clause on the constraint's existence. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(atrophied_optimization_piton, scaffold,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(atrophied_optimization_piton_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(atrophied_optimization_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(atrophied_optimization_piton, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(atrophied_optimization_piton, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(atrophied_optimization_piton, TR),
    TR >= 0.70.

:- end_tests(atrophied_optimization_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48): This value is higher than typical for a Piton because it captures not just direct extraction but the immense opportunity cost of misallocating skilled human labor to a defunct process. Suppression (0.80): High, as the workflows are mandatory and deviation is penalized, leaving employees with no alternative. Theater Ratio (0.85): Very high. This is the defining metric. The actions are almost entirely performative, satisfying a procedural requirement that has been decoupled from its original optimization goal. This score firmly triggers the Piton classification gate.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. Frontline employees experience the constraint as a Tangled Rope — a coercive, extractive system they are trapped in. Legacy enforcers, who benefit from the stability and their role in maintaining it, see a functional Rope. The analytical observer, however, identifies the core pathology: the massive disconnect between action and purpose, classifying it as a Piton. This highlights how a Piton, while analytically defined by its theatricality, can be experienced as a highly coercive Snare or Tangled Rope by those subject to its rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (legacy_process_enforcers) have a constrained exit, as their roles depend on the system, leading to a low but positive directionality (d). Victims (frontline_employees) are trapped, leading to a very high directionality (d≈0.95) and thus high effective extraction (χ). This structural difference in power and exit options drives the perspectival gap between seeing the system as a coordinating Rope versus an extractive Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case prevents mandatrophy by showing that a 'Piton' is not necessarily benign. While its analytical classification is based on atrophied function and high theater, its high suppression and moderate extraction can create severe negative consequences for its victims. The framework correctly identifies the analytical type as Piton while simultaneously capturing the victim's experience as a Tangled Rope. It avoids mislabeling a coercive system as merely 'inertial' by allowing both classifications to be true from their respective indices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inertia_vs_hidden_function,
    'Is the process''s persistence purely due to bureaucratic inertia, or does it serve a hidden, unstated function (e.g., satisfying an obscure compliance requirement)?',
    'A full process audit and dependency mapping to trace the outputs of the workflow to any downstream consumers or regulatory reports.',
    'If a hidden function exists, the theater_ratio would decrease, potentially reclassifying it as a Tangled Rope from the analytical view. If not, the Piton classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_hidden_function, empirical, 'Distinguishing between pure inertia and a hidden, unstated function.').

omega_variable(
    quantifying_opportunity_cost,
    'What is the true economic cost of forcing skilled employees to adhere to this suboptimal, atrophied process?',
    'Time-tracking studies comparing the legacy process to a rationalized alternative, combined with analysis of employee turnover and morale data in affected departments.',
    'A higher quantified cost would increase the base_extractiveness (ε) score, reinforcing the Tangled Rope classification from the victim''s perspective and highlighting the malignancy of the Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantifying_opportunity_cost, empirical, 'Measuring the full opportunity cost of the atrophied workflow.').

omega_variable(
    potential_for_reactivation,
    'Could the atrophied process be revitalized by integrating a modern AI, turning the Piton back into a functional Rope?',
    'A pilot project to re-instrument the workflow with a new optimization engine, measuring efficiency gains and adaptability.',
    'Successful reactivation would represent a resolution of the constraint, transforming it from a Piton back into a Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_for_reactivation, conceptual, 'Assessing the feasibility of re-integrating AI to restore function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(atrophied_optimization_piton, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atro_tr_t0, atrophied_optimization_piton, theater_ratio, 0, 0.1).
narrative_ontology:measurement(atro_tr_t5, atrophied_optimization_piton, theater_ratio, 5, 0.4).
narrative_ontology:measurement(atro_tr_t10, atrophied_optimization_piton, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(atro_be_t0, atrophied_optimization_piton, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(atro_be_t5, atrophied_optimization_piton, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(atro_be_t10, atrophied_optimization_piton, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(atrophied_optimization_piton, resource_allocation).
narrative_ontology:affects_constraint(atrophied_optimization_piton, kpi_driven_management).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
