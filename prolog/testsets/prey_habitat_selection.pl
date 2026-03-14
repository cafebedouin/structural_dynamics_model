% ============================================================================
% CONSTRAINT STORY: prey_habitat_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prey_habitat_selection, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: prey_habitat_selection
 *   human_readable: Prey Habitat Selection as Natural Law
 *   domain: evolutionary_biology/behavioral_ecology
 *
 * SUMMARY:
 *   Prey habitat selection is the behavioral and evolutionary constraint
 *   whereby prey organisms choose to occupy habitats that maximize survival
 *   probability and reproductive success subject to trade-offs between
 *   resource availability, predation risk, and energetic cost. This
 *   constraint emerges from fundamental principles of natural selection and
 *   is invariant across species, ecosystems, and timescales. The constraint
 *   manifests as an immutable law of behavioral ecology: individuals that
 *   select suboptimal habitats experience reduced fitness, and populations
 *   that fail to match habitat use to habitat quality face local extinction.
 *   No agent can 'exit' this constraint — it is a structural feature of how
 *   organisms interact with their environment, not a negotiable social
 *   arrangement. The mountainous character derives from the absence of any
 *   alternative pathway: organisms cannot survive while systematically
 *   ignoring fitness-relevant environmental variation.
 *
 * KEY AGENTS:
 *   - Individual Prey Organism: The unit experiencing the constraint (powerless/trapped) — must select a habitat or face starvation or predation
 *   - Prey Population: The level at which the constraint operates across generations (organized/constrained) — populations converge toward optimal habitat use or become extinct
 *   - Evolutionary Fitness Function: The mechanism enforcing the constraint (analytical/analytical) — the invariant trade-off structure between energy intake, predation risk, and cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prey_habitat_selection, 0.12).
domain_priors:suppression_score(prey_habitat_selection, 0.03).
domain_priors:theater_ratio(prey_habitat_selection, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prey_habitat_selection, extractiveness, 0.12).
narrative_ontology:constraint_metric(prey_habitat_selection, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(prey_habitat_selection, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prey_habitat_selection, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(prey_habitat_selection, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prey_habitat_selection, mountain).
narrative_ontology:human_readable(prey_habitat_selection, "Prey Habitat Selection as Natural Law").
narrative_ontology:topic_domain(prey_habitat_selection, "evolutionary_biology/behavioral_ecology").

domain_priors:emerges_naturally(prey_habitat_selection).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PREY INDIVIDUAL (MOUNTAIN) — Individual prey organisms have zero degrees of freedom in habitat selection once environmental fitness functions are fixed. The constraint emerges from the physics of energy intake, predation risk, and metabolic cost. No alternative pathway exists that trades survival probability against habitat choice — the organism either selects the optimal habitat or faces fitness penalties converging to zero across evolutionary time.
constraint_indexing:constraint_classification(prey_habitat_selection, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EVOLUTIONARY ANALYST (MOUNTAIN) — From the analytical perspective examining civilizational timescales, prey habitat selection is an invariant property of selection dynamics. The constraint arises from fundamental trade-offs between resource availability, predation risk, and energy expenditure. These trade-offs are structural features of how populations evolve, not contingent institutional arrangements. The law holds across all ecosystems, taxa, and environmental conditions because it reflects underlying physical and biological constraints.
constraint_indexing:constraint_classification(prey_habitat_selection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: POPULATION-LEVEL VIEW (MOUNTAIN) — At the generational timescale across a regional population, habitat selection constraint remains mountain-class. Individual variation in selection behavior exists, but the constraint operates through differential survival and reproduction — populations converge toward optimal habitat use or face local extinction. The mechanism is immutable across generations because it reflects invariant fitness trade-offs, not changing environmental conditions or preferences.
constraint_indexing:constraint_classification(prey_habitat_selection, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prey_habitat_selection_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prey_habitat_selection, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prey_habitat_selection, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prey_habitat_selection, ExtMetricName, E),
    domain_priors:suppression_score(prey_habitat_selection, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prey_habitat_selection),
    narrative_ontology:constraint_metric(prey_habitat_selection, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prey_habitat_selection, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prey_habitat_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint extracts nothing from prey organisms — it does not deprive them of resources, impose coercive barriers, or create asymmetric benefit flows. Rather, the constraint reflects the invariant structure of survival: organisms that make better habitat choices survive and reproduce more; those making worse choices die or leave fewer offspring. The 'extraction' is zero because the mechanism operates through differential survival, not through redistribution. Suppression (0.03): Minimal. Prey organisms face no artificial suppression of alternatives — they face only the objective consequences of their choices via predation, starvation, or reproductive failure. The suppression is a fact of ecology, not a social artifact. Theater ratio (0.08): Essentially zero. Habitat selection is a pure functional mechanism with minimal performative component. Organisms select habitats based on fitness consequences, not on ritual or signaling. The small non-zero value reflects measurement uncertainty and the possibility that some habitat choice behaviors have minor ceremonial components (territory displays, for instance), but the primary mechanism is purely functional.
 *
 * PERSPECTIVAL GAP:
 *   There is no meaningful perspectival gap in this constraint. All perspectives converge on mountain classification because the constraint is observer-independent. The prey individual experiences it as immutable law (trap). The population experiences it as the selection gradient (mountain across generations). The analyst sees it as a fundamental principle of evolution (universal law). No stakeholder group benefits asymmetrically; no coalition opposes the constraint; no alternative framing renders it contingent. This is the diagnostic signature of a true mountain: unanimous classification across all indexical positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not require mandatrophy resolution. Prey habitat selection is not misclassified as pure extraction (snare) because there is no extraction — the mechanism operates entirely through fitness consequences to the organism. It is not misclassified as coordination (rope) because there is no coordination problem — each individual organism independently solves the habitat selection optimization problem. The constraint's mountain status is robust across all perspectives and all measurement approaches because it reflects invariant physical and biological trade-offs, not contingent social or institutional arrangements. No observer position reveals hidden coordination or extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    habitat_optimality_definition,
    'What constitutes ''optimal'' habitat selection when energy intake, predation risk, and territorial costs trade off nonlinearly?',
    'Empirical fitness measurement across gradient of habitat choices; calculation of expected lifetime reproductive success for each habitat type; comparison to observed selection patterns',
    'If observable habitat choice matches predicted optimal habitat: mountain classification confirmed. If systematic divergence exists: suggests additional constraints (behavioral inertia, information limitation, or physiological constraint) that should be modeled as separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(habitat_optimality_definition, empirical, 'Definition of habitat optimality under nonlinear trade-offs').

omega_variable(
    phenotypic_plasticity_bounds,
    'Are constraints on habitat selection developmental/physiological (hard constraints) or learned/behavioral (soft constraints with plasticity)?',
    'Cross-species comparison of habitat selection flexibility; experimental manipulation of early developmental conditions; measurement of within-individual habitat switching rates',
    'If primarily hard constraints: mountain classification strengthened (immutable across organism lifetime). If primarily soft constraints: reclassify to rope (coordination problem between state and environment) or scaffold (plasticity provides adaptation mechanism with potential sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phenotypic_plasticity_bounds, empirical, 'Whether habitat selection constraints are physiological or behavioral').

omega_variable(
    environment_stationarity,
    'Over what timescale does the constraint remain invariant? Do climate change, landscape alteration, or invasive competitor dynamics render optimal habitat patterns non-stationary?',
    'Long-term comparative studies of habitat selection in stable vs rapidly changing environments; modeling of evolutionary lag when environmental conditions shift faster than selection can track',
    'If environment is stationary on generational timescale: mountain persists. If environment changes rapidly relative to prey generation time: constraint becomes non-stationary, effectively a scaffold (prey habitat selection with contingent sunset) or tangled rope (coordination with environmental change creates extraction lag).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environment_stationarity, empirical, 'Stationarity of environment and invariance of habitat selection constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prey_habitat_selection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prey_tr_t0, prey_habitat_selection, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prey_tr_t50, prey_habitat_selection, theater_ratio, 50, 0.08).
narrative_ontology:measurement(prey_tr_t100, prey_habitat_selection, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(prey_be_t0, prey_habitat_selection, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(prey_be_t50, prey_habitat_selection, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(prey_be_t100, prey_habitat_selection, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prey_habitat_selection, resource_allocation).
narrative_ontology:affects_constraint(prey_habitat_selection, predator_prey_dynamics).
narrative_ontology:affects_constraint(prey_habitat_selection, carrying_capacity_limit).

% DUAL FORMULATION NOTE:
% Prey habitat selection is downstream of predator distribution and resource spatial patterns. It is upstream of population density, carrying capacity dynamics, and community assembly. The constraint is part of a larger ecological constraint network but is itself immutable and invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
