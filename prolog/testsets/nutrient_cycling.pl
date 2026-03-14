% ============================================================================
% CONSTRAINT STORY: nutrient_cycling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nutrient_cycling, []).

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
 *   constraint_id: nutrient_cycling
 *   human_readable: Nutrient Cycling Constraint
 *   domain: ecology/biogeochemistry
 *
 * SUMMARY:
 *   Nutrient cycling is the biogeochemical process by which essential
 *   elements (nitrogen, phosphorus, potassium, carbon, sulfur, calcium,
 *   magnesium) cycle between the biotic and abiotic components of ecosystems.
 *   No organism, population, or human system can exit this constraint. The
 *   biochemical requirement for nutrient uptake, metabolic processing, and
 *   decomposition/return is immutable — it is a natural law of biochemistry
 *   rather than a social or institutional arrangement. The constraint
 *   exhibits zero extractiveness in the Deferential Realism sense because
 *   there is no agent who benefits at another's expense through the cycling
 *   mechanism itself. Rather, the cycling is a universal requirement that all
 *   agents must satisfy. The theater ratio is minimal because the cycling
 *   process has no performative component — it either occurs (via microbial
 *   decomposition, plant uptake, animal metabolism) or the system fails. No
 *   organizational layer mediates or simulates nutrient cycling.
 *
 * KEY AGENTS:
 *   - Organisms: Universally subject (powerless) — all living things require and depend on nutrient cycling with no exit option
 *   - Agricultural Systems: Institutionally dependent (institutional) — must manage nutrient flows but cannot escape the constraint, only negotiate terms through input sourcing
 *   - Soil Microbiota: Primary functional agent (analytical) — drives decomposition and nutrient availability; operates without agency but is the mechanism that makes cycling possible
 *   - Analytical Observer: Civilizational perspective (analytical) — recognizes nutrient cycling as a natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nutrient_cycling, 0.08).
domain_priors:suppression_score(nutrient_cycling, 0.03).
domain_priors:theater_ratio(nutrient_cycling, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nutrient_cycling, extractiveness, 0.08).
narrative_ontology:constraint_metric(nutrient_cycling, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(nutrient_cycling, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nutrient_cycling, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nutrient_cycling, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nutrient_cycling, mountain).
narrative_ontology:human_readable(nutrient_cycling, "Nutrient Cycling Constraint").
narrative_ontology:topic_domain(nutrient_cycling, "ecology/biogeochemistry").

domain_priors:emerges_naturally(nutrient_cycling).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An individual organism cannot exit nutrient cycling — the constraint is immutable from any biographical or shorter timescale. Phosphorus, nitrogen, carbon must cycle through metabolic processes. No organism can choose not to require these elements.
constraint_indexing:constraint_classification(nutrient_cycling, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% Even the most advanced agricultural technology cannot escape nutrient cycling constraints. Phosphorus mining depletes finite reserves; nitrogen fixation requires energy; carbon balance determines productivity. The constraint is immutable across all technological timescales.
constraint_indexing:constraint_classification(nutrient_cycling, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a universal/civilizational perspective, nutrient cycling is a natural law of biochemistry and ecology. The stoichiometric requirements for life, the thermodynamic constraints on energy transfer, and the finite abundance of elements are immutable properties of the physical world.
constraint_indexing:constraint_classification(nutrient_cycling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nutrient_cycling_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nutrient_cycling, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nutrient_cycling, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nutrient_cycling, ExtMetricName, E),
    domain_priors:suppression_score(nutrient_cycling, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nutrient_cycling),
    narrative_ontology:constraint_metric(nutrient_cycling, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nutrient_cycling, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nutrient_cycling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint is purely functional — it distributes no benefit to beneficiaries or cost to victims because cycling is a universal requirement. The minimal non-zero value reflects the thermodynamic cost of nutrient mobilization (energy required for decomposition, nutrient transport, metabolic processing) which is inherent to the process, not extractive overhead. Suppression (0.03): Very low. Nutrient cycling operates automatically through ecological processes without requiring coercion or alternatives suppression. Organisms cannot choose not to participate, but this is immutability of natural law, not suppression through coercion. Theater ratio (0.15): Very low. The cycling process is functional and transparent — decomposition, plant uptake, and animal metabolism either occur or don't. Modern soil management and crop rotation practices are primarily functional rather than performative, though there exists a small performative layer in 'regenerative agriculture' marketing that may exceed actual nutrient cycling improvement.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify nutrient cycling as mountain with minimal gap. The immutability is universal across power levels, time horizons, and exit options. The analytical observer's perspective is identical to the powerless organism's perspective — the constraint is equally immutable whether viewed from the cellular or civilizational level. This uniformity across perspectives is diagnostic of a true natural law constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Nutrient cycling has no inherent directionality — no agent benefits from the constraint while another bears costs. The constraint is symmetric across all agents: all require nutrient uptake; all generate waste that must be decomposed. The derivation of d from beneficiary/victim declarations does not apply because nutrient cycling has neither beneficiaries nor victims in the extraction sense. The constraint is a universal requirement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anthropogenic_disruption_threshold,
    'At what scale of anthropogenic perturbation does nutrient cycling transition from natural constraint to managed system?',
    'Historical analysis of pre-industrial vs industrial-era nitrogen and phosphorus cycles; comparison of natural cycling rates to anthropogenic flux magnitudes',
    'If threshold is exceeded: nutrient cycling becomes a tangled_rope (mixed coordination and extraction). If threshold remains beyond current human activity: mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(anthropogenic_disruption_threshold, empirical, 'Whether human activity has transformed nutrient cycling into a managed extraction system').

omega_variable(
    regeneration_vs_depletion_dominance,
    'Do regenerative practices (cover crops, rotational grazing, compost cycling) meaningfully restore nutrient stocks, or does depletion from extraction remain the dominant flow?',
    'Long-term field studies measuring soil nutrient content, crop yields, and carbon sequestration under regenerative vs extractive management across climate zones',
    'If regenerative practices achieve true renewal: nutrient cycling persists as a mountain with negotiable extraction rates. If depletion dominates: the constraint becomes tangled_rope with unsustainable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regeneration_vs_depletion_dominance, empirical, 'Whether nutrient cycling can be sustained through regenerative practices').

omega_variable(
    finite_phosphorus_peak,
    'What is the remaining timescale for economically recoverable phosphorus reserves, and does this represent an immutable hard constraint or a remediable resource problem?',
    'Mining cost curves, reserve estimates, secondary phosphorus recovery rates, and substitution technology development; comparison to historical resource depletion curves',
    'If peak phosphorus is imminent (< 50 years): nutrient cycling transitions from mountain to snare for agricultural extraction. If recovery/substitution is viable: mountain classification persists with different cost structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finite_phosphorus_peak, empirical, 'Whether phosphorus depletion creates an immutable extraction constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nutrient_cycling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nutcycle_tr_t0, nutrient_cycling, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nutcycle_tr_t5, nutrient_cycling, theater_ratio, 5, 0.14).
narrative_ontology:measurement(nutcycle_tr_t10, nutrient_cycling, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(nutcycle_be_t0, nutrient_cycling, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(nutcycle_be_t5, nutrient_cycling, base_extractiveness, 5, 0.075).
narrative_ontology:measurement(nutcycle_be_t10, nutrient_cycling, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nutrient_cycling, global_infrastructure).
narrative_ontology:affects_constraint(nutrient_cycling, agricultural_productivity).
narrative_ontology:affects_constraint(nutrient_cycling, soil_carbon_sequestration).
narrative_ontology:affects_constraint(nutrient_cycling, freshwater_eutrophication).
narrative_ontology:affects_constraint(nutrient_cycling, phosphorus_peak).

% DUAL FORMULATION NOTE:
% Nutrient cycling is a foundational constraint that upstream of multiple domain-specific constraints: agricultural productivity depends on nitrogen availability; soil carbon sequestration depends on decomposition rates; freshwater eutrophication is driven by anthropogenic nitrogen and phosphorus cycling disruption; phosphorus peak depletion is a derivative constraint on the finite phosphorus cycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
