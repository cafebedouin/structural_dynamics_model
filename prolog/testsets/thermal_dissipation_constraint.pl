% ============================================================================
% CONSTRAINT STORY: thermal_dissipation_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thermal_dissipation_constraint, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: thermal_dissipation_constraint
 *   human_readable: Thermal Dissipation Constraint in Schottky Diode Rectification
 *   domain: electrical_engineering/power_systems/off_grid_infrastructure
 *
 * SUMMARY:
 *   The thermal dissipation constraint in Schottky diode rectification is a
 *   direct consequence of semiconductor physics: forward voltage drop
 *   (0.5-0.6V for Schottky barriers) multiplied by load current produces
 *   continuous heat dissipation that must be removed to prevent junction
 *   temperature rise and device failure. At 13.33A load current, worst-case
 *   dissipation is 7.33W, requiring thermal sandwich assembly
 *   (diode-to-heatsink mounting with thermal interface material) to maintain
 *   junction temperature within safe operating limits. This constraint is
 *   downstream of the transfer gap physics (the Schottky barrier height that
 *   determines forward voltage) and represents the thermodynamic cost of
 *   rectification. Unlike institutional or policy constraints, no agent
 *   benefits from this dissipation — it is pure loss, and all parties (system
 *   operators, designers, manufacturers) work to minimize it within the
 *   bounds of physics. The constraint is universal: any rectification
 *   mechanism that passes current through a potential barrier dissipates
 *   power proportional to barrier height and current. Alternative
 *   technologies (SiC, GaN, synchronous rectification) reduce the magnitude
 *   of dissipation but do not eliminate the fundamental relationship P = V_f
 *   × I.
 *
 * KEY AGENTS:
 *   - Off-Grid System Operator: Primary target (powerless/trapped) — must manage thermal dissipation to prevent device failure; no exit from the physics
 *   - Power Electronics Designer: Secondary target (moderate/constrained) — can choose technologies to reduce losses but cannot eliminate the constraint; constrained by cost and component availability
 *   - Semiconductor Manufacturer: No extraction role (institutional/arbitrage) — sells solutions to a problem created by physics, not by the manufacturer; no beneficiary relationship to the constraint
 *   - Analytical Observer: Civilizational view (analytical/analytical) — confirms the constraint as genuine natural law with no institutional or extractive component
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thermal_dissipation_constraint, 0.02).
domain_priors:suppression_score(thermal_dissipation_constraint, 0.01).
domain_priors:theater_ratio(thermal_dissipation_constraint, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermal_dissipation_constraint, extractiveness, 0.02).
narrative_ontology:constraint_metric(thermal_dissipation_constraint, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(thermal_dissipation_constraint, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thermal_dissipation_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thermal_dissipation_constraint, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thermal_dissipation_constraint, mountain).
narrative_ontology:human_readable(thermal_dissipation_constraint, "Thermal Dissipation Constraint in Schottky Diode Rectification").
narrative_ontology:topic_domain(thermal_dissipation_constraint, "electrical_engineering/power_systems/off_grid_infrastructure").

domain_priors:emerges_naturally(thermal_dissipation_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OFF-GRID SYSTEM OPERATOR (MOUNTAIN) — Faces the thermal dissipation constraint as an immutable physical law. Cannot exit the requirement for thermal management when using Schottky diodes at load. The 7.33W dissipation at 13.33A is a direct consequence of semiconductor physics, not a policy choice or institutional arrangement.
constraint_indexing:constraint_classification(thermal_dissipation_constraint, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: POWER ELECTRONICS DESIGNER (MOUNTAIN) — Constrained by component availability and cost, but the thermal dissipation itself is non-negotiable. Can choose different diode technologies (SiC, GaN) or topologies (synchronous rectification) to reduce losses, but cannot eliminate the fundamental relationship P = V_f × I. The constraint is a design parameter, not a design choice.
constraint_indexing:constraint_classification(thermal_dissipation_constraint, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SEMICONDUCTOR MANUFACTURER (MOUNTAIN) — Has arbitrage options across technologies and markets, but the thermal dissipation constraint is a physical limit they work within, not against. Can optimize junction design, package thermal resistance, and material properties, but the forward voltage drop and resulting dissipation follow from band structure and carrier transport physics. No extraction mechanism — the manufacturer sells solutions to a problem they did not create.
constraint_indexing:constraint_classification(thermal_dissipation_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The thermal dissipation constraint is a direct consequence of semiconductor physics: forward voltage drop across a Schottky barrier is determined by metal-semiconductor work function difference and barrier height. P = V_f × I is not a convention, policy, or institutional arrangement — it is a thermodynamic necessity. No alternatives collapse because no alternatives exist: any rectification mechanism that passes current through a potential barrier dissipates power. This is a genuine natural law.
constraint_indexing:constraint_classification(thermal_dissipation_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermal_dissipation_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thermal_dissipation_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermal_dissipation_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thermal_dissipation_constraint, ExtMetricName, E),
    domain_priors:suppression_score(thermal_dissipation_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thermal_dissipation_constraint),
    narrative_ontology:constraint_metric(thermal_dissipation_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thermal_dissipation_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thermal_dissipation_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.02): Near-zero. The thermal dissipation is pure loss — no agent collects from it. The minimal non-zero value reflects only the indirect coordination cost: designers must account for thermal management in system design, which adds complexity. But this is not extraction in the DR sense (asymmetric benefit capture); it is the unavoidable cost of working within physical limits. Suppression (0.01): Near-zero. No coercion or enforcement mechanism. The constraint operates whether or not anyone acknowledges it. Alternatives are not suppressed — they are physically unavailable. The minimal non-zero value reflects only that the constraint forecloses certain design choices (e.g., cannot use bare diode without heatsink at high current), but this is physical foreclosure, not institutional suppression. Theater ratio (0.03): Near-zero. Thermal management is functional, not performative. Heatsink sizing, thermal interface material selection, and junction temperature monitoring are all directly tied to preventing device failure. The minimal non-zero value reflects only that some thermal management practices may be over-conservative (safety margins beyond strict necessity), but this is engineering prudence, not theater. Accessibility collapse (0.92): Very high. Once the physics is understood, alternatives collapse nearly completely. The only 'alternatives' are different rectification technologies (SiC, GaN, synchronous rectification) that reduce the magnitude of dissipation but do not eliminate the constraint. Resistance (0.02): Near-zero. No active resistance to the constraint because it is not a policy or institutional arrangement. The minimal non-zero value reflects only that engineers work to minimize thermal losses through better designs, but this is optimization within the constraint, not resistance to it.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists for this constraint. All four perspectives classify as mountain because the thermal dissipation is a direct consequence of semiconductor physics with no institutional, extractive, or coordinative component. The off-grid operator, the designer, the manufacturer, and the analytical observer all experience the constraint as an immutable physical law. The operator cannot exit it, the designer cannot negotiate it away, the manufacturer cannot profit from it, and the analytical observer confirms it as genuine natural law. This uniform classification across all perspectives is the signature of a true mountain: the constraint would persist regardless of who observes it, who enforces it, or whether anyone benefits from it. The only open question (omega: technology_substitution_boundary) is whether future cost reductions in alternative technologies will shift the constraint from universal (mountain) to contingent (rope: coordination on legacy technology). But as of the current interval, the constraint is mountain from all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents have directionality values near 0.5 (symmetric) because no agent is a beneficiary or victim of the thermal dissipation — it is pure loss. The off-grid operator and designer are targets in the sense that they must manage the dissipation, but they are not victims of extraction because no other agent benefits from their cost. The manufacturer is not a beneficiary because the dissipation is not a source of rent — the manufacturer sells components that minimize losses, not components that create them. The analytical observer is neutral. The engine will derive d ≈ 0.5 for all agents, producing low chi across all perspectives, consistent with the mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mountain end of the mandatrophy spectrum: a genuine natural law with no extractive, coordinative, or institutional component. The mandate (thermal management to prevent device failure) is not separable from the function (safe operation of the rectification circuit) because the mandate is a direct consequence of thermodynamics. There is no gap between the stated purpose and the actual operation, no beneficiary capturing rents, and no enforcement mechanism beyond physics itself. The constraint's mandate has not outlived its function because the mandate IS the function. This is the opposite of a piton (degraded function maintained as performance) or a snare (extraction disguised as coordination). The thermal dissipation constraint is what it claims to be: an unavoidable cost of rectification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_substitution_boundary,
    'At what point does the availability of alternative rectification technologies (SiC, GaN, synchronous rectification) transform this from a universal constraint to a contingent design choice?',
    'Cost-performance crossover analysis: when do alternative technologies become economically accessible for off-grid applications? Track adoption rates and cost curves for wide-bandgap semiconductors in power systems under 1kW.',
    'If alternatives become cost-competitive within 5 years, the constraint shifts from mountain (universal physical limit) to rope (coordination problem: standardizing on legacy technology). If alternatives remain cost-prohibitive for 20+ years, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_boundary, empirical, 'Technology substitution timeline for alternative rectification methods').

omega_variable(
    thermal_management_overhead,
    'Does the thermal management requirement (heatsink, thermal interface material, mounting hardware) constitute a separable extractive layer above the base physical constraint?',
    'Decompose total system cost and complexity into (1) irreducible physical dissipation and (2) thermal management overhead. If overhead exceeds 30% of total thermal subsystem cost, it may warrant a separate constraint story for the thermal management supply chain.',
    'If thermal management overhead is substantial and has identifiable beneficiaries (heatsink manufacturers, thermal interface material suppliers), the constraint family should be decomposed: one mountain story for the base physics, one tangled_rope or rope story for the thermal management ecosystem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thermal_management_overhead, conceptual, 'Whether thermal management overhead warrants decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thermal_dissipation_constraint, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(therm_diss_tr_t0, thermal_dissipation_constraint, theater_ratio, 0, 0.03).
narrative_ontology:measurement(therm_diss_tr_t10, thermal_dissipation_constraint, theater_ratio, 10, 0.03).
narrative_ontology:measurement(therm_diss_tr_t20, thermal_dissipation_constraint, theater_ratio, 20, 0.03).

% Extraction over time
narrative_ontology:measurement(therm_diss_be_t0, thermal_dissipation_constraint, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(therm_diss_be_t10, thermal_dissipation_constraint, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(therm_diss_be_t20, thermal_dissipation_constraint, base_extractiveness, 20, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thermal_dissipation_constraint, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of transfer_gap_physics (the Schottky barrier height that determines forward voltage drop). The upstream constraint is also a mountain (band structure physics), and the downstream constraint (thermal dissipation) inherits the mountain classification. If the thermal management overhead (heatsink supply chain, thermal interface materials) were found to have substantial extractive components, that would warrant a separate constraint story linked via affects_constraints, but the base thermal dissipation itself remains a mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
