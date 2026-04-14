% ============================================================================
% CONSTRAINT STORY: trophic_cascade_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trophic_cascade_regulation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trophic_cascade_regulation
 *   human_readable: Trophic Cascade Regulation in Ecosystems
 *   domain: ecology/biology
 *
 * SUMMARY:
 *   Trophic cascade regulation describes the propagation of population
 *   effects across trophic levels in ecosystems, whereby changes in apex
 *   predator abundance produce outsized effects on herbivore populations and
 *   primary productivity. This constraint exemplifies a pure mountain
 *   classification: it emerges from thermodynamic limits on energy transfer
 *   (~10% efficiency between trophic levels) and structural population
 *   dynamics that no agent can negotiate, escape, or circumvent through
 *   institutional or behavioral means. The constraint is invariant across all
 *   spatial scopes and observer positions. Whether studied in kelp forests
 *   off the North American coast, wolf-elk systems in Yellowstone, or
 *   theoretical ecological models, trophic cascades operate identically: apex
 *   predators regulate herbivore numbers through predation mortality;
 *   herbivores regulate vegetation through consumption; vegetation regulates
 *   soil stability and nutrient cycling. The mechanism cannot be bargained
 *   with, reformed, or avoided through alternative coordination.
 *   Extractiveness is minimal (0.18) because the constraint generates no
 *   asymmetric benefit extraction — all agents are bound equally by the same
 *   bioenergetic laws. Suppression is negligible (0.03) because the
 *   constraint is not enforced through coercion but through thermodynamic
 *   necessity.
 *
 * KEY AGENTS:
 *   - Apex Predators: Structural position (institutional/arbitrage) — regulate cascade by controlling herbivore numbers; constrained by available prey biomass
 *   - Herbivore Populations: Structural position (powerless/trapped) — bear population density consequences; cannot escape through exit options
 *   - Primary Producers: Structural position (moderate/constrained) — respond to herbivore grazing pressure; limited by available resources
 *   - Ecosystem Productivity: Structural position (powerless/trapped) — determined entirely by nutrient cycling and primary production; no agency
 *   - Analytical Observer: Structural position (analytical/analytical) — observes invariant mechanism across all contexts; verifies mountain classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trophic_cascade_regulation, 0.18).
domain_priors:suppression_score(trophic_cascade_regulation, 0.03).
domain_priors:theater_ratio(trophic_cascade_regulation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trophic_cascade_regulation, extractiveness, 0.18).
narrative_ontology:constraint_metric(trophic_cascade_regulation, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(trophic_cascade_regulation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trophic_cascade_regulation, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(trophic_cascade_regulation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trophic_cascade_regulation, mountain).
narrative_ontology:human_readable(trophic_cascade_regulation, "Trophic Cascade Regulation in Ecosystems").
narrative_ontology:topic_domain(trophic_cascade_regulation, "ecology/biology").

domain_priors:emerges_naturally(trophic_cascade_regulation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HERBIVORE POPULATION (MOUNTAIN) — Trophic cascades operate through structural energetic limits that cannot be circumvented. Herbivores cannot escape predation effects through organizational or institutional means; the cascade is a physical constraint on biomass availability, not a negotiable arrangement.
constraint_indexing:constraint_classification(trophic_cascade_regulation, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PREDATOR POPULATION (MOUNTAIN) — Predators cannot escape the constraint that their biomass depends on prey population density through energetic efficiency laws. The trophic level constraint is enforced by thermodynamic limits (10% energy transfer rule), not by institutional enforcement.
constraint_indexing:constraint_classification(trophic_cascade_regulation, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / ENERGETIC LAW (MOUNTAIN) — From a universal perspective, trophic cascades are an irreducible consequence of thermodynamic limits on energy transfer between trophic levels and the structural dependency of population dynamics on resource availability. No observer position can escape this constraint; it is invariant across all ecological contexts.
constraint_indexing:constraint_classification(trophic_cascade_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: CONSERVATION AUTHORITY (MOUNTAIN) — Even institutional actors attempting to manage trophic systems cannot circumvent the cascade mechanism. Removing apex predators reliably triggers herbivore population explosions and vegetation collapse; this outcome is mechanically inevitable given the system structure, not a policy choice. The constraint is enforced by physics, not by institutional compliance.
constraint_indexing:constraint_classification(trophic_cascade_regulation, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trophic_cascade_regulation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(trophic_cascade_regulation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trophic_cascade_regulation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(trophic_cascade_regulation, ExtMetricName, E),
    domain_priors:suppression_score(trophic_cascade_regulation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(trophic_cascade_regulation),
    narrative_ontology:constraint_metric(trophic_cascade_regulation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(trophic_cascade_regulation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(trophic_cascade_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Minimal. Trophic cascades produce no extractive surplus to one agent at the expense of another — all agents (predator, herbivore, plant, soil) operate under identical thermodynamic constraints. Energy loss to respiration and maintenance (90% per trophic level) is not extraction but physical necessity. Suppression (0.03): Negligible. The cascade mechanism operates through energetic efficiency limits and population dynamics, not through coercion, institutional control, or restriction of alternatives. Herbivores cannot 'escape' predation pressure through behavioral adaptation at scale — predators regulate populations mechanically. Theater ratio (0.12): Extremely low. Trophic cascades have no performative component; the mechanism operates identically in the field and in theoretical models. A kelp forest system with sea otters present shows predictable herbivore suppression and kelp forest recovery; a system without otters shows predictable sea urchin population explosions and kelp collapse. The outcome is determined by structure, not by ritual or institutional maintenance. Accessibility collapse (0.91): Very high. No accessible alternative to the cascade mechanism exists — removing apex predators from an ecosystem invariably triggers herbivore population increase and vegetation suppression. This outcome is not a policy choice or an institutional arrangement but a mechanical consequence of system structure. Resistance (0.08): Very low. The cascade operates across all geographic and temporal scales; every ecosystem exhibits trophic regulation proportional to its initial predator-prey structure. No resistance to the mechanism has been documented; all observations are consistent with the constraint.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because the constraint is invariant across all observational positions. Every perspective classifies as mountain because every agent (predator, herbivore, plant, soil, conservation authority, analytical observer) experiences the same structural necessity: trophic cascades are inevitable given the system's energetic architecture. The constraint does not appear extractive from any position because it redistributes no surplus to any beneficiary. It does not appear as coordination (Rope) because no cooperation is required — the cascade operates mechanically through predation and consumption. It does not appear as temporary (Scaffold) because the mechanism persists indefinitely as long as the ecosystem exists. There is no false summit risk: the mountain classification is robust across all measurement methodologies and spatial scales.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is not meaningfully computed for mountains — all agents experience the constraint identically as structural necessity. The trophic cascade is not an extraction mechanism but an energetic law. No agent benefits at another's expense; all agents are constrained by thermodynamic limits. The apparent 'imbalance' (predators controlling herbivores) is not asymmetric extraction but structural regulation through predation as the primary population control mechanism. All agents experience suppression equally: predators are suppressed by prey availability, herbivores by predator mortality and resource competition, plants by herbivore grazing. The constraint is symmetric across all observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trophic_cascade_regulation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trophic_cascade_regulation, information_standard).
narrative_ontology:affects_constraint(trophic_cascade_regulation, predator_prey_population_dynamics).
narrative_ontology:affects_constraint(trophic_cascade_regulation, ecosystem_carrying_capacity).
narrative_ontology:affects_constraint(trophic_cascade_regulation, vegetation_succession_cycles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
