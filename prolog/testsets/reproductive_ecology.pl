% ============================================================================
% CONSTRAINT STORY: reproductive_ecology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reproductive_ecology, []).

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
 *   constraint_id: reproductive_ecology
 *   human_readable: Reproductive Ecology: Life History Trade-offs and Resource Allocation
 *   domain: evolutionary_biology/ecology
 *
 * SUMMARY:
 *   Reproductive ecology describes the set of constraints organisms face when
 *   allocating finite resources among reproduction, growth, and survival. The
 *   core constraint is that every unit of energy or material invested in
 *   current reproduction cannot be allocated to somatic maintenance, future
 *   growth, or future reproductive capacity. This constraint emerges from
 *   fundamental bioenergetic principles and the mathematics of resource
 *   allocation under scarcity. It is universal across all sexually
 *   reproducing organisms and has remained invariant since the origin of
 *   multicellular life. The constraint is not enforced by any external agent;
 *   it arises from the physical and thermodynamic limits of biological
 *   systems. No organism has ever evolved the capacity to exceed its energy
 *   budget or to allocate the same resource to multiple competing demands
 *   simultaneously. The constraint is immutable: it cannot be eliminated by
 *   natural selection, technological intervention, or environmental change.
 *   It can only be navigated — evolution tunes the allocation ratio based on
 *   ecological context, but the underlying trade-off persists.
 *
 * KEY AGENTS:
 *   - Individual Organism: Powerless structural agent (trapped by bioenergetic limits) — must allocate finite energy across competing demands with no escape mechanism
 *   - Population Under Selection: Moderate structural agent (constrained by demographic mathematics) — evolution optimizes life history allocation but cannot eliminate the trade-off
 *   - Evolutionary Lineage: Analytical observer (civilizational scope) — sees the constraint as a fundamental law of biology that applies universally and unchangeably
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reproductive_ecology, 0.18).
domain_priors:suppression_score(reproductive_ecology, 0.03).
domain_priors:theater_ratio(reproductive_ecology, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reproductive_ecology, extractiveness, 0.18).
narrative_ontology:constraint_metric(reproductive_ecology, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(reproductive_ecology, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reproductive_ecology, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reproductive_ecology, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reproductive_ecology, mountain).
narrative_ontology:human_readable(reproductive_ecology, "Reproductive Ecology: Life History Trade-offs and Resource Allocation").
narrative_ontology:topic_domain(reproductive_ecology, "evolutionary_biology/ecology").

domain_priors:emerges_naturally(reproductive_ecology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANISM'S ENERGY BUDGET (MOUNTAIN) — The organism is constrained by fundamental thermodynamic limits on energy allocation. Every unit invested in reproduction is unavailable for somatic growth or maintenance. This trade-off is immutable: no organism can simultaneously maximize reproduction, growth, and longevity given finite energy uptake. The constraint emerges from basic bioenergetics and applies universally across taxa.
constraint_indexing:constraint_classification(reproductive_ecology, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: POPULATION UNDER SELECTION (MOUNTAIN) — From a population perspective, life history evolution is constrained by the cost of reproduction. Increased reproductive effort reduces survival or growth, directly reducing future reproductive potential. This trade-off cannot be escaped — it is a structural consequence of finite resources and the mathematics of population growth. Selection cannot eliminate the trade-off; it can only optimize allocation across it.
constraint_indexing:constraint_classification(reproductive_ecology, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — The reproductive ecology constraint is a fundamental law of biology, not a contingent institutional arrangement. It follows from the second law of thermodynamics, the finiteness of resources, and the mathematics of life history evolution. The constraint exhibits zero degrees of freedom: no evolutionary trajectory, no environmental condition, no technological intervention can eliminate the core trade-off between reproduction and other fitness components. This is a natural law of biology.
constraint_indexing:constraint_classification(reproductive_ecology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reproductive_ecology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reproductive_ecology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reproductive_ecology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reproductive_ecology, ExtMetricName, E),
    domain_priors:suppression_score(reproductive_ecology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reproductive_ecology),
    narrative_ontology:constraint_metric(reproductive_ecology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reproductive_ecology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reproductive_ecology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The reproductive ecology constraint does not extract value from any agent to benefit another — it is a fundamental limit on resource allocation, not a mechanism for asymmetric gain. The value is above zero only because the constraint imposes costs (energy spent on reproduction cannot be spent on growth), and these costs are 'extracted' by the constraint itself (the thermodynamic limit) rather than by any external actor. Base extraction is low because there is no parasitic agent receiving the extracted value. Suppression (0.03): Minimal. The constraint operates through transparent physical limits, not through coercion or obscured alternatives. Organisms cannot exit the constraint because no alternatives exist, but this is not suppression in the sense of blocked information or artificially restricted options — it is the absence of alternatives as a structural fact. Theater ratio (0.15): Minimal. The constraint is functionally transparent — life history evolution responds directly to the trade-off with no performative component. Fitness consequences of allocation decisions are immediately observable (in reproduction, growth, or survival), not masked by ritual or institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives converge on the Mountain classification. The organism experiences the constraint as immutable resource limits. The population experiences it as inescapable selection pressure. The analytical observer sees it as a natural law. There is no perspectival gap here — the constraint is invariant across observation positions. This is the defining property of a true Mountain: it appears the same from all positions because it is genuinely universal and unchanging.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation is needed for this constraint because there are no beneficiaries or victims. The reproductive ecology constraint does not involve extraction from one agent to benefit another. It is a fundamental limitation on all agents equally, albeit with different consequences depending on ecological context. The constraint is not enforced by power; it is enforced by physics and thermodynamics. Every organism is equally 'constrained' — there is no asymmetry of position that would generate a directionality value. This absence of directionality is itself diagnostic of a true Mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents no mandatrophy because it is not candidates for misclassification. It does not exhibit coordination function that might be mistaken for extraction (no agent benefits disproportionately). It does not involve enforcement that might be mistaken for natural law (it is genuinely natural, not contingently enforced). The constraint is stably classified as Mountain from all perspectives because it is a natural law, not a contingent institutional arrangement. The analytical observer is not at risk of naturalizing a contingent mechanism — the mechanism IS natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phenotypic_plasticity_scope,
    'Do organisms with high phenotypic plasticity actually escape the fundamental trade-off, or do they merely shift its expression?',
    'Experimental analysis of plastic life history responses under resource limitation; measurement of actual energy allocation across conditions',
    'If truly escaped: classification should be Rope (coordination with environment). If shifted: classification remains Mountain (trade-off persists, only its form changes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phenotypic_plasticity_scope, empirical, 'Whether phenotypic plasticity constitutes escape from reproductive ecology trade-offs').

omega_variable(
    bet_hedging_universality,
    'Do bet-hedging strategies (variable reproductive effort across time) represent a genuine alternative strategy or merely a context-dependent solution to uncertainty that still obeys the fundamental trade-off?',
    'Long-term demographic analysis of bet-hedging populations; comparison of lifetime reproductive success against constant-allocation strategies across environmental regimes',
    'If genuine alternative: some organisms may achieve higher lifetime fitness by violating apparent trade-offs through temporal variation. If context-dependent: the trade-off holds at the lifetime level even if violated in any single season.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bet_hedging_universality, empirical, 'Whether bet-hedging strategies escape or reframe the reproductive ecology constraint').

omega_variable(
    metabolic_level_universality,
    'Does metabolic level (resting metabolic rate) itself represent a fundamental constraint that determines life history trajectory, or is it itself a life history trait subject to the same allocation constraints?',
    'Comparative analysis of metabolic rates and life history variation across clades; experimental manipulation of metabolic demand and measurement of life history evolution',
    'If metabolic rate is fundamental: it creates a meta-constraint that some organisms cannot escape (affects classification interpretability). If it is itself a trait: the trade-off operates at a deeper level but the meta-constraint dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metabolic_level_universality, conceptual, 'Whether metabolic rate is a fundamental constraint or a derived life history trait').

omega_variable(
    technological_transcendence,
    'Could human technological intervention — environmental control, energy subsidies, genetic modification — allow organisms to escape the fundamental reproductive ecology trade-off?',
    'Analysis of domesticated and agriculturally managed organisms; measurement of reproductive traits under unlimited resource conditions; synthetic biology experiments testing resource allocation under artificial energy abundance',
    'If transcendence possible: the constraint is contingent on energy scarcity (demotes to Rope or Tangled Rope). If impossible: the constraint remains Mountain even under artificial conditions (reflects fundamental biological organization).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_transcendence, preference, 'Whether the reproductive ecology constraint can be technologically transcended').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reproductive_ecology, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repro_ecol_tr_t0, reproductive_ecology, theater_ratio, 0, 0.15).
narrative_ontology:measurement(repro_ecol_tr_t1000, reproductive_ecology, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(repro_ecol_tr_t2000, reproductive_ecology, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(repro_ecol_be_t0, reproductive_ecology, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(repro_ecol_be_t1000, reproductive_ecology, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement(repro_ecol_be_t2000, reproductive_ecology, base_extractiveness, 2000, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reproductive_ecology, information_standard).
narrative_ontology:affects_constraint(reproductive_ecology, sexual_selection_dynamics).
narrative_ontology:affects_constraint(reproductive_ecology, parental_care_evolution).
narrative_ontology:affects_constraint(reproductive_ecology, senescence_evolution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
