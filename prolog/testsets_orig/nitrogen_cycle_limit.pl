% ============================================================================
% CONSTRAINT STORY: nitrogen_cycle_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nitrogen_cycle_limit, []).

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
 *   constraint_id: nitrogen_cycle_limit
 *   human_readable: The Nitrogen Cycle Limit
 *   domain: biogeochemistry/planetary_boundaries
 *
 * SUMMARY:
 *   The nitrogen cycle limit is a planetary boundary constraint rooted in the
 *   biogeochemical cycles that govern nutrient availability on Earth. Natural
 *   nitrogen fixation — the enzymatic reduction of atmospheric N₂ to
 *   bioavailable ammonia and nitrate — is the exclusive gateway through which
 *   inert atmospheric nitrogen enters the biosphere. This process is
 *   catalyzed by nitrogenase enzymes in nitrogen-fixing bacteria and archaea,
 *   and the rate of fixation is constrained by enzyme kinetics, energy
 *   availability, and the finite population of microorganisms capable of
 *   catalyzing the reaction. For millennia, global agriculture was bounded by
 *   the rate of natural nitrogen cycling. The development of the Haber-Bosch
 *   process (industrial nitrogen fixation) in the early 20th century appeared
 *   to overcome this boundary by allowing synthetic production of reactive
 *   nitrogen at rates matching or exceeding natural fixation. However, this
 *   expansion revealed a coupled constraint: while industrial nitrogen
 *   fixation increased agricultural nitrogen availability, it simultaneously
 *   increased eutrophication of aquatic ecosystems, contamination of
 *   groundwater, accumulation of nitrous oxide in the atmosphere, and
 *   long-term soil degradation. The nitrogen cycle limit persists — not as a
 *   hard ceiling on nitrogen supply, but as a biogeochemical carrying
 *   capacity constraint: the planetary system has a maximum rate at which it
 *   can process reactive nitrogen without triggering cascading ecological
 *   dysfunction.
 *
 * KEY AGENTS:
 *   - Agricultural Communities: Primary victims (powerless/trapped) — face nitrogen limitation without ability to overcome constraint through local action; constrained by natural fixation rates and soil microbial capacity.
 *   - Global Agricultural System: Primary beneficiary (institutional/arbitrage) — benefits from industrial nitrogen fixation breakthrough; experiences the constraint as a coordination and optimization problem rather than immutable limit.
 *   - Soil Microbial Communities: Structural actor (analytical/analytical) — nitrogen-fixing bacteria and their enzyme systems are the physical mechanism that enforces the constraint; their metabolic capabilities and population dynamics determine the constraint's rigidity.
 *   - Biogeochemical System: Constraint origin (analytical/analytical) — the planet's capacity to cycle nitrogen without exceeding carrying capacity is the immutable property; all human agents are embedded within this system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nitrogen_cycle_limit, 0.18).
domain_priors:suppression_score(nitrogen_cycle_limit, 0.03).
domain_priors:theater_ratio(nitrogen_cycle_limit, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nitrogen_cycle_limit, extractiveness, 0.18).
narrative_ontology:constraint_metric(nitrogen_cycle_limit, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(nitrogen_cycle_limit, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nitrogen_cycle_limit, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nitrogen_cycle_limit, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nitrogen_cycle_limit, mountain).
narrative_ontology:human_readable(nitrogen_cycle_limit, "The Nitrogen Cycle Limit").
narrative_ontology:topic_domain(nitrogen_cycle_limit, "biogeochemistry/planetary_boundaries").

domain_priors:emerges_naturally(nitrogen_cycle_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGRICULTURAL COMMUNITIES (MOUNTAIN) — Face the nitrogen cycle limit as an immutable physical constraint. Soil nitrogen availability is determined by nitrogen fixation rates, mineralization kinetics, and denitrification losses — none of which are negotiable. Soil microbial communities cannot be forced to fix nitrogen at rates exceeding their thermodynamic capability. The constraint appears unchangeable at the scale of individual farms or even regions.
constraint_indexing:constraint_classification(nitrogen_cycle_limit, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL AGRICULTURAL SYSTEM (MOUNTAIN) — Even with industrial nitrogen fixation (Haber-Bosch process), the constraint manifests as a coupled biogeochemical limit: increasing reactive nitrogen production increases eutrophication, groundwater contamination, and atmospheric N₂O emissions. The Haber-Bosch solution trades one constraint (soil nitrogen deficiency) for another (nitrogen cycle disruption). The underlying limit — the planet's capacity to process reactive nitrogen without system-level dysfunction — remains immutable.
constraint_indexing:constraint_classification(nitrogen_cycle_limit, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a biogeochemical perspective, the nitrogen cycle limit is a natural law: the rate at which atmospheric N₂ can be reduced to bioavailable forms is constrained by enzyme kinetics (nitrogenase), thermodynamics (energy cost of N-N bond breaking), and the finite number of nitrogen-fixing microorganisms on Earth. These constraints are not contingent on human policy, technology, or economic incentives — they are properties of chemistry and microbial ecology.
constraint_indexing:constraint_classification(nitrogen_cycle_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nitrogen_cycle_limit_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nitrogen_cycle_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nitrogen_cycle_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nitrogen_cycle_limit, ExtMetricName, E),
    domain_priors:suppression_score(nitrogen_cycle_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nitrogen_cycle_limit),
    narrative_ontology:constraint_metric(nitrogen_cycle_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nitrogen_cycle_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nitrogen_cycle_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The nitrogen cycle limit does not extract value from anyone — it is a physical/chemical boundary condition. The low extractiveness reflects that the constraint is not an asymmetric transfer mechanism but rather a boundary on total available flux. No agent benefits disproportionately from the limit itself; the limit is equally constraining to all. Suppression (0.03): Minimal. The constraint permits multiple response strategies: selective breeding for nitrogen-use efficiency, legume intercropping, precision fertilization, symbiotic management of nitrogen-fixing microorganisms, and industrial nitrogen fixation. Suppression is low because the constraint leaves significant agency for adaptation. Theater ratio (0.05): Negligible. The nitrogen cycle limit is not performed or theater-dependent; it is a quantifiable biogeochemical property. Accessibility collapse (0.92): Very high. Understanding nitrogen cycle limits requires access to soil microbiology, biogeochemistry, and planetary-scale data — not accessible to most individual actors. The constraint's mechanism is opaque at the local scale; its effects are visible but its causes are hidden in soil processes. Resistance (0.08): Very low. The constraint cannot be resisted, negotiated, or appealed. It manifests through the laws of chemistry and microbial metabolism.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the nitrogen cycle limit as mountain. This uniformity reflects that the constraint is genuinely invariant across observation contexts: agricultural communities, global systems, and analytical observers all encounter the same biogeochemical boundary. The perspectival gap appears not in classification type (all mountain) but in the meaning of the constraint. For agricultural communities at a generational time horizon, the limit appears as a persistent ceiling on productivity — the natural background of soil fertility. For the global agricultural system at a civilizational horizon, the limit appears as a problem that has been partly overcome (Haber-Bosch) but replaced with a new constraint (planetary nitrogen cycle saturation). For the analytical observer, the limit appears as a natural law of chemistry and microbiology — immutable and universal. These differences in meaning do not change the classification but reveal how mountain constraints are experienced differently depending on power level and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   The nitrogen cycle limit has no directionality in the sense of directed extraction. There are no beneficiaries and victims; the constraint is distributed across all agents proportional to their nitrogen demands. If a directionality were computed, it would be symmetrical across all observers because the constraint is indifferent to human agency — it enforces equally on all attempts to extract nitrogen flux beyond its rate. The absence of beneficiary/victim structure confirms the mountain classification: the constraint appears as a physical boundary, not as an asymmetric transfer mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    haber_bosch_substitution_boundary,
    'Does industrial nitrogen fixation (Haber-Bosch) represent a genuine expansion of Earth''s nitrogen cycle capacity, or merely temporal borrowing against future biogeochemical debt?',
    'Long-term trajectory of eutrophication, hypoxic zone expansion, atmospheric N₂O accumulation, and soil health degradation. If trajectory remains stable despite Haber-Bosch expansion: substitution boundary is real (extraction of future capacity). If trajectory shows saturation and reversal: the boundary has been overcome.',
    'If substitution: the nitrogen cycle limit remains mountain-class; industrial nitrogen is a Snare constraint (extraction masquerading as coordination). If overcome: the limit transitions to Rope (coordination of nitrogen allocation) or disappears entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(haber_bosch_substitution_boundary, empirical, 'Whether Haber-Bosch represents genuine capacity expansion or temporal borrowing').

omega_variable(
    biological_nitrogen_fixation_ceiling,
    'Is there a hard thermodynamic ceiling on biological nitrogen fixation capacity, or can selective breeding and genetic engineering push the boundary indefinitely?',
    'Comparative analysis of nitrogenase efficiency across species, theoretical maximum enzyme turnover rates, energy requirements for N-N bond breaking under different environmental conditions, and upper bounds on microbial population size in agricultural ecosystems.',
    'If hard ceiling exists: mountain classification holds across all time horizons. If ceiling is indefinitely movable: the constraint is better classified as Tangled Rope (coordination of nitrogen allocation with embedded technological extraction) or shifts to Rope as technology generalizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_nitrogen_fixation_ceiling, empirical, 'Thermodynamic ceiling on biological nitrogen fixation').

omega_variable(
    planetary_nitrogen_budget_equilibrium,
    'Can global nitrogen cycling reach a new equilibrium that sustains agriculture, reduces eutrophication, and stabilizes atmospheric N₂O without triggering systemic biogeochemical collapse?',
    'Integrated Earth system modeling; paleoclimatic nitrogen cycle reconstructions; empirical measurement of nitrogen fluxes under various agricultural management scenarios; monitoring of watershed and atmospheric nitrogen concentrations.',
    'If equilibrium is achievable at current or higher agricultural productivity: the limit becomes a coordination problem (Rope, Tangled Rope) rather than mountain. If equilibrium requires significant productivity reduction: mountain remains, but the constraint moves from physical limit to resource scarcity limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(planetary_nitrogen_budget_equilibrium, empirical, 'Whether sustainable biogeochemical equilibrium is achievable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nitrogen_cycle_limit, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncl_tr_t0, nitrogen_cycle_limit, theater_ratio, 0, 0.03).
narrative_ontology:measurement(ncl_tr_t50, nitrogen_cycle_limit, theater_ratio, 50, 0.04).
narrative_ontology:measurement(ncl_tr_t100, nitrogen_cycle_limit, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(ncl_be_t0, nitrogen_cycle_limit, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(ncl_be_t50, nitrogen_cycle_limit, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(ncl_be_t100, nitrogen_cycle_limit, base_extractiveness, 100, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nitrogen_cycle_limit, global_infrastructure).
narrative_ontology:affects_constraint(nitrogen_cycle_limit, phosphorus_cycle_limit).
narrative_ontology:affects_constraint(nitrogen_cycle_limit, agricultural_yield_ceiling).
narrative_ontology:affects_constraint(nitrogen_cycle_limit, eutrophication_boundary).

% DUAL FORMULATION NOTE:
% The nitrogen cycle limit is a natural law constraint that structures downstream constraints in agricultural and aquatic systems. Phosphorus cycle limit is a parallel planetary boundary. Agricultural yield ceiling and eutrophication boundary are downstream constraints that emerge from the nitrogen cycle limit's interaction with human economic activity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
