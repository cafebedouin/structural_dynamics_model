% ============================================================================
% CONSTRAINT STORY: ecological_carrying_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecological_carrying_capacity, []).

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
 *   constraint_id: ecological_carrying_capacity
 *   human_readable: Ecological Carrying Capacity of Earth
 *   domain: environmental_systems/ecology
 *
 * SUMMARY:
 *   Ecological carrying capacity represents the maximum population size and
 *   material throughput that Earth's renewable and non-renewable resources
 *   can sustain indefinitely. The constraint is classified as mountain — a
 *   natural law of biogeochemical systems — from nearly all perspectives
 *   because it emerges from immutable physical principles: photosynthetic
 *   efficiency bounds primary productivity; nutrient cycling rates are
 *   determined by microbial metabolism and geological timescales; energy
 *   dissipation follows thermodynamic limits. The constraint exists at
 *   multiple scales (local carrying capacity for a forest, regional capacity
 *   for a watershed, planetary capacity for human civilization) but the
 *   indexical classification treats planetary-scale capacity as the primary
 *   story. Humanity has measurably exceeded planetary carrying capacity on
 *   multiple dimensions: carbon emission rates exceed sequestration,
 *   biodiversity loss exceeds speciation, nutrient runoff exceeds
 *   biogeochemical cycling capacity, and freshwater withdrawal exceeds
 *   sustainable recharge. The constraint manifests not as a negotiable
 *   institutional limit but as planetary dysregulation: climate disruption,
 *   ecosystem collapse, soil degradation, ocean dead zones. The exceptional
 *   perspective comes from institutional actors and technological optimists
 *   who frame carrying capacity as a manageable coordination problem rather
 *   than a natural law — this tangled_rope perspective represents genuine
 *   coordination (efficiency optimization) alongside extraction (differential
 *   bearing of environmental costs), but it operates under the structural
 *   reality that the underlying mountain constraint remains unchanged.
 *
 * KEY AGENTS:
 *   - Non-human ecosystems (powerless/trapped): Experience carrying capacity as immutable physical reality determining population dynamics and species viability
 *   - Subsistence populations (powerless/trapped): Direct resource dependence makes carrying capacity constraint appear as immediate material scarcity and starvation risk
 *   - Industrial economies (institutional/arbitrage): Claim technological decoupling from material throughput; experience constraint as coordination problem rather than law
 *   - Capital-intensive sectors (institutional/arbitrage): Primary beneficiaries of resource extraction systems; experience carrying capacity as constraint on scale, not on feasibility
 *   - Low-income and dependent populations (powerless/constrained): Bear disproportionate environmental costs through pollution exposure, resource scarcity, and climate impacts while bearing least responsibility for excess
 *   - State actors (powerful/constrained): Experience carrying capacity through territorial resource availability and sovereignty claims over extraction
 *   - Climate and conservation science consensus (organized/constrained): Institutional measurement and modeling consensus that carrying capacity has been exceeded on multiple dimensions
 *   - Analytical observer (analytical/analytical): Universal perspective recognizing carrying capacity as emergent from thermodynamic and biogeochemical fundamentals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecological_carrying_capacity, 0.18).
domain_priors:suppression_score(ecological_carrying_capacity, 0.03).
domain_priors:theater_ratio(ecological_carrying_capacity, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecological_carrying_capacity, extractiveness, 0.18).
narrative_ontology:constraint_metric(ecological_carrying_capacity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(ecological_carrying_capacity, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecological_carrying_capacity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ecological_carrying_capacity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecological_carrying_capacity, mountain).
narrative_ontology:human_readable(ecological_carrying_capacity, "Ecological Carrying Capacity of Earth").
narrative_ontology:topic_domain(ecological_carrying_capacity, "environmental_systems/ecology").

domain_priors:emerges_naturally(ecological_carrying_capacity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-HUMAN ECOSYSTEMS (MOUNTAIN) — Ecosystems have no exit options from the carrying capacity constraint. Species composition, migration patterns, and population dynamics are bound by finite nutrient cycling, water availability, and energy flux. The constraint is natural law: exceed carrying capacity and collapse follows with certainty.
constraint_indexing:constraint_classification(ecological_carrying_capacity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSISTENCE POPULATIONS (MOUNTAIN) — Populations dependent on direct resource extraction (small-scale agriculture, fishing, hunting) experience carrying capacity as immutable physical reality. Exceeding local carrying capacity produces starvation, resource depletion, and migration — immediate material constraints, not institutional arrangements that could be negotiated.
constraint_indexing:constraint_classification(ecological_carrying_capacity, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a biogeochemical perspective, carrying capacity emerges from immutable physical laws: photosynthetic efficiency bounds primary productivity; nutrient cycling rates are determined by microbial metabolism and geological processes; energy dissipation follows thermodynamic limits. The constraint is universal and irreducible.
constraint_indexing:constraint_classification(ecological_carrying_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INDUSTRIAL ECONOMIES / DECOUPLING AGENTS (TANGLED ROPE) — From the perspective of institutional actors claiming relative decoupling of GDP from material throughput, carrying capacity appears as a manageable constraint rather than immutable law. Beneficiaries (capital-intensive, technology-intensive sectors) experience the constraint as coordination problem: optimize resource efficiency to sustain extraction flows. Victims (low-income populations bearing environmental externalities) experience it as snare. This perspective's tangled structure reflects the genuine coordination of technological efficiency alongside asymmetric extraction of environmental rent.
constraint_indexing:constraint_classification(ecological_carrying_capacity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE ACTORS / RESOURCE SOVEREIGNTY (MOUNTAIN) — States experience carrying capacity as constraint on national resource availability, but frame it through sovereignty and extraction logic rather than as law of nature. Yet from the state's perspective: territorial carrying capacity for timber, fish, minerals is treated as naturally immutable — you can only extract what the territory contains. This perspective maintains the mountain classification despite the institutional mediation of 'sovereignty'.
constraint_indexing:constraint_classification(ecological_carrying_capacity, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLIMATE AND CONSERVATION SCIENCE (MOUNTAIN) — From the consensus scientific perspective, planetary carrying capacity is a rock-solid mountain: we have exceeded it measurably. The constraint is now manifesting as climate disruption, biodiversity loss, nutrient cycle saturation, and ocean acidification. Scientists see no negotiable exits from these biophysical realities — the physics is immutable, the time to mitigation is constrained, and exceeded boundaries have consequences that cannot be negotiated away.
constraint_indexing:constraint_classification(ecological_carrying_capacity, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecological_carrying_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecological_carrying_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecological_carrying_capacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ecological_carrying_capacity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecological_carrying_capacity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ecological_carrying_capacity, ExtMetricName, E),
    domain_priors:suppression_score(ecological_carrying_capacity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ecological_carrying_capacity),
    narrative_ontology:constraint_metric(ecological_carrying_capacity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ecological_carrying_capacity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ecological_carrying_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low, but non-zero. The constraint itself is not extractive — it is a natural law. The low score reflects that no agent extracts from others through the constraint itself. However, extractiveness is not zero because human institutions *respond* to carrying capacity constraints through unequal distribution of burden: wealthy actors externalize environmental costs onto poor ones, and current systems concentrate resource consumption among high-income deciles. This represents institutional extraction operating *within* the natural constraint. As a pure natural law, extractiveness should be near zero; the 0.18 reflects measurement of institutional overlay. Suppression (0.03): Minimal. The constraint cannot be suppressed — it operates universally. The low score reflects that there is no coercive apparatus maintaining the constraint; it maintains itself through physics. Theater ratio (0.12): Very low. The constraint is functionally pure — no performative activity generates the limit. However, it is non-zero because some institutional responses to carrying capacity (carbon accounting, sustainability certifications, ESG metrics) have performative elements. This indicates that human institutions layering on top of the constraint introduce theater, but the constraint itself is theater-free. Accessibility collapse (0.92): Very high. The carrying capacity constraint permits no escape routes or alternative trajectories — it is universally binding. This is the hallmark mountain signature. Resistance (0.08): Very low. The constraint is not resisted through force or negotiation because it cannot be. It is accepted universally once understood.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives divide into two groups. Mountain perspectives (ecosystems, subsistence populations, science consensus, analytical observer) recognize carrying capacity as immutable physical law. The tangled_rope institutional perspective (industrial economies claiming decoupling) introduces a alternative framing: carrying capacity is a coordination problem to be managed through technological efficiency and resource reallocation. This perspective is structurally tangled because it contains genuine coordination (efficiency gains reduce resource throughput per unit output) alongside extraction (institutional actors concentrated in high-income deciles bear less of the climate and ecological cost). The gap is real and diagnostic: the institutional perspective is not wrong about efficiency gains or technological innovation, but it is incomplete — it cannot escape the underlying mountain. Technology can shift which resources become binding constraints; it cannot transcend carrying capacity itself. The analytical observer's mountain perspective is strengthened by noting that all five institutional perspectives operate within the same biogeochemical bounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for natural law constraints is undefined in the classical sense because there are no beneficiaries or victims within the constraint mechanism itself — the constraint is not extraction. However, the institutional *response* to carrying capacity creates differential directionality: wealthy agents with technological access and arbitrage options (d ≈ 0.15, 'arbitrage') experience lower effective extraction of their resource access. Powerless agents trapped in subsistence dependence (d ≈ 0.95, 'trapped') experience the carrying capacity constraint as immediate material scarcity with no alternatives. The slope of directionality becomes a measure of institutional inequality in bearing the cost of the natural constraint. This is not a flaw in the mountain classification — it is a demonstration that mountains constrain all agents, but institutional arrangements create unequal burden distribution *around* the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. This constraint is a canonical mountain: it exhibits no confusion between coordination and extraction because no beneficiaries or victims exist at the level of the constraint mechanism itself. The constraint is a natural law, not an institutional arrangement. The institutional responses to carrying capacity (resource distribution, pollution, climate impacts) create secondary constraints (inequality, environmental injustice) that are tangled ropes or snares, but these are *responses to* carrying capacity, not carrying capacity itself. The classification is singular across observational methodologies — this is the diagnostic signature of a true mountain. No mandate reduction is necessary; the constraint's type is invariant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carrying_capacity_measurability,
    'Is carrying capacity a precise, measurable quantity or a fuzzy ecological concept whose boundaries depend on observational scope and definition?',
    'Comparison of carrying capacity estimates across different measurement methodologies (planetary boundaries framework, ecological footprint, net primary productivity models, nutrient cycling models). Analysis of whether different methods converge on a single value or produce ranges that differ by factors of 2-10.',
    'If carrying capacity is precisely measurable: mountain classification is secure — the constraint is a definite physical boundary. If fuzzy: carrying capacity functions more like a negotiable boundary in institutional contexts, potentially reclassifying perspectives as tangled_rope or rope depending on institutional power and technological capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carrying_capacity_measurability, empirical, 'Whether carrying capacity is a precise boundary or fuzzy ecological concept').

omega_variable(
    technological_escape_feasibility,
    'Can technological innovation fundamentally alter carrying capacity (e.g., through energy transition, synthetic agriculture, closed-loop materials cycles), or does technology merely shift which resources become the binding constraint?',
    'Longitudinal analysis of historical technological transitions (Haber-Bosch, Green Revolution, renewable energy scaling). Measurement of whether technology increased absolute carrying capacity or merely deferred the constraint to different resource types (phosphorus after nitrogen, rare earths after fossil fuels).',
    'If technology can escape carrying capacity: industrial decoupling perspective becomes valid, reclassifying as tangled_rope from institutional perspective with genuine upside to coordination. If technology merely shifts constraint: mountain remains unchanged — we are not transcending limits, just moving them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_escape_feasibility, empirical, 'Whether technology can fundamentally alter carrying capacity or merely shift constraints').

omega_variable(
    planetary_boundary_coupling,
    'Are the nine planetary boundaries (climate, biodiversity, land use, freshwater, nutrient cycles, ocean acidification, ozone, aerosol, chemical pollution) independently binding constraints, or are they coupled such that breaching one causes cascade failures in others?',
    'Earth system modeling; empirical tracking of correlation between boundary exceedances; analysis of whether reducing one boundary violation (e.g., carbon emissions) improves others (e.g., biodiversity loss) or leaves them independent.',
    'If independent: carrying capacity is modular — we might stay within some boundaries while exceeding others. If coupled: exceeding any boundary triggers cascades that compress the remaining carrying capacity, making the constraint more severe than single-boundary models suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planetary_boundary_coupling, empirical, 'Whether planetary boundaries are independent or coupled constraints').

omega_variable(
    equity_vs_aggregate_capacity,
    'Does carrying capacity distribute equally across the human population, or do institutional arrangements concentrate carrying capacity consumption among high-income actors, thereby reducing effective capacity for others?',
    'Comparative analysis of per-capita resource consumption (carbon, water, land, minerals) by income decile; calculation of how many Earths would be required if global population consumed at high-income levels; measurement of whether redistribution toward equity shrinks aggregate capacity or reallocates existing capacity.',
    'If equal distribution possible: carrying capacity is fundamentally about per-capita limits, and institutional arrangements are secondary. If extraction concentrates capacity: carrying capacity is a constraint on total human flourishing, with current institutional arrangements already representing a compressed effective capacity for lower-income populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_vs_aggregate_capacity, empirical, 'Whether carrying capacity distributes equally or concentrates through institutional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecological_carrying_capacity, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecc_tr_t0, ecological_carrying_capacity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ecc_tr_t100, ecological_carrying_capacity, theater_ratio, 100, 0.09).
narrative_ontology:measurement(ecc_tr_t200, ecological_carrying_capacity, theater_ratio, 200, 0.12).

% Extraction over time
narrative_ontology:measurement(ecc_be_t0, ecological_carrying_capacity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ecc_be_t100, ecological_carrying_capacity, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(ecc_be_t200, ecological_carrying_capacity, base_extractiveness, 200, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecological_carrying_capacity, global_infrastructure).
narrative_ontology:affects_constraint(ecological_carrying_capacity, climate_stability_boundary).
narrative_ontology:affects_constraint(ecological_carrying_capacity, biodiversity_loss_rate).
narrative_ontology:affects_constraint(ecological_carrying_capacity, freshwater_depletion).
narrative_ontology:affects_constraint(ecological_carrying_capacity, nutrient_cycle_saturation).
narrative_ontology:affects_constraint(ecological_carrying_capacity, land_use_conversion).

% DUAL FORMULATION NOTE:
% Ecological carrying capacity is the parent constraint; specific boundary transgressions (climate change, biodiversity loss, nutrient runoff) are downstream manifestations of the single universal constraint. Each planetary boundary can be analyzed as a separate constraint story with its own extractiveness and institutional overlay, linked to this parent story through network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
