% ============================================================================
% CONSTRAINT STORY: deep_earth_hydrogen_availability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deep_earth_hydrogen_availability, []).

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
 *   constraint_id: deep_earth_hydrogen_availability
 *   human_readable: Deep Earth Hydrogen Availability Limit
 *   domain: technological/geophysics/planetary_science
 *
 * SUMMARY:
 *   The total inventory of hydrogen in the Earth's interior — housed in the
 *   core, mantle, and deep crustal reservoirs — is a fixed planetary property
 *   determined by accretion history and geochemical partitioning. Unlike
 *   energy sources that are continuously replenished (solar, geothermal
 *   gradients), the mantle's hydrogen inventory represents a depletable stock
 *   that can only be renewed by slow abiotic generation (water-rock
 *   reactions, radiolytic splitting) or external input (meteoritic delivery).
 *   As industrial hydrogen demand grows — driven by decarbonization goals,
 *   synthetic fuel production, and potential space-based energy exports — the
 *   question of whether the Earth's accessible hydrogen reserves can sustain
 *   long-term extraction becomes a hard physical constraint. This constraint
 *   exhibits the core characteristics of a Mountain: immutable physical law,
 *   irreducible inventory, no technological bypass, and universal
 *   applicability across all observer positions.
 *
 * KEY AGENTS:
 *   - Hydrogen-Dependent Industrial Economy: Primary target (powerless/trapped) — future economic activity constrained by hard physical limit on hydrogen supply
 *   - Planetary Science Community: Primary observer (institutional/arbitrage) — documents and measures the constraint; benefits from understanding it
 *   - Energy Transition Planners: Secondary organized agents (organized/constrained) — must factor the constraint into long-term infrastructure planning
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a non-negotiable property of physics and planetary geology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deep_earth_hydrogen_availability, 0.12).
domain_priors:suppression_score(deep_earth_hydrogen_availability, 0.03).
domain_priors:theater_ratio(deep_earth_hydrogen_availability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, extractiveness, 0.12).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deep_earth_hydrogen_availability, mountain).
narrative_ontology:human_readable(deep_earth_hydrogen_availability, "Deep Earth Hydrogen Availability Limit").
narrative_ontology:topic_domain(deep_earth_hydrogen_availability, "technological/geophysics/planetary_science").

domain_priors:emerges_naturally(deep_earth_hydrogen_availability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HYDROGEN-DEPENDENT ECONOMY (MOUNTAIN) — Future industries relying on deep mantle hydrogen access face an immutable physical limit. No technological bypass exists for the total hydrogen inventory in the Earth's interior. The constraint is structural: geochemical partitioning and planetary formation history determined how much hydrogen exists in accessible deep reservoirs. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.17. The constraint appears as natural law.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLANETARY SCIENCE COMMUNITY (MOUNTAIN) — Observes the constraint as a closed empirical question: mantle hydrogen inventory is determined by formation history and can be measured (or bounded) through geochemistry, seismic tomography, and spectroscopy. No negotiation possible; the constraint pre-exists human activity. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.02. Negative extraction; the constraint is a fact to be documented.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the universal/civilizational view, the hydrogen inventory of the Earth's interior is an invariant physical property. It does not change based on human preference, institutional design, or technological capability. The constraint is logically and physically irreducible: formed during planetary accretion, partitioned by core-mantle differentiation, subject to slow diffusion and chemical reactions, but fundamentally bounded by total planetary inventory. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. Observer sees the constraint as natural law.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ENERGY TRANSITION PLANNERS (MOUNTAIN) — Organized agents (energy policy bodies, industrial hydrogen consortia) face a hard constraint: if hydrogen economy planning assumes unlimited deep mantle access, the constraint will bind within 2-3 generations. No organizational mechanism can increase the total hydrogen inventory. The constraint is structural and invariant. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.11. Perceives constraint as immutable limit on industrial capacity.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deep_earth_hydrogen_availability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, ExtMetricName, E),
    domain_priors:suppression_score(deep_earth_hydrogen_availability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deep_earth_hydrogen_availability),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deep_earth_hydrogen_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract value in the economic sense — it is a physical limit on total available hydrogen. There is no beneficiary capturing extraction (unlike a Snare) and no coordination problem (unlike a Rope). The modest non-zero value reflects the minimal 'cost' of observing and understanding the constraint; knowledge work is required to quantify the inventory. Suppression (0.03): Negligible. The constraint cannot be suppressed because it is not a social or institutional mechanism — it is a planetary property. The low value reflects the transparency of the constraint once geophysical data is available. Theater ratio (0.15): Very low. There is almost no performative element. The constraint is measured through rigorous geochemistry, seismic analysis, and spectroscopy. The modest value accounts for uncertainty bands and modeling assumptions in inventory estimates — not theatrical performance, but epistemic humility.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification. The perspectival gap is not in type, but in *how* the constraint manifests. The industrial economy experiences it as a hard ceiling on future growth. The scientific community experiences it as a closed empirical question. The energy planners experience it as a fixed planning parameter. The analytical observer experiences it as a law of nature. There is no disagreement on the classification — all agree it is immutable — but the *relevance* of the constraint differs by perspective. For the analytical observer, it is one among many planetary physical laws. For the industrial economy, it is an existential planning challenge.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive directionality from an analytical/observer position because the constraint is a natural law, not a human institution with identifiable beneficiaries or victims. The constraint pre-exists human decision-making and binds equally on all agents. Hydrogen-dependent economy: victim (d≈0.95) — trapped in the constraint, cannot exit. Planetary science community: neutral observer (d≈0.05) — benefits from scientific understanding, not extraction. Energy planners: partially constrained victim (d≈0.55) — can optimize within the constraint but cannot escape it. Analytical observer: pure observer (d≈0.72) — sees the constraint as invariant.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mantle_hydrogen_inventory_quantification,
    'What is the total hydrogen content in the Earth''s mantle and core, and what fraction is accessible to technological extraction?',
    'High-pressure mineral physics experiments; geochemical analysis of mantle nodules; seismic constraints on phase transitions; volatile element abundance modeling from meteorite composition',
    'If total mantle hydrogen is >1e18 kg with >10% accessible: hydrogen economy can sustain for centuries. If <1e17 kg or <1% accessible: constraint will bind within 50-100 years of heavy industrial use.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mantle_hydrogen_inventory_quantification, empirical, 'Total mantle hydrogen inventory and accessibility fraction').

omega_variable(
    hydrogen_diffusion_escape_rate,
    'How rapidly does hydrogen diffuse out of the mantle and escape to the atmosphere or hydrogen space?',
    'Isotopic ratios of hydrogen and helium in mantle plumes; paleomagnetic and atmospheric composition records; laboratory diffusion experiments at mantle conditions; modeling of hydrogen escape rates from high atmosphere',
    'If escape rate is high (geological timescale: hundreds of millions of years): steady-state inventory is determined by input-output balance, not total store. If escape is negligible: inventory is a fixed planetary property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hydrogen_diffusion_escape_rate, empirical, 'Hydrogen diffusion and escape flux from mantle').

omega_variable(
    abiotic_hydrogen_generation_rate,
    'How much hydrogen is generated in situ within the mantle through water-rock reactions, serpentinization, and radiolytic water splitting?',
    'Theoretical models of water-olivine reaction kinetics; field measurements of hydrogen in hydrothermal systems; isotopic tracers to distinguish cosmogenic and abiotic hydrogen; experimental constraints on radiolytic H2 generation rates',
    'If abiotic generation is significant: mantle hydrogen is a renewable resource (flow) rather than a depletable stock (store), changing constraint classification. If negligible: inventory is predominantly primordial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abiotic_hydrogen_generation_rate, empirical, 'In situ hydrogen generation in the mantle').

omega_variable(
    extraction_technology_frontier,
    'What depths and geological conditions can future technology access for hydrogen extraction, and at what cost?',
    'Roadmaps for deep drilling (beyond current ~12 km limit); supercritical fluid dynamics and heat management at extreme depths; economic modeling of extraction cost as function of depth',
    'If extraction is limited to <30 km depth: accessible hydrogen is much smaller than total inventory. If technology can reach 400+ km (upper mantle): accessible fraction increases significantly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_technology_frontier, preference, 'Future technology access depth and economic viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deep_earth_hydrogen_availability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dehyd_tr_t0, deep_earth_hydrogen_availability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dehyd_tr_t50, deep_earth_hydrogen_availability, theater_ratio, 50, 0.14).
narrative_ontology:measurement(dehyd_tr_t100, deep_earth_hydrogen_availability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(dehyd_be_t0, deep_earth_hydrogen_availability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(dehyd_be_t50, deep_earth_hydrogen_availability, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(dehyd_be_t100, deep_earth_hydrogen_availability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deep_earth_hydrogen_availability, global_infrastructure).
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, hydrogen_economy_sustainability).
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, deep_ocean_hydrothermal_hydrogen_production).
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, abiotic_hydrogen_generation_rate).

% DUAL FORMULATION NOTE:
% This constraint is upstream of economic and technological hydrogen extraction scenarios. It provides the hard physical bounds within which all hydrogen-based industrial planning must operate. Related constraints model the abiotic hydrogen generation rate and hydrothermal production mechanisms — these represent the flow of renewably generated hydrogen, which is independent of (but constrained by) the total inventory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
