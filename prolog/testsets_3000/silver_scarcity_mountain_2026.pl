% ============================================================================
% CONSTRAINT STORY: silver_scarcity_mountain_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silver_scarcity_mountain_2026, []).

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
 *   constraint_id: silver_scarcity_mountain_2026
 *   human_readable: The Silver Physical Scarcity Mountain
 *   domain: economic/industrial/geopolitical
 *
 * SUMMARY:
 *   Silver physical scarcity in 2026 reflects a fundamental geological
 *   constraint: crustal abundance of ~0.08 ppm silver cannot be arbitrarily
 *   increased. High-grade accessible deposits have been depleted over
 *   centuries of mining. Ore grades have declined from ~10% in 1900 to ~0.05%
 *   in 2024 — a 200-fold drop. Primary silver is now predominantly extracted
 *   as a byproduct of copper, zinc, and lead mining. Growing demand from
 *   photovoltaics (PV now consumes 8% of global silver supply, up from <1% in
 *   2000), renewable energy infrastructure, and electronics collides with
 *   constrained geological supply. The USGS Critical Minerals designation in
 *   2024-2025 formalizes what geology already determined: silver supply
 *   cannot freely scale with demand. This constraint operates identically
 *   across all observational contexts and power positions. No agent can
 *   negotiate with planetary chemistry. No institution can dissolve the
 *   constraint through policy. No substitute fully eliminates demand without
 *   efficiency losses or cost premiums. The mountain is invariant.
 *
 * KEY AGENTS:
 *   - Geological Systems: Primary constraint bearer (universal/analytical) — determines absolute silver abundance and ore distribution; immutable across all timescales
 *   - Global Mining Industry: Primary extractor (institutional/arbitrage) — operates under geological constraint; faces margin compression as ore grades decline
 *   - Photovoltaic Manufacturers: Demand concentration (powerful/mobile) — PV consumes largest marginal demand increment; can substitute at cost but not without limits
 *   - Recycling Infrastructure: Secondary supplier (organized/constrained) — recovers ~55% of dissipated silver; cannot exceed dissipation losses without new circular technology
 *   - Industrial Electronics Sector: Dispersed demander (powerful/arbitrage) — consumes silver across conductors, solder, batteries; faces supply rationing
 *   - Artisanal Mining Communities: Dependent agents (powerless/trapped) — rely on depleting ore deposits with no alternative livelihoods
 *   - Energy Transition Planners: Policy-constrained (institutional/constrained) — must plan renewable deployment around silver availability, not vice versa
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees constraint as axiomatic geochemistry, invariant across all scenarios
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silver_scarcity_mountain_2026, 0.18).
domain_priors:suppression_score(silver_scarcity_mountain_2026, 0.03).
domain_priors:theater_ratio(silver_scarcity_mountain_2026, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, extractiveness, 0.18).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silver_scarcity_mountain_2026, mountain).
narrative_ontology:human_readable(silver_scarcity_mountain_2026, "The Silver Physical Scarcity Mountain").
narrative_ontology:topic_domain(silver_scarcity_mountain_2026, "economic/industrial/geopolitical").

domain_priors:emerges_naturally(silver_scarcity_mountain_2026).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — Silver's geochemical abundance in Earth's crust (~0.08 ppm) is a fixed physical property. The concentration gradient that enabled historical easy mining is exhausted. Primary ore deposition follows geological processes operating over millions of years — no human intervention can accelerate crustal redistribution or increase absolute elemental abundance. This is a civilizational-scale constraint: even with infinite capital and perfect technology, secondary extraction from complex mineral assemblages faces thermodynamic limits. No degrees of freedom exist to escape the fundamental scarcity.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: INDUSTRIAL SUPPLY CHAIN (MOUNTAIN) — Silver extraction as an economic activity faces geological constraints independent of market structure. Primary deposits are finite and non-renewable on industrial timescales. Ore grades have declined monotonically for 150 years. Recycling rates cap at ~55% due to dissipative use (photovoltaics, electronics, catalysis). Even with perfect substitution technology and circular economy, the system cannot increase total silver availability beyond what geology permits. The constraint is invariant across all reasonable economic regimes.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PHOTOVOLTAIC INDUSTRY (MOUNTAIN) — Silver is a high-conductivity conductor essential for solar cell busbars and contacts. Alternatives (copper, aluminum, graphene composites) exist but incur efficiency losses or cost premiums. The industry can substitute, but substitution itself encounters physical limits: copper is not as conductive, graphene is not yet manufacturing-scale. The underlying constraint remains — there is insufficient silver to supply unrestricted solar deployment at current efficiency targets without rationing or substitution. This is a resource allocation problem constrained by physics, not by policy.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ARTISANAL MINING COMMUNITY (MOUNTAIN) — Silver-bearing ore deposits are geologically distributed and non-renewable. Communities dependent on mining artisanal silver from remaining accessible deposits cannot increase ore availability through effort. Depletion is a hardwired constraint. Exit options (economic diversification, relocation) exist but carry catastrophic personal cost. The fundamental scarcity is not a function of their choices — it is imposed by planetary geology.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: CLEAN ENERGY COALITION (MOUNTAIN) — The transition to renewable energy infrastructure (solar PV, wind, grid storage, EV electronics) requires silver-intensive components. The silver scarcity is a material constraint on the pace of energy transition, independent of policy or financing. Coalition members cannot coordinate around zero-sum silver allocation without accepting lower renewable capacity or lower efficiency targets. The constraint emerges from planetary chemistry, not institutional structure.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GEOCHEMIST / UNIVERSAL NATURAL LAW (MOUNTAIN) — Silver concentration in accessible ore deposits is a fixed geochemical property determined by planetary formation history and crustal differentiation. The constraint is axiomatic: Earth has a finite inventory of silver. Human demand exceeds biological replacement rate (zero). Therefore, cumulative consumption must eventually exceed supply. This is not policy or market failure — it is a theorem. No institution can rescind it.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silver_scarcity_mountain_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, ExtMetricName, E),
    domain_priors:suppression_score(silver_scarcity_mountain_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(silver_scarcity_mountain_2026),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(silver_scarcity_mountain_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low-to-moderate. This reflects that silver scarcity is not about extraction overhead in the economic sense — it is about absolute physical limitation. No amount of capital, technology, or political will can increase Earth's crustal silver inventory. The extractiveness value is bounded below by the fundamental scarcity itself: demand will always exceed geological supply at any price that sustains industrial use. It is low because there is no extraction to optimize — there is only rationing and allocation. Suppression (0.03): Minimal. There are no suppressive mechanisms or alternatives being hidden. Silver scarcity is transparently true. All agents observe the same geological facts. The suppression floor reflects only unavoidable information asymmetries (mining companies may not disclose exact ore grades, recycling rates vary by region). These are trivial. Theater ratio (0.12): Very low. The constraint is not performative. Agents are not enacting a scarcity ritual — the scarcity is materially real. PV manufacturers cannot claim high efficiency without silver-based contacts. Miners cannot extract from depleted veins. The 0.12 value captures minimal administrative or reporting overhead, not substitution of function with spectacle.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, silver scarcity exhibits NO perspectival gap. All six perspectives produce MOUNTAIN classification. The original research group sees constraint (Mountain). The replication group sees constraint (Mountain). The journal system sees constraint (Mountain). The analytical observer sees constraint (Mountain). This uniformity is the signature of a true natural law constraint: the structure is invariant across all observational contexts and power positions. No agent experiences silver scarcity as coordination (Rope) or temporary (Scaffold). No agent has arbitrage options that dissolve the constraint. The powerless artisanal miner, the powerful PV manufacturer, the institutional mining company, and the analytical geochemist all confront the same immovable fact: crustal silver is finite and non-renewable. The absence of perspectival gap is the defining feature that certifies this as mountain-class.
 *
 * DIRECTIONALITY LOGIC:
 *   Silver scarcity is NOT a constraint with beneficiaries and victims in the usual sense. No agent benefits from scarcity. All agents are negatively affected by it. Demand exceeds supply universally. The constraint does not extract from one group to benefit another — it constrains everyone proportionally. Therefore, directionality (d) is NOT derived from beneficiary/victim declarations (which are absent). Instead, all perspectives use the canonical fallback d values for their power atoms, applied to a constraint with uniform impact. The analytical and institutional perspectives derive low d (beneficiary-like) not because they benefit from scarcity but because they have analytical exit or arbitrage capacity to recontextualize or substitute. The powerless perspective derives high d (victim-like) not because it is uniquely targeted but because it has no escape routes. The directionality spread reflects power asymmetry in coping capacity, not structural extraction. This is the key insight: mountain constraints often have uniform negative impact but still exhibit directionality spread based on coping asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. Silver scarcity is a pure mountain constraint with extractiveness 0.18 << 0.46, below the mandatrophy threshold. The constraint requires no resolution between coordination and extraction interpretations because it is neither — it is purely a physical limit. All perspectives produce the same classification (Mountain), eliminating ambiguity. The constraint is not extracted from anything or anyone. It is not a coordination mechanism that disguises extraction. It is simply a natural law: planetary chemistry determines available silver, and demand exceeds supply. The USGS Critical Minerals designation does NOT resolve a mandatrophy — it merely formalizes a geological fact that was already true. The mountain certification is certain: accessibility_collapse (0.91) and resistance (0.08) both pass their gates. Emerges naturally (true) is declared. This constraint is among the highest-confidence mountains in the corpus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_technology_breakthrough,
    'Can synthetic substitutes (graphene composites, novel conductors, or efficiency gains via new cell architectures) reduce silver demand by >50% within 20 years?',
    'Technology roadmap analysis; laboratory-to-manufacturing pilot programs for graphene busbars, perovskite cells, or alternative contact schemes; cost curves and efficiency benchmarks vs incumbent silver-based designs',
    'If breakthrough succeeds: constraint shifts from absolute scarcity to relative scarcity (moderate resource competition, not zero-sum). Effective extractiveness drops. If breakthrough fails: scarcity remains absolute, forcing rationing or energy transition slowdown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technology_breakthrough, empirical, 'Technological breakthrough in substitutes for silver in solar and electronics').

omega_variable(
    ore_grade_collapse_timeline,
    'At what ore grade percentage do mining costs exceed commodity price for primary silver extraction?',
    'Economic modeling of extraction cost vs silver concentration; historical ore grade decline rates (Nordhaus-adjusted); breakeven analysis for different mining technologies (underground, heap leach, flotation)',
    'If breakeven is reached before 2035: primary mining becomes uneconomical; supply depends entirely on secondary (recycled) silver at ~55% efficiency. If breakeven is 2050+: primary supply remains viable but increasingly constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ore_grade_collapse_timeline, empirical, 'Timeline for ore grade decline to economic unviability').

omega_variable(
    recycling_rate_ceiling,
    'Is the current 55% secondary silver recovery rate a physical ceiling or a policy/economic artifact that could be raised to 75%+?',
    'Life-cycle analysis of dissipative uses (photovoltaics, electronics, catalysis, antimicrobial); post-consumer collection rates; urban mining economics; electrolytic recovery efficiency for dilute streams',
    'If ceiling is ~55% (dissipation losses inevitable): absolute scarcity persists. If ceiling is 75%+: recycling can offset more primary depletion, extending timeline of resource stress.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recycling_rate_ceiling, empirical, 'Physical and economic ceiling on secondary silver recovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silver_scarcity_mountain_2026, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silver_tr_t0, silver_scarcity_mountain_2026, theater_ratio, 0, 0.08).
narrative_ontology:measurement(silver_tr_t13, silver_scarcity_mountain_2026, theater_ratio, 13, 0.11).
narrative_ontology:measurement(silver_tr_t26, silver_scarcity_mountain_2026, theater_ratio, 26, 0.12).

% Extraction over time
narrative_ontology:measurement(silver_be_t0, silver_scarcity_mountain_2026, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(silver_be_t13, silver_scarcity_mountain_2026, base_extractiveness, 13, 0.18).
narrative_ontology:measurement(silver_be_t26, silver_scarcity_mountain_2026, base_extractiveness, 26, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silver_scarcity_mountain_2026, resource_allocation).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, photovoltaic_deployment_ceiling).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, electronics_supply_chain_fragility).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, battery_chemistry_limitations).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, rare_earth_cluster_coupling).

% DUAL FORMULATION NOTE:
% Silver scarcity is a single, unified constraint. It does not decompose into multiple constraint stories with different epsilon values. The scarcity manifests identically whether measured by ore grade decline, byproduct extraction limits, recycling rates, or absolute crustal inventory. All measurement bases yield the same epsilon (~0.18) and the same classification (Mountain). Unlike the BGS decomposition (where spectral universality and eigenvector thermalization have genuinely different epsilon values and different empirical status), silver scarcity is axiomatically unified. The network edges point to downstream constraints (PV deployment ceilings, electronics supply fragility) that are consequences of silver scarcity, not alternative framings of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
