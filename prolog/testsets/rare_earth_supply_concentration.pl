% ============================================================================
% CONSTRAINT STORY: rare_earth_supply_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_supply_concentration, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rare_earth_supply_concentration
 *   human_readable: Rare Earth Supply Concentration
 *   domain: geopolitics/industrial_dependency
 *
 * SUMMARY:
 *   Rare earth supply concentration represents a structural asymmetry in the
 *   global industrial supply chain where processing capacity and refining
 *   technology for rare earth elements (used in permanent magnets, catalysts,
 *   phosphors, and high-tech applications) are heavily concentrated in a
 *   single nation (China), creating dependency for downstream manufacturers
 *   across defense, renewable energy, and consumer electronics sectors. The
 *   constraint generates a tangled interaction between genuine coordination
 *   problems (global supply chain management, environmental remediation of
 *   extraction sites, manufacturing continuity) and extractive power
 *   asymmetries (pricing leverage during supply shortages, geopolitical
 *   coercion capacity, supply cutoff threats). The theater_ratio (0.35)
 *   reflects that the constraint has minimal performative content — the
 *   extraction mechanism is direct and material (supply bottlenecks,
 *   transportation costs, processing queues), not ritualistic. Industrial
 *   policy responses (US critical minerals initiatives, EU rare earth
 *   processing investment, recycling infrastructure) are creating alternative
 *   pathways, suggesting a scaffold sunset trajectory over a 10-20 year
 *   horizon.
 *
 * KEY AGENTS:
 *   - Chinese Integrated Producers: Primary beneficiary (institutional/arbitrage) — Myanmar acquisition strategy, vertical integration into processing, supply pricing leverage
 *   - Dependent Downstream Manufacturers: Primary victim (powerless/trapped) — automotive, renewable energy, defense, consumer electronics sectors with no credible exit alternatives
 *   - Allied Industrial Coalition: Organized victim (organized/constrained) — automotive associations, defense contractors, renewable energy manufacturers negotiating collective contracts and funding alternative sourcing
 *   - Industrial Policy Coalition: Powerful agent (powerful/mobile) — US, EU, Japan implementing critical minerals strategies, subsidizing rare earth processing, building strategic reserves
 *   - Mining-Source Countries: Hybrid beneficiary-victim (institutional/constrained) — Myanmar, Vietnam, Brazil receiving revenue and investment but bearing environmental costs and geopolitical leverage
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent processing concentration as geological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_supply_concentration, 0.58).
domain_priors:suppression_score(rare_earth_supply_concentration, 0.72).
domain_priors:theater_ratio(rare_earth_supply_concentration, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_supply_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_supply_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_supply_concentration, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_supply_concentration, tangled_rope).
narrative_ontology:human_readable(rare_earth_supply_concentration, "Rare Earth Supply Concentration").
narrative_ontology:topic_domain(rare_earth_supply_concentration, "geopolitics/industrial_dependency").

domain_priors:requires_active_enforcement(rare_earth_supply_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_supply_concentration, chinese_processing_industry).
narrative_ontology:constraint_beneficiary(rare_earth_supply_concentration, integrated_rare_earth_producers).
narrative_ontology:constraint_victim(rare_earth_supply_concentration, dependent_downstream_manufacturers).
narrative_ontology:constraint_victim(rare_earth_supply_concentration, supply_security_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT DOWNSTREAM MANUFACTURER (SNARE) — A manufacturer of advanced electronics, renewable energy systems, or defense equipment cannot exit rare earth supply chains without decades of alternative sourcing and domestic processing infrastructure. High suppression, no viable alternatives, full extraction of supply pricing. The manufacturer faces pricing power with no credible exit threat.
constraint_indexing:constraint_classification(rare_earth_supply_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED INDUSTRIAL COALITION (TANGLED ROPE) — Organized coalitions of dependent manufacturers (automotive, defense, renewable energy sectors) negotiate collective contracts and fund alternative processing capacity. They receive genuine coordination benefit (collective bargaining power, shared R&D on substitution) but face extraction through sustained supply bottlenecks and geopolitical leverage. Exit is possible but requires continental-scale industrial policy coordination.
constraint_indexing:constraint_classification(rare_earth_supply_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CHINESE INTEGRATED PRODUCER (ROPE) — Major rare earth producers (Molycorp acquisition strategy, vertically integrated operations) experience the constraint as coordination: managing global supply, stabilizing processing infrastructure, facilitating technology transfer. The constraint enables their market position. Exit options via arbitrage (supply flexibility, processing capacity deployment) mean they do not experience suppression.
constraint_indexing:constraint_classification(rare_earth_supply_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDUSTRIAL POLICY COALITION (SCAFFOLD) — Wealthy nations (US, EU, Japan) implementing critical minerals strategies, subsidizing rare earth processing, and building strategic reserves see the bottleneck as a temporary coordination failure with a sunset. Industrial policy investments (Mountain Pass reopening, Vietnamese processing plants, recycling infrastructure) are creating alternative supply pathways. Effective extraction is low because the coalition has resources and credible exit timelines.
constraint_indexing:constraint_classification(rare_earth_supply_concentration, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: MINING-SOURCE COUNTRIES (TANGLED ROPE) — Nations with rare earth deposits (Myanmar, Vietnam, Brazil) benefit from extraction rents and processing investment but face genuine coordination problems with environmental remediation, local community impact, and supply chain security. They are both beneficiaries (revenue) and victims (environmental cost, geopolitical leverage by downstream users). Their constrained exit reflects resource dependency — switching away from rare earth revenue is difficult without development alternatives.
constraint_indexing:constraint_classification(rare_earth_supply_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, rare earth concentration reflects irreducible geological facts: rare earth elements are geographically concentrated in specific deposits with distinct chemical extraction requirements. The physics and chemistry of ore processing create inherent technical barriers that make decentralized production difficult. However, the structural data contradicts the mountain classification — processing capacity concentration in China is institutional policy, not geological necessity. The engine will flag this as false naturalization.
constraint_indexing:constraint_classification(rare_earth_supply_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_supply_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_supply_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_supply_concentration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_supply_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rare_earth_supply_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original research group (Chinese processing industry) captures supply pricing advantages and can weaponize disruption, but the constraint is not maximum extraction because dependent manufacturers retain some pricing discipline (threat of industrial policy investment, substitution R&D, strategic reserves) and the supply chain has maintained basic functionality. The value reflects sustained but not catastrophic extraction. Extractiveness has increased from 0.42 to 0.58 over the 15-year interval as Chinese processing dominance consolidated and alternative processing capacity failed to scale. Suppression (0.72): High. Significant barriers to independent verification and exit include specialized processing technology, environmental remediation capital requirements, 5-10 year timelines for processing plant buildout, and geopolitical leverage. But suppression is not absolute (industrial policy has resources) — downward mobility exists for wealthy coalitions at continental scale. Theater ratio (0.35): Low. The extraction mechanism is direct material dependency, not performative. Rare earth processing is a technical function with measurable yields and costs; the constraint operates through bottlenecks, not ritual. The slight increase (0.28 to 0.35) reflects heightened rhetoric around critical minerals strategy but no change in underlying functional dependency.
 *
 * PERSPECTIVAL GAP:
 *   The original research group (Chinese processors) sees a Rope: they solve the genuine coordination problem of maintaining global rare earth supply amid environmental and technical constraints. The dependent manufacturers see a Snare: they face bottleneck pricing with no escape. The industrial policy coalition sees a Scaffold: temporary bottleneck with an explicit sunset (within 10-20 years, alternative processing capacity + recycling + substitution reduce dependence below critical levels). Mining-source countries see a hybrid Tangled Rope: coordination of extraction, processing, and environmental remediation alongside extraction of rents and geopolitical leverage. The analytical observer risks seeing a Mountain (rare earths are geologically scarce, therefore concentration is inevitable) but the structural data contradicts this — processing concentration is institutional policy (China's decades-long vertical integration strategy) not geological necessity. The false summit detector should flag this.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Chinese integrated producers, processing industry) derive d from institutional power + arbitrage exit options, producing low/negative chi. The institutional power means they can navigate the constraint; arbitrage options mean they can deploy capacity strategically. Victims (dependent manufacturers) derive d from powerless/trapped or organized/constrained — highest d values because exit is structurally difficult (decades of infrastructure buildout required). Mining-source countries occupy a hybrid position: institutional power but constrained exit (revenue dependency) produces moderate d. The Industrial Policy Coalition derives d from powerful/mobile — they have resources and exit options (alternative processing, recycling, substitution R&D), so experienced extraction is moderate despite high base extractiveness. The Scaffold classification emerges from this perspective's capacity to build exit pathways within a generational timescale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the coordination problem (managing global supply chains for geographically concentrated minerals) from the extraction mechanism (pricing leverage and supply cutoff coercion enabled by processing monopoly). The genuine coordination function exists and is valuable — Chinese integrated producers do solve a real problem of transforming raw rare earth ore into refined elements suitable for manufacturing. The extraction overlaid on top of this coordination is the asymmetric power to disrupt supply or raise prices unilaterally. The Tangled Rope classification captures both functions. The Scaffold classification from the industrial policy perspective is not misidentification of a Rope — it correctly identifies that there is a sunset mechanism (alternative pathways being built at continental scale) that will reduce the extraction mechanism's force over time. Mandatrophy is resolved: the constraint is not purely coordination (rule out Rope), not purely extraction (rule out Snare as primary), but a genuine hybrid with an identifiable exit pathway.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    processing_capacity_irreversibility,
    'Is the concentration of processing capacity in China irreversible at meaningful timescales, or can alternative processing infrastructure be built within a decade?',
    'Tracking of rare earth processing capacity buildout: Mountain Pass processing timeline, Vietnam processing plant operational capacity, EU processing investment ROI, recycling yield improvements. Compare projected timelines to manufacturing demand growth.',
    'If irreversible (10+ years): constraint remains Snare/Tangled Rope. If reversible (5-10 years): Industrial Policy Coalition''s Scaffold classification is validated and suppression should decline measurably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(processing_capacity_irreversibility, empirical, 'Whether processing capacity concentration is reversible at policy timescales').

omega_variable(
    substitution_technical_viability,
    'Can critical applications (permanent magnets in wind turbines, EV motors, military systems) substitute rare earth elements with alternative materials without unacceptable performance loss?',
    'Engineering feasibility studies; laboratory demonstration of substitute materials at scale; performance testing against military and industrial specifications; manufacturing cost comparison.',
    'If viable: dependent manufacturers have exit option via substitution, d decreases, classification shifts toward Rope. If not viable: trapped exit persists, extraction mechanism strengthens, Snare classification hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_technical_viability, empirical, 'Technical viability of rare earth substitution').

omega_variable(
    recycling_loop_closure,
    'Can closed-loop rare earth recycling from end-of-life electronics and batteries meet 20-30% of manufacturing demand within 10-15 years, reducing primary extraction dependency?',
    'Tracking of rare earth recovery rates from e-waste and battery recycling; cost comparison to primary extraction; throughput scaling from pilot to industrial capacity; technological barriers to purification.',
    'If achievable: Industrial Policy Coalition''s industrial recycling infrastructure accelerates sunset of suppression. If not achievable: primary extraction concentration remains structural and suppression increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_loop_closure, empirical, 'Feasibility of closing rare earth recycling loop').

omega_variable(
    geopolitical_leverage_weaponization,
    'Will rare earth supply concentration be explicitly weaponized as a geopolitical tool (supply cutoffs, pricing coercion, export restrictions) or will market mechanisms and industrial policy pressure prevent escalation?',
    'Historical pattern of supply disruptions: 2010 China export restrictions, 2020-2021 pricing volatility, sanctions-related supply impacts. Tracking of policy rhetoric, export licensing behavior, and supply shock frequency.',
    'If weaponized: suppression and theater_ratio increase, constraint hardens toward pure Snare. If market mechanisms dominate: suppression remains at current levels, scaffold exit pathways remain credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_leverage_weaponization, empirical, 'Whether rare earth supply is weaponized as geopolitical tool').

omega_variable(
    false_naturalization_diagnosis,
    'Is the rare earth supply concentration a geological necessity (mountain) or a contingent institutional arrangement of mining localization and processing centralization that could be reorganized?',
    'Historical comparison: rare earth distribution in 1970s (before Chinese dominance) vs. today; technical feasibility of distributed small-scale processing; policy counterfactuals (what if US had sustained processing investment post-1990).',
    'If contingent institution: mountain classification is false summit. If geological necessity: mountain reclassification. Current evidence suggests contingent — processing concentrated due to cost arbitrage and policy choices, not physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_naturalization_diagnosis, conceptual, 'Whether rare earth concentration is geological or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_supply_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resc_tr_t0, rare_earth_supply_concentration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(resc_tr_t5, rare_earth_supply_concentration, theater_ratio, 5, 0.32).
narrative_ontology:measurement(resc_tr_t10, rare_earth_supply_concentration, theater_ratio, 10, 0.35).
narrative_ontology:measurement(resc_tr_t15, rare_earth_supply_concentration, theater_ratio, 15, 0.38).

% Extraction over time
narrative_ontology:measurement(resc_be_t0, rare_earth_supply_concentration, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(resc_be_t5, rare_earth_supply_concentration, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(resc_be_t10, rare_earth_supply_concentration, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(resc_be_t15, rare_earth_supply_concentration, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_supply_concentration, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_supply_concentration, semiconductor_supply_concentration).
narrative_ontology:affects_constraint(rare_earth_supply_concentration, critical_minerals_dependency).
narrative_ontology:affects_constraint(rare_earth_supply_concentration, renewable_energy_supply_chain_resilience).

% DUAL FORMULATION NOTE:
% Rare earth supply concentration is structurally upstream of semiconductor supply concentration and renewable energy supply chain constraints. The three constraints form a critical materials dependency family. Rare earth processing concentration creates extraction pressure on dependent manufacturing sectors; semiconductor supply concentration operates through similar mechanisms (Taiwan concentration, lithography equipment monopoly). All three are tangled_rope constraints with industrial policy scaffolds. Decompose if analyzing sector-specific impacts (rare earths for magnets vs. phosphors vs. catalysts each have distinct supply profiles and substitution pathways).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_supply_concentration, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
