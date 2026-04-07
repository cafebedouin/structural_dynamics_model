% ============================================================================
% CONSTRAINT STORY: big_data_astrophysics_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_big_data_astrophysics_arbitrage, []).

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
 *   constraint_id: big_data_astrophysics_arbitrage
 *   human_readable: Big Data Arbitrage in Modern Astrophysics
 *   domain: technological/astronomy
 *
 * SUMMARY:
 *   Modern time-domain astronomy (LSST, Pan-STARRS, ZTF) generates
 *   petabyte-scale data streams requiring specialized computational
 *   infrastructure and algorithmic expertise to extract scientific value.
 *   This creates a structural arbitrage mechanism: institutions and
 *   researchers with capital and ML talent can access high-value discoveries
 *   (supernova candidates, kilonovae, Fast Radio Bursts) within hours;
 *   resource-limited observers and developing-nation programs access the same
 *   data months or years later, after embargo periods, with lower processing
 *   capacity, and often through derivative analyses. The constraint exhibits
 *   mixed character depending on observation point: pure snare for trapped
 *   small observatories; tangled rope for organized mid-tier institutions
 *   with consortium access; rope for data infrastructure operators; scaffold
 *   for open-science efforts building alternatives; piton for traditional
 *   observing committees whose role has degraded. Base extractiveness (0.58)
 *   reflects substantial asymmetry in data access, algorithmic gatekeeping,
 *   and career/funding concentration, but not total suppression—open-source
 *   pipelines (LSST public releases, ZTF forced photometry, AstroML tools)
 *   are creating alternative pathways. Suppression (0.68) is high: bandwidth
 *   costs, computational resource barriers, proprietary algorithm access, and
 *   publication embargo periods together lock smaller actors into constrained
 *   or trapped positions. Theater ratio (0.45) is relatively low because the
 *   real action is in automated alert systems and proprietary reductions, not
 *   in traditional peer-review decision-making, though classical observing
 *   proposals retain performative status.
 *
 * KEY AGENTS:
 *   - Data Infrastructure Operators (Caltech, observatories, cloud providers): Institutional/arbitrage beneficiaries — control computational and storage infrastructure that transforms raw data into discovery candidates
 *   - Machine Learning Specialists: Institutional/arbitrage beneficiaries — control algorithmic gatekeeping via neural networks and anomaly detection pipelines; trainable talent with commercial exit options
 *   - Resource-Limited Observatories: Powerless/trapped victims — cannot afford computational scaling or proprietary algorithm access; structurally excluded from time-domain discovery advantage
 *   - Developing Nation Astronomy Programs: Powerless/trapped victims — face bandwidth costs, computational resource barriers, and knowledge gaps; no domestic exit path without external partnership
 *   - Mid-Tier Research Institutions: Organized/constrained actors — benefit from consortium data access but face algorithmic constraints, embargo delays, and processing bottlenecks
 *   - Open-Source Astronomy Coalition (Rubin Observatory, LSST public engagement, AstroML): Organized/constrained actors — building alternative pipelines (public data releases, open algorithms) with sunset logic; constrained by funding and institutional inertia
 *   - Traditional Observing Committees: Institutional/arbitrage actors — maintain performative role in proposal review despite functional degradation by real-time data streams; persist through funding structure inertia
 *   - Analytical Observer: Civilizational/analytical — risks naturalizing contingent infrastructure asymmetries as immutable laws of big-data science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, 0.58).
domain_priors:suppression_score(big_data_astrophysics_arbitrage, 0.68).
domain_priors:theater_ratio(big_data_astrophysics_arbitrage, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(big_data_astrophysics_arbitrage, snare).
narrative_ontology:human_readable(big_data_astrophysics_arbitrage, "Big Data Arbitrage in Modern Astrophysics").
narrative_ontology:topic_domain(big_data_astrophysics_arbitrage, "technological/astronomy").

domain_priors:requires_active_enforcement(big_data_astrophysics_arbitrage).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, data_infrastructure_operators).
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, machine_learning_specialists).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, resource_limited_observatories).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, developing_nation_astronomers).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, small_research_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL OBSERVATORY (SNARE) — Locked into traditional observing modes or data access tiers. Cannot afford computational infrastructure or storage to compete in time-domain analysis. Bears full cost of data arbitrage through reduced competitiveness and funding loss to data-rich competitors.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION ASTRONOMY PROGRAMS (SNARE) — Structural barriers: bandwidth costs, computational resources, and proprietary data pipeline access. No exit from the data arbitrage system without external funding or partnership subordination. Excluded from high-value transient discovery and characterization.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER RESEARCH INSTITUTIONS (TANGLED ROPE) — Can access data through consortiums (LSST, ZTF) but face algorithmic constraints and processing delays. Benefits from data access and collaborative pipelines but also bears extraction through data-access tiers, embargo periods, and algorithmic opacity. Constrained exit: can partially build alternative pipelines but at significant opportunity cost.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DATA INFRASTRUCTURE OPERATORS (ROPE) — Primary beneficiary (Caltech, La Silla Paramounts, AWS-backed pipelines). Controls the computational and storage infrastructure that transforms raw observational data into actionable science. Experiences the constraint as coordination: data pipeline management, standardization, and distribution enable global time-domain astronomy. Arbitrage optionality: can switch data partners, negotiate terms, or commercialize algorithms.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MACHINE LEARNING SPECIALISTS (ROPE) — Secondary beneficiary. Controls algorithmic gatekeeping: neural networks, anomaly detection, classification pipelines that convert terabyte streams into discovery-ready candidates. Benefits from data volume and proprietary algorithm training. High arbitrage optionality: can license algorithms, shift to industry applications, or migrate to different observatories.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-SOURCE ASTRONOMY COALITION (SCAFFOLD) — Organized effort (Rubin Observatory mandate, AstroML, open-data initiatives) to create alternative verification and analysis pathways. Sees the data arbitrage as a temporary coordination failure with a sunset: public data release windows, open-source pipelines (ZTF Forced Photometry, LSST pipelines), and democratized ML tools reduce the extraction barrier. Theater ratio low because open-source alternatives bypass proprietary bottlenecks. Constrained exit: can build alternatives but within broader funding/policy constraints.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL OBSERVATIONAL PROTOCOLS (PITON) — Classical observing proposals, peer-reviewed time allocation, and publication-first data release norms are increasingly performative. The real science happens in real-time alert streams and proprietary reductions. The old institutional process persists through inertia (funding structures, career evaluation metrics) despite its functional degradation. Theater ratio high because formal observing committee decisions often ratify pre-existing data-pipeline discoveries.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a physics/information-theory perspective, terabyte-scale data streams inherently require specialized computational infrastructure; time-domain science inherently privileges those with processing capacity. The constraint appears as an immutable natural consequence of the data deluge. However, the structural data reveals this as a false summit: institutional choices (proprietary algorithms, access tiers, funding concentration) naturalizes what could be structured as open-source coordination.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(big_data_astrophysics_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(big_data_astrophysics_arbitrage, TR),
    TR >= 0.70.

:- end_tests(big_data_astrophysics_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The data arbitrage captures substantial value through time advantage (days to weeks for alert stream access vs months for public data release), algorithmic advantage (proprietary ML models vs open-source baselines), and computational advantage (real-time processing vs batch). But extraction is not maximal (0.70+) because: (1) public data releases and open-source tools provide real alternatives, not complete closure; (2) collaborative consortium access (LSST ZTF partnership models) offer constrained but genuine participation pathways; (3) simple algorithmic approaches (forced photometry, basic classification) can be replicated by resourced institutions. The trajectory (0.32 → 0.58 over 10 years) reflects the transition from traditional observing to real-time alert science; as time-domain discovery becomes central to astrophysics funding and hiring, the data infrastructure asymmetry has become more extractive. Suppression (0.68): High. Multiple reinforcing barriers lock smaller actors into constrained positions: (a) bandwidth and storage costs scale non-linearly with data volume; (b) proprietary algorithms (RealBogus, transient classifiers) lack open equivalents; (c) publication embargo periods (6-12 months for ZTF, LSST data release policy) enforce temporal gatekeeping; (d) career prestige concentrates on first discoveries; (e) funding allocation favors institutions with proven data infrastructure. Suppression is not total (0.80+) because escape routes exist: open-source pipelines, consortium partnerships, secondary science tracks. Theater ratio (0.45): Moderate-low. The classical observing proposal system persists (proposal deadlines, peer-review committees, time allocation) but is increasingly theatrical—the real discoveries happen in automated alert streams that bypass proposal review entirely. New-time-domain discoveries often pre-date the traditional data release that would have justified the original proposal. The ratio is lower than piton-level (0.70) because alert systems do provide real functional value, not just performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how indexical position determines the classification. Data infrastructure operators experience rope (coordination that enables their work). Small observatories experience snare (locked out of high-value discovery). Mid-tier institutions experience tangled rope (benefits from data access, extraction from embargo delays and algorithmic opacity). Open-science coalition experiences scaffold (temporary failure with sunset timeline). Observing committees experience piton (degraded role maintained by inertia). The false mountain perspective naturalizes institutional asymmetries as physical inevitabilities—'big data requires big infrastructure' sounds like a law of nature but is actually a choice about who controls the infrastructure and how algorithms are distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural position relative to the data arbitrage flow. Data infrastructure operators and ML specialists are beneficiaries with arbitrage optionality (can license algorithms, shift observatory partnerships, commercialize tools)—low d, negative experienced extraction (they capture value). Small observatories are victims with trapped exit (cannot build million-dollar computational infrastructure unilaterally, cannot train ML specialists at scale without collaboration)—high d, high experienced extraction. Mid-tier institutions are partial beneficiaries with constrained exit (can participate in consortiums but cannot unilaterally escape algorithmic gatekeeping or embargo periods)—intermediate d, moderate extraction. Developing-nation programs are victims with trapped-to-constrained exit (structural bandwidth barriers, no domestic computational capacity, knowledge gaps in proprietary tools)—very high d, high extraction. The open-science coalition is organized with constrained exit (can build alternatives but within funding/policy constraints)—intermediate d, experienced as mixed coordination-extraction. Traditional observing committees are institutional actors whose functional role has degraded but who retain positional arbitrage (prestige, funding authority)—low d but high theater because the arbitrage has shifted to algorithms outside their control.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH PARTIAL SCAFFOLD RESOLUTION: The base classification is Snare (ε=0.58, suppression=0.68, χ > 0.66 for trapped victims), but the mandate problem is complex. The data arbitrage meets snare criteria for small observatories and developing-nation programs: high extraction, suppression via bandwidth/algorithm access, and no self-correction mechanism (embargo periods and proprietary algorithms prevent learning or exit). However, the open-science coalition perspective reveals genuine structural change: public data release mandates (Rubin Observatory, LSST, ZTF), open-source algorithmic frameworks (AstroML, open photometry pipelines), and increasing pressure for democratized ML access are creating alternative pathways with real sunset logic. Mandatrophy is NOT fully resolved because: (1) even 'public' data often requires million-dollar compute clusters (barrier remains, just shifted); (2) algorithm feature parity timelines are uncertain (10-15 years vs 50+ years); (3) career prestige still concentrates on first discoveries. The constraint is transitional: currently a snare for powerless agents, but with measurable scaffold structure for organized agents building alternatives. If the open-source timeline achieves feature parity and funding structures shift toward equitable access, snare will degrade toward tangled rope (extraction with genuine coordination function) or rope (pure coordination around data standards). If proprietary infrastructure consolidates, snare hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_provenance_independence,
    'Are the highest-value algorithmic transformations (RealBogus classification, transient type prediction) sufficiently novel that proprietary training is necessary, or are they teachable commodities that would commoditize under open-source release?',
    'Comparative analysis of open-source vs proprietary algorithm performance on public datasets; skill distribution analysis of ML specialists trained in each paradigm; licensing data for algorithmic tools',
    'If algorithms are teachable commodities: open-source alternatives can dissolve the arbitrage within 3-5 years, confirming scaffold sunset. If truly novel and resource-limited: extraction barrier is structural, snare classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_provenance_independence, empirical, 'Whether proprietary algorithms are necessarily novel or teachable commodities').

omega_variable(
    bandwidth_infrastructure_equity,
    'What fraction of the extraction comes from raw bandwidth/storage costs vs algorithmic gatekeeping? If bandwidth were free, would the arbitrage collapse?',
    'Cost accounting for data pipeline infrastructure (compute, storage, bandwidth) vs algorithmic development; international bandwidth cost analysis; counterfactual modeling of free data access scenarios',
    'If bandwidth >> algorithms: developing-nation programs could exit by scaling infrastructure investment. If algorithms >> bandwidth: institutional asymmetry is harder to solve and snare classification is more structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bandwidth_infrastructure_equity, empirical, 'Relative contribution of bandwidth costs vs algorithmic control to data arbitrage').

omega_variable(
    discovery_latency_consequence,
    'Does the multi-month traditional publication + data release cycle for time-domain discoveries create genuine scientific consequence (lost confirmations, wrong follow-up targets) or is it primarily a priority/credit arbitrage?',
    'Historical analysis of follow-up success rates for early vs late discoveries; missed-opportunity analysis for transients; publication citation patterns for open vs embargoed datasets',
    'If consequence is scientific (lost transients, contaminated targets): snare extraction harms field outcomes, mandatrophy unresolved. If consequence is primarily credit/career: extraction is pure rent-seeking, mandatrophy may resolve toward pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discovery_latency_consequence, empirical, 'Whether publication embargo creates scientific consequence or primarily credit arbitrage').

omega_variable(
    coalition_sunset_timeline_achievability,
    'Given current funding trends and open-source ML momentum, can public data pipelines and open-source algorithms reach feature parity with proprietary systems within a realistic timeline (10-15 years)?',
    'Progress benchmarks for open-source ML frameworks (PyTorch, JAX) vs proprietary tools; funding analysis for open-science infrastructure; talent migration patterns from proprietary to open-source astrophysics',
    'If yes: scaffold perspective is structural, has real sunset. If no: open-source coalition is aspirational theater, and snare extraction persists. May require analysis of whether ''open'' really means ''requires million-dollar compute clusters'' = functional barrier remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_sunset_timeline_achievability, empirical, 'Feasibility of open-source algorithms reaching feature parity within 10-15 years').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(big_data_astrophysics_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bdaa_tr_t0, big_data_astrophysics_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bdaa_tr_t5, big_data_astrophysics_arbitrage, theater_ratio, 5, 0.4).
narrative_ontology:measurement(bdaa_tr_t10, big_data_astrophysics_arbitrage, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(bdaa_be_t0, big_data_astrophysics_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bdaa_be_t5, big_data_astrophysics_arbitrage, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(bdaa_be_t10, big_data_astrophysics_arbitrage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(big_data_astrophysics_arbitrage, information_standard).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, gravitational_wave_electromagnetic_follow_up).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, pulsar_population_inference_bias).

% DUAL FORMULATION NOTE:
% The big data arbitrage is upstream of specific astrophysics discoveries (gravitational wave follow-up, pulsar timing inference) whose outcomes depend on access to alert streams and algorithmic processing power. The upstream constraint (data infrastructure control) structurally determines which populations and phenomena get studied (selection bias). Each specific discovery domain has its own extractiveness reflecting empirical status, but all are downstream of this data arbitrage gate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(big_data_astrophysics_arbitrage, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
