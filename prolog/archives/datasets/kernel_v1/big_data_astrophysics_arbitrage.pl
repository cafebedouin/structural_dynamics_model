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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Big Data Arbitrage in Modern Time-Domain Astrophysics
 *   domain: technological/astronomy
 *
 * SUMMARY:
 *   Modern time-domain astronomy generates petabyte-scale data streams from
 *   distributed surveys (LSST, Pan-STARRS, ZTF, Zwicky Transient Facility).
 *   This data is publicly released, but access to scientific value is gated
 *   by computational infrastructure and machine learning expertise.
 *   Well-resourced institutions with capital for real-time processing
 *   pipelines and ML talent can identify high-value discoveries (supernovae,
 *   kilonovae, Fast Radio Bursts) within hours; resource-limited observers
 *   and developing-nation programs access the same data weeks or months
 *   later. This creates a structural arbitrage: discovery priority—and the
 *   citation and funding advantages that follow—flows toward institutions
 *   with computational capital. The constraint exhibits genuine coordination
 *   function (data openness enables follow-up science, international
 *   collaboration, constraint reduction through shared pipelines) alongside
 *   asymmetric extraction (discovery lag creates systematic advantage for
 *   well-resourced actors). This makes it a canonical Tangled Rope from
 *   multiple perspectives. The low theater ratio (0.38) reflects that the
 *   bottleneck is functionally real computation, not ritual—resource-limited
 *   observers genuinely cannot process petabyte streams with limited
 *   infrastructure. The rising extractiveness trajectory (0.42 → 0.58) tracks
 *   increasing data volume and complexity, which amplifies the infrastructure
 *   advantage. Suppression is structural and rising: the barriers to entry
 *   include capital for computation, talent for ML engineering, and
 *   institutional network access for follow-up observing time.
 *
 * KEY AGENTS:
 *   - Well-Resourced Institutions: Primary beneficiary (institutional/arbitrage) — captures discovery priority and citation advantage through infrastructure investment; experiences constraint as coordination problem
 *   - Resource-Limited Observers: Primary victim (powerless/trapped) — face computational barriers structurally; cannot exit the field or access discoveries in real-time; no material alternatives
 *   - Developing-Nation Programs: Secondary victim (moderate/constrained) — benefit from open data access and international collaboration but face discovery lag and computational barriers that constrain career advancement
 *   - Public Survey Institutions (LSST, Pan-STARRS, ZTF): Institutional actors (institutional/constrained) — manage dual mandate: release data openly while managing finite processing resources; enforce alert prioritization through infrastructure choices
 *   - Open-Data Coalition: Organized actors (organized/constrained) — building shared pipelines, cloud platforms, and democratized analysis tools; see current arbitrage as temporary with sunset clause
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent infrastructure distribution as inherent law of data processing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, 0.58).
domain_priors:suppression_score(big_data_astrophysics_arbitrage, 0.62).
domain_priors:theater_ratio(big_data_astrophysics_arbitrage, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(big_data_astrophysics_arbitrage, tangled_rope).
narrative_ontology:human_readable(big_data_astrophysics_arbitrage, "Big Data Arbitrage in Modern Time-Domain Astrophysics").
narrative_ontology:topic_domain(big_data_astrophysics_arbitrage, "technological/astronomy").

domain_priors:requires_active_enforcement(big_data_astrophysics_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, well_resourced_institutions).
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, ml_talent_centers).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, resource_limited_observers).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, developing_nation_programs).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, discovery_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-LIMITED OBSERVER (SNARE) — Faces petabyte-scale data streams that are structurally inaccessible without capital investment in computation, storage, and ML expertise. No viable alternative to the publicly-funded survey data; cannot walk away from the field. Extraction is maximal: competitors with infrastructure access high-value discoveries (supernovae, kilonovae) within hours; this observer sees the same alerts weeks or months later, after discovery priority is already claimed. Suppression is structural: computational barriers are material and enforced by capital requirements.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING-NATION PROGRAM (TANGLED_ROPE) — Benefits from public data access and international collaboration networks (coordination function); also faces extraction through discovery lag and computational barriers. Can constrain costs through cloud-computing partnerships and open-source software, but faces currency exchange barriers and limited compute budgets. Genuine asymmetric extraction: gets real coordination benefits from data openness but bears disproportionate cost of computational latency.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELL-RESOURCED INSTITUTION (ROPE) — Sees the data stream as a pure coordination mechanism. Has capital for infrastructure investment, ML talent recruitment, and real-time processing pipelines. Experiences the constraint as a coordination problem to solve: collecting, processing, and publishing data enables follow-up observations and cross-institutional collaboration. Net beneficiary — discovery priority flows toward this agent through infrastructure advantage.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-DATA COALITION (SCAFFOLD) — Organized agents (LSST, Pan-STARRS, international astronomy collaborations) are building shared processing pipelines, cloud-based analysis platforms, and democratized alert systems to reduce computational barriers. See the current arbitrage window as a temporary coordination problem with a sunset: as infrastructure costs drop and open-source algorithmic tools mature, the latency gap for resource-limited observers will shrink. Theater is low (0.38) — the constraint is functionally real computational bottleneck, not ritualistic.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC SURVEY INSTITUTION (TANGLED_ROPE) — LSST, Pan-STARRS, ZTF face conflicting mandates: release data openly (coordination function) while managing finite computational resources (enforcement need). They benefit from data-release norms that attract follow-up science; they also enforce discovery advantage by controlling alert distribution and real-time processing. Active enforcement: deciding who gets real-time alerts, which queries get priority processing, which follow-up data streams are available immediately vs delayed. Genuine hybrid: coordination (open-data mandate) + asymmetric extraction (infrastructure bottleneck creates discovery lag).
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, computational latency is an inherent property of data pipelines: petabyte-scale streams always require specialized infrastructure to extract signals, and the gap between data generation and insight is a structural feature of the scientific process. This perspective risks naturalizing what is actually a contingent infrastructure arrangement (capital availability, ML talent concentration) as an immutable law. Engine false summit detector will flag this as naturalization.
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
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(big_data_astrophysics_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from resource-limited observers through discovery lag and priority capture. However, it is not maximal (not a pure snare) because genuine coordination function exists: public data release, international collaboration norms, and shared infrastructure development create real benefits for all participants. The extraction is structured around unequal timing access, not denial of access. Suppression (0.62): High. Barriers to entry are material: petabyte-scale computational infrastructure requires capital investment; ML expertise concentration is real; real-time data processing requires specialized engineering. However, suppression is not total because open-data policies prevent complete exclusion—the barrier is latency and cost, not denial. Theater ratio (0.38): Low. The computational bottleneck is functionally real, not ritualistic. Resource-limited observers literally cannot process petabyte streams without specialized infrastructure. Unlike some institutional constraints, the theater here reflects genuine technical constraint, not performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The well-resourced institution sees Rope—they are solving a legitimate coordination problem of processing and distributing petabyte-scale data. The resource-limited observer sees Snare—they are trapped with data they cannot timely exploit and no material exit. The developing-nation program sees Tangled Rope—they benefit from open data and collaboration but face extraction through computational latency. The open-data coalition sees Scaffold—they are building infrastructure to reduce the arbitrage window, with a sunset clause contingent on cost curves and open-source tool maturation. The public survey institution sees Tangled Rope—managing the genuine coordination function (data release, follow-up enabling) while being forced by finite resources to enforce discovery advantage. The civilizational analytical observer risks seeing Mountain—treating computational latency as an immutable property of large-scale data processing. The perspectival gaps are real structural differences, not mere disagreement: the well-resourced institution structurally benefits; the resource-limited observer structurally bears cost; the coalition structurally works to reduce the arbitrage.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) derives from their position relative to the computational arbitrage flow. Well-resourced institutions are beneficiaries with arbitrage exit options—they can shift computational investment elsewhere, access multiple data streams, or collaborate with competitors. This produces low d and negative χ: they experience the constraint as a coordination mechanism they control. Resource-limited observers are victims with trapped exit options—they cannot invest in infrastructure, cannot access compute elsewhere at scale, and cannot exit the field. This produces high d and high χ: they experience maximal extraction. Developing-nation programs are partial victims with constrained exit—they can work with international partners and use open-source tools to reduce costs, but face currency and capital constraints. This produces moderate-high d and moderate χ. The coalition is organized and constrained—they have collective agency and see an exit path through infrastructure maturation, but current activities are labor-intensive and require sustained commitment. The public survey institutions are beneficiaries but constrained—they benefit from data-release norms that attract follow-up science, but are constrained by finite computing budgets. The suppression metric (0.62) is unscaled and structural—it reflects the actual barriers to real-time data access across all contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing extraction (discovery priority capture through infrastructure advantage) from coordination (data openness, collaboration norms, shared tool development). The Tangled Rope classification certifies that both mechanisms are present: genuine coordination function (verifiable through collaboration networks and publication patterns) AND asymmetric extraction (verifiable through discovery-lag statistics and citation advantage for fast-turnaround institutions). The mandatrophy is resolved by recognizing that the constraint can be reformed—open-source algorithm development, cloud-computing cost reduction, and shared processing pipelines can flatten the arbitrage without losing the coordination benefits. The scaffold perspective documents this: organized actors are working on the exit path. The false summit risk is that the civilizational observer naturally law-izes the latency ('it's just how data processing works') when it is actually contingent on capital distribution and infrastructure design. The omega variables document the specific levers: capital concentration trajectory, algorithm portability, alert-system design choices. The constraint is not immutable; it is a structural feature of current institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_concentration_trajectory,
    'Will ML infrastructure and computational power continue to concentrate in well-resourced institutions, or will cost-reduction curves and open-source tool maturation flatten the bottleneck?',
    'Historical tracking of cloud computing costs, GPU availability in developing regions, adoption rates of open-source pipelines (AstroML, Palomar inference systems), and discovery-lag statistics over 5-10 year intervals',
    'If concentration persists: constraint remains Snare/Tangled Rope. If flattening occurs: constraint transitions toward Rope/Scaffold. The ''sunset'' of the scaffold depends entirely on this trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_concentration_trajectory, empirical, 'Whether computational barriers will persist or flatten over time').

omega_variable(
    discovery_priority_legitimacy,
    'Is the discovery advantage captured by fast-turnaround institutions a fair reward for infrastructure investment, or an extractive mechanism that contradicts the scientific norm of open data access?',
    'Analysis of publication patterns: do fast-turnaround discoveries receive disproportionate citation and career reward? Do resource-limited researchers gain equal scientific credit through follow-up work? Survey of astronomy community norms on ''first detection'' vs ''first confirmation''.',
    'If fair reward: perceived extraction decreases (Tangled Rope → Rope from many perspectives). If extractive: perceived extraction increases (Tangled Rope → Snare from more perspectives). This is a value judgment with structural consequences for institutional behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discovery_priority_legitimacy, preference, 'Whether discovery advantage is fair reward or extractive mechanism').

omega_variable(
    algorithmic_portability,
    'How transferable are the ML algorithms and signal-detection pipelines developed by well-resourced institutions? Can they be democratized as open-source tools that reduce the technical barrier for resource-limited observers?',
    'Evaluation of open-source pipeline maturity, documentation quality, training requirements, and adoption rates among developing-nation astronomy programs. Correlation between pipeline release and discovery-lag reduction.',
    'If algorithms port well and are open-sourced: technical barrier drops, latency advantage shrinks (Snare → Tangled Rope). If algorithms remain proprietary or require deep ML expertise: technical barrier persists, latency advantage stays (Snare persists). This is the primary lever for the scaffold perspective''s sunset clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_portability, empirical, 'Whether ML algorithms can be effectively open-sourced and democratized').

omega_variable(
    alert_system_design_choice,
    'Is the latency gap between real-time and delayed alerts a technical necessity or a design choice by survey institutions? Could alerts be democratized (simultaneous worldwide distribution) at higher infrastructure cost?',
    'Technical analysis of alert generation pipelines; cost modeling for simultaneous global distribution; examination of historical design decisions in LSST, Pan-STARRS, ZTF alert architecture.',
    'If design choice: the constraint is partly manufactured (could be reformed without losing coordination function). If technical necessity: the constraint is more inherent. This affects how we classify the extractive mechanism (intentional vs structural).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alert_system_design_choice, empirical, 'Whether discovery latency is technical necessity or design choice').

omega_variable(
    false_summit_natural_law,
    'Is the discovery latency an immutable property of large-scale data processing (natural law), or a contingent outcome of capital concentration, infrastructure distribution, and ML talent clustering?',
    'Comparison with alternative coordination models: if data processing were fully decentralized, would latency persist? If ML infrastructure were universally accessible, would discovery advantage remain? Counterfactual analysis of infrastructure redesign scenarios.',
    'If natural law: mountain classification is correct. If contingent institutional arrangement: mountain is a false summit, reclassified to Tangled Rope or Snare. This determines whether the constraint is immutable or reformable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether discovery latency is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(big_data_astrophysics_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bigdata_tr_t0, big_data_astrophysics_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bigdata_tr_t3, big_data_astrophysics_arbitrage, theater_ratio, 3, 0.37).
narrative_ontology:measurement(bigdata_tr_t6, big_data_astrophysics_arbitrage, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(bigdata_be_t0, big_data_astrophysics_arbitrage, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bigdata_be_t3, big_data_astrophysics_arbitrage, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(bigdata_be_t6, big_data_astrophysics_arbitrage, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bigdata_su_t0, big_data_astrophysics_arbitrage, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bigdata_su_t3, big_data_astrophysics_arbitrage, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(bigdata_su_t6, big_data_astrophysics_arbitrage, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(big_data_astrophysics_arbitrage, resource_allocation).
narrative_ontology:boltzmann_floor_override(big_data_astrophysics_arbitrage, 0.18).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, transient_discovery_priority_cascade).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, ml_talent_concentration).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, cloud_computing_access_equity).

% DUAL FORMULATION NOTE:
% Big data arbitrage in astrophysics decomposes into three structurally linked constraints: (1) computational latency bottleneck (this story, ε=0.58, Tangled Rope); (2) discovery priority cascade (downstream, higher ε, pure extraction mechanism); (3) ML talent concentration in well-resourced institutions (upstream, affects computational infrastructure distribution). This story addresses the immediate arbitrage mechanism. Upstream story (talent concentration) creates the capital gradient that enables downstream story (discovery priority capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(big_data_astrophysics_arbitrage, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
