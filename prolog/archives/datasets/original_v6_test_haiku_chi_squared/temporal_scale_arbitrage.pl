% ============================================================================
% CONSTRAINT STORY: temporal_scale_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_scale_arbitrage, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_scale_arbitrage
 *   human_readable: Temporal Scale Arbitrage in Astronomy
 *   domain: technological/observational_astronomy
 *
 * SUMMARY:
 *   Astronomy observes cosmic events across an enormous range of timescales:
 *   gravitational waves merge in milliseconds, X-ray transients flare in
 *   seconds, optical supernovae brighten in days, accretion state changes
 *   occur over weeks, orbital variations span months to years. The discovery
 *   and characterization of these phenomena require coordinated observations
 *   across multiple facilities, instruments, and wavebands. Temporal scale
 *   arbitrage emerges when the institutions controlling alert distribution
 *   and real-time observation scheduling exploit the time-domain mismatch to
 *   extract asymmetric benefits. Well-resourced observatories with automated
 *   pipelines, real-time alert integration, and flexible scheduling capture
 *   priority access to transients. Under-resourced observatories and amateur
 *   astronomers, locked into traditional proposal cycles and reactive
 *   observation modes, cannot participate in the rapid discovery race. The
 *   constraint exhibits characteristics of both coordination (genuine need
 *   for multi-wavelength follow-up) and extraction (asymmetric control over
 *   alert distribution and scheduling priority). The theater ratio has
 *   increased over the 30-year interval as automated surveys have
 *   proliferated—traditional proposal-based observation now appears largely
 *   performative when rapid transients demand real-time scheduling.
 *
 * KEY AGENTS:
 *   - Well-Resourced Observatories: Primary beneficiary (institutional/arbitrage) — automated pipelines, real-time alert integration, flexible scheduling enable rapid follow-up and publication priority
 *   - Institutional Time-Domain Survey Campaigns: Primary beneficiary (institutional/arbitrage) — own alert streams, control follow-up coordination, capture publication priority and prestige
 *   - Amateur Astronomers: Primary victim (powerless/trapped) — locked out by equipment requirements, alert system access barriers, cannot compete with automated networks
 *   - Under-Resourced Observatories: Secondary victim (moderate/constrained) — regional facilities with limited bandwidth, obsolete instrumentation, cannot integrate with real-time alert systems
 *   - Follow-Up Observation Community: Hybrid (organized/constrained) — benefit from coordinated alerts enabling multi-wavelength science; constrained by survey-controlled alert distribution
 *   - Alert Distribution Networks: Institutional coordinator (institutional/arbitrage) — GCN, TNS, ZTF alert infrastructure; genuinely coordinate observations but extract prestige and priority
 *   - Time-Domain Science Community: Collective beneficiary (institutional/arbitrage) — benefit from network effects of coordinated observation; low extraction cost because alternative (isolated observations) produces worse science
 *   - Traditional Proposal-Driven Model: Institutional actor (institutional/arbitrage) — maintains six-month planning cycles despite transient timescales; persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_scale_arbitrage, 0.52).
domain_priors:suppression_score(temporal_scale_arbitrage, 0.68).
domain_priors:theater_ratio(temporal_scale_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_scale_arbitrage, extractiveness, 0.52).
narrative_ontology:constraint_metric(temporal_scale_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(temporal_scale_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_scale_arbitrage, tangled_rope).
narrative_ontology:human_readable(temporal_scale_arbitrage, "Temporal Scale Arbitrage in Astronomy").
narrative_ontology:topic_domain(temporal_scale_arbitrage, "technological/observational_astronomy").

domain_priors:requires_active_enforcement(temporal_scale_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_scale_arbitrage, well_resourced_observatories).
narrative_ontology:constraint_beneficiary(temporal_scale_arbitrage, time_domain_survey_campaigns).
narrative_ontology:constraint_beneficiary(temporal_scale_arbitrage, institutional_astronomers).
narrative_ontology:constraint_victim(temporal_scale_arbitrage, transient_discovery_equity).
narrative_ontology:constraint_victim(temporal_scale_arbitrage, small_resource_observatories).
narrative_ontology:constraint_victim(temporal_scale_arbitrage, amateur_astronomers).
narrative_ontology:constraint_victim(temporal_scale_arbitrage, follow_up_observations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMATEUR ASTRONOMER (SNARE) — Locked out of transient discovery by equipment and scheduling constraints. Cannot access real-time alert systems, cannot compete with automated survey networks, cannot negotiate telescope time. d≈0.93, f(d)≈1.38, σ=1.2 → χ≈0.76. Pure extraction: commitment without control.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDER-RESOURCED OBSERVATORY (SNARE) — Regional facility with limited bandwidth, no automated alert integration, antiquated instrumentation. Constrained by capital costs of real-time infrastructure. d≈0.82, f(d)≈1.22, σ=0.9 → χ≈0.54. Cannot exit observation networks; bears extraction cost of being too slow to participate in rapid transients.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL SURVEY OBSERVATORY (TANGLED ROPE) — Coordinates multi-temporal follow-up campaigns (optical, infrared, X-ray, radio across hours to months). Genuinely solves collective action: alerts enable synergistic observations that no single facility could achieve. Also extracts: survey owns the alert stream, sells access to follow-up observatories, captures priority for publication. d≈0.55, f(d)≈0.72, σ=1.0 → χ≈0.37. Hybrid: coordination infrastructure that captures asymmetric rents.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ALERT DISTRIBUTION NETWORK (ROPE) — Rapid institutional coordination: Gamma-ray Burst Coordinates Network (GCN), Transient Name Server (TNS), Zwicky Transient Facility (ZTF) alert stream. Solves the genuine coordination problem of getting rapid follow-up observations. Benefits from network effects (more observers → better science). d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary: orchestrates coordination, captures prestige and priority but low extraction cost.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOLLOW-UP OBSERVATION COMMUNITY (TANGLED ROPE) — Organized astronomers (radio, X-ray, infrared specialties) benefit from coordinated alerts enabling science that wouldn't exist without transient discovery alerts. Also constrained: must work through survey-controlled alert systems, must schedule around survey priorities, must publish on survey timelines. d≈0.60, f(d)≈0.82, σ=1.1 → χ≈0.47. Hybrid: genuine coordination but under asymmetric control.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TIME-DOMAIN SCIENCE COMMUNITY (ROPE) — Astronomers collectively benefit from the infrastructure enabling multi-timescale observation (optical to X-ray, milliseconds to years). The transient discovery ecosystem creates a genuine scientific commons. d≈0.12, f(d)≈-0.05, σ=1.2 → χ≈-0.06. Net beneficiary from network effects; extraction is low because the alternative (isolated observations) produces worse science for everyone.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL PROPOSAL-DRIVEN MODEL (PITON) — Six-month planning cycles, semester-long schedules, reactive observations only after published discoveries. Theater_ratio=0.58 reflects the substantial performative component: proposals justify timescale choices theoretically, but actual discoveries rarely follow predicted timescales. The proposal ritual persists despite being mismatched to transient timescales — maintained through institutional inertia rather than functional necessity. Survey-driven models are replacing it, but slowly.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, the multivariate timescale of cosmic phenomena is an irreducible feature of the universe: gravitational waves merge in milliseconds, supernovae brighten in days, accretion states evolve over years, orbital periods span decades. The constraint might appear to be an immutable property of observation itself. However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts this — the bottleneck is institutional (capital barriers to real-time infrastructure, alert system monopoly) not physical. False summit detection: the 'natural' timescale mismatch is actually a contingent coordination failure.
constraint_indexing:constraint_classification(temporal_scale_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_scale_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_scale_arbitrage, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_scale_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_scale_arbitrage, TR),
    TR >= 0.70.

:- end_tests(temporal_scale_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through capital barriers (automated infrastructure is expensive), alert system monopoly (survey campaigns own the alert stream), and scheduling priority (well-resourced facilities get preferential access). However, extraction is not maximal because: (1) open-access alert networks (GCN, TNS) provide some transparency, (2) amateur discoveries still occur (though rare), (3) multi-wavelength coordination genuinely creates new science. The value reflects honest hybrid: some extraction, genuine coordination. Suppression (0.68): Moderate-high. Significant barriers to alternative transient discovery paths include: (1) capital costs of real-time infrastructure, (2) proprietary alert system access for major surveys, (3) proposal-cycle mismatch with transient timescales, (4) publication bias favoring well-resourced coordinated observations. But suppression is not total: open-access alert networks reduce barriers, amateur discoveries occur, distributed observation networks are emerging. Theater ratio (0.58): Moderate. The traditional proposal-driven observation model is substantially performative—proposals justify timescale choices theoretically, but actual transient timescales rarely match predicted schedules. However, the theater is not overwhelming: the Real-time scheduling for GCN/TNS alerts has low theater; the value reflects the mixed landscape where automated survey pipelines reduce theater while traditional observatories maintain proposal rituals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence. Amateur astronomers see pure extraction (Snare)—locked out entirely. Under-resourced observatories see extraction (Snare)—too slow to compete. Survey campaigns see coordination (Rope/Tangled Rope)—genuinely solving multi-wavelength follow-up. Follow-up observation community sees hybrid constraint (Tangled Rope)—benefits from alerts but constrained by survey control. Alert distribution networks see pure coordination (Rope)—orchestrating necessary collaboration. Time-domain science community sees coordination (Rope)—benefits from network effects. Traditional proposal-driven observatories see degraded ritual (Piton)—theater ratio 0.58 indicates proposal justifications are mismatched to reality. Analytical observer risks false summit (Mountain)—temptation to naturalize timescale mismatch as inherent to astronomy. The perspectival range is extreme because the constraint operates through institutional asymmetry (capital barriers, alert system monopoly) masquerading as immutable timescale physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Amateur astronomers: Victim + trapped → d≈0.93, f(d)≈1.38. Maximum extraction—no alternatives, no control. Under-resourced observatories: Victim + constrained → d≈0.82, f(d)≈1.22. High extraction—can exit through capital investment (expensive), cannot exit through discovery access. Survey campaigns: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary—own the alert stream, can arbitrage to other institutions. Alert networks: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary—coordinate collaboration, low cost. Follow-up community: Hybrid victim/beneficiary + constrained → d≈0.60, f(d)≈0.82. Mixed—benefit from alerts, constrained by survey control. Time-domain community: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary—benefit from network effects. Traditional observatories: Neutral + arbitrage but degraded → d≈0.08, f(d)≈-0.09, but piton gate (theater≥0.70 fails here, theater=0.58) indicates institutional inertia drives persistence, not genuine benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE TENSION: The constraint genuinely solves a coordination problem (multi-wavelength follow-up of transients is intrinsically valuable and requires coordination) AND genuinely extracts rents (survey campaigns control alerts, amateur/small observatories are locked out). This is the classical Tangled Rope: neither pure coordination nor pure extraction, but honest hybrid. The mandatrophy resolves by recognizing that extraction does NOT invalidate the coordination function—ZTF, GCN, TNS genuinely enable science that wouldn't exist in isolation. The constraint would persist even if extraction were eliminated. The mandatrophy test: 'Would this constraint exist if extraction were zero?' YES—multi-wavelength transient follow-up would still require coordination. 'Is the coordination function genuinely reducing relative to extractiveness?' Partially yes—automated pipelines and real-time alert systems are increasing efficiency (reducing coordination overhead), but capital barriers and alert system monopoly are simultaneously increasing extraction barriers. The measurement data (theater_ratio rising from 0.32 to 0.58, extractiveness rising from 0.28 to 0.52) shows both metrics increasing, which indicates growing asymmetry: the coordination infrastructure is becoming more powerful (more transients discovered, faster follow-up), but the extraction mechanism is also strengthening (capital barriers are higher, survey control is tighter). The constraint is in the hybrid zone and trending toward higher extraction relative to pure coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_feasibility_ceiling,
    'What fraction of transient discovery can be automated without human expert validation, and at what latency threshold does human bottleneck become dominant?',
    'Empirical measurement of false positive rates in fully automated pipelines (ZTF, ATLAS) vs expert-filtered subsamples; correlation of pipeline latency vs discovery probability; cost-benefit analysis of automated false positives',
    'If ceiling > 95% at <1 minute latency: automation removes extraction mechanism entirely, transitions constraint from Tangled Rope to pure Rope. If ceiling < 70% at >10 minute latency: expert bottleneck becomes primary, maintains extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automation_feasibility_ceiling, empirical, 'Automation capability ceiling for transient discovery').

omega_variable(
    follow_up_coordination_necessity,
    'Is the survey-to-follow-up coordination genuinely solving a collective action problem, or is it primarily a monopoly mechanism for controlling alert distribution?',
    'Counterfactual analysis: compare multi-wavelength science outcomes in fields with open-access alert systems vs proprietary systems; measure publication rates and discovery completeness under different coordination architectures',
    'If genuinely solving coordination: constraint remains Tangled Rope (honest hybrid). If primarily monopoly: constraint degrades to Snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(follow_up_coordination_necessity, empirical, 'Whether survey coordination is solving genuine collective action or extracting monopoly rents').

omega_variable(
    capital_amortization_timescale,
    'Over what discovery-rate improvement period do infrastructure capital costs amortize for well-resourced institutions, and does this timeline create systematic unfairness?',
    'Cost-benefit analysis of real-time infrastructure investments (cloud computing, automated pipelines, alert system maintenance); correlation of institution size with cost-per-discovery and discovery-per-investment returns',
    'If amortization timescale < 2 years for large institutions but > 10 years for small institutions: systematic unfairness is structural and persists. If timescales converge: market-driven reduction in extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_amortization_timescale, empirical, 'Infrastructure capital amortization timeline by institution size').

omega_variable(
    natural_law_vs_contingent_arrangement,
    'Is the timescale mismatch between transient phenomena and observation planning cycles an inherent feature of astronomy or a contingent institutional design choice?',
    'Historical analysis of observational capability evolution; comparison across different coordinate systems (optical surveys, gravitational wave networks, neutrino detectors); identification of coordination successes (e.g., GCN for GRBs) vs failures',
    'If inherent: mountain classification is correct, no intervention possible, constraint is immutable. If contingent: constraint is Tangled Rope or Snare, and policy intervention (open-access alerts, real-time scheduling, distributed observation networks) can reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_arrangement, conceptual, 'Whether timescale mismatch is inherent or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_scale_arbitrage, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsa_tr_t0, temporal_scale_arbitrage, theater_ratio, 0, 0.32).
narrative_ontology:measurement(tsa_tr_t15, temporal_scale_arbitrage, theater_ratio, 15, 0.45).
narrative_ontology:measurement(tsa_tr_t30, temporal_scale_arbitrage, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(tsa_be_t0, temporal_scale_arbitrage, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tsa_be_t15, temporal_scale_arbitrage, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(tsa_be_t30, temporal_scale_arbitrage, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_scale_arbitrage, information_standard).
narrative_ontology:affects_constraint(temporal_scale_arbitrage, gravitational_wave_alert_latency).
narrative_ontology:affects_constraint(temporal_scale_arbitrage, multi_messenger_observation_bottleneck).
narrative_ontology:affects_constraint(temporal_scale_arbitrage, survey_observation_time_allocation).

% DUAL FORMULATION NOTE:
% Temporal scale arbitrage is downstream of the multi-messenger observation ecosystem but represents a distinct structural constraint on discovery equity. The upstream constraints (gravitational wave latency, multi-messenger coordination) have their own ε values reflecting technical/infrastructural properties; temporal scale arbitrage has ε=0.52 reflecting the institutional control of alert distribution and scheduling priority. These constraints are linked through network effects: faster alerts enable larger follow-up networks, which increases extraction barriers for under-resourced observatories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
