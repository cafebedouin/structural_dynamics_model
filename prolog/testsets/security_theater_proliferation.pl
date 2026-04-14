% ============================================================================
% CONSTRAINT STORY: security_theater_proliferation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_security_theater_proliferation, []).

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
 *   constraint_id: security_theater_proliferation
 *   human_readable: Security Theater Proliferation Across Institutional Systems
 *   domain: institutional_governance/security_systems
 *
 * SUMMARY:
 *   Security theater proliferation describes the institutional dynamic where
 *   security measures persist and expand primarily for their symbolic and
 *   legitimacy-signaling function rather than their actual threat-prevention
 *   capability. This constraint operates across transportation systems (TSA
 *   screening), border control, corporate cybersecurity, institutional access
 *   control, and government facilities. The phenomenon exhibits a distinctive
 *   signature: theater measures accumulate over time as institutional
 *   responses to threat narratives; each measure justifies its continuation
 *   through threat prevention claims even after threat models shift; the
 *   combination creates suppressive burden on trapped populations while
 *   concentrating benefits among security apparatus and compliance vendors.
 *   Theater ratio (0.78) reflects that most security measures are evaluable
 *   primarily by visibility (we can see screening happening) rather than
 *   outcome (detectable prevented incidents). The extractiveness value (0.58)
 *   indicates moderate-to-high extraction overlaid on genuine coordination
 *   function: institutions genuinely need security architecture, but
 *   theatrical measures extract excess compliance costs and dignity costs
 *   beyond what genuine threat prevention requires. The constraint
 *   demonstrates how institutional coordination problems can be solved
 *   through extraction-based theater rather than efficiency-based function.
 *
 * KEY AGENTS:
 *   - General Population (Powerless/Trapped): Bears full suppression cost (time, dignity, privacy); no exit option; no security benefit
 *   - Business Sector (Moderate/Constrained): Mandatory compliance costs; benefits from liability protection and regulatory compliance appearance; can exit with significant disruption
 *   - Security Apparatus (Institutional/Arbitrage): Primary beneficiary; controls budget and mandate; arbitrages across institutional contexts; zero suppression experienced
 *   - Compliance Vendor Ecosystem (Institutional/Arbitrage): Secondary beneficiary; profits from expanding security systems; arbitrages across sectors
 *   - Privacy-Conscious Coalition (Organized/Mobile): Building alternative paradigms; perceives sunset clause; can exit to alternative security models
 *   - Institutional Leadership (Institutional/Constrained): Caught between genuine security requirements and political pressure to demonstrate responsiveness; theater provides visible compliance but locks institutions into escalating measures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(security_theater_proliferation, 0.58).
domain_priors:suppression_score(security_theater_proliferation, 0.65).
domain_priors:theater_ratio(security_theater_proliferation, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(security_theater_proliferation, extractiveness, 0.58).
narrative_ontology:constraint_metric(security_theater_proliferation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(security_theater_proliferation, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(security_theater_proliferation, tangled_rope).
narrative_ontology:human_readable(security_theater_proliferation, "Security Theater Proliferation Across Institutional Systems").
narrative_ontology:topic_domain(security_theater_proliferation, "institutional_governance/security_systems").

domain_priors:requires_active_enforcement(security_theater_proliferation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(security_theater_proliferation, security_apparatus).
narrative_ontology:constraint_beneficiary(security_theater_proliferation, compliance_vendors).
narrative_ontology:constraint_beneficiary(security_theater_proliferation, institutional_administrators).
narrative_ontology:constraint_victim(security_theater_proliferation, general_population).
narrative_ontology:constraint_victim(security_theater_proliferation, economic_efficiency).
narrative_ontology:constraint_victim(security_theater_proliferation, genuine_security_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Subject to mandatory security procedures with no genuine exit option. Cannot board planes, enter government buildings, or participate in major institutions without complying with theater. Trapped agents experience maximum extraction: time cost, dignity cost, privacy violation. No coordination benefit — the measures demonstrably fail to improve security. Pure extraction.
constraint_indexing:constraint_classification(security_theater_proliferation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BUSINESS SECTOR (TANGLED ROPE) — Constrained by mandatory compliance costs (security systems, personnel training, audit requirements). Benefits from appearance of security and regulatory compliance that provides liability protection. Asymmetric extraction overlaid on genuine coordination function (need to maintain secure infrastructure). Can exit with significant cost (business disruption, legal liability).
constraint_indexing:constraint_classification(security_theater_proliferation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY APPARATUS (ROPE) — Primary beneficiary. Controls expansive organizational mandate, budget allocation, personnel deployment. Experiences constraint as coordination mechanism: security theater coordinates institutional responses, justifies resource allocation, maintains organizational coherence. Net beneficiary — extraction flows toward this agent. Can arbitrage between different institutional contexts.
constraint_indexing:constraint_classification(security_theater_proliferation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLIANCE VENDOR ECOSYSTEM (ROPE) — Primary beneficiary. Grows through mandate for ever-expanding security systems (screening equipment, software, consulting, certification). Experiences pure coordination: vendors solve the institutional problem of demonstrable security response. Can arbitrage across sectors and geographies. Low suppression experienced by this agent — they profit and exit freely.
constraint_indexing:constraint_classification(security_theater_proliferation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY-CONSCIOUS COALITION (SCAFFOLD) — Organized resistance (civil liberties groups, privacy advocates, data protection regulators) perceives security theater as temporary institutional arrangement with a sunset clause. Building alternative security paradigms (risk-based screening, behavioral profiling, decentralized verification) that bypass blanket theatrical measures. High agency and exit pathways. Sunset horizon: 15-25 years as alternatives mature and public tolerance for surveillance degrades.
constraint_indexing:constraint_classification(security_theater_proliferation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: HISTORICAL SECURITY REVIEW (PITON) — From civilizational timescale, security theater represents degraded institutional function: measures persist decades after threat model changes, original justification becomes obsolete, but architectural inertia maintains them. Theater ratio (0.78) reflects purely performative character — ritualized compliance that persists because alternatives haven't fully supplanted it, not because it effectively detects threats. No genuine coordination function remains.
constraint_indexing:constraint_classification(security_theater_proliferation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing security theater as an inherent feature of complex societies. The argument: large-scale institutions managing risk must perform security rituals for legitimacy; this is an immutable constraint on governance. However, structural data contradicts mountain classification — security theater is contingent on institutional choice (benefit flows, suppression levels, theater ratio) and alternative paradigms exist. Engine will flag as false summit.
constraint_indexing:constraint_classification(security_theater_proliferation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(security_theater_proliferation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(security_theater_proliferation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(security_theater_proliferation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(security_theater_proliferation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(security_theater_proliferation, TR),
    TR >= 0.70.

:- end_tests(security_theater_proliferation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Security theater extracts time (TSA screening averages 30-45 minutes per crossing), dignity (mandatory compliance with invasive procedures), privacy (data collection), and opportunity cost. But extraction is not maximal because: (1) some genuine security coordination function exists (institutions do need to maintain security architecture), (2) some populations (frequent travelers, security professionals) can develop workarounds or arbitrage, (3) alternative security paradigms are technically feasible. The measurement trajectory (0.35→0.58 over 20 years) shows accumulation through scope creep and institutional inertia. Suppression (0.65): Moderate-high. Trapped populations face high barriers to exit: security theater is legally mandated for air travel, border crossing, and institutional access. But suppression is not total because some alternatives exist (driving instead of flying, avoiding institutions, accepting consequences). The suppression reflects institutional mandates (law, regulation, private policy) rather than physical barriers — hence constrained rather than trapped for business sector. Theater ratio (0.78): High. The measurement shows steady increase over the interval (0.55→0.78) reflecting Goodhart drift: theater becomes the primary objective, displacing the original goal of threat prevention. Current security procedures (full-body scanning, extensive questioning, no-fly lists) are primarily visible compliance signals. Effectiveness per dollar is negative when false-positive costs are included. Claimed type: Tangled Rope. The constraint has genuine coordination function (institutions need security architecture) overlaid with asymmetric extraction (trapped populations bear suppression while beneficiaries profit). Active enforcement required: yes (legal mandates, institutional policies). Beneficiaries identified: security apparatus, compliance vendors. Victims identified: general population, genuine security capacity (theater diverts resources from actual threat prevention), economic efficiency (time/resource waste).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The security apparatus sees coordination (Rope) — theater solves the institutional problem of demonstrating security responsiveness. The trapped citizen sees pure extraction (Snare) — no coordination benefit, only suppression. The business sector sees mixed coordination and extraction (Tangled Rope) — genuine security coordination overlaid with mandatory compliance costs. The organized coalition sees a temporary problem with exit pathways (Scaffold) — alternative security paradigms are emerging. The institutional steward sees degraded ritual (Piton) — theater persists through inertia. The analytical observer risks false summit (Mountain) — security theater as inherent to governance. The perspectival gap reveals that the same constraint (security measures) is experienced as coordination by those who benefit, extraction by those who bear cost, and theater by those who maintain it. No single perspective is 'correct' — the presheaf captures institutional reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from the structural asymmetry: security apparatus and vendors benefit with arbitrage-level exit (d ≈ 0.05-0.20), while trapped populations bear maximum costs (d ≈ 0.95). The business sector occupies middle ground with constrained exit (d ≈ 0.60-0.70). The beneficiary/victim declarations produce a clear directionality gradient through the sigmoid f(d): low d for beneficiaries produces negative effective extraction (they profit); high d for trapped agents produces high f(d) ≈ 1.4+ and maximum experienced extraction. The piton perspective uses constrained exit (d ≈ 0.65) because institutional actors maintaining theatrical measures face political and legitimacy costs if they unilaterally reduce theater, even though they possess formal authority. This captures the institutional lock-in: administrators know theater is wasteful but exit has legitimacy costs. The analytical/mountain perspective risks collapsing this gradient by naturalizing theater as an inherent feature of governance, which the oracle gap omega would detect.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that security theater is NOT a false labeling of coordination as extraction (or vice versa). It IS a genuine hybrid: institutions coordinate on threat response through theater, AND trapped populations are extracted from. The confusion arises from conflating the beneficiary's perspective (Rope) with the victim's perspective (Snare). From the beneficiary view, theater is pure coordination — it solves the problem of demonstrating institutional security response. From the trapped citizen view, theater is pure extraction — no coordination benefit exists for them. The Tangled Rope classification from the moderate/business perspective is the structural resolution: the constraint genuinely coordinates security architecture while asymmetrically extracting from those unable to exit. The mandatrophy is resolved by recognizing that both readings are structural facts simultaneously — theater provides genuine coordination function for institutional actors while providing zero coordination benefit (pure extraction) for trapped agents. The asymmetry is what makes it tangled rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_model_currency,
    'What proportion of current security theater directly addresses active threats versus historical threats that have been mitigated or eliminated?',
    'Risk assessment audit comparing deployed theater to current threat landscape; historical analysis of threat model evolution vs security measure evolution lag',
    'If < 30% current-threat-relevant: theater is primarily historical inertia (piton classification confirmed). If > 60% current-threat-relevant: some legitimate coordination function remains (tangled_rope appropriate). Determines sunset timeline plausibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_model_currency, empirical, 'Proportion of security theater addressing current vs historical threats').

omega_variable(
    actual_threat_prevention_rate,
    'What is the demonstrated prevented-incident rate of security theater measures versus the false-positive rate?',
    'Meta-analysis of TSA/airport security effectiveness studies, border screening ROI, cybersecurity theater detection rates; comparison of prevented-incidents-per-dollar to false-positive costs',
    'If prevented-incidents-per-dollar < false-positive costs: classification is snare-dominant (pure extraction). If ratio > 1: some genuine coordination benefit exists (tangled_rope confirmed). Affects suppression metric calibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_threat_prevention_rate, empirical, 'Threat prevention effectiveness versus false-positive rate').

omega_variable(
    alternative_security_paradigm_viability,
    'Are risk-stratified, behavioral, or decentralized security approaches technically feasible as replacements for blanket theatrical measures?',
    'Comparative pilot studies of alternative screening methodologies; analysis of risk-based TSA programs, trusted-traveler effectiveness; technological feasibility assessment of decentralized verification',
    'If viable: scaffold sunset clause is realistic (organized agents can exit within generational timescale). If infeasible: constraint remains locked (theater perpetuates because no exit exists). Affects scaffold classification validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_security_paradigm_viability, empirical, 'Technical viability of alternative security paradigms').

omega_variable(
    legitimacy_performance_coupling,
    'To what extent does institutional survival depend on demonstrable (theatrical) security compliance versus actual security outcome?',
    'Analysis of institutional liability exposure; comparison of institution reputation impact from theater visibility vs actual security incidents; regulatory requirement audit',
    'If institutional survival depends primarily on theater visibility: suppression is high because exit creates legitimacy crisis (suppression ≥ 0.65 confirmed, extractiveness > 0.50). If outcome-driven: suppression can be negotiated downward. Affects beneficiary incentive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_performance_coupling, conceptual, 'Coupling between institutional legitimacy and security theater performance').

omega_variable(
    public_tolerance_trajectory,
    'Is public tolerance for pervasive security theater increasing, stable, or declining across generational cohorts?',
    'Longitudinal survey data on privacy concerns, willingness to endure security procedures, civil liberties support; generational attitude shifts; political backlash tracking',
    'If declining: scaffold sunset clause is driven by bottom-up pressure (coalition power increases). If stable/increasing: scaffold may not materialize. Affects long-term classification trajectory and coalition power assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_tolerance_trajectory, empirical, 'Public tolerance trajectory for security theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(security_theater_proliferation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sectheat_tr_t0, security_theater_proliferation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sectheat_tr_t10, security_theater_proliferation, theater_ratio, 10, 0.68).
narrative_ontology:measurement(sectheat_tr_t20, security_theater_proliferation, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(sectheat_be_t0, security_theater_proliferation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sectheat_be_t10, security_theater_proliferation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sectheat_be_t20, security_theater_proliferation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(security_theater_proliferation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(security_theater_proliferation, 0.12).
narrative_ontology:affects_constraint(security_theater_proliferation, institutional_legitimacy_signaling).
narrative_ontology:affects_constraint(security_theater_proliferation, privacy_suppression_infrastructure).
narrative_ontology:affects_constraint(security_theater_proliferation, risk_bureaucratization_coupling).

% DUAL FORMULATION NOTE:
% Security theater proliferation is downstream of institutional risk-aversion (which drives demand for visible security response) and upstream of specific security measures (TSA screening, border control, cybersecurity theater). The family decomposition: (1) institutional_legitimacy_signaling (ε=0.45, Rope) — institutions coordinate on visible response to threats; (2) privacy_suppression_infrastructure (ε=0.72, Snare) — surveillance systems lock in suppression; (3) security_theater_proliferation (ε=0.58, Tangled Rope) — the hybrid coordination-extraction dynamic across the ecosystem. Each story tracks distinct ε values reflecting different observable bases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(security_theater_proliferation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
