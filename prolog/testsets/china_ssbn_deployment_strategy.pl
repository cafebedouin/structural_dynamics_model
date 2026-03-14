% ============================================================================
% CONSTRAINT STORY: china_ssbn_deployment_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_ssbn_deployment_strategy, []).

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
 *   constraint_id: china_ssbn_deployment_strategy
 *   human_readable: China SSBN Deployment Strategy and Strategic Stability
 *   domain: geopolitical/military/strategic_stability
 *
 * SUMMARY:
 *   China's SSBN deployment strategy represents a structural tension between
 *   strategic autonomy (building credible second-strike deterrent capability)
 *   and regional stability mechanisms (traditional arms control frameworks,
 *   mutual vulnerability assumptions, escalation control protocols). This
 *   constraint exhibits the full range of DR types from different
 *   perspectives, revealing how the same military modernization program
 *   appears as coordination, extraction, natural law, degraded ritual, and
 *   temporary problem depending on the observer's structural position. The
 *   constraint's extractiveness (0.58) reflects that SSBN deployment creates
 *   permanent pressure on defensive systems, destabilizes arms control
 *   frameworks, and locks regional actors into strategic dependencies. The
 *   rising extractiveness over the measurement interval (0.32 → 0.58)
 *   indicates accumulation of strategic pressure and compression of strategic
 *   stability margins. The declining theater ratio (0.55 → 0.48) indicates
 *   that the deployment mechanism is becoming more functionally real and less
 *   dependent on rhetorical justification — it is consolidating as structural
 *   policy rather than temporary contingency.
 *
 * KEY AGENTS:
 *   - PRC Military Modernization Program: Primary beneficiary (institutional/arbitrage) — controls deployment tempo and doctrine; captures strategic autonomy and deterrent credibility
 *   - Strategic Stability Regime: Primary victim (powerless/trapped) — Cold War frameworks cannot accommodate multi-actor SSBN dynamics; no exit mechanism from coordination erosion
 *   - U.S. Ballistic Missile Defense Architecture: Secondary victim (moderate/constrained) — forced into permanent defensive investment responsive to deployment patterns; constrained by technical and fiscal limits
 *   - Allied Regional Security Architecture: Mixed (organized/constrained) — benefits from extended deterrence (coordination function) while trapped in strategic dependency (extraction function)
 *   - Cold War Strategic Stability Doctrines: Degraded institutional framework (piton) — arms control processes persist as theater despite diminished functional coordination capacity
 *   - Emerging Multilateral Arms Control Advocacy: Scaffold perspective (organized/constrained) — emerging protocols could create sunset if negotiated, but currently aspirational
 *   - Physics of Naval Stealth: Naturalization risk (analytical/analytical) — risk of treating contingent strategic choices as immutable physical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_ssbn_deployment_strategy, 0.58).
domain_priors:suppression_score(china_ssbn_deployment_strategy, 0.72).
domain_priors:theater_ratio(china_ssbn_deployment_strategy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_ssbn_deployment_strategy, extractiveness, 0.58).
narrative_ontology:constraint_metric(china_ssbn_deployment_strategy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(china_ssbn_deployment_strategy, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_ssbn_deployment_strategy, tangled_rope).
narrative_ontology:human_readable(china_ssbn_deployment_strategy, "China SSBN Deployment Strategy and Strategic Stability").
narrative_ontology:topic_domain(china_ssbn_deployment_strategy, "geopolitical/military/strategic_stability").

domain_priors:requires_active_enforcement(china_ssbn_deployment_strategy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_ssbn_deployment_strategy, prc_military_deterrent_capability).
narrative_ontology:constraint_beneficiary(china_ssbn_deployment_strategy, regional_power_projection).
narrative_ontology:constraint_beneficiary(china_ssbn_deployment_strategy, strategic_autonomy).
narrative_ontology:constraint_victim(china_ssbn_deployment_strategy, strategic_stability_framework).
narrative_ontology:constraint_victim(china_ssbn_deployment_strategy, escalation_control_mechanisms).
narrative_ontology:constraint_victim(china_ssbn_deployment_strategy, us_ballistic_missile_defense_doctrine).
narrative_ontology:constraint_victim(china_ssbn_deployment_strategy, regional_freedom_of_navigation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRATEGIC STABILITY REGIME (SNARE) — The classical Cold War framework for managing nuclear deterrence (mutual vulnerability, transparency norms, arms control treaties) has no mechanism to exit the SSBN deployment dynamic. The regime is trapped by the emergence of second-strike capability deployment at scale. Theater of strategic stability conferences and transparency proposals persists, but the actual coordination mechanism (MAD assumptions) no longer contains the behavior. Maximum extraction from the regime itself — its assumptions have been made inoperable.
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US BMD ARCHITECTURE (SNARE) — Constrained by technical requirements for global coverage and cost, but also structurally targeted by SSBN deployment. The system cannot opt out of the competition — defensive architecture is forced to respond to deployment patterns. High extraction: each new SSBN platform requires defensive investment, detection research, and doctrine revision. Theater element: much BMD spending is justified by the SSBN threat (real), but deployment patterns create permanent pressure regardless of BMD effectiveness. Constrained exit: US can reduce BMD investment, but cannot exit the response requirement without accepting strategic risk.
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ALLIED REGIONAL SECURITY ARCHITECTURE (TANGLED ROPE) — Japan, South Korea, Australia, and Philippines benefit from US extended deterrence and alliance security guarantees. The SSBN deployment constraint simultaneously coordinates (provides credible second-strike assurance for allies) and extracts (locks allies into dependency on US nuclear guarantee, prevents independent strategic autonomy, constrains freedom of negotiation with China). Organized but constrained: allies can coordinate internal responses, but exit from the US security umbrella is costly and limited. Genuine coordination function (deterrence of direct attack) alongside asymmetric extraction (strategic dependency).
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRC MILITARY MODERNIZATION PROGRAM (ROPE) — Primary beneficiary (institutional/arbitrage). SSBN deployment is the core outcome of strategic choice and resource allocation. The constraint exists because the PRC actively chose to develop and deploy SSBN capability. The regime solves the coordination problem of committing resources to second-strike capability with high confidence. Extraction runs toward this agent — they capture strategic autonomy, deterrent credibility, and regional power projection from the deployment. Can arbitrage: can choose deployment tempo, doctrine, operational posture. Experiences the constraint as coordination of their own military modernization priorities.
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: EMERGING MULTILATERAL ARMS CONTROL ADVOCACY (SCAFFOLD) — Arms control scholars, progressive states, and multilateral institutions see SSBN deployment as a temporary coordination failure addressable through new treaty frameworks (SSBN notification protocols, operational transparency, cyber-security agreements on naval systems). Theater element: many proposals are aspirational rather than operationalized. Sunset logic: if protocols mature (estimated 15-25 years), the deployment dynamic could be partially contained through constraints on deployment zones, notification requirements, or crisis communication channels. Organized agents with constrained exit from current geopolitics but agency in shaping future norms.
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR STRATEGIC STABILITY DOCTRINES (PITON) — Traditional arms control frameworks (NPT, ABM Treaty norms, SLBM verification protocols) persist as institutional theater despite diminished functional relevance to modern SSBN dynamics. The doctrines assume transparent, relatively stable bilateral competition. Multi-actor SSBN deployment, submarine AI, autonomous ASW systems, and space-based detection create structural conditions that the doctrinal framework cannot accommodate. Theater ratio high: arms control conferences, transparency initiatives, and verification discussions continue as performative ritual. Actual coordination of SSBN behavior operates outside these frameworks. The Cold War framework has become degraded institutional inertia.
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICS OF NAVAL CONCEALMENT (MOUNTAIN) — From a universal/civilizational perspective, submarine stealth is a physics-based constraint that no strategic actor can fully escape: acoustic stealth, electromagnetic concealment, and detection avoidance impose hard limits on ASW effectiveness relative to deployment scale. This perspective sees SSBN deployment as governed by natural law — the acoustic properties of the ocean, the noise floor of detection systems, the speed-stealth tradeoff. However, the structural data contradicts true mountain classification. SSBN deployment scales and tempos are contingent geopolitical choices, not physical inevitabilities. The 'laws of physics' framing naturalizes what is actually institutional choice and strategic doctrine. False summit risk: the engine's natural law detector should flag this as naturalization.
constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_ssbn_deployment_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_ssbn_deployment_strategy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_ssbn_deployment_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_ssbn_deployment_strategy, TR),
    TR >= 0.70.

:- end_tests(china_ssbn_deployment_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. SSBN deployment creates sustained pressure on defensive systems and destabilizes bilateral strategic frameworks that assume stable dyadic competition. However, extractiveness is not maximum (0.72+) because some coordination function genuinely exists — second-strike credibility does deter direct attack on the core strategic actor. The extraction represents the cost imposed on others (defensive actors, stability frameworks, regional security architecture) relative to the benefit captured by the PRC. Suppression (0.72): High. Multiple barriers prevent exit or resistance: (1) Technological barriers — detection of SSBNs remains difficult and resource-intensive; (2) Institutional barriers — naval powers cannot unilaterally opt out of deployment competition without strategic risk; (3) Doctrinal barriers — first-strike/second-strike asymmetries create escalation pressures; (4) Information barriers — SSBN capabilities and deployment patterns are classified, preventing accurate threat assessment and calibrated response. Theater ratio (0.48): Moderate. The deployment mechanism is substantially functional — SSBNs are real platforms with real deterrent capability. However, significant theater exists in strategic communication and arms control theater (declaratory doctrine exceeds actual operational posture, arms control conferences continue despite low coordination effect). The declining theater ratio reflects that deployment is becoming more structural and less dependent on rhetorical justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp divergence between perspectives. The PRC military program sees coordination (Rope) — they are voluntarily developing capability that solves their strategic security problem. The strategic stability regime sees pure extraction (Snare) — the regime's core assumptions (transparent bilateral competition, mutual vulnerability) are becoming operationally irrelevant. US BMD architecture sees extraction with constrained response (Snare) — forced into permanent investment with uncertain payoff. Allied regional security sees mixed coordination-extraction (Tangled Rope) — genuine deterrent benefit (coordination) combined with permanent strategic dependency (extraction). Cold War doctrines see their own degradation (Piton) — arms control processes persist without functional coordination effect. Emerging arms control advocacy sees a temporary problem with a solution path (Scaffold) — multilateral protocols could stabilize deployment if negotiated. The analytical observer risks seeing immutable physics (Mountain) — treating SSBN stealth as a law of nature rather than a contingent strategic choice. The gap reveals that SSBN deployment is structurally extractive for some actors (defenders, stability frameworks, regional allies) while being coordinative for the deploying power.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the constraint. The PRC military program (beneficiary, institutional, arbitrage exit) experiences low directionality (d ≈ 0.10) — extraction flows toward them. The strategic stability regime (victim, powerless, trapped) experiences high directionality (d ≈ 0.95) — maximum extraction burden. US BMD architecture (victim, moderate, constrained) experiences medium-high directionality (d ≈ 0.75) — significant extraction with some response capacity. Allied regional architecture (mixed role, organized, constrained) experiences medium directionality (d ≈ 0.50) — coordination benefits offset by dependency extraction. Cold War doctrines (institutional, arbitrage, performative role) experience low directionality (d ≈ 0.15) — they nominally benefit from being the framework, but are actually degraded. The analytical observer (analytical, analytical) operates at d ≈ 0.72 canonical value, creating the risk of naturalization. The divergence between beneficiary and victim directionality drives the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by acknowledging that SSBN deployment is genuinely both coordination and extraction depending on whose strategic requirement is being met. For the PRC, it is coordination of their security objectives (second-strike credibility). For everyone else, it is extraction of strategic options and stability margins. The constraint avoids the trap of calling it 'purely' either one. The Tangled Rope classification captures the genuine coordination function (deterring direct attack) while acknowledging the asymmetric extraction (locking others into defensive competition, constraining negotiating room, destabilizing frameworks). The measurement trajectory shows extraction accumulating over time while theater declines — this pattern is diagnostically clean: a constraint that is becoming more structurally real and less dependent on rhetorical justification. The Piton classification for Cold War doctrines shows that the institutional framework is inertial theater, not active coordination. The Scaffold classification for emerging arms control shows the possibility of future sunset, but only if protocols can be negotiated — the sunset is not automatic. The false summit risk on the analytical/mountain perspective is correctly identified: the physics of stealth is real, but the deployment strategy is political choice, not physical inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ssbn_stability_paradox,
    'Does SSBN deployment increase or decrease strategic stability in a multi-actor context?',
    'Scenario modeling of escalation dynamics with and without second-strike capability; historical analysis of crisis behavior (1962, 1973, 1999) with attention to how credible retaliation affected restraint; game-theoretic analysis of signaling in first-strike/second-strike asymmetries',
    'If increases stability: constraint is coordination mechanism (Rope from some perspectives). If decreases stability: constraint is extraction of stability margin for strategic autonomy (Snare/Tangled Rope). Classification sensitive to this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ssbn_stability_paradox, empirical, 'Whether SSBN deployment increases or decreases strategic stability').

omega_variable(
    ballistic_missile_defense_feasibility,
    'What is the achievable coverage and intercept confidence of a regional ballistic missile defense architecture against a mature SSBN fleet?',
    'Classified technical assessments; open-source modeling of sensor architectures and intercept physics; test data from actual BMD systems; signal intelligence on SSBN detection capabilities',
    'If BMD coverage < 50%: constraint is extraction (US forced into permanent defensive investment with low ROI). If BMD coverage > 80%: constraint is coordination (US can stabilize defense; PRC deployment loses credibility). Classification and chi values shift significantly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ballistic_missile_defense_feasibility, empirical, 'Feasible coverage of regional ballistic missile defense').

omega_variable(
    autonomous_asw_deployment_threshold,
    'At what point do autonomous anti-submarine systems (AI-driven, networked detection, autonomous engagement) make SSBN concealment strategy obsolete?',
    'Technical forecasting of AI/autonomous system capabilities; monitoring of military R&D in automated underwater surveillance; testing of decoys and countermeasure effectiveness against autonomous systems; geopolitical signaling about deployment plans',
    'If threshold crossed within 10-15 years: current SSBN strategy becomes obsolete (entire deployment strategy reclassified as inertial/piton). If threshold remains distant (>25 years): current deployment pressure persists. Affects long-term classification and measurement trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_asw_deployment_threshold, empirical, 'Timeline for autonomous ASW deployment rendering SSBN concealment obsolete').

omega_variable(
    extraction_vs_security_motivation,
    'To what extent is SSBN deployment driven by genuine second-strike security requirement versus regional power projection and strategic autonomy extraction?',
    'Analysis of deployment doctrine and operational posture (deterrent-minimalist vs force-projection-maximalist); comparison of actual deployment pace to technical requirements for credible deterrence; interviews with strategic planners; analysis of PRC declaratory policy evolution',
    'If security-motivated (>70%): constraint is partially coordination (Tangled Rope justified). If extraction-motivated (>50%): constraint is primarily extraction with coordination cover (Snare or high-chi Tangled Rope). Affects beneficiary/victim framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_security_motivation, empirical, 'Proportion of SSBN deployment driven by security versus strategic autonomy extraction').

omega_variable(
    multilateral_arms_control_feasibility,
    'Can multilateral SSBN protocols (notification, transparency, operational zones) be negotiated and verified without requiring full technical disclosure of submarine stealth capabilities?',
    'Analysis of past arms control agreements with verification provisions; technical feasibility of confidence-building measures that avoid full disclosure; political willingness assessments from major naval powers; modeling of incentives for compliance and defection',
    'If feasible: Scaffold perspective is realistic, sunset clause has structural basis. If infeasible: Scaffold is aspirational theater, sunset is unlikely. Affects Scaffold classification confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_arms_control_feasibility, empirical, 'Feasibility of multilateral SSBN arms control without full technical disclosure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_ssbn_deployment_strategy, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ssbn_tr_t0, china_ssbn_deployment_strategy, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ssbn_tr_t7, china_ssbn_deployment_strategy, theater_ratio, 7, 0.51).
narrative_ontology:measurement(ssbn_tr_t14, china_ssbn_deployment_strategy, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(ssbn_be_t0, china_ssbn_deployment_strategy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ssbn_be_t7, china_ssbn_deployment_strategy, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(ssbn_be_t14, china_ssbn_deployment_strategy, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_ssbn_deployment_strategy, enforcement_mechanism).
narrative_ontology:affects_constraint(china_ssbn_deployment_strategy, us_ballistic_missile_defense_doctrine).
narrative_ontology:affects_constraint(china_ssbn_deployment_strategy, regional_freedom_of_navigation_indo_pacific).
narrative_ontology:affects_constraint(china_ssbn_deployment_strategy, strategic_stability_arms_control_framework).
narrative_ontology:affects_constraint(china_ssbn_deployment_strategy, allied_extended_deterrence_dependency).

% DUAL FORMULATION NOTE:
% SSBN deployment strategy is upstream of multiple regional security constraints. The deployment creates structural pressure that cascades into allied defense budgets, naval freedom-of-navigation claims, arms control negotiations, and extended deterrence commitments. Each downstream constraint has its own extractiveness value reflecting how the SSBN dynamic manifests in specific institutional contexts. The network links show how degradation or escalation in one propagates through the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_ssbn_deployment_strategy, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
