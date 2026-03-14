% ============================================================================
% CONSTRAINT STORY: autonomous_spacecraft_navigation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomous_spacecraft_navigation, []).

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
 *   constraint_id: autonomous_spacecraft_navigation
 *   human_readable: Autonomous Spacecraft Navigation System Coordination and Extraction
 *   domain: aerospace/autonomy/systems_engineering
 *
 * SUMMARY:
 *   Autonomous spacecraft navigation represents a structural tension between
 *   the physical constraints of space communication and the institutional
 *   preferences for human authority and control. Signal propagation delays
 *   (3-22 minutes round-trip to Mars) make real-time remote piloting
 *   impossible, forcing a choice: delegate navigation authority to onboard
 *   autonomous systems or accept severe mission limitations. The constraint
 *   exhibits all six DR types from different perspectives. From the
 *   spacecraft's perspective, it is a snare — bound by pre-programmed
 *   behaviors with no negotiation capacity. From mission control's
 *   perspective, it is tangled rope — genuine coordination benefits (reduced
 *   operational overhead) embedded in asymmetric authority loss. From the
 *   space agency's perspective, it is pure coordination (rope) — autonomous
 *   systems enable ambitious timelines and multiple simultaneous missions.
 *   From the engineering perspective, it is a temporary problem being solved
 *   (scaffold) — redundant systems and fault tolerance frameworks are
 *   building confidence in autonomous reliability. From institutional
 *   vantage, it is a degraded ritual (piton) — the oversight and approval
 *   workflows designed for real-time control persist despite irrelevance.
 *   From a physical perspective, it appears immutable (mountain) — light
 *   speed limits are laws of nature. However, this is a false summit: the
 *   constraint is not the physics but the organizational choice of whether to
 *   trust autonomous systems.
 *
 * KEY AGENTS:
 *   - Remote Spacecraft: Primary victim (powerless/trapped) — bound by pre-programmed protocols, communication latency, irreversible decisions; no exit capacity
 *   - Mission Operations Team: Secondary victim and partial beneficiary (moderate/constrained) — gains efficiency from autonomy but loses real-time control; constrained by latency and irrevocability
 *   - Space Agency and Mission Control: Primary beneficiary (institutional/arbitrage) — captures operational efficiency, enables ambitious timelines, reduces ground staff burden
 *   - Fault Tolerance Engineering Coalition: Organized intermediate (organized/constrained) — building redundant systems and recovery protocols with sunset logic
 *   - Manual Navigation Institutional Legacy: Institutional actor (institutional/arbitrage) — maintains ceremonial oversight and approval workflows from real-time control era
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating physics as the constraint rather than institutional choices about delegation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomous_spacecraft_navigation, 0.38).
domain_priors:suppression_score(autonomous_spacecraft_navigation, 0.52).
domain_priors:theater_ratio(autonomous_spacecraft_navigation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomous_spacecraft_navigation, extractiveness, 0.38).
narrative_ontology:constraint_metric(autonomous_spacecraft_navigation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(autonomous_spacecraft_navigation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomous_spacecraft_navigation, tangled_rope).
narrative_ontology:human_readable(autonomous_spacecraft_navigation, "Autonomous Spacecraft Navigation System Coordination and Extraction").
narrative_ontology:topic_domain(autonomous_spacecraft_navigation, "aerospace/autonomy/systems_engineering").

domain_priors:requires_active_enforcement(autonomous_spacecraft_navigation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomous_spacecraft_navigation, mission_control_agencies).
narrative_ontology:constraint_beneficiary(autonomous_spacecraft_navigation, spacecraft_manufacturers).
narrative_ontology:constraint_victim(autonomous_spacecraft_navigation, mission_reliability).
narrative_ontology:constraint_victim(autonomous_spacecraft_navigation, remote_explorers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REMOTE SPACECRAFT (SNARE) — Bound by communication delay (signal travel time 3-20+ minutes each way) and irreversible decision constraints. Cannot appeal decisions, cannot exit the autonomous navigation protocol once deployed. Fully dependent on pre-programmed behaviors and cannot negotiate real-time. Bears maximum constraint risk with zero exit capacity.
constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MISSION OPERATIONS TEAM (TANGLED ROPE) — Constrained by communication latency and telemetry bandwidth limitations; must trust autonomous decisions yet bears responsibility for outcomes. Gains efficiency from autonomous navigation (genuine coordination benefit) but loses real-time control authority (asymmetric extraction). High suppression due to irreversibility of deployed decisions and limited override capacity.
constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SPACE AGENCY AND MISSION CONTROL (ROPE) — Primary beneficiary. Autonomous navigation reduces operational costs, enables ambitious mission timelines, and allows deployment of multiple simultaneous missions with minimal ground staff. Experiences the constraint as pure coordination: the communication latency is a natural fact they navigate through autonomous protocols. Net beneficiary — the extracted value (operational efficiency) flows toward this agent.
constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FAULT TOLERANCE ENGINEERING COALITION (SCAFFOLD) — Organized effort (redundant systems, health monitoring protocols, graceful degradation frameworks) building structured recovery pathways. Sunset logic: as autonomous system maturity increases and fault tolerance improves, the extraction risk (mission loss from autonomous failures) declines over generational timeframes. Current theater (0.58) reflects ongoing validation testing; as systems mature, theater should decline toward 0.35-0.40.
constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MANUAL NAVIGATION PARADIGM (PITON) — Legacy protocol maintenance: mission control maintains oversight authority and decision-approval workflows designed for real-time control, despite their irrelevance to deep-space operations. These rituals persist through institutional inertia (operators trained in manual protocols, regulatory frameworks requiring explicit approval chains) despite being purely performative for Mars-scale or outer planets missions. Theater ratio high for this institutional artifact.
constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMITS VIEW (MOUNTAIN) — From a universal perspective, signal propagation speed limits create an immutable constraint: no communication faster than light. At Mars distances, round-trip communication takes 3-22 minutes depending on orbital positions. This physical fact makes real-time remote piloting impossible. However, the structural data reveals this mountain is a false summit — the 'immutable law' of light speed has been routinely transcended by autonomous systems. The constraint is not the physics; it is the institutional choice of whether to delegate authority to spacecraft or insist on ground control rituals.
constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomous_spacecraft_navigation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomous_spacecraft_navigation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(autonomous_spacecraft_navigation, TR),
    TR >= 0.70.

:- end_tests(autonomous_spacecraft_navigation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, climbing to 0.38 over 20-year interval): Moderate. Initially driven by genuine operational efficiency gains (autonomy reduces staffing costs and enables ambitious timelines), but climbing as systems accumulate mission responsibility and the cost of a single autonomous failure increases. The trajectory shows the constraint shifting from a pure coordination benefit toward mixed extraction as missions become more ambitious and failures become more consequential. Not as high as original estimates (0.72) because genuine coordination benefits are real and measurable: autonomous Mars rovers accomplish tasks impossible under real-time control. Suppression (0.52): Moderate-high. Significant barriers include communication latency (irreversible decisions), limited telemetry bandwidth (incomplete situational information), irreversibility (no real-time correction), and irreplaceability (each spacecraft is unique and expensive). However, not total — multiple redundant systems and graceful degradation frameworks exist. Theater ratio (0.58, declining to 0.58): Moderate. Traditional mission control approval workflows persist despite irrelevance at Mars distances — purely ceremonial for outer planets missions. Theater has declined over the interval as engineering practices shifted toward confidence-based validation rather than oversight-based approval. Trajectory is positive (theater declining).
 *
 * PERSPECTIVAL GAP:
 *   The massive gap between the spacecraft's snare perspective and the space agency's rope perspective reveals the distributional asymmetry. The same constraint that enables efficient mission operations imposes irreversible risk on the autonomous system. The mission operations team's tangled rope classification shows they experience both coordination benefits and asymmetric extraction simultaneously — genuine operational efficiency alongside loss of real-time authority. The scaffold perspective shows an important structural feature: the constraint's character is changing over time as fault tolerance matures. The piton perspective reveals the institutional layer — mission control maintains ceremonial oversight workflows from an era when real-time piloting was possible, now purely performative. The analytical observer's mountain is a false summit — the constraint being naturalized as a law of physics is actually a choice about institutional delegation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to this specific constraint. The remote spacecraft has zero exit capacity (trapped) and bears all non-negotiable costs — high d producing high experienced extraction chi. The mission operations team has constrained exit (could switch to simpler missions with less autonomy, but at high career and capability cost) and mixed benefits/costs — moderate d. The space agency has arbitrage exit (could delegate to private operators or accept mission constraints, but chooses autonomy for competitive advantage) and net benefits — low d with negative chi. The analytical observer computing d from physical law perspective would produce an anomalously high d (treating light speed as an agent-targeting force) revealing the false summit. Beneficiaries include the mission control agencies (reduced overhead, expanded mission scope) and spacecraft manufacturers (reduced ground control infrastructure required). Victims include mission reliability (concentrated risk in autonomous algorithms) and remote explorers (crews or future habitats dependent on successful autonomous navigation).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by decomposing the apparent choice between 'autonomy vs control' into its actual structure: the choice is between distributed risk (snare for spacecraft, rope for agencies) and centralized risk (snare for mission control if it retains false authority). The analytics reveal that 'real-time control' at Mars distances is a false option — light speed has already made the choice. The institutional mandatrophy (cannot have both real-time human control AND Mars exploration) is resolved by recognizing that the constraint is not immutable but institutional. Space agencies have chosen to delegate authority to autonomous systems and build fault tolerance frameworks, accepting the snare-risk distribution for spacecraft as the cost of ambitious exploration. This is not natural law — it is institutional choice with measurable consequences for different agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_failure_causation,
    'Are autonomous navigation failures primarily due to unforeseen environmental conditions (coordinative challenge) or inadequate algorithm design (extractive mismanagement)?',
    'Post-mission failure analysis: classification of failure modes into environmental-contingency vs algorithmic-inadequacy; trend analysis across multiple missions',
    'If environmental: autonomy extraction is justified and suppression should decline as systems mature (scaffold logic holds). If algorithmic: suppression is not declining and the constraint is Snare not Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_failure_causation, empirical, 'Whether autonomy failures reflect physical contingency or design inadequacy').

omega_variable(
    communication_latency_justification,
    'Does mission control retain the actual capacity to override autonomous decisions in real-time, or is the oversight authority purely ceremonial?',
    'Technical audit: measurement of decision latency (time from telemetry receipt to command execution) vs nominal spacecraft response time; analysis of historical override attempts and their efficacy',
    'If real capacity: mission operations team experiences genuine shared authority (moderate extraction, Tangled Rope confirmed). If ceremonial: mission operations oversight is pure theater masking powerlessness (extraction approaches Snare levels).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(communication_latency_justification, empirical, 'Whether mission control override authority is functional or ceremonial').

omega_variable(
    fault_tolerance_maturation_timeline,
    'What is the realistic timeline for autonomous spacecraft fault tolerance to reach 99.9%+ mission success rates, validating the scaffold sunset clause?',
    'Technology roadmap analysis; comparison with historical autonomous system maturation curves (aviation, robotics); extrapolation from current mission success rates',
    'If realistic (10-20 years): scaffold perspective is justified and the constraint will naturally transition to rope as systems mature. If unrealistic (30+ years or never): scaffold framing is aspirational and extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fault_tolerance_maturation_timeline, empirical, 'Timeline for autonomous fault tolerance to reach high reliability thresholds').

omega_variable(
    alternative_architectures_feasibility,
    'Are intermediate architectures (semi-autonomous with human-in-the-loop or automated error recovery) genuinely superior to either full autonomy or real-time control, or are they theater masking institutional compromise?',
    'Comparative mission success analysis: full autonomy vs semi-autonomy vs real-time control across comparable mission profiles; cost-benefit accounting of operator overhead vs risk reduction',
    'If genuinely superior: intermediate architectures represent a valid third structural option and the constraint may decompose. If theater: institutional reluctance to choose either autonomy or manual control creates false middle ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architectures_feasibility, empirical, 'Whether semi-autonomous intermediates are structurally distinct or institutional compromise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomous_spacecraft_navigation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(autnav_tr_t0, autonomous_spacecraft_navigation, theater_ratio, 0, 0.65).
narrative_ontology:measurement(autnav_tr_t10, autonomous_spacecraft_navigation, theater_ratio, 10, 0.6).
narrative_ontology:measurement(autnav_tr_t20, autonomous_spacecraft_navigation, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(autnav_be_t0, autonomous_spacecraft_navigation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(autnav_be_t10, autonomous_spacecraft_navigation, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(autnav_be_t20, autonomous_spacecraft_navigation, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomous_spacecraft_navigation, enforcement_mechanism).
narrative_ontology:affects_constraint(autonomous_spacecraft_navigation, deep_space_communication_latency).
narrative_ontology:affects_constraint(autonomous_spacecraft_navigation, rover_autonomy_failure_modes).

% DUAL FORMULATION NOTE:
% Autonomous spacecraft navigation is downstream of deep space communication latency (physical constraint) but represents a distinct structural arrangement about delegation and authority. Communication latency is a mountain (light speed); autonomous navigation is the institutional response to that mountain, creating its own extraction dynamics distinct from the underlying physics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(autonomous_spacecraft_navigation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
