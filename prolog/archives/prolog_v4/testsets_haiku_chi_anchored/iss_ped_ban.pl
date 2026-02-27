% ============================================================================
% CONSTRAINT STORY: iss_ped_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iss_ped_ban, []).

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
 *   constraint_id: iss_ped_ban
 *   human_readable: NASA's Historical Ban on Personal Electronic Devices on the ISS
 *   domain: technological/space_operations
 *
 * SUMMARY:
 *   NASA's blanket prohibition on personal electronic devices aboard the
 *   International Space Station, maintained from the station's establishment
 *   through the 2020s, exemplifies how a coordination mechanism for
 *   legitimate safety concerns can degrade into institutional theater while
 *   maintaining enforcement. The ban ostensibly protected mission safety
 *   through RF interference prevention and orbital debris mitigation, yet the
 *   constraint exhibits all hallmarks of institutional drift: the original
 *   risk model was never quantified, authorized alternatives (crew tablets,
 *   commercial terminals) proved the ban unnecessarily restrictive, and
 *   enforcement persisted long after its functional justification eroded. The
 *   constraint demonstrates the tangled rope archetype: NASA's mission
 *   operations genuinely benefited from unified communication control (rope
 *   coordination), but astronauts experienced meaningful extraction of
 *   autonomy, privacy, and communication efficiency (asymmetric burden). The
 *   theater ratio's rise from 0.35 to 0.68 over two decades reveals the ban's
 *   transition from functional safety mechanism to performative compliance
 *   ritual. By 2020, the International Partnership (ISS partners authorized
 *   crews to use approved tablets), the constraint entered scaffold
 *   transition phase with documented sunset logic. This story illustrates how
 *   indexical classification prevents false mountain claims: the ban might
 *   naturalize as an immutable requirement of orbital operations ('RF
 *   environment always requires strict control'), but the structural data and
 *   authorization of personal devices in practice proves it a contingent
 *   institutional arrangement.
 *
 * KEY AGENTS:
 *   - NASA Mission Operations: Primary beneficiary (institutional/arbitrage) — captures unified RF spectrum control and communication dominance
 *   - Isolated Astronaut: Primary victim (powerless/trapped) — bears extraction of autonomy, privacy, and emergency communication capacity for mission duration
 *   - Scientific Research Community: Secondary victim (moderate/constrained) — real-time data analysis and research coordination constrained; also benefits from mission continuity
 *   - ISS International Partners: Organized coalition (organized/constrained) — ESA, Roscosmos, JAXA, CSA building authorized device pathway with sunset logic
 *   - Orbital Debris Mitigation Authority: Institutional actor (institutional/arbitrage) — maintains degraded justification; original function (debris prevention) attenuated but enforcement continues
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy as immutable space law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iss_ped_ban, 0.38).
domain_priors:suppression_score(iss_ped_ban, 0.62).
domain_priors:theater_ratio(iss_ped_ban, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iss_ped_ban, extractiveness, 0.38).
narrative_ontology:constraint_metric(iss_ped_ban, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(iss_ped_ban, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iss_ped_ban, tangled_rope).
narrative_ontology:human_readable(iss_ped_ban, "NASA's Historical Ban on Personal Electronic Devices on the ISS").
narrative_ontology:topic_domain(iss_ped_ban, "technological/space_operations").

domain_priors:requires_active_enforcement(iss_ped_ban).
narrative_ontology:has_sunset_clause(iss_ped_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iss_ped_ban, mission_safety_protocols).
narrative_ontology:constraint_beneficiary(iss_ped_ban, nasa_operational_control).
narrative_ontology:constraint_beneficiary(iss_ped_ban, orbital_debris_mitigation).
narrative_ontology:constraint_victim(iss_ped_ban, astronaut_autonomy).
narrative_ontology:constraint_victim(iss_ped_ban, crew_communication_efficiency).
narrative_ontology:constraint_victim(iss_ped_ban, scientific_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED ASTRONAUT (SNARE) — Trapped for 6-month mission duration without exit. Cannot bring personal communication devices despite 400km altitude. Bears extraction of autonomy and privacy. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(iss_ped_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by device ban's impact on real-time data analysis and research coordination, yet benefits from mission safety protocols that enable extended operations. d≈0.68, f(d)≈1.05, σ=1.1 → χ≈0.44.
constraint_indexing:constraint_classification(iss_ped_ban, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NASA MISSION OPERATIONS (ROPE) — Benefits from unified communication control and RF spectrum management. Experiences constraint as coordination mechanism for crew safety and orbital asset protection. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(iss_ped_ban, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL PARTNERSHIP COALITION (SCAFFOLD) — Roscosmos, ESA, JAXA, and CSA collectively see the ban as a transitional coordination mechanism with explicit sunset: approved crew tablets (iPad, Samsung Galaxy) and commercial RF terminals are gradually replacing the blanket prohibition. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.15. Low extraction because the coalition has agency and documented sunset timeline (2020-2028 transition complete by 2030).
constraint_indexing:constraint_classification(iss_ped_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ORBITAL DEBRIS MITIGATION AUTHORITY (PITON) — The original justification for the ban (preventing device loss creating trackable debris) has degraded into theater. Modern ISS has 16+ active RF systems, GPS constellation, and Starlink traffic; marginal risk from a lost smartphone is negligible. The ban persists through institutional inertia. theater_ratio=0.68 satisfies piton gate. Original function (debris prevention) is attenuated; ban maintained because 20-year legacy bureaucracy hasn't updated risk models.
constraint_indexing:constraint_classification(iss_ped_ban, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep-time view, the ban naturalizes as inherent to space operations: RF interference, communication protocol fragility, and mission-critical override requirements might be immutable constraints. However, structural data (ε=0.38, suppression=0.62, theater=0.68) contradicts this. The engine will detect false summit: the constraint is contingent institutional policy, not natural law. Modern ISS successfully operates with approved personal devices, proving the 'immutable' justification was false.
constraint_indexing:constraint_classification(iss_ped_ban, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iss_ped_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iss_ped_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iss_ped_ban, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(iss_ped_ban, TR),
    TR >= 0.70.

:- end_tests(iss_ped_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The astronaut autonomy extraction is real but not maximal — missions succeed, communication is maintained through NASA-approved channels, and research proceeds. However, the extraction is genuine: lost opportunity for family communication during high-stress operations, inability to document personal observations, constraint on emergency decision-making autonomy. The value reflects that extraction is structural (institutional control of communication) but not punitive (astronauts are not tortured or abused). Suppression (0.62): Moderate-high. The ban's enforcement rests on astronauts' trapped status (cannot exit during mission) and institutional control of launch manifests (cannot negotiate as independent contractors). However, suppression is not total — astronauts can refuse missions, request policy changes, or use approved devices. The suppression value reflects the genuine difficulty of organizing opposition and the career risk of non-compliance. Theater ratio (0.68): High. The ban persists despite authorization of tablets and personal devices for approved use cases. The enforcement is substantially performative — astronauts cannot bring smartphones but can use SpaceX-provided communications, raising the question: what genuine safety difference justifies the prohibition? The theater has increased over time as the risk justification decayed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival disagreement despite identical base metrics. NASA operations perceives a pure coordination mechanism (rope) solving genuine RF spectrum management. The isolated astronaut perceives pure extraction (snare) with no coordination benefit. The international coalition perceives temporary scaffolding with a documented sunset (2020-2030 transition). The debris mitigation authority perceives its own degraded institutional ritual (piton) maintained through bureaucratic inertia. The analytical observer risks naturalizing the constraint as immutable space law (mountain), but the post-2020 authorization of personal devices proves the 'immutability' claim false. The perspectival gap reveals that the constraint is not a natural law but a contingent institutional arrangement with differential beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   NASA Mission Operations: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Unified RF control is a genuine operational advantage. Isolated astronaut: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction — no exit options during mission. ISS partners: Organized + constrained → d≈0.35, f(d)≈0.32. Low effective extraction; they retain agency to authorize alternatives. Debris authority: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification emerges from theater gate, not high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain claim is perspectival naturalization; engine's false summit detector catches it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same structural data, with a crucial temporal dimension. In 2001-2010, the constraint was genuine tangled rope: real RF hazards justified coordination, but astronaut autonomy extraction was non-trivial. By 2015-2020, theater ratio rose to 0.60+ as the risk justification atrophied — piton classification became appropriate (degraded institutional ritual). By 2025, the constraint is actively transitioning to scaffold as authorized devices prove the ban unnecessarily restrictive and explicit sunset logic emerges. The mandatrophy is resolved not by selecting a single type but by recognizing temporal drift: tangled rope → piton → scaffold transition reflects genuine institutional change. The false summit (mountain classification) is caught by the engine's NL metrics: the constraint lacks the accessibility_collapse (≥0.85) and resistance (≤0.15) signatures of true natural law. The ban is contingent, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rf_interference_threshold,
    'What RF interference threshold from personal devices genuinely threatens ISS communication integrity vs. what is regulatory precaution?',
    'Controlled RF emission testing in ISS RF environment; comparative analysis with Soyuz/Progress RF specifications; empirical data from post-2020 approved device operations',
    'If threshold very low: piton classification confirmed (fear-driven theater). If threshold high: mountain perspective gains credibility (genuine hazard). Device authorization timeline 2020-2030 suggests threshold was misidentified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rf_interference_threshold, empirical, 'Whether RF interference from personal devices genuinely threatens ISS communications').

omega_variable(
    orbital_debris_tracking_gap,
    'Does the marginal debris risk from a single lost smartphone justify a two-decade blanket ban given modern tracking and Starlink constellation degradation effects?',
    'Comparative orbital debris analysis: tracked objects <10cm from ISS altitude; debris generation statistics from known on-orbit device losses (ISS solar panel arrays, cargo pallet releases); risk modeling vs. grandiose Starlink constellation contribution',
    'If smartphone debris negligible: ban is pure institutional inertia (high theater). If risk material: piton classification is overstated, functional justification remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orbital_debris_tracking_gap, empirical, 'Whether smartphone debris risk justifies the blanket ban policy').

omega_variable(
    crew_autonomy_extraction_quantification,
    'How much psychological/operational extraction does the communication ban impose, and does it degrade crew morale, scientific productivity, or emergency response capacity?',
    'Longitudinal crew debriefs comparing pre-2020 (strict ban) and post-2020 (approved devices) missions; cognitive load studies; emergency communication drill performance metrics; family communication quality assessments',
    'If extraction is high: snare classification confirmed. If extraction is low/illusory: tangled rope perspective is stronger (genuine coordination benefit dominates). Psychological effects determine whether beneficiary/victim split is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crew_autonomy_extraction_quantification, empirical, 'Quantification of crew autonomy extraction and operational impact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iss_ped_ban, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ped_tr_t0, iss_ped_ban, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ped_tr_t12, iss_ped_ban, theater_ratio, 12, 0.52).
narrative_ontology:measurement(ped_tr_t24, iss_ped_ban, theater_ratio, 24, 0.68).

% Extraction over time
narrative_ontology:measurement(ped_be_t0, iss_ped_ban, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ped_be_t12, iss_ped_ban, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(ped_be_t24, iss_ped_ban, base_extractiveness, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iss_ped_ban, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(iss_ped_ban, 0.35).
narrative_ontology:affects_constraint(iss_ped_ban, iss_crew_autonomy_degradation).
narrative_ontology:affects_constraint(iss_ped_ban, space_rf_spectrum_management).

% DUAL FORMULATION NOTE:
% The PED ban is downstream of RF spectrum management constraints but represents a distinct institutional policy layer. The upstream RF constraint has structural roots in communication protocol fragility; the PED ban is a specific enforcement choice that degraded into theater. They should be analyzed as separate constraint stories linked by institutional coupling, not as a single phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iss_ped_ban, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
