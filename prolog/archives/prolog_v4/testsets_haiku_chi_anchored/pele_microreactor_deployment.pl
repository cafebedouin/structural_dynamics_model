% ============================================================================
% CONSTRAINT STORY: pele_microreactor_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pele_microreactor_deployment, []).

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
 *   constraint_id: pele_microreactor_deployment
 *   human_readable: Pele Mobile Micro-Reactor Deployment Protocol
 *   domain: technological/geopolitical/energy_security
 *
 * SUMMARY:
 *   Project Pele is the U.S. Department of Defense's mobile micro-reactor
 *   program designed to power forward-deployed military bases with
 *   transportable 10 MW thermal reactors. The deployment protocol governing
 *   Pele creates a structural constraint that distributes costs and benefits
 *   asymmetrically: the U.S. military and contractors capture energy
 *   independence and operational flexibility benefits, while host nations and
 *   the international nuclear safety commons bear long-term liability,
 *   security risks, and regulatory circumvention. The constraint exhibits
 *   characteristics of a tangled rope (hybrid coordination and extraction)
 *   from most perspectives, but appears as pure extraction (snare) to host
 *   nations and as performance theater (piton) from the standpoint of Cold
 *   War nuclear doctrine. The constraint intensifies over time as theater
 *   ratio rises (increasing performative narrative relative to actual
 *   operational energy contribution) and extractiveness increases (liability
 *   and sovereignty costs accumulate). The renewable energy transition
 *   perspective introduces a sunset logic: advanced battery storage and
 *   distributed solar could obsolete the need for mobile reactors within
 *   15-20 years, making the constraint temporarily justified as a bridge
 *   technology.
 *
 * KEY AGENTS:
 *   - U.S. Department of Defense Nuclear Program Management: Primary institutional beneficiary (institutional/arbitrage) — controls deployment, gains energy independence and strategic signaling
 *   - Defense Contractors (Westinghouse, GE Hitachi): Secondary institutional beneficiary (institutional/arbitrage) — secure contracts and advanced reactor development funding
 *   - Host Nation (Allied Powers: Japan, South Korea, UK, Germany): Primary victim (powerless/trapped) — bears long-term nuclear liability and environmental risk without operational control
 *   - International Nuclear Safety Regulatory Community (IAEA, national regulators): Organized partial victim (organized/constrained) — constrained by military exemptions from civilian oversight, simultaneously expected to provide legitimacy
 *   - Allied Combatant Commanders (INDOPACOM, EUCOM, CENTCOM): Secondary beneficiary-victim (powerful/constrained) — benefit from energy autonomy but constrained by U.S. approval process and operational protocols
 *   - Renewable Energy Transition Coalition (climate advocates, grid modernization engineers): Organized agents (organized/mobile) — see deployment as temporary bridge; advocate for sunset transition to renewables
 *   - Cold War Nuclear Strategic Doctrine Apparatus: Institutional inertial beneficiary (institutional/constrained) — maintains nuclear deterrence narrative; gate-keeps technology deployment
 *   - Environmental and Nuclear Safety Commons: Powerless victim (powerless/trapped) — abstract collective bearing diffuse long-term risks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pele_microreactor_deployment, 0.58).
domain_priors:suppression_score(pele_microreactor_deployment, 0.68).
domain_priors:theater_ratio(pele_microreactor_deployment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pele_microreactor_deployment, extractiveness, 0.58).
narrative_ontology:constraint_metric(pele_microreactor_deployment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pele_microreactor_deployment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pele_microreactor_deployment, tangled_rope).
narrative_ontology:human_readable(pele_microreactor_deployment, "Pele Mobile Micro-Reactor Deployment Protocol").
narrative_ontology:topic_domain(pele_microreactor_deployment, "technological/geopolitical/energy_security").

domain_priors:requires_active_enforcement(pele_microreactor_deployment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, us_military_logistics).
narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, defense_contractors).
narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, energy_independence_advocates).
narrative_ontology:constraint_victim(pele_microreactor_deployment, host_nation_sovereignty).
narrative_ontology:constraint_victim(pele_microreactor_deployment, environmental_liability_bearers).
narrative_ontology:constraint_victim(pele_microreactor_deployment, nuclear_safety_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOST NATION (SNARE) — Once a Pele reactor is deployed on sovereign territory, the host nation bears long-term environmental and security liability without proportional control over operational parameters. Cannot exit without geopolitical cost. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72. The constraint extracts liability asymmetrically.
constraint_indexing:constraint_classification(pele_microreactor_deployment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ALLIED COMBATANT COMMANDER (TANGLED ROPE) — Benefits from energy independence and extended operational range, but constrained by U.S. approval process and security protocols. Experiences the system as both coordination (energy access) and extraction (operational constraints). d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.44. Hybrid structure confirmed by beneficiary status (energy access) + victim status (constrained autonomy).
constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DoD NUCLEAR PROGRAM MANAGEMENT (ROPE) — Primary beneficiary. Controls deployment decisions, technology transfer, and security protocols. Experiences the constraint as a coordination mechanism enabling extended operational reach and competitive advantage against peer adversaries. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(pele_microreactor_deployment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Significant beneficiaries. Secure long-term contracts and technology development funding. The deployment protocol legitimizes advanced reactor R&D. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.09. Strong net beneficiary position.
constraint_indexing:constraint_classification(pele_microreactor_deployment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTL NUCLEAR SAFETY REGULATORS (TANGLED ROPE) — Partially benefits from advanced reactor technology demonstration and safety innovation. Simultaneously constrained by military exemptions from civilian oversight and the creation of nuclear liability precedents outside traditional regulatory frameworks. d≈0.55, f(d)≈0.74, σ=1.2 → χ≈0.55. Active enforcement via IAEA protocols exists, but military deployments circumvent civilian regulation.
constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR STRATEGIC DOCTRINE (PITON) — The deployment protocol maintains narratives of nuclear deterrence and strategic advantage that predate advanced reactor technology. The real function is political messaging and alliance commitment signaling; actual operational energy contribution is minimal relative to grid power availability. theater_ratio=0.65 reflects the gap between strategic narrative and actual operational utility. The constraint persists through institutional inertia and doctrinal momentum.
constraint_indexing:constraint_classification(pele_microreactor_deployment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: RENEWABLE TRANSITION COALITION (SCAFFOLD) — Views Pele as a temporary energy bridge for military bases during a 15-20 year transition to renewable microgrids. The deployment creates fiscal and political momentum for base energy autonomy, but the constraint has a sunset: distributed solar, battery storage, and microgrids will obsolete the need for transportable reactors within a generational timeframe. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.19. Suppression declines as alternatives mature.
constraint_indexing:constraint_classification(pele_microreactor_deployment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL / THERMODYNAMIC LIMITS (MOUNTAIN) — From a civilizational/physical perspective, advanced reactors represent fundamental efficiency gains and decay-heat management that are structural properties of reactor design, not political choices. However, the base properties (ε=0.58, suppression=0.68, theater=0.65) contradict mountain classification — this is a false summit. The 'natural law' framing naturalizes geopolitical choices (deployment protocol, liability assignment, security controls) that are contingent.
constraint_indexing:constraint_classification(pele_microreactor_deployment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pele_microreactor_deployment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pele_microreactor_deployment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pele_microreactor_deployment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pele_microreactor_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pele_microreactor_deployment, TR),
    TR >= 0.70.

:- end_tests(pele_microreactor_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant asymmetries: U.S. captures energy independence benefits and strategic messaging while host nations assume liability and environmental risk. However, extractiveness is not as severe as pure predatory extraction (snare range ≥0.66) because allied combatant commanders do benefit operationally and some coordination function exists (energy provision). The value reflects that the extraction is substantial but not maximal — host nations do gain from expanded U.S. military presence and energy reliability, even if the liability asymmetry is unfair. Suppression (0.68): High. Significant barriers to challenging or refusing deployment include alliance dependency, geopolitical coercion, information asymmetry about long-term liability, and the absence of international oversight mechanisms that could protect host nation interests. Host nations have limited veto power and face retaliation costs for asserting nuclear sovereignty concerns. Theater ratio (0.65): Moderate-high. Strategic narrative (advanced reactor technology, energy independence, deterrence signaling) exceeds actual operational contribution. Most bases maintain grid connections and diesel fallback; actual energy contribution from Pele is estimated at 30-50% in practice. The performative element has grown over the interval as political messaging has intensified while technical readiness delays accumulate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a classic extraction-as-coordination collapse. The U.S. military and contractors see the deployment protocol as pure coordination (rope) — solving a real logistical problem of powering remote bases. Allied combatant commanders see tangled rope — they benefit from energy autonomy but are constrained by U.S. approval gates and operational restrictions. Host nations see pure extraction (snare) — they absorb environmental liability and security risks without proportional control or compensation. The international nuclear regulatory community sees its own institutional bypassing (tangled rope) — they are expected to legitimize a system that exempts military deployments from civilian safeguard oversight. The Cold War strategic doctrine apparatus sees the deployment as validating deterrence narratives (piton) — the real function is political messaging, not energy provision. The renewable transition coalition sees a temporary problem with a sunset (scaffold) — the constraint will dissolve as battery storage and distributed solar mature. The analytical observer risks naturalizing the entire arrangement as inherent to nuclear technology (false mountain). These perspectival differences are not measurement ambiguities — they reflect structurally real differences in who pays, who decides, and who benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Host nation: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Faces long-term liability without exit or meaningful control over deployment parameters. Energy benefits do not proportionally compensate for the asymmetric risk. U.S. DoD: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Controls deployment, retains technology, can exit if alliance relationships change. Allied combatant commander: Mixed beneficiary-victim + constrained → d≈0.58, f(d)≈0.75. Moderate extraction. Benefits from energy access but cannot independently refuse deployment or modify security protocols. Regulatory community: Institutional + constrained → d≈0.55, f(d)≈0.74. Significant extraction via institutional bypass. Expected to legitimize a system from which military actors are exempt. Renewable coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; they have agency to develop alternatives and a clear exit path (renewable replacement). Cold War doctrine: Institutional + arbitrage → d≈0.10 but theater_ratio ≥0.65 triggers piton classification. Strategic benefit without real operational energy (d_value indicates beneficiary position, but performative theater signals degradation). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival and likely false; the constraint is geopolitical, not thermodynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits at the boundary between tangled rope and snare, with the classification depending critically on whose perspective dominates and how liability is assigned. The mandatrophy is resolved by showing that the constraint exhibits genuine coordination benefits (energy provision, operational independence) AND irreducible extraction (liability asymmetry, sovereignty circumvention, regulatory bypass). The challenge is distinguishing fair coordination incentives from extractive asymmetry. From the U.S. perspective, the constraint coordinates the solution to an energy logistics problem. From the host nation perspective, the constraint is coercive liability extraction. Both are structurally correct. The constraint avoids false snare classification (pure extraction without coordination) because energy provision is real. It avoids false rope classification (pure coordination without extraction) because liability and sovereignty asymmetries are real. The intermediate classification (tangled rope) is accurate. However, the extractiveness (0.58) approaches the snare threshold (0.66), and the mandatrophy is not resolved until the liability assignment mechanisms are clarified via omega resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_assignment_mechanism,
    'How is long-term nuclear waste and accident liability assigned between the U.S., host nation, and contractors when a Pele reactor operates outside civilian regulatory jurisdiction?',
    'Examination of deployment agreements, contractor indemnification clauses, and precedent from similar military-controlled nuclear installations (Camp Humphreys, Rota AB, Thule AB)',
    'If liability remains with host nation: snare classification confirmed. If liability remains with U.S.: tangled rope classification more likely. If contractually ambiguous: classification drifts between snare and piton depending on enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_assignment_mechanism, empirical, 'Liability assignment mechanism in deployment agreements').

omega_variable(
    operational_energy_contribution_actual,
    'What fraction of a deployed base''s total energy demand is actually supplied by Pele reactors in practice, versus grid connection fallback and emergency diesel?',
    'Operational data from Pele pilot installations; comparison to contractor performance claims; analysis of grid interconnection capacity requirements.',
    'If contribution < 40%: theater_ratio and piton classification are understated (actual theater ≥ 0.75). If contribution > 60%: rope classification becomes more plausible for combatant commanders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_energy_contribution_actual, empirical, 'Actual energy contribution of deployed Pele reactors').

omega_variable(
    host_nation_approval_veto_power,
    'Can a host nation deny or revoke Pele deployment consent without triggering U.S. alliance sanctions or geopolitical retaliation?',
    'Analysis of Status of Forces Agreements (SOFAs), deployment frameworks, and historical precedent for host nation veto power (e.g., Okinawa basing disputes, Philippine VFA suspension threats)',
    'If veto power exists and is credible: victim classification changes from trapped to constrained; snare classification shifts toward tangled rope. If veto is nominal: trapped exit confirmed; snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_nation_approval_veto_power, empirical, 'Host nation veto power over reactor deployment').

omega_variable(
    cybersecurity_attack_surface_scope,
    'What is the actual surface of cyber/kinetic attack surface introduced by integrating a transportable reactor with a deployed base''s power grid and command infrastructure?',
    'CISA / DoD cybersecurity assessments; comparison to legacy diesel generator attack surface; modeling of cascade failure risk from single-point-of-failure reactor integration.',
    'If attack surface increase is > 2x baseline: suppression and snare classification strengthened. If increase is minimal (isolated reactor with fiber barriers): tangled rope more plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cybersecurity_attack_surface_scope, empirical, 'Cyber/kinetic attack surface of integrated Pele reactor systems').

omega_variable(
    renewable_alternative_timeline_viability,
    'Can military bases achieve equivalent energy autonomy via distributed renewable + battery storage on the same 20-year timeline as Pele deployment, or is the renewable path technically or politically blocked?',
    'Technical assessment of base-scale renewable capacity, battery storage economics, and grid-independent microgrid viability; policy analysis of military procurement timelines and R&D funding allocation.',
    'If renewable path is viable: scaffold sunset logic is real. If blocked by technical/political factors: scaffold degrades to piton or tangled rope (constraint becomes permanent, not temporary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_alternative_timeline_viability, empirical, 'Viability of renewable energy alternative for military base autonomy').

omega_variable(
    international_treaty_regime_applicability,
    'Are deployed Pele reactors subject to NPT safeguards, IAEA verification, or other international nuclear oversight, or are they classified as military installations exempt from civilian regulatory frameworks?',
    'Analysis of U.S. NPT declarations, IAEA safeguards agreements, military exemption precedents, and State Department policy on military reactor disclosure',
    'If subject to IAEA oversight: international regulators have constrained agency; tangled rope from their perspective confirmed. If exempt: victims (host nation, safety commons) have no institutional recourse; snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_treaty_regime_applicability, empirical, 'International treaty regime applicability to military Pele reactors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pele_microreactor_deployment, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pele_tr_t0, pele_microreactor_deployment, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pele_tr_t8, pele_microreactor_deployment, theater_ratio, 8, 0.6).
narrative_ontology:measurement(pele_tr_t16, pele_microreactor_deployment, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(pele_be_t0, pele_microreactor_deployment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pele_be_t8, pele_microreactor_deployment, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(pele_be_t16, pele_microreactor_deployment, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pele_microreactor_deployment, resource_allocation).
narrative_ontology:boltzmann_floor_override(pele_microreactor_deployment, 0.48).
narrative_ontology:affects_constraint(pele_microreactor_deployment, alliance_energy_security).
narrative_ontology:affects_constraint(pele_microreactor_deployment, military_nuclear_proliferation_risk).
narrative_ontology:affects_constraint(pele_microreactor_deployment, host_nation_sovereign_liability_regimes).

% DUAL FORMULATION NOTE:
% The Pele deployment protocol decomposes into three structurally distinct constraints: (1) the resource allocation problem (energy provision at remote bases) — separable, lower extraction; (2) the geopolitical alliance signaling function (nuclear deterrence narratives) — theater-dominated, piton characteristics; (3) the liability regime externalization (host nation assumption of long-term nuclear risk) — extraction-dominated, snare characteristics from host nation perspective. This story treats the deployment protocol as the unified constraint; decomposition into separate stories may be warranted if omega resolution yields distinct ε values for the energy coordination vs. the liability externalization components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pele_microreactor_deployment, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
