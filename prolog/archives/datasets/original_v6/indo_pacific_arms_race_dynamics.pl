% ============================================================================
% CONSTRAINT STORY: indo_pacific_arms_race_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_pacific_arms_race_dynamics, []).

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
 *   constraint_id: indo_pacific_arms_race_dynamics
 *   human_readable: Indo-Pacific Arms Race Dynamics
 *   domain: geopolitical/security/military
 *
 * SUMMARY:
 *   The Indo-Pacific arms race represents a structural tension between
 *   legitimate security coordination and asymmetric extraction of resources.
 *   Rising tensions between regional powers, particularly China and the US,
 *   drive military modernization across the region. Developing economies face
 *   a security dilemma: they must invest in military capability to deter
 *   threats, yet this diverts resources from development and creates regional
 *   escalation dynamics. The constraint exhibits hybrid properties: genuine
 *   coordination functions (credible deterrence, alliance stability, conflict
 *   prevention) coexist with significant extraction mechanisms (opportunity
 *   costs for development spending, rent-seeking by defense industrial
 *   complex, proliferation risks). The theater ratio of 0.48 reflects
 *   moderate performative content — Cold War deterrence doctrine persists
 *   despite reduced superpower direct confrontation, but military procurement
 *   also reflects real security requirements. The extractiveness value (0.58)
 *   captures moderate-high rent-seeking overlaid on legitimate security
 *   spending. Different regional actors experience the constraint
 *   differently: developing economies see a trap, middle powers see mixed
 *   costs-benefits, hegemons see institutional opportunities, and the
 *   nonproliferation regime sees a temporary problem with structural
 *   solutions.
 *
 * KEY AGENTS:
 *   - Developing Regional Economies: Primary victims (powerless/trapped) — small nations bearing disproportionate arms spending burden with no viable exit from security dilemma
 *   - Regional Middle Powers: Secondary actors (moderate/constrained) — nations like India, Japan, South Korea, Vietnam face real security threats but constrained by budgetary limits and proliferation risks
 *   - Defense Industrial Complex: Primary beneficiary (institutional/arbitrage) — US, European, Russian, Chinese defense firms expand markets through regional competition and arms sales
 *   - Regional Hegemons (US, China): Powerful actors (powerful/mobile) — benefit from arms race through technological advancement, alliance consolidation, market control; can exit through strategic realignment
 *   - Nonproliferation Regime Coalition: Organized agents (organized/constrained) — NPT signatories, IAEA, export control regimes seek to stabilize through arms control agreements and transparency
 *   - Cold War Strategic Doctrine: Institutional framework (institutional/arbitrage) — military establishments, strategic doctrines maintain deterrence logic through institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — can see full structure from outside competitive dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_pacific_arms_race_dynamics, 0.58).
domain_priors:suppression_score(indo_pacific_arms_race_dynamics, 0.65).
domain_priors:theater_ratio(indo_pacific_arms_race_dynamics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_pacific_arms_race_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(indo_pacific_arms_race_dynamics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indo_pacific_arms_race_dynamics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_pacific_arms_race_dynamics, tangled_rope).
narrative_ontology:human_readable(indo_pacific_arms_race_dynamics, "Indo-Pacific Arms Race Dynamics").
narrative_ontology:topic_domain(indo_pacific_arms_race_dynamics, "geopolitical/security/military").

domain_priors:requires_active_enforcement(indo_pacific_arms_race_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_pacific_arms_race_dynamics, defense_industrial_complex).
narrative_ontology:constraint_beneficiary(indo_pacific_arms_race_dynamics, regional_hegemons).
narrative_ontology:constraint_beneficiary(indo_pacific_arms_race_dynamics, arms_exporters).
narrative_ontology:constraint_victim(indo_pacific_arms_race_dynamics, developing_regional_economies).
narrative_ontology:constraint_victim(indo_pacific_arms_race_dynamics, civilian_populations).
narrative_ontology:constraint_victim(indo_pacific_arms_race_dynamics, nonproliferation_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING REGIONAL ECONOMIES (SNARE) — Caught in security dilemma with no exit. Must allocate scarce resources to military procurement or face security vulnerability. Arms race competition imposes extraction on growth and development spending. Trapped by geopolitical forces beyond their control.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL MIDDLE POWERS (TANGLED ROPE) — Experience genuine security coordination need (deterrence, stability) but also face extraction through arms race escalation. Benefits from having modern military capability, but costs accumulate through budgetary pressure and proliferation risk. Can articulate exit (negotiated arms control) but face career/credibility costs.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEFENSE INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. Experiences arms race as pure coordination mechanism: demand for advanced systems drives innovation and market expansion. Arbitrage capacity through dual-use technology exports and strategic partnerships with regional clients. Net positive extraction flow.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NONPROLIFERATION REGIME COALITION (SCAFFOLD) — Organized agents (NPT signatories, IAEA, export control regimes) see arms race as temporary security failure with structural sunset: confidence-building measures, transparency mechanisms, and multilateral arms control agreements are designed to establish stable equilibrium with lower extraction. Sunset mechanism: verified arms control treaties that create credible commitment to reduced military spending.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR STRATEGIC DOCTRINE (PITON) — Institutional framework (deterrence theory, mutually assured destruction logic, forward basing) persists through inertia despite reduced direct superpower competition. Theater ratio (0.48) reflects that much arms spending continues despite diminished existential threat. The doctrine has lost functional coherence but institutional actors (military establishments, defense bureaucracies) maintain it through performative commitment to 'credible deterrence.' Degraded but locked in.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL HEGEMONS (TANGLED ROPE) — Powerful actors benefit from arms race (technological innovation, alliance consolidation, market dominance) while also bearing genuine security coordination burden. Can exit through strategic partnership or conflict, but both options carry geopolitical costs. Mobile at highest level but constrained by reputational commitments and alliance obligations.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the arms race exhibits both genuine coordination function (establishing credible deterrence, signaling capability, maintaining alliance stability) and significant extraction mechanisms (rent-seeking, proliferation risk, opportunity cost of development spending). The constraint is neither immutable law nor pure coordination but a hybrid where asymmetric extraction concentrates on developing economies while powerful actors benefit disproportionately from the security structure.
constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_pacific_arms_race_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_pacific_arms_race_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_pacific_arms_race_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_pacific_arms_race_dynamics, TR),
    TR >= 0.70.

:- end_tests(indo_pacific_arms_race_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The arms race imposes real opportunity costs on developing economies — military spending diverts resources from infrastructure, education, healthcare. However, the constraint also provides genuine security benefits (deterrence against regional aggression, alliance stability), so extraction is not maximal. The value reflects that beneficiaries (defense contractors, hegemonic states) capture disproportionate gains while costs concentrate on weaker actors. Suppression (0.65): High. Multiple barriers prevent exit from the arms race: strategic competition makes unilateral disarmament suicidal for smaller states; alliance commitments lock in continued procurement; international arms markets create dependency; domestic political actors (military establishments, nationalist constituencies) resist arms control. Yet suppression is not total — multilateral arms control and confidence-building measures offer gradual reduction pathways. Theater ratio (0.48): Moderate. Cold War deterrence doctrine contains significant performative elements (forward basing, military displays, strategic rhetoric) that persist despite reduced direct superpower confrontation. However, real military modernization addresses genuine technological advancement and emerging threats (hypersonic weapons, AI-enabled systems, space militarization), so theater is not dominant. The trajectory shows theater rising slightly (0.38 → 0.48) as arms spending accelerates but rhetoric about defense necessity intensifies without corresponding threat escalation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural inequality produces conflicting classifications from different positions. Developing economies see Snare (trapped by security dilemma, bearing maximum costs, no exit); middle powers see Tangled Rope (mixed benefits and burdens, some constrained exit options); hegemons see Rope (coordination mechanism that serves their interests); defense contractors see Rope (pure market coordination); nonproliferation regime sees Scaffold (temporary problem with institutional sunset through arms control); Cold War doctrine appears as Piton (degraded ritual persisting through inertia); analytical observer sees Tangled Rope (hybrid with genuine security function and significant extraction). The perspectival gap is not measurement uncertainty but structural inequality — different agents occupy genuinely different positions in the extraction flow. The constraint cannot appear as Mountain from any position because the security dilemma, while stable, is not immutable — history shows arms races can be reversed through negotiation and confidence-building.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary sharply across agent positions. Developing economies appear as powerless/trapped victims — high d (0.92-0.95) reflecting that they bear costs without controlling conditions. Regional middle powers show moderate d (0.55-0.65) reflecting mixed costs and benefits — they gain some deterrent capability but face significant budgetary pressure. Defense contractors show low d (0.10-0.15) as institutional beneficiaries — the arms race drives their business model. Regional hegemons show intermediate d (0.45-0.55) reflecting powerful but constrained positions — they benefit from the arms race but cannot simply exit without losing regional influence. The analytical observer at civilizational scope shows d (0.68-0.72) reflecting balanced assessment of both coordination function and extraction mechanism. No agent experiences pure coordination (d near 0.5 from all perspectives) — the perspectival gap is fundamental and derives from structural inequality in exit options and cost distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that this constraint is genuinely hybrid — it coordinates security relationships while extracting resources disproportionately from weaker actors. The analytical classification as Tangled Rope is correct: (1) coordination function is real — the constraint does establish credible deterrence and prevent escalation to overt conflict in some contexts; (2) asymmetric extraction is real — benefits concentrate on defense contractors and hegemons while costs fall on developing economies; (3) active enforcement is real — military establishments, alliance commitments, and strategic doctrine actively maintain the constraint; (4) both beneficiaries (defense contractors, hegemons) and victims (developing economies, populations bearing opportunity costs) exist. The mandatrophy is not 'which type is correct?' but 'how much coordination benefit justifies the extraction cost?' The analytical perspective answers: moderate coordination function (deterrence is real but theater is also significant) + moderate extraction (costs are substantial but not maximal) = Tangled Rope is stable classification. The constraint could transition to pure Rope if confidence-building measures reduced perceived threat (lowering extraction), or to Snare if hegemonic competition intensified and developing economies faced maximal security pressure with no exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_dilemma_irreducibility,
    'Is the Indo-Pacific arms race an irreducible security dilemma (each actor''s defensive actions appear offensive to others) or a contingent institutional arrangement that could be dissolved through confidence-building measures?',
    'Comparison of scenarios: (1) track record of multilateral arms control agreements and their stability; (2) analysis of offense/defense balance in regional military technologies; (3) assessment of whether actors perceive each other''s weapons as defensive or threatening',
    'If irreducible: constraint approaches Mountain from some perspectives; exit becomes impossible regardless of negotiation. If contingent: constraint remains Tangled Rope; arms control offers real pathway to reduced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_irreducibility, empirical, 'Whether arms race is irreducible security dilemma or contingent institutional arrangement').

omega_variable(
    extraction_flow_concentration,
    'Does extraction concentrate disproportionately on developing regional economies (small nations bearing arms race burden) or distribute relatively evenly across all regional actors?',
    'Comparative analysis of military spending as percentage of GDP, development impact, and opportunity cost across regional economies; identify threshold where arms spending begins displacing critical infrastructure and human development investments',
    'If concentrated: Snare classification for developing economies is confirmed; poweerless perspective is accurate. If distributed: constraint approaches Rope from more perspectives; coordination benefit is more evenly shared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_flow_concentration, empirical, 'Whether extraction concentrates on developing economies').

omega_variable(
    defense_industrial_coordination_necessity,
    'To what extent does innovation and technological advancement in defense systems depend on arms race competition versus would occur through other research incentives (academic, space, climate)?',
    'Historical analysis of dual-use technology development; comparison of innovation rates in competitive vs collaborative defense research models; identification of weapons systems that emerged from arms race pressure vs other drivers',
    'If arms race is primary innovation driver: defense industrial complex''s rope classification is accurate; removing constraint would reduce technological capability. If innovation would occur through alternative channels: defense complex benefits are partly rent-seeking; extraction is higher than functional coordination requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_industrial_coordination_necessity, empirical, 'Whether arms race is necessary for defense innovation').

omega_variable(
    confidence_building_mechanism_effectiveness,
    'Do confidence-building measures (transparency, communication channels, joint exercises, verification protocols) actually reduce arms race escalation or merely create performative appearances of cooperation?',
    'Analysis of arms spending trajectories in regions with active CBMs vs regions without; track record of treaty compliance; assessment of whether CBMs correlate with reduced military procurement or merely with reduced rhetoric',
    'If effective: Scaffold perspective is correct; sunset mechanism (institutional arms control regime) offers real exit path. If performative: CBMs are theatrical, and constraint remains Snare for developing economies despite diplomatic appearance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confidence_building_mechanism_effectiveness, empirical, 'Whether confidence-building measures effectively reduce escalation').

omega_variable(
    alliance_lock_in_vs_choice,
    'Do regional actors actively choose security partnerships with hegemons (mobile exit option) or are they locked in through structural dependency and path dependence?',
    'Historical analysis of alliance switching costs; examination of whether alternatives to hegemon partnerships exist; assessment of domestic political constraints on alliance realignment',
    'If active choice: all perspectives should classify constraint as lower extraction; agents have genuine exit options. If locked in: constraint is more extractive; suppression and trap indicators are higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_lock_in_vs_choice, empirical, 'Whether alliance partnerships are chosen or locked-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_pacific_arms_race_dynamics, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipard_tr_t0, indo_pacific_arms_race_dynamics, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ipard_tr_t10, indo_pacific_arms_race_dynamics, theater_ratio, 10, 0.43).
narrative_ontology:measurement(ipard_tr_t20, indo_pacific_arms_race_dynamics, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ipard_tr_t5, indo_pacific_arms_race_dynamics, theater_ratio, 5, 0.4).

% Extraction over time
narrative_ontology:measurement(ipard_be_t0, indo_pacific_arms_race_dynamics, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ipard_be_t10, indo_pacific_arms_race_dynamics, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ipard_be_t20, indo_pacific_arms_race_dynamics, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ipard_be_t5, indo_pacific_arms_race_dynamics, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_pacific_arms_race_dynamics, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(indo_pacific_arms_race_dynamics, 0.12).
narrative_ontology:affects_constraint(indo_pacific_arms_race_dynamics, south_china_sea_maritime_competition).
narrative_ontology:affects_constraint(indo_pacific_arms_race_dynamics, semiconductor_supply_chain_militarization).
narrative_ontology:affects_constraint(indo_pacific_arms_race_dynamics, nuclear_proliferation_regional_dynamics).

% DUAL FORMULATION NOTE:
% Indo-Pacific arms race dynamics is upstream of specific territorial disputes (South China Sea) and technology competition (semiconductors). It provides the structural framework within which more granular conflicts operate. The arms race extractiveness value (0.58) reflects general regional militarization; specific disputes have their own extraction metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_pacific_arms_race_dynamics, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
