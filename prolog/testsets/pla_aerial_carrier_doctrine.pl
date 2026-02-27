% ============================================================================
% CONSTRAINT STORY: pla_aerial_carrier_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pla_aerial_carrier_doctrine, []).

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
 *   constraint_id: pla_aerial_carrier_doctrine
 *   human_readable: China's Development of an Aerial Drone Carrier Doctrine
 *   domain: technological/military/political
 *
 * SUMMARY:
 *   The PLA's development of an aerial carrier doctrine — a large unmanned
 *   aerial vehicle capable of launching and recovering smaller drone swarms —
 *   represents a strategic constraint that operates across multiple layers:
 *   technological capability, regional power projection, doctrinal
 *   competition, and international rules-based order legitimacy. The
 *   constraint exhibits classic snare mechanics at the regional victim level
 *   (powerless states in contested waters face drone dominance with no exit)
 *   but generates complex tangled-rope dynamics at the coalition/great-power
 *   level (forced military escalation and counter-capability investment). The
 *   extractiveness trajectory reflects increasing operational maturity and
 *   doctrinal integration from 2014-2024, while theater ratio decreases as
 *   the capability shifts from speculative concept to operational doctrine.
 *   The constraint is downstream of Chinese strategic ambitions in the
 *   Indo-Pacific but represents a distinct structural innovation that
 *   reshapes air power dynamics independent of broader geopolitical
 *   competition.
 *
 * KEY AGENTS:
 *   - PLA Strategic Command: Primary beneficiary (institutional/arbitrage) — doctrine enables force projection and aerial carrier logistics solution
 *   - Contested Airspace States (Vietnam, Philippines, Taiwan neighbors): Primary victims (powerless/trapped) — face unilateral drone capability with no defense or exit
 *   - Regional Coalition (QUAD/AUKUS): Secondary victim and organized responder (organized/constrained) — forced into counter-capability development; constrained by geography and logistics
 *   - Contested Territory Civilian Populations: Secondary victims (moderate/mobile) — face surveillance and strike capability; theoretical mobility (emigration) but practically trapped
 *   - US Military Establishment: Mixed (powerful/arbitrage) — benefits from justified budget increases but forced into reactive posture; arbitrage exit constrained politically
 *   - International Rules-Based Order: Institutional actor (institutional/arbitrage) — doctrine operates under veneer of 'international waters freedom' but actually extracts de facto sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, 0.58).
domain_priors:suppression_score(pla_aerial_carrier_doctrine, 0.68).
domain_priors:theater_ratio(pla_aerial_carrier_doctrine, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pla_aerial_carrier_doctrine, snare).
narrative_ontology:human_readable(pla_aerial_carrier_doctrine, "China's Development of an Aerial Drone Carrier Doctrine").
narrative_ontology:topic_domain(pla_aerial_carrier_doctrine, "technological/military/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, regional_air_sovereignty).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, contested_airspace_states).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, asymmetric_defense_postures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTESTED AIRSPACE STATE (SNARE) — Small or medium-sized nation in contested waters (South China Sea, East China Sea) faces PLA aerial carrier doctrine with no practical exit. Cannot match capability; constrained politically to accept de facto aerial dominance. Full victim — bears extraction without recourse or deterrent capability.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL COALITION (TANGLED ROPE) — Organized states (US, Japan, India, Australia) respond with counter-doctrine (distributed air defense, sensor networks, allied air patrols). The doctrine creates a coordination problem (building allied response) but also extracts through forced military investment and doctrinal escalation. Coalition members constrained by geography and logistics but retain strategic agency.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLA STRATEGIC COMMAND (ROPE) — Experiences the doctrine as a coordination mechanism — pooling drone assets under a carrier platform solves logistics and deployment problems. Net beneficiary; the constraint enables force projection. Arbitrage exit: can shift to hypersonic delivery, satellite swarms, or other escalation vectors.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTESTED TERRITORY CIVILIAN POPULATION (SNARE) — Taiwan, Philippines, Vietnam, or other adjacent populations face escalated drone surveillance and strike capability with limited protection. Mobility is hypothetical (expensive, slow); suppression is high (cannot organize counter-capability). Extraction visible as loss of airspace freedom and privacy.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: US MILITARY ESTABLISHMENT (TANGLED ROPE) — Benefits from doctrine validation (justifies budget increases, force posture shifts, allied dependency). Also bears extraction through forced doctrinal response and deployment costs. Powerful actor with arbitrage exit (shift posture to Middle East, reassess alliance burden) — but faces political constraints on exit.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL RULES-BASED ORDER (PITON) — Doctrine is marketed as operating within 'freedom of navigation' and 'international waters' norms, but its actual function is to establish de facto sovereignty over contested zones. The performative claim of rules-based operation masks unilateral extraction. Theater ratio high (doctrine presented as defensive/normal) despite function as offense. Piton classification: the institutional inertia of rules-based framing persists despite the mechanism having shifted to extraction.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_aerial_carrier_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pla_aerial_carrier_doctrine, TR),
    TR >= 0.70.

:- end_tests(pla_aerial_carrier_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The doctrine extracts regional air sovereignty from contested states through unilateral capability and creates asymmetric deterrence against coalition responses. The value reflects operational maturity (not purely theoretical) and functional integration into regional coercion (not yet fully strategic). If operational platforms exceed 5-6 units and integrate into offensive doctrine, extractiveness would rise to 0.70+. Current value (0.58) assumes 2-4 operational platforms with specialized reconnaissance/harassment role. Suppression (0.68): High. Contested states have no practical defense against aerial carriers (cost, complexity, political constraints prevent matching). Coalition response is possible but expensive and logistically constrained. Asymmetric power difference creates suppression through inability to counter-escalate symmetrically. Theater ratio (0.35): Low and declining. The doctrine's function is increasingly real (operational deployment, system integration) rather than performative. Initial theater ratio was higher (0.42) when capability was speculative; as it matures, the function becomes visible and the performance justification necessary decreases. This inverse relationship (theater declining while extractiveness rising) indicates transition from threat signaling to operational extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals how the same capability is experienced as pure extraction (snare) by powerless victims, forced coordination (tangled rope) by organized responders, beneficial logistics (rope) by the beneficiary, and legitimacy maintenance (piton) by the rules-based order. Contested states see unilateral coercion; coalitions see doctrinal competition requiring investment; the PLA sees a coordination solution to force projection. The US military sees doctrine validation (justifies budget) but also sees forced posture shift (extraction through required response). The piton perspective is critical: the doctrine operates within claimed 'international waters freedom' norms, yet its actual function is to establish de facto aerial sovereignty. The international order sees its own norms being used to legitimize capability that undermines those norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position. Contested states are full victims (trapped exit, no arbitrage, no mobility) — d approaches 1.0, experiencing maximum f(d). The PLA is beneficiary with institutional power and arbitrage exit — d approaches 0.0-0.1, experiencing low or negative extraction. Coalition members are organized with constrained (not mobile) exit — d ~0.5-0.6, experiencing moderate extraction despite organized power. The US military is powerful but politically constrained on exit — d ~0.4-0.5, experiencing moderate extraction despite power. The rules-based order is an institutional actor experiencing extraction through norms-erosion but maintains arbitrage (can theoretically redefine norms) — d ~0.3-0.4, experiencing low-moderate extraction. The civilians in contested territory have theoretical mobility but practical traps — d ~0.8-0.9, experiencing high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exceed extractiveness 0.70, so mandatrophy resolution is not required. However, mandatrophy tension is visible: the doctrinal claim is that aerial carriers are 'defensive force projection' enabling 'freedom of navigation.' The structural reality is asymmetric capability enabling de facto sovereignty extraction. The false mountain perspective (international rules-based order / piton) attempts to naturalize this by framing it as normal naval doctrine evolution. Mandatrophy resolution would require either: (a) accepting that the doctrine is extractive (snare classification) and abandoning rules-based framing, or (b) accepting that extraction is justified because it enforces international order (tangled-rope with coordination benefits). Current framing (piton/theater) defers this resolution by maintaining performative rules-compliance while functional extraction proceeds. If operational maturity increases to 5+ platforms with clear offensive integration, extractiveness will exceed 0.70 and mandatrophy resolution will become unavoidable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_maturity_timeline,
    'How many operational aerial carrier platforms does the PLA actually possess, and at what operational maturity?',
    'Satellite imagery analysis, technical documentation capture, defector testimony, operational deployment evidence',
    'If 1-2 prototypes: doctrine is aspirational (Piton-Rope hybrid). If 5+: doctrine is functional (full Snare/Tangled Rope). Maturity affects extraction timeline by 5-10 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_maturity_timeline, empirical, 'Actual capability maturity and operational platform count').

omega_variable(
    doctrinal_integration_scope,
    'Is the aerial carrier doctrine integrated into offensive PLA planning or remain a specialized reconnaissance/harassment capability?',
    'PLA strategic doctrine publications, military exercise patterns, force procurement correlations, defector technical analysis',
    'If integrated into offensive doctrine: extractiveness rises to 0.70+ (Mountain-breaking snare). If specialized harassment: remains 0.55-0.65 (tactical snare). Classification impact is significant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_integration_scope, empirical, 'Integration of aerial carrier into PLA strategic offensive doctrine').

omega_variable(
    escalation_response_coupling,
    'Does allied counter-development (directed energy, autonomous swarms, hypersonic defense) couple tightly to PLA doctrine evolution, or operate on independent timelines?',
    'Correlation analysis of PLA capability announcements vs allied budget/procurement cycles; doctrine publication timing; deployment pattern responses',
    'If tightly coupled: tangled-rope perspectives are correct (forced coordination). If independent: snare perspectives dominate (extraction without genuine response requirement). Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_response_coupling, empirical, 'Coupling between PLA doctrine evolution and allied counter-development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pla_aerial_carrier_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plaacd_tr_t0, pla_aerial_carrier_doctrine, theater_ratio, 0, 0.42).
narrative_ontology:measurement(plaacd_tr_t5, pla_aerial_carrier_doctrine, theater_ratio, 5, 0.38).
narrative_ontology:measurement(plaacd_tr_t10, pla_aerial_carrier_doctrine, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(plaacd_be_t0, pla_aerial_carrier_doctrine, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(plaacd_be_t5, pla_aerial_carrier_doctrine, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(plaacd_be_t10, pla_aerial_carrier_doctrine, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pla_aerial_carrier_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, south_china_sea_sovereignty_dispute).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, us_indo_pacific_force_posture).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, regional_air_defense_procurement).

% DUAL FORMULATION NOTE:
% The aerial carrier doctrine is downstream of Chinese strategic ambitions in the Indo-Pacific but represents a technologically distinct constraint. The doctrine itself creates extraction independent of the broader geopolitical competition. Upstream constraints (strategic ambition, naval modernization) influence doctrine development; downstream constraints (allied counter-capability, regional militarization) are structured by doctrine operational maturity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
