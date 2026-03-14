% ============================================================================
% CONSTRAINT STORY: autonomous_weapons_international_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomous_weapons_international_law, []).

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
 *   constraint_id: autonomous_weapons_international_law
 *   human_readable: Autonomous Weapons Systems International Legal Framework
 *   domain: international_law/military_technology/arms_control
 *
 * SUMMARY:
 *   The international legal framework governing autonomous weapons systems
 *   embodies a core structural tension between legitimate military
 *   coordination needs and extractive normative degradation. Technologically
 *   advanced militaries require systems capable of operating in contested,
 *   high-speed environments where human reaction time becomes operationally
 *   impossible. This creates a genuine coordination problem: all major powers
 *   need some minimum autonomy capability to maintain strategic stability.
 *   However, layered atop this coordination function is an asymmetric
 *   extraction mechanism: the development of autonomous weapons concentrates
 *   military capability in technologically advanced states, erodes civilian
 *   protection norms that were built on assumptions of human intentionality
 *   and proportionality reasoning, and creates irreversible risks that weaker
 *   states and civilian populations cannot meaningfully exit. The constraint
 *   is neither pure coordination (which would make it Rope) nor pure
 *   extraction (which would make it simple Snare). It is Tangled Rope:
 *   genuine coordination wrapped around asymmetric extraction. The theater
 *   ratio of 0.68 reflects how much of the constraint's institutional
 *   apparatus is performative—legal language about human control, command
 *   procedures, proportionality review—rather than functionally protective of
 *   civilians when actual systems operate at machine-decision speeds.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Trapped victims (powerless/trapped) — cannot exit risk zones or negotiation processes; bear extraction cost of normative degradation
 *   - International Humanitarian Law: Normative system victim (powerless/trapped) — principles of distinction and proportionality become structurally unenforceable as technology outpaces human cognition; norms extracted from without restoration mechanism
 *   - Non-Aligned and Weaker Military States: Constrained victims (moderate/constrained) — face security dilemma: adoption accelerates vulnerability, non-adoption ensures strategic disadvantage; exit options are expensive or dangerous
 *   - Advanced Military Powers: Primary beneficiaries (institutional/arbitrage) — capture technological advantage, strategic deterrent benefit, and economic value of dual-use AI development; can exit constraint through economic and military diversification
 *   - Defense Industry and Military R&D: Beneficiaries (institutional/arbitrage) — receive research funding, strategic importance, and barriers to civilian oversight; dual-use technology markets insulate from accountability
 *   - Arms Control Coalitions: Organized advocates (organized/constrained) — have real agency through treaty negotiation but face suppression from military interests, verification challenges, and free-rider incentives
 *   - International Humanitarian Law Framework: Institutional actor (institutional/arbitrage) — persists through performative compliance rituals despite functional degradation; maintains legitimacy through traditional legal language while autonomy erodes actual protective capacity
 *   - Emerging AI Governance Movement: Organized agents (organized/mobile) — see autonomous weapons as temporary coordination failure with sunset; believe broader AI safety frameworks will eventually address military autonomy as subset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomous_weapons_international_law, 0.58).
domain_priors:suppression_score(autonomous_weapons_international_law, 0.65).
domain_priors:theater_ratio(autonomous_weapons_international_law, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomous_weapons_international_law, extractiveness, 0.58).
narrative_ontology:constraint_metric(autonomous_weapons_international_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(autonomous_weapons_international_law, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomous_weapons_international_law, tangled_rope).
narrative_ontology:human_readable(autonomous_weapons_international_law, "Autonomous Weapons Systems International Legal Framework").
narrative_ontology:topic_domain(autonomous_weapons_international_law, "international_law/military_technology/arms_control").

domain_priors:requires_active_enforcement(autonomous_weapons_international_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomous_weapons_international_law, military_technology_developers).
narrative_ontology:constraint_beneficiary(autonomous_weapons_international_law, technologically_advanced_nations).
narrative_ontology:constraint_victim(autonomous_weapons_international_law, civilian_protection_norms).
narrative_ontology:constraint_victim(autonomous_weapons_international_law, non_aligned_states).
narrative_ontology:constraint_victim(autonomous_weapons_international_law, international_humanitarian_law_efficacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Civilians in conflict zones cannot exit the constraint. They bear the extraction risk without agency. No alternative protection mechanism exists while warfare autonomy expands. Maximum experienced extraction.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% IHL principles (distinction, proportionality, necessity) cannot exit the constraint of technological obsolescence. As autonomous systems operate at decision speeds below human cognition, the normative framework degrades structurally. The norms are being extracted from (undermined) without meaningful enforcement.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Moderate nations face constrained exit: adoption accelerates their vulnerability, non-adoption leaves them militarily disadvantaged. The constraint coordinates collective security (all nations need some deterrent) while extracting asymmetrically (weaker nations cannot afford the technology or risk losing strategic advantage). Significant agency but high cost to meaningful exit.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Advanced states experience the constraint as coordination with asymmetric benefit. They can develop, deploy, and iterate on autonomous systems faster than peer competitors. The constraint enables them to coordinate deterrence while capturing technological and strategic advantage. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized actors (civil society, humanitarian organizations, some states) seek to coordinate autonomous weapons restrictions. They have real agency through treaty negotiation and advocacy, but face significant suppression: military and tech industry pressure, verification challenges, free-rider incentives, and the coordination problem of ensuring all powers comply. Asymmetric extraction via regulatory capture and verification failure.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Defense contractors and military R&D establishments experience the constraint as pure coordination and innovation incentive. They benefit from: research funding, strategic importance, dual-use technology markets, and barriers to civilian oversight. Exit option is arbitrage (shift to commercial AI, invest in other military domains). Net beneficiary.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% New coalitions around AI governance, lethal autonomous weapons bans, and human-in-the-loop requirements see autonomous weapons as a temporary coordination failure with a sunset: as AI capabilities mature and become economically critical across civilian domains, the military autonomy debate will shift from abstract principle to practical safety standards. The constraint is transitory, not permanent. Relatively low extraction because exit pathways are visible and coalition power is growing.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Classical IHL (Geneva Conventions, Protocol Additional I) is substantially performative in the autonomous weapons context. The framework was designed for human decision-makers with intent, proportionality reasoning, and accountability. When systems operate at machine-decision speeds with probabilistic targeting, the legal machinery persists through institutional inertia—review mechanisms, commander intent language, principle statements—but has degraded functional capacity. Theater ratio (0.68) reflects this: formal legal compliance rituals (documented targeting procedures, command review protocols) substitute for actual human deliberation capability.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, autonomous weapons represent a genuine hybrid: legitimate military coordination problem (all powers need systems capable of operating in contested information environments) layered with extraction mechanism (concentration of capability, asymmetric civilian risk, normative degradation). The constraint both solves a real strategic problem and creates irreversible vulnerabilities. Neither pure coordination nor pure extraction—the analytical classification must capture both.
constraint_indexing:constraint_classification(autonomous_weapons_international_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomous_weapons_international_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autonomous_weapons_international_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomous_weapons_international_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autonomous_weapons_international_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(autonomous_weapons_international_law, TR),
    TR >= 0.70.

:- end_tests(autonomous_weapons_international_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from civilian populations and weaker states through irreversible normative degradation and strategic asymmetry. However, extraction is not total (snare-level 0.66+) because the constraint does solve a genuine coordination problem—all major powers do need some autonomy capability. The value reflects asymmetric capture: advanced states solve their coordination problem while externalizing risks to others. Suppression (0.65): High. Multiple suppression mechanisms: verification difficulty (autonomous systems can be hidden or mischaracterized), military bureaucracy (resistance to civilian oversight), information asymmetry (technical complexity prevents meaningful public participation), free-rider incentives (all states benefit from non-compliance if others comply), and institutional inertia (military doctrine highly resistant to change). Theater ratio (0.68): High, rising. International legal discourse about autonomous weapons employs extensive performative machinery—command oversight procedures, proportionality review language, human-control principles—that become increasingly decoupled from actual system operation. As autonomy levels increase and decision speeds exceed human cognition, the theater expands to maintain legitimacy while functional protection erodes. The measurement trajectory (0.42→0.68) reflects how as systems mature, the gap between legal procedures and operational reality widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates why single-perspective classification fails for hybrid constraints. From the advanced-power institutional perspective, the constraint is pure coordination—solving a genuine strategic problem. From the civilian perspective, it is pure extraction—increasing risk without consent or exit option. From the non-aligned state perspective, it is a dilemma—genuine security coordination mixed with technological dependence. From the arms control perspective, it is a temporary failure—governance problem with a sunset when AI safety standards mature. From the piton perspective, it is degraded ritual—legal machinery persisting through momentum rather than function. The perspectival gap reveals that the constraint's type depends entirely on the observer's structural position. The analytical classification as Tangled Rope is the only one that acknowledges all six perspectives as structurally legitimate readings of the same phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from their structural position. Technologically advanced powers (institutional/arbitrage) have low d because they are beneficiaries with exit options—the constraint enables their security strategy. Non-aligned states (moderate/constrained) have moderate-high d because they lack arbitrage options—they must either adopt (accepting technological dependence) or refuse (accepting strategic vulnerability). Civilians (powerless/trapped) have maximum d—they have no exit options and bear costs without agency. Arms control advocates (organized/constrained) have significant d despite organization because they face structural suppression (verification problems, military pressure, coordination difficulties). The analytical observer (analytical/analytical) has derived d reflecting the observer's position external to the military-strategic system—high enough to see asymmetric extraction, stable enough to recognize genuine coordination benefits. The directionality computation captures why the same constraint appears as Rope to one power and Snare to civilian populations.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival pluralism. The constraint resolves extractiveness > 0.70 mandatrophy by demonstrating that the classification is not ambiguous—all six types are correct from their respective structural positions. The resolution is not 'the constraint is actually Tangled Rope' (though that is the analytical classification), but rather 'the constraint IS all six types simultaneously, depending on which agent's experience you are measuring.' This dissolves the mandatrophy: there is no contradiction between the powerless agent perceiving Snare and the institutional beneficiary perceiving Rope. They are measuring different directionality values (d ≈ 0.95 vs d ≈ 0.10) on the same base ε = 0.58. The chi formula explains the entire perspectival range. For mandatrophy compliance, this constraint must be marked with 'mandatrophy_resolved: true' and the commentary must explicitly document that resolution comes from indexed classification plurality, not from reclassifying the constraint to a higher type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_control_threshold_definition,
    'What threshold of human decision-making constitutes ''meaningful human control'' under international law?',
    'Comparative legal analysis of proposed treaties (CCWC discussions, bilateral agreements); examination of actual military doctrine and command procedures; empirical testing of human override effectiveness in experimental systems',
    'If threshold requires constant human monitoring: most autonomous systems fail compliance, pushing toward global ban. If threshold permits human-in-the-loop only at strategic levels: massive autonomy at tactical levels permitted, extraction mechanism largely unopposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_control_threshold_definition, conceptual, 'Definition of meaningful human control threshold').

omega_variable(
    verification_asymmetry_insurmountability,
    'Can any verification regime detect autonomous weapons development with sufficient confidence to enforce treaty obligations?',
    'Technical analysis of detection capabilities (satellite, signals intelligence, human intelligence penetration); historical analysis of verification failures in analogous domains (nuclear proliferation, cyber weapons); game-theoretic modeling of incentives to cheat',
    'If verification is feasible: treaty obligation becomes binding coordination mechanism (shifts constraint toward Rope). If fundamentally asymmetric: treaty becomes purely performative ritual (shifts constraint toward Piton), and constraint remains extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_insurmountability, empirical, 'Whether autonomous weapons treaty compliance is verifiable').

omega_variable(
    civilian_risk_asymmetry_irreversibility,
    'Once autonomous weapons are deployed at scale, can civilian protection norms be meaningfully restored through international agreement?',
    'Historical analysis of norm recovery after weapon escalation (chemical weapons, landmines, cluster munitions); modeling of irreversibility thresholds; analysis of technological lock-in effects in military doctrine',
    'If restoration is possible: constraint has temporary character, making Scaffold classification appropriate. If irreversible: normative extraction is permanent, confirming Snare and Tangled Rope classifications for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_risk_asymmetry_irreversibility, empirical, 'Whether civilian protection norms can be restored post-deployment').

omega_variable(
    deterrence_paradox_resolution,
    'Does autonomous weapons technology actually increase or decrease overall conflict risk by stabilizing or destabilizing mutual deterrence?',
    'Game-theoretic analysis of first-strike advantage with autonomous systems; comparative analysis of crisis behavior in deterrence scenarios with/without autonomous weapons; modeling of command-and-control vulnerability to cyber attack',
    'If technology stabilizes deterrence: coordination benefit is genuine, supporting Rope/Tangled Rope classification. If technology destabilizes: extraction mechanism is more severe (increased war risk), supporting Snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_paradox_resolution, conceptual, 'Whether autonomous weapons stabilize or destabilize deterrence').

omega_variable(
    commercial_ai_constraint_transfer,
    'As AI capabilities with military applications become economically indispensable in civilian sectors, does the autonomous weapons constraint transfer to commercial AI governance?',
    'Tracking of dual-use AI regulation (export controls, safety standards, content moderation systems); analysis of commercial AI companies'' relationships with military procurement; observation of whether military autonomy norms influence commercial AI ethics frameworks',
    'If constraint transfers: autonomous weapons becomes part of larger AI governance constraint family, enabling Scaffold logic (broader governance addresses military autonomy as subset). If constraint remains compartmentalized: military autonomy persists as extractive constraint distinct from commercial AI.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_ai_constraint_transfer, empirical, 'Transfer of military autonomy constraints to commercial AI governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomous_weapons_international_law, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t0, autonomous_weapons_international_law, theater_ratio, 0, 0.42).
narrative_ontology:measurement(auto_tr_t4, autonomous_weapons_international_law, theater_ratio, 4, 0.55).
narrative_ontology:measurement(auto_tr_t8, autonomous_weapons_international_law, theater_ratio, 8, 0.68).
narrative_ontology:measurement(auto_tr_t12, autonomous_weapons_international_law, theater_ratio, 12, 0.72).

% Extraction over time
narrative_ontology:measurement(auto_be_t0, autonomous_weapons_international_law, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(auto_be_t4, autonomous_weapons_international_law, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(auto_be_t8, autonomous_weapons_international_law, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(auto_be_t12, autonomous_weapons_international_law, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomous_weapons_international_law, enforcement_mechanism).
narrative_ontology:affects_constraint(autonomous_weapons_international_law, ai_arms_race_dynamics).
narrative_ontology:affects_constraint(autonomous_weapons_international_law, lethal_targeting_civilian_harm_asymmetry).
narrative_ontology:affects_constraint(autonomous_weapons_international_law, command_and_control_vulnerability_windows).

% DUAL FORMULATION NOTE:
% Autonomous weapons constraint is upstream of several derivative constraints: arms race dynamics (how autonomy development affects strategic competition), lethal targeting asymmetries (how specific autonomy failures harm civilians), and command-and-control vulnerabilities (how autonomy enables new attack vectors). Each derivative has its own ε value reflecting specific observable domain; the parent constraint represents the general international legal framework governing all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(autonomous_weapons_international_law, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
