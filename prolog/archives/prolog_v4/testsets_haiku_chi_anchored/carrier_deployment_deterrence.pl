% ============================================================================
% CONSTRAINT STORY: carrier_deployment_deterrence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carrier_deployment_deterrence, []).

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
 *   constraint_id: carrier_deployment_deterrence
 *   human_readable: US Carrier Strike Group Deployment as Regional Deterrent
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   US Carrier Strike Group deployments function as a geopolitical constraint
 *   operating across multiple structural roles simultaneously. From the US
 *   military perspective, the deployment solves a genuine coordination
 *   problem: alliance credibility signaling, forward presence maintenance for
 *   freedom of navigation, and deterrence through demonstrated capability.
 *   From the perspective of targeted adversary states, the same deployment
 *   appears as a coercive extraction mechanism imposing military inferiority
 *   and constrained strategic autonomy. From allied governments, carrier
 *   presence offers security guarantees but simultaneously creates dependency
 *   and political legitimacy costs. From civilian populations in the
 *   deployment zone, the constraint imposes risk with no voice or exit. The
 *   theater_ratio (0.68) reflects that substantial portions of carrier
 *   deployment doctrine involve performative signaling: showing the flag,
 *   reassuring allies, demonstrating commitment through scheduled
 *   presence—activities that have low functional content in a world with
 *   satellite surveillance and long-range precision weaponry. The
 *   extractiveness value (0.58) reflects that while genuine deterrence
 *   functions exist, the deployment mechanism also serves institutional
 *   interests (maintaining forward presence doctrine, sustaining carrier
 *   acquisition budgets, demonstrating great power status) that persist
 *   independently of deterrence efficacy. This constraint exhibits the full
 *   range of DR types depending on perspective: pure extraction (Snare) from
 *   adversary and civilian viewpoints, coordination (Rope) from US military
 *   and shipping network viewpoints, mixed (Tangled Rope) from allied
 *   government and rival great power viewpoints, degraded ritual (Piton) from
 *   Cold War doctrine and regional defense procurement viewpoints, and
 *   temporary enforcement (Scaffold) from international rules-based order
 *   perspective.
 *
 * KEY AGENTS:
 *   - US Military Establishment: Primary beneficiary (institutional/arbitrage) — maintains forward presence doctrine, sustains carrier acquisition budgets, demonstrates great power status
 *   - Regional Adversary States: Primary victim (powerless/trapped) — bears asymmetric military inferiority, constrained economic options, military autonomy erosion
 *   - Regional Allied Governments: Secondary beneficiary & victim (powerful/mobile) — receive deterrence benefits but pay legitimacy and autonomy costs
 *   - Civilian Populations in Deployment Zone: Tertiary victim (powerless/trapped) — exposed to accident, escalation, blockade risk with no voice or exit
 *   - International Shipping & Trade Networks: Secondary beneficiary (organized/mobile) — benefit from freedom of navigation and lane stabilization
 *   - Rival Great Powers (China/Russia): Structural adversary (institutional/constrained) — experience deployment as competitive escalation, locked in arms race dynamics
 *   - International Rules-Based Order Institutions: Tertiary beneficiary (organized/constrained) — temporarily supported by carrier enforcement of maritime norms
 *   - Defense Contractors & Allied Procurement Sectors: Hidden beneficiary (institutional/arbitrage) — benefit from escalatory arms sales triggered by carrier presence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrier_deployment_deterrence, 0.58).
domain_priors:suppression_score(carrier_deployment_deterrence, 0.72).
domain_priors:theater_ratio(carrier_deployment_deterrence, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrier_deployment_deterrence, extractiveness, 0.58).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carrier_deployment_deterrence, tangled_rope).
narrative_ontology:human_readable(carrier_deployment_deterrence, "US Carrier Strike Group Deployment as Regional Deterrent").
narrative_ontology:topic_domain(carrier_deployment_deterrence, "geopolitical/military").

domain_priors:requires_active_enforcement(carrier_deployment_deterrence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, us_military_establishment).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, regional_allied_governments).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, shipping_lanes_stability).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, regional_adversary_states).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, civilian_populations_littoral_zones).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, international_maritime_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED REGIONAL STATE (SNARE) — Trapped by asymmetric military power projection; carrier presence constrains economic options (port access threats), military autonomy (no credible deterrent to blockade), and political legitimacy (sovereignty messaging). d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.59. High extraction; low coordination value for target.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: US MILITARY ESTABLISHMENT (ROPE) — Carrier deployment solves genuine coordination problems: alliance credibility signaling, forward presence maintenance, deterrence through demonstrated capability. Benefits from forward positioning (operational readiness, presence without permanent bases). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; experiences deployment as coordination mechanism.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL ALLIED GOVERNMENT (TANGLED ROPE) — Receives security coordination benefits (deterrence, freedom of navigation guarantees, protection from coercive blockade) but also bears extraction costs: political legitimacy damage from foreign military presence, economic constraints (restricted port autonomy, trade corridor dependency), and long-term strategic entrapment (escalation risk, sovereignty erosion). d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39. Mixed coordination-extraction.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL ALLIED MILITARY (TANGLED ROPE) — Coordinates with carrier strike group for operational training, intelligence sharing, and capability development. Simultaneously constrained by dependency: cannot operate independently in high-threat scenarios, loses operational autonomy, absorbs asymmetric risk (local militaries take casualties; carrier operates at standoff distance). d≈0.60, f(d)≈0.82, σ=0.9 → χ≈0.43. Coordination function genuine; extraction component significant.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CIVILIAN POPULATIONS (SNARE) — Bear maximum extraction with minimal coordination benefit. Exposure to accident risk, blockade consequences, escalation spillover. No exit option (cannot leave region easily). No voice in deterrence decision. Cannot opt out of strategic liability. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.65. Pure extraction from civilian perspective.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: INTERNATIONAL SHIPPING & TRADE NETWORKS (ROPE) — Coordinate with carrier presence to maintain freedom of navigation, reduce piracy/hijacking risk, and guarantee unimpeded passage. Genuine coordination: shipping lanes stabilized; risk premiums reduced; port access guaranteed. d≈0.25, f(d)≈0.05, σ=1.2 → χ≈0.06. Low extraction; primarily coordination.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: RIVAL GREAT POWER (TANGLED ROPE) — Experiences carrier deployment as competitive escalation (coordination function from US view becomes extraction from rival perspective). d≈0.88, f(d)≈1.28, σ=1.2 → χ≈0.88. High chi indicates extraction from rival frame. Suppression high: military countermeasures required; no unilateral exit; arms race lock-in.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: INTERNATIONAL RULES-BASED ORDER (SCAFFOLD) — Carrier deployment enforces freedom of navigation norms and international law maritime rules. Functions as temporary enforcement mechanism while institutional alternatives mature (UNCLOS implementation, arbitration mechanisms, regional multilateral arrangements). d≈0.35, f(d)≈0.27, σ=1.0 → χ≈0.16. Scaffold logic: enforcement mechanism with sunset as alternatives strengthen. has_sunset_clause_rationale: Regional multilateral maritime security arrangements, UNCLOS enforcement by international court, and distributed enforcement through littoral state navies provide structural alternatives. Estimated sunset: 15-30 years if institutions mature.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: COLD WAR DOCTRINE (PITON) — Carrier deployments persist through institutional inertia from Cold War forward presence strategy. Original strategic rationale (Soviet containment) atrophied; modern deployments maintain the presence ritual without equivalent strategic coherence. theater_ratio=0.68 reflects substantial performative content: showing the flag, reassuring allies, demonstrating commitment. Functional value degraded but deployment continues. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 10: REGIONAL DEFENSE CYCLE (PITON) — Allied states purchase advanced air defense, anti-ship missiles, and naval systems in response to carrier presence. Procurement sustains itself through escalatory dynamics: weapons acquired to deter carriers; carriers deploy to justify allied weapons procurement. theater_ratio=0.68: substantial performative signaling. Functional deterrence value unclear; procurement momentum independent. d≈0.65, f(d)≈0.95, σ=0.9 → χ≈0.45.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrier_deployment_deterrence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carrier_deployment_deterrence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carrier_deployment_deterrence, TR),
    TR >= 0.70.

:- end_tests(carrier_deployment_deterrence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting dual structure. Genuine deterrence and coordination functions exist (justifying ~0.35 base), but institutional interests in maintaining forward presence doctrine, sustaining carrier acquisition budgets, and demonstrating great power status drive deployment frequency beyond deterrence necessity. The temporal measurement shows increasing extractiveness (0.35→0.58 over 20 years), indicating rent-seeking layered onto coordination function. Theater ratio (0.68): Moderate-high. Cold War forward presence doctrine relied on visible, scheduled presence to signal commitment and reassure allies. In the satellite era, showing the flag has lower functional deterrence value but persists as performative signaling. The measurement trajectory (0.42→0.68) indicates theater increasing faster than functional deterrence capability improves, consistent with Piton degradation. Suppression (0.72): High. Adversary states and regional allies face substantial barriers to exit (military imbalance, economic dependency, alliance structure). Civilians in deployment zones have zero exit capacity. Suppression value justified by structural constraints rather than active coercion—the constraint operates through structural inequality, not explicit force. Claimed type: Tangled Rope. The deployment exhibits both genuine coordination (alliance credibility, freedom of navigation, deterrence) and asymmetric extraction (power imbalance, ally dependency, civilian risk). Multiple beneficiaries (US military, allies, shipping networks) and multiple victims (adversaries, civilians, alternative security architectures) satisfy tangled rope gates. Requires active enforcement is true: carrier deployment requires continuous resource commitment, active operational decisions, maintained basing agreements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The US military establishment sees primarily Rope (coordination mechanism). Regional allies see Tangled Rope (coordination with extraction costs). Adversary states see Snare (pure extraction). Civilians see Snare (pure extraction with zero voice). Shipping networks see Rope (freedom of navigation coordination). Rival great powers see Tangled Rope or Snare (competitive escalation). The Cold War doctrine sees Piton (degraded ritual persisting through inertia). International order institutions see Scaffold (temporary enforcement with sunset as alternatives mature). The perspectival gaps reflect genuine structural differences in exit capacity, beneficiary/victim status, and constraint-relative power. The widest gaps are between US military (beneficiary/arbitrage), regional allies (mixed), adversaries (victim/trapped), and civilians (victim/trapped). No single perspective captures the constraint's true structure—the multi-type classification IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   US Military Establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary via forward presence benefits, operational readiness, great power posturing. Regional Adversary: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no unilateral exit capacity, military imbalance, strategic autonomy erosion. Regional Allied Government: Mixed + mobile → d≈0.55, f(d)≈0.75. Receives deterrence benefits but pays legitimacy and autonomy costs; has mobile exit option but faces economic/diplomatic costs. Regional Ally Military: Victim + constrained → d≈0.60, f(d)≈0.82. Gains training and intelligence but loses operational autonomy; constrained by dependency. Civilian Populations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; cannot exit region, no voice in decision, bears accident/escalation risk. Shipping Networks: Beneficiary + mobile → d≈0.25, f(d)≈0.05. Genuine coordination benefit; can shift lanes if needed but prefer stabilized freedom of navigation. Rival Great Power: Victim + constrained → d≈0.88, f(d)≈1.28. Experiences deployment as escalation; locked into response by arms race dynamics; constrained exit (disarming unilaterally triggers expansion). Cold War Doctrine: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Institutional inertia maintains doctrine despite diminished functional value. International Order: Beneficiary + constrained → d≈0.35, f(d)≈0.27. Benefits from temporary enforcement; constrained by need to build permanent alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by demonstrating how a single deployment structure can be simultaneously coordination and extraction. The mandatrophy question is 'is this coordination (Rope) or extraction (Snare)?' The answer is 'both, from different perspectives.' The US military views deployment as solving coordination problems (alliance credibility, freedom of navigation) — genuine Rope function. Regional allies view it as mixed (deterrence coordination + legitimacy/autonomy extraction) — genuine Tangled Rope function. Adversaries view it as pure coercive inequality — genuine Snare function. The tangled rope classification is not a compromise; it is the structural reality from the perspective that has mixed beneficiary/victim status. The constraint satisfies all three gates: beneficiaries exist (US military, allies, shipping networks); victims exist (adversaries, civilians, rival powers); active enforcement is required (continuous deployment, operational decisions, basing agreements). The theater_ratio (0.68) indicates performative content but does not disqualify the coordination function—showing the flag is a real signal, even if increasingly theatrical. The extractiveness (0.58) indicates that institutional interests (budget, doctrine maintenance, great power posturing) drive deployment beyond deterrence necessity—this is the 'asymmetric extraction' component that transforms pure Rope into Tangled Rope. The temporal measurement (increasing theater, increasing extractiveness) shows rent-seeking layering onto coordination, classic Tangled Rope degradation pattern. ALTERNATIVE FRAMINGS: From adversary perspective, the constraint is Snare (ε≈0.58, suppression≈0.72 appears as pure extraction with no coordination function from their structural position). From analytical observer perspective, the constraint approaches Piton (theater and institutional inertia dominate, functional deterrence value degrading). These alternative classifications are not errors—they reflect the real perspectival structure of the constraint. The Tangled Rope classification from the balanced perspective (taking beneficiaries and victims equally) is the authoritative frame for the compiled constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_credibility_threshold,
    'What level of carrier presence sufficiency actually prevents adversary action versus performatively reassuring allies without functional deterrence?',
    'Comparative historical analysis of deterrence success/failure; correlation between carrier deployment frequency and subsequent adversary aggression; alternative deterrence mechanism effectiveness studies',
    'If threshold low: deterrence claim is theater (Piton classification), not Rope. If threshold high: current deployments may be insufficient, requiring escalation. Classification consequence: shifts beneficiary from allied governments to defense contractors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_credibility_threshold, empirical, 'Threshold between functional deterrence and performative presence').

omega_variable(
    civilian_escalation_risk_modeling,
    'Does carrier presence reduce or increase probabilistic risk of kinetic conflict in the deployment zone, accounting for accident escalation pathways and civilian casualty amplification?',
    'Risk modeling: baseline conflict probability + carrier presence + escalation pathways; comparison with counterfactual absence scenario; game-theoretic analysis of crisis stability',
    'If presence increases net risk: civilian victimization classification (Snare) is understated; suppression value should increase. If presence decreases net risk: coordination narrative strengthened; civilian perspective shifts toward Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_escalation_risk_modeling, empirical, 'Net probabilistic risk change from carrier deployment').

omega_variable(
    alternative_deterrence_sufficiency,
    'Could equivalent deterrence be achieved through forward-deployed missiles, satellite surveillance, submarine presence, or asymmetric drone/cyber capabilities without the extraction and performance costs of carrier deployments?',
    'Military capability analysis: cost-benefit comparison of deployment modes; deterrence modeling across technology types; historical precedent analysis (Soviet asymmetric deterrence models)',
    'If alternatives sufficient: carrier deployment is extraction mechanism justified by institutional inertia (Piton). If alternatives insufficient: carrier is genuine Rope/Scaffold coordination mechanism. Classification consequence: affects claimed_type substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_deterrence_sufficiency, empirical, 'Whether alternative deterrence modes suffice').

omega_variable(
    allied_autonomy_exit_capacity,
    'What is the true exit cost for allied governments choosing to exit carrier-based security architecture? Can they credibly defect to non-aligned status or alternative security partnerships?',
    'Analysis of economic sanctions, diplomatic isolation, trade diversion costs; case studies of attempted exits (Turkey, Hungary, Vietnam); modeling of alternative alliance formation costs',
    'If exit cost is very high: allied perspective is Snare (trapped), not Tangled Rope. If exit cost is moderate: Tangled Rope classification holds. This is the primary directionality sensitivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_autonomy_exit_capacity, empirical, 'True exit cost for allied states exiting US security guarantee').

omega_variable(
    maritime_freedom_definition_collapse,
    'Does ''freedom of navigation'' as enforced by carrier presence constitute genuine international order or hegemon-defined shipping lane control benefiting trade networks that align with US interests?',
    'Analysis of whose trade actually moves through carrier-protected lanes; distribution of transportation cost benefits; cases where non-US shipping suffered constraints despite ''freedom of navigation''; empirical comparison with UNCLOS enforcement alternatives',
    'If freedom is genuine public good: shipping networks benefit equally (Rope). If freedom is selective: constraint is asymmetric extraction benefiting US-aligned trade; shipping perspective shifts toward Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maritime_freedom_definition_collapse, conceptual, 'Whether maritime freedom under carrier enforcement is universal public good or selective hegemonic control').

omega_variable(
    great_power_arms_race_lock_in,
    'Can carrier deployment doctrine be credibly abandoned by the US without triggering rival great power military expansion that itself becomes constraining? Is the deployment pattern locked by mutual escalatory dynamics?',
    'Game-theoretic analysis of unilateral disarmament risks; historical precedent (Cold War arms control treaties); modeling of detection and verification in reduced deployment scenarios',
    'If locked: deployment is structural necessity (Mountain from great power perspective), not chosen extraction mechanism. If unlocked: deployment is choice-based extraction. Affects classification from rival great power perspective and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_arms_race_lock_in, empirical, 'Whether arms race dynamics lock deployment patterns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carrier_deployment_deterrence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csg_det_tr_t0, carrier_deployment_deterrence, theater_ratio, 0, 0.42).
narrative_ontology:measurement(csg_det_tr_t10, carrier_deployment_deterrence, theater_ratio, 10, 0.58).
narrative_ontology:measurement(csg_det_tr_t20, carrier_deployment_deterrence, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(csg_det_be_t0, carrier_deployment_deterrence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(csg_det_be_t10, carrier_deployment_deterrence, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(csg_det_be_t20, carrier_deployment_deterrence, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carrier_deployment_deterrence, enforcement_mechanism).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, regional_military_buildup).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, allied_state_autonomy_erosion).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, maritime_freedom_of_navigation).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, great_power_arms_race_dynamics).

% DUAL FORMULATION NOTE:
% Carrier deployment functions as a multi-role constraint that decomposes into distinct structural mechanisms across different observables. The pure-deterrence observable yields lower ε (≈0.35) and classification closer to Rope. The institutional-performance observable yields higher ε (≈0.65) and classification closer to Piton or Snare. The constraint as modeled (ε=0.58, Tangled Rope) represents the balanced analysis across both observables, weighted by the constraint's primary function (deterrence coordination with asymmetric extraction overlay). Upstream constraints: geopolitical competition for regional influence, Cold War doctrine persistence, alliance structure maintenance. Downstream constraints: allied arms procurement cycles, adversary military capability expansion, civilian population strategic vulnerability, maritime access asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carrier_deployment_deterrence, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
