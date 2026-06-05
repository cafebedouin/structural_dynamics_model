% ============================================================================
% CONSTRAINT STORY: carrier_deployment_deterrence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The deployment of a US Carrier Strike Group (CSG) into a contested region
 *   functions simultaneously as a coordination mechanism (deterrence, freedom
 *   of navigation assurance, allied security provision) and as an extraction
 *   mechanism (regional hegemony maintenance, suppression of competing naval
 *   powers' autonomy, enforcement of US-preferred regional hierarchy). The
 *   constraint exhibits all six classification types from different
 *   perspectives. From the US military establishment's viewpoint, the
 *   deployment is pure coordination (Rope) — solving the problem of regional
 *   security provision with minimal cost to itself. From a target state's
 *   perspective, it is pure extraction (Snare) — coercive power projection
 *   with no coordination benefit. From regional allied states, it is a hybrid
 *   (Tangled Rope) — genuine security benefit coupled with strategic
 *   dependence. From the perspective of international maritime commerce, it
 *   is temporary coordination being replaced by multilateral alternatives
 *   (Scaffold). From the perspective of Cold War-era strategic doctrine, it
 *   is an institutional ritual maintained by inertia despite degraded
 *   function in the era of hypersonic weapons and asymmetric strategies
 *   (Piton). From competing naval powers, it is a mixed signal (Tangled Rope)
 *   — coordinating mutual deterrence stability while suppressing their
 *   preferred regional hierarchy. The analytical observer risks naturalizing
 *   the carrier system as an immutable constraint on regional power
 *   (Mountain), but the structural data reveals this as a contingent
 *   institutional arrangement. The constraint's theater ratio (0.64) reflects
 *   the performative aspects of deterrence signaling — the carrier's presence
 *   communicates resolve and capability, but the actual deterrent effect
 *   depends on credibility assumptions that may be eroding as near-peer
 *   anti-ship capabilities mature.
 *
 * KEY AGENTS:
 *   - US Military Establishment: Primary beneficiary (institutional/arbitrage) — deploys carrier, captures deterrence effect and regional influence; high exit flexibility
 *   - Target Regional State: Primary victim (powerless/trapped) — faces constrained autonomy and suppressed military options; no exit from deterrent presence
 *   - Regional Allied States: Secondary beneficiary and victim (moderate/constrained) — gain security coordination but lose strategic autonomy; constrained exit due to alliance dependence
 *   - Competing Naval Powers: Secondary victim and agent (organized/constrained) — experience deterrent suppression and regional power ceiling; constrained counter-deterrence options
 *   - International Maritime Commerce: Tertiary beneficiary (organized/mobile) — benefits from enforced free navigation; exit options emerging via multilateral institutions
 *   - Cold War Strategic Doctrine: Institutional momentum actor (institutional/constrained) — maintains deployment ritual through inertia; constrained by budget and planning cycles
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent arrangement as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrier_deployment_deterrence, 0.58).
domain_priors:suppression_score(carrier_deployment_deterrence, 0.68).
domain_priors:theater_ratio(carrier_deployment_deterrence, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrier_deployment_deterrence, extractiveness, 0.58).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carrier_deployment_deterrence, tangled_rope).
narrative_ontology:human_readable(carrier_deployment_deterrence, "US Carrier Strike Group Deployment as Regional Deterrent").
narrative_ontology:topic_domain(carrier_deployment_deterrence, "geopolitical/military").

domain_priors:requires_active_enforcement(carrier_deployment_deterrence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, us_military_establishment).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, regional_allied_states).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, global_maritime_commerce).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, target_state_autonomy).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, regional_power_balance).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, competing_naval_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET REGIONAL STATE (SNARE) — Faced with a carrier strike group deployed to constrain its military options, this state experiences maximum extraction with minimal coordination benefit. Its own strategic autonomy is suppressed; alternative responses (military escalation, counter-deterrence) are all costly. No exit option — the carrier's presence imposes constraints regardless of state consent. Extraction is experienced as coercive power projection.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL ALLIED STATES (TANGLED ROPE) — Benefit from the deterrent effect (security coordination) but are also constrained by the carrier's presence — their own strategic options are partially dictated by the US naval posture. They cannot freely pursue independent regional policies. Mixed experience: genuine security benefit coupled with asymmetric dependence. Constrained exit because abandoning the alliance carries significant costs.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY ESTABLISHMENT (ROPE) — Deploys the carrier as a coordination mechanism for allied deterrence and free navigation assurance. Primary beneficiary with arbitrage options — can redeploy, can choose which threats to deter, can extract geopolitical leverage without significant constraint on its own options. Experiences the deployment as solving a coordination problem (regional security provision) with minimal cost to itself.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL MARITIME COMMERCE COALITION (SCAFFOLD) — The carrier deployment enforces free navigation and passage through contested straits (Strait of Malacca, South China Sea, Strait of Hormuz). This is temporary coordination with a sunset: as regional security institutions mature (ASEAN protocols, multilateral dispute resolution), distributed enforcement replaces unilateral carrier presence. Low effective extraction because the coordination function is being transferred to alternative mechanisms. Theater is moderate — the carrier performs both deterrence and genuine traffic protection.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR DETERRENCE DOCTRINE (PITON) — The carrier deployment strategy is substantially inertial, maintained from Cold War territorial competition logic even where regional threat environments have shifted. The deployment ritual (forward presence, freedom of navigation operations, allied reassurance messaging) persists through institutional momentum — Navy fleet sizing, budget allocation, strategic planning all assume carrier forward presence. The functional deterrence value has degraded as near-peer competitors possess hypersonic anti-ship weapons and asymmetric strategies that reduce carrier invulnerability. Theater ratio reflects performative reassurance (allies see presence; threat is credible but containable) masking reduced functional deterrent effect. Maintained by inertia, not because it works as designed.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETING NAVAL POWERS (TANGLED ROPE) — Experience the carrier deployment as both a coordinating signal (establishing mutual strategic awareness and deterrence stability through visible presence) and an extractive constraint on their own regional influence. The carrier's presence enforces a regional power ceiling — prevents unilateral military dominance but also forces them to bear costs of counter-deterrence (their own naval buildout, asymmetric strategies). Mixed extraction: genuine strategic communication coupled with suppression of their preferred regional hierarchy.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From a civilizational view, global maritime commerce requires enforcement against piracy and regional disruption. The carrier is one solution among many (multilateral coast guard cooperation, private security, international law enforcement). The observer risks naturalizing the US carrier system as inevitable, necessary, and immutable — but the structural data contradicts this: the deployment is contingent on specific institutional arrangements (US naval industrial base, alliance politics, strategic doctrine), not on laws of physics or irreducible logical limits. This is a false summit — carrier deterrence is coordination + extraction, not natural law.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrier_deployment_deterrence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The carrier deployment extracts substantial benefits for the US military establishment (geopolitical leverage, force projection capability, alliance dependence maintenance) while imposing costs on target states (autonomy suppression), competing powers (deterrent ceiling), and allied states (strategic dependence). However, extraction is not maximal (not 0.70+) because the deployment genuinely provides coordination functions — deterrence prevents more costly outcomes (regional conflict escalation), and free navigation enforcement is a real public good. The tension between coordination and extraction is what makes this Tangled Rope rather than pure Snare. Suppression (0.68): Moderate-high. Target states face significant constraints on military options (cannot escalate without facing carrier-backed response); competing powers face constraints on regional influence expansion; allied states face constraints on independent policy formulation. However, suppression is not total — alternative strategies exist (asymmetric warfare, diplomatic negotiation, building counter-deterrent capabilities), though all carry costs. Theater ratio (0.64): Moderate-high. The deterrent effect depends substantially on signaling and credibility — much of the carrier's impact comes from visible presence, photogenic operations, and repeated deployments that communicate resolve. The actual military capability against near-peer adversaries with hypersonic weapons is contested and possibly degraded. The theater has increased over the interval (42 → 64) as deployment doctrine has shifted toward more frequent rotations and public visibility operations, partly compensating for reduced operational effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here — observers see radically different classifications based on their structural position relative to the carrier. The beneficiary (US military) sees rope — a coordination mechanism with benefits and no extraction. The victim (target state) sees snare — pure extraction. The mixed agent (regional allies) sees tangled rope — both coordination and extraction. The beneficiary of secondary functions (maritime commerce) sees scaffold — temporary coordination being replaced. The institutional actor maintaining inertial deployment sees piton — ritual without function. The competing power sees tangled rope — deterrent suppression coupled with mutual strategic awareness. The civilizational observer risks mountain — naturalizing a contingent arrangement as immutable structural necessity. This gap demonstrates why indexical classification is essential: the 'true' nature of the carrier deployment is not a single type but a presheaf of types across the observation sites, each capturing something real about the constraint from that perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position: beneficiary vs victim status, power level, and exit options. The US military establishment has low d (high beneficiary, arbitrage exit) → low f(d) → extraction runs toward them. Target states have high d (victim, trapped exit) → high f(d) → maximum experienced extraction. Regional allies have moderate d (mixed beneficiary/victim, constrained exit) → moderate f(d) → mixed experience. Competing powers have moderate-high d (victim status, constrained exit for counter-deterrence) → moderate-high f(d) → significant extraction. Maritime commerce has low d (beneficiary, mobile exit) → low f(d) → low experienced extraction. Cold War doctrine has moderate d (institutional, constrained exit from budget/planning cycles) → moderate f(d) → moderate theater-driven cost. Analytical observer has high d (analytical exit, observing all perspectives) → high f(d) → sees full extraction spectrum. The directionality derivation confirms tangled_rope classification: coordination function (deterrence, free navigation) is real, but asymmetric extraction (beneficiaries insulated by arbitrage/mobility, victims trapped by geography/power asymmetry) is also real.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification resolves the mandatrophy by demonstrating that the constraint has BOTH genuine coordination function (deterrence prevents escalation, free navigation assurance is real public good) AND asymmetric extraction (benefits flow disproportionately to the US military, costs are borne by target states and competing powers, regional allies lose autonomy). The gate requirements are met: (1) beneficiaries declared (US military, regional allies, maritime commerce), (2) victims declared (target state autonomy, competing power influence, allied strategic autonomy), (3) active enforcement required (yes — carrier deployment requires continuous operational commitment, rules of engagement, threat assessment). The false summit (mountain classification from analytical perspective) is correctly identified: the deployment is not an immutable law but a contingent institutional arrangement dependent on specific strategic doctrines, budgetary choices, and alliance politics. If alternative regional institutions mature, or if carrier vulnerability to hypersonic weapons is confirmed, the constraint's function degrades — it becomes piton rather than mountain. The mandatrophy is resolved by showing that carrier deterrence is *contingently* necessary for current regional stability, not *necessarily* immutable for all possible regional orderings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypersonic_weapon_vulnerability,
    'Do hypersonic anti-ship weapons fundamentally eliminate carrier invulnerability, converting the deterrent from credible to performative?',
    'Technical assessment of hypersonic missile interception rates; wargaming outcomes with near-peer carrier engagement; vulnerability modeling against distributed swarms',
    'If vulnerable: carrier deterrent shifts from mountain/rope (works) to piton (inertial ritual). Extraction mechanism weakens because threatened states see carrier presence as theater rather than real suppression. If defended: mountain classification gains support — carrier remains immutable constraint on regional power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypersonic_weapon_vulnerability, empirical, 'Whether hypersonic weapons render carriers operationally vulnerable').

omega_variable(
    regional_institution_adequacy,
    'Can ASEAN, QUAD, or multilateral maritime protocols provide equivalent security coordination without unilateral US carrier presence?',
    'Comparative analysis of dispute resolution success rates pre/post carrier deployment; regional maritime incident escalation patterns under different enforcement regimes; state security perception surveys',
    'If adequate: scaffold perspective confirmed — carrier deployment has a real sunset as multilateral alternatives mature. If inadequate: deterrent function remains structurally necessary, and scaffold is aspirational rather than structural. Affects classification: adequate alternatives → stronger scaffold gate; inadequate → tangled_rope becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_institution_adequacy, empirical, 'Whether regional institutions can substitute for carrier deterrence').

omega_variable(
    extraction_mechanism_clarity,
    'Is the carrier deployment primarily a coordination mechanism (deterrence + free navigation) or primarily an extraction mechanism (regional hegemony maintenance + cost imposition on competitors)?',
    'Historical analysis of carrier deployment decisions: correlation with genuine regional threats vs geopolitical opportunity; allied state benefit analysis (security gain vs dependency cost); cost-benefit accounting for US burden vs allied benefit distribution',
    'If primarily coordination: rope or scaffold classification dominant. If primarily extraction: snare and tangled_rope dominant. Affects tangled_rope gate closure: requires demonstrating BOTH coordination function (yes) AND asymmetric extraction (yes); if only one present, misclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_clarity, conceptual, 'Whether carrier deployment is coordination or extraction mechanism').

omega_variable(
    allied_autonomy_constraint,
    'Does the carrier''s presence constrain regional allied states'' strategic autonomy more than it enables their security?',
    'Analysis of allied state military spending trends, regional policy initiatives, and stated strategic preferences; comparison of allied autonomy pre/post deployment; survey of allied strategic perception',
    'If constraints exceed enablement: allied perspective shifts from tangled_rope toward snare; extraction mechanism revealed as dominant. If benefits exceed constraints: rope perspective strengthened; coordination dominates. Affects directionality derivation: constrained exit + victim status → higher d → higher chi for allies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_autonomy_constraint, empirical, 'Whether carrier presence constrains allied strategic autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carrier_deployment_deterrence, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carrier_tr_t0, carrier_deployment_deterrence, theater_ratio, 0, 0.42).
narrative_ontology:measurement(carrier_tr_t20, carrier_deployment_deterrence, theater_ratio, 20, 0.55).
narrative_ontology:measurement(carrier_tr_t40, carrier_deployment_deterrence, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(carrier_be_t0, carrier_deployment_deterrence, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(carrier_be_t20, carrier_deployment_deterrence, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(carrier_be_t40, carrier_deployment_deterrence, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carrier_deployment_deterrence, enforcement_mechanism).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, south_china_sea_freedom_of_navigation).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, strait_of_hormuz_passage_security).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, regional_military_balance_stability).
narrative_ontology:affects_constraint(carrier_deployment_deterrence, hypersonic_weapon_development_race).

% DUAL FORMULATION NOTE:
% Carrier deployment as deterrent is upstream of specific regional disputes (South China Sea, Strait of Hormuz) and downstream of broader US military strategy and alliance architecture. The coordination function (free navigation, deterrence) is decomposed from the extraction function (regional hegemony maintenance, power suppression). Stories can be written separately for each functional aspect, with the deterrent story linking both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carrier_deployment_deterrence, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
